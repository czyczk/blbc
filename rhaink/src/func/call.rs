//! Implement function-calling mechanism for [`Engine`].

use crate::{
    api::default_limits::MAX_DYNAMIC_PARAMETERS,
    eval::{
        cache::{Caches, FnResolutionCacheEntry},
        global_state::GlobalRuntimeState,
    },
    module::Module,
    types::{dynamic::Dynamic, immutable_string::ImmutableString},
    Engine, FnArgsVec,
};

use super::{callable_function::CallableFunction, hashing::combine_hashes};
use super::{hashing::calc_fn_params_hash, native::FnAny};
use core::{
    any::{type_name, TypeId},
    convert::TryFrom,
    mem,
};

/// Arguments to a function call, which is a list of [`&mut Dynamic`][Dynamic].
pub type FnCallArgs<'a> = [&'a mut Dynamic];

/// A type that temporarily stores a mutable reference to a `Dynamic`,
/// replacing it with a cloned copy.
#[derive(Debug)]
struct ArgBackup<'a> {
    orig_mut: Option<&'a mut Dynamic>,
    value_copy: Dynamic,
}

impl<'a> ArgBackup<'a> {
    /// Create a new `ArgBackup`.
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            orig_mut: None,
            value_copy: Dynamic::UNIT,
        }
    }
    /// This function replaces the first argument of a method call with a clone copy.
    /// This is to prevent a pure function unintentionally consuming the first argument.
    ///
    /// `restore_first_arg` must be called before the end of the scope to prevent the shorter
    /// lifetime from leaking.
    ///
    /// # Safety
    ///
    /// This method blindly casts a reference to another lifetime, which saves allocation and
    /// string cloning.
    ///
    /// As long as `restore_first_arg` is called before the end of the scope, the shorter lifetime
    /// will not leak.
    ///
    /// # Panics
    ///
    /// Panics when `args` is empty.
    #[inline(always)]
    pub fn change_first_arg_to_copy(&mut self, args: &mut FnCallArgs<'a>) {
        // Clone the original value.
        self.value_copy = args[0].clone();

        // Replace the first reference with a reference to the clone, force-casting the lifetime.
        // Must remember to restore it later with `restore_first_arg`.
        //
        // # Safety
        //
        // Blindly casting a reference to another lifetime saves allocation and string cloning,
        // but must be used with the utmost care.
        //
        // We can do this here because, before the end of this scope, we'd restore the original
        // reference via `restore_first_arg`. Therefore this shorter lifetime does not leak.
        self.orig_mut = Some(mem::replace(&mut args[0], unsafe {
            mem::transmute(&mut self.value_copy)
        }));
    }
    /// This function restores the first argument that was replaced by `change_first_arg_to_copy`.
    ///
    /// # Safety
    ///
    /// If `change_first_arg_to_copy` has been called, this function **MUST** be called _BEFORE_
    /// exiting the current scope.  Otherwise it is undefined behavior as the shorter lifetime will leak.
    #[inline(always)]
    pub fn restore_first_arg(mut self, args: &mut FnCallArgs<'a>) {
        if let Some(p) = self.orig_mut.take() {
            args[0] = p;
        }
    }
}

impl Drop for ArgBackup<'_> {
    #[inline(always)]
    fn drop(&mut self) {
        // Panic if the shorter lifetime leaks.
        assert!(
            self.orig_mut.is_none(),
            "ArgBackup::restore_first_arg has not been called prior to existing this scope"
        );
    }
}

#[cfg(not(feature = "no_closure"))]
#[inline]
pub fn ensure_no_data_race(
    fn_name: &str,
    args: &FnCallArgs,
    is_method_call: bool,
) -> RhaiResultOf<()> {
    if let Some((n, ..)) = args
        .iter()
        .enumerate()
        .skip(if is_method_call { 1 } else { 0 })
        .find(|(.., a)| a.is_locked())
    {
        return Err(ERR::ErrorDataRace(
            format!("argument #{} of function '{}'", n + 1, fn_name),
            Position::NONE,
        )
        .into());
    }

    Ok(())
}

impl Engine {
    /// Generate the signature for a function call.
    #[inline]
    #[must_use]
    fn gen_call_signature(
        &self,
        #[cfg(not(feature = "no_module"))] namespace: &crate::ast::Namespace,
        fn_name: &str,
        args: &[&mut Dynamic],
    ) -> String {
        #[cfg(not(feature = "no_module"))]
        let (ns, sep) = (
            namespace.to_string(),
            if !namespace.is_empty() {
                crate::tokenizer::Token::DoubleColon.literal_syntax()
            } else {
                ""
            },
        );
        #[cfg(feature = "no_module")]
        let (ns, sep) = ("", "");

        format!(
            "{}{}{} ({})",
            ns,
            sep,
            fn_name,
            args.iter()
                .map(|a| if a.is::<ImmutableString>() {
                    "&str | ImmutableString | String"
                } else {
                    self.map_type_name(a.type_name())
                })
                .collect::<FnArgsVec<_>>()
                .join(", ")
        )
    }

    /// Resolve a function call.
    ///
    /// Search order:
    /// 1) AST - script functions in the AST
    /// 2) Global namespace - functions registered via Engine::register_XXX
    /// 3) Global registered modules - packages
    /// 4) Imported modules - functions marked with global namespace
    /// 5) Static registered modules
    #[must_use]
    fn resolve_fn<'s>(
        &self,
        _global: &GlobalRuntimeState,
        state: &'s mut Caches,
        lib: &[&Module],
        fn_name: &str,
        hash_script: u64,
        args: Option<&mut FnCallArgs>,
        allow_dynamic: bool,
        is_op_assignment: bool,
    ) -> Option<&'s FnResolutionCacheEntry> {
        if hash_script == 0 {
            return None;
        }

        let mut hash = args.as_ref().map_or(hash_script, |args| {
            combine_hashes(
                hash_script,
                calc_fn_params_hash(args.iter().map(|a| a.type_id())),
            )
        });

        let result = state
            .fn_resolution_cache_mut()
            .entry(hash)
            .or_insert_with(|| {
                let num_args = args.as_ref().map_or(0, |a| a.len());
                let max_bitmask = if !allow_dynamic {
                    0
                } else {
                    1usize << usize::min(num_args, MAX_DYNAMIC_PARAMETERS)
                };
                let mut bitmask = 1usize; // Bitmask of which parameter to replace with `Dynamic`

                loop {
                    let func = lib
                        .iter()
                        .find_map(|&m| {
                            m.get_fn(hash).cloned().map(|func| FnResolutionCacheEntry {
                                func,
                                source: m.id_raw().clone(),
                            })
                        })
                        .or_else(|| {
                            self.global_modules.iter().find_map(|m| {
                                m.get_fn(hash).cloned().map(|func| FnResolutionCacheEntry {
                                    func,
                                    source: m.id_raw().clone(),
                                })
                            })
                        });

                    #[cfg(not(feature = "no_module"))]
                    let func = func
                        .or_else(|| {
                            _global.get_qualified_fn(hash).map(|(func, source)| {
                                FnResolutionCacheEntry {
                                    func: func.clone(),
                                    source: source
                                        .map_or_else(|| Identifier::new_const(), Into::into),
                                }
                            })
                        })
                        .or_else(|| {
                            self.global_sub_modules.values().find_map(|m| {
                                m.get_qualified_fn(hash).cloned().map(|func| {
                                    FnResolutionCacheEntry {
                                        func,
                                        source: m.id_raw().clone(),
                                    }
                                })
                            })
                        });

                    match func {
                        // Specific version found
                        Some(f) => return Some(Box::new(f)),

                        // Stop when all permutations are exhausted
                        None if bitmask >= max_bitmask => {
                            if num_args != 2 {
                                return None;
                            }

                            return args.and_then(|args| {
                                if !is_op_assignment {
                                    get_builtin_binary_op_fn(fn_name, &args[0], &args[1]).map(|f| {
                                        FnResolutionCacheEntry {
                                            func: CallableFunction::from_method(
                                                Box::new(f) as Box<FnAny>
                                            ),
                                            source: Identifier::new_const(),
                                        }
                                    })
                                } else {
                                    let (first_arg, rest_args) = args.split_first().unwrap();

                                    get_builtin_op_assignment_fn(fn_name, *first_arg, rest_args[0])
                                        .map(|f| FnResolutionCacheEntry {
                                            func: CallableFunction::from_method(
                                                Box::new(f) as Box<FnAny>
                                            ),
                                            source: Identifier::new_const(),
                                        })
                                }
                                .map(Box::new)
                            });
                        }

                        // Try all permutations with `Dynamic` wildcards
                        None => {
                            let hash_params = calc_fn_params_hash(
                                args.as_ref()
                                    .expect("no permutations")
                                    .iter()
                                    .enumerate()
                                    .map(|(i, a)| {
                                        let mask = 1usize << (num_args - i - 1);
                                        if bitmask & mask != 0 {
                                            // Replace with `Dynamic`
                                            TypeId::of::<Dynamic>()
                                        } else {
                                            a.type_id()
                                        }
                                    }),
                            );
                            hash = combine_hashes(hash_script, hash_params);

                            bitmask += 1;
                        }
                    }
                }
            });

        result.as_ref().map(Box::as_ref)
    }

    /// # Main Entry-Point
    ///
    /// Call a native Rust function registered with the [`Engine`].
    ///
    /// # WARNING
    ///
    /// Function call arguments be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    ///
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument -
    /// all others are silently replaced by `()`!
    pub(crate) fn call_native_fn(
        &self,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        name: &str,
        hash: u64,
        args: &mut FnCallArgs,
        is_ref_mut: bool,
        is_op_assign: bool,
        pos: Position,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, pos)?;

        let parent_source = global.source.clone();

        // Check if function access already in the cache
        let func = self.resolve_fn(
            global,
            caches,
            lib,
            name,
            hash,
            Some(args),
            true,
            is_op_assign,
        );

        if func.is_some() {
            let is_method = func.map(|f| f.func.is_method()).unwrap_or(false);

            // Push a new call stack frame
            #[cfg(feature = "debugging")]
            let orig_call_stack_len = global.debugger.call_stack().len();

            let mut _result = if let Some(FnResolutionCacheEntry { func, source }) = func {
                assert!(func.is_native());

                let mut backup = ArgBackup::new();

                // Calling pure function but the first argument is a reference?
                if is_ref_mut && func.is_pure() && !args.is_empty() {
                    // Clone the first argument
                    backup.change_first_arg_to_copy(args);
                }

                let source = match (source.as_str(), parent_source.as_str()) {
                    ("", "") => None,
                    ("", s) | (s, ..) => Some(s),
                };

                #[cfg(feature = "debugging")]
                if self.debugger.is_some() {
                    global.debugger.push_call_stack_frame(
                        name,
                        args.iter().map(|v| (*v).clone()).collect(),
                        source.unwrap_or(""),
                        pos,
                    );
                }

                // Run external function
                let context = (self, name, source, &*global, lib, pos, level).into();

                let result = if func.is_plugin_fn() {
                    func.get_plugin_fn()
                        .expect("plugin function")
                        .call(context, args)
                } else {
                    func.get_native_fn().expect("native function")(context, args)
                };

                // Restore the original reference
                backup.restore_first_arg(args);

                result
            } else {
                unreachable!("`Some`");
            };

            #[cfg(feature = "debugging")]
            {
                let trigger = match global.debugger.status {
                    crate::eval::DebuggerStatus::FunctionExit(n) => n >= level,
                    crate::eval::DebuggerStatus::Next(.., true) => true,
                    _ => false,
                };
                if trigger {
                    let scope = &mut &mut Scope::new();
                    let node = crate::ast::Stmt::Noop(pos);
                    let node = (&node).into();
                    let event = match _result {
                        Ok(ref r) => crate::eval::DebuggerEvent::FunctionExitWithValue(r),
                        Err(ref err) => crate::eval::DebuggerEvent::FunctionExitWithError(err),
                    };
                    match self.run_debugger_raw(scope, global, lib, &mut None, node, event, level) {
                        Ok(_) => (),
                        Err(err) => _result = Err(err),
                    }
                }

                // Pop the call stack
                global.debugger.rewind_call_stack(orig_call_stack_len);
            }

            // Check the return value (including data sizes)
            let result = self.check_return_value(_result, pos)?;

            // Check the data size of any `&mut` object, which may be changed.
            #[cfg(not(feature = "unchecked"))]
            if is_ref_mut && args.len() > 0 {
                self.check_data_size(&args[0], pos)?;
            }

            // See if the function match print/debug (which requires special processing)
            return Ok(match name {
                KEYWORD_PRINT => {
                    let text = result.into_immutable_string().map_err(|typ| {
                        let t = self.map_type_name(type_name::<ImmutableString>()).into();
                        ERR::ErrorMismatchOutputType(t, typ.into(), pos)
                    })?;
                    ((&*self.print)(&text).into(), false)
                }
                KEYWORD_DEBUG => {
                    let text = result.into_immutable_string().map_err(|typ| {
                        let t = self.map_type_name(type_name::<ImmutableString>()).into();
                        ERR::ErrorMismatchOutputType(t, typ.into(), pos)
                    })?;
                    let source = if global.source.is_empty() {
                        None
                    } else {
                        Some(global.source.as_str())
                    };
                    ((&*self.debug)(&text, source, pos).into(), false)
                }
                _ => (result, is_method),
            });
        }

        // Error handling

        match name {
            // index getter function not found?
            #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
            crate::engine::FN_IDX_GET => {
                assert!(args.len() == 2);

                let t0 = self.map_type_name(args[0].type_name());
                let t1 = self.map_type_name(args[1].type_name());

                Err(ERR::ErrorIndexingType(format!("{} [{}]", t0, t1), pos).into())
            }

            // index setter function not found?
            #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
            crate::engine::FN_IDX_SET => {
                assert!(args.len() == 3);

                let t0 = self.map_type_name(args[0].type_name());
                let t1 = self.map_type_name(args[1].type_name());
                let t2 = self.map_type_name(args[2].type_name());

                Err(ERR::ErrorIndexingType(format!("{} [{}] = {}", t0, t1, t2), pos).into())
            }

            // Getter function not found?
            #[cfg(not(feature = "no_object"))]
            _ if name.starts_with(crate::engine::FN_GET) => {
                assert!(args.len() == 1);

                let prop = &name[crate::engine::FN_GET.len()..];
                let t0 = self.map_type_name(args[0].type_name());

                Err(ERR::ErrorDotExpr(
                    format!(
                        "Unknown property '{}' - a getter is not registered for type '{}'",
                        prop, t0
                    ),
                    pos,
                )
                .into())
            }

            // Setter function not found?
            #[cfg(not(feature = "no_object"))]
            _ if name.starts_with(crate::engine::FN_SET) => {
                assert!(args.len() == 2);

                let prop = &name[crate::engine::FN_SET.len()..];
                let t0 = self.map_type_name(args[0].type_name());
                let t1 = self.map_type_name(args[1].type_name());

                Err(ERR::ErrorDotExpr(
                    format!(
                        "No writable property '{}' - a setter is not registered for type '{}' to handle '{}'",
                        prop, t0, t1
                    ),
                    pos,
                )
                .into())
            }

            // Raise error
            _ => Err(ERR::ErrorFunctionNotFound(
                self.gen_call_signature(
                    #[cfg(not(feature = "no_module"))]
                    &crate::ast::Namespace::NONE,
                    name,
                    args,
                ),
                pos,
            )
            .into()),
        }
    }

    /// # Main Entry-Point
    ///
    /// Perform an actual function call, native Rust or scripted, taking care of special functions.
    ///
    /// # WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by
    /// value. All function arguments not in the first position are always passed by value and thus consumed.
    ///
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument -
    /// all others are silently replaced by `()`!
    pub(crate) fn exec_fn_call(
        &self,
        _scope: Option<&mut Scope>,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        fn_name: &str,
        hashes: FnCallHashes,
        args: &mut FnCallArgs,
        is_ref_mut: bool,
        _is_method_call: bool,
        pos: Position,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        fn no_method_err(name: &str, pos: Position) -> RhaiResultOf<(Dynamic, bool)> {
            Err(ERR::ErrorRuntime(
                (format!("'{0}' should not be called this way. Try {0}(...);", name)).into(),
                pos,
            )
            .into())
        }

        // Check for data race.
        #[cfg(not(feature = "no_closure"))]
        ensure_no_data_race(fn_name, args, is_ref_mut)?;

        // These may be redirected from method style calls.
        match fn_name {
            // Handle type_of()
            KEYWORD_TYPE_OF if args.len() == 1 => {
                let typ = self.map_type_name(args[0].type_name()).to_string().into();
                return Ok((typ, false));
            }

            // Handle is_def_fn()
            #[cfg(not(feature = "no_function"))]
            crate::engine::KEYWORD_IS_DEF_FN
                if args.len() == 2 && args[0].is::<FnPtr>() && args[1].is::<crate::INT>() =>
            {
                let fn_name = args[0].read_lock::<ImmutableString>().expect("`FnPtr`");
                let num_params = args[1].as_int().expect("`INT`");

                return Ok((
                    if num_params < 0 {
                        false
                    } else {
                        let hash_script = calc_fn_hash(fn_name.as_str(), num_params as usize);
                        self.has_script_fn(Some(global), caches, lib, hash_script)
                    }
                    .into(),
                    false,
                ));
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if args.len() == 1 => {
                return no_method_err(fn_name, pos)
            }

            KEYWORD_FN_PTR | KEYWORD_EVAL | KEYWORD_IS_DEF_VAR if args.len() == 1 => {
                return no_method_err(fn_name, pos)
            }

            KEYWORD_FN_PTR_CALL | KEYWORD_FN_PTR_CURRY if !args.is_empty() => {
                return no_method_err(fn_name, pos)
            }

            _ => (),
        }

        let level = level + 1;

        // Script-defined function call?
        #[cfg(not(feature = "no_function"))]
        if let Some(FnResolutionCacheEntry { func, mut source }) = self
            .resolve_fn(
                global,
                caches,
                lib,
                fn_name,
                hashes.script,
                None,
                false,
                false,
            )
            .cloned()
        {
            // Script function call
            assert!(func.is_script());

            let func = func.get_script_fn_def().expect("script-defined function");

            if func.body.is_empty() {
                return Ok((Dynamic::UNIT, false));
            }

            let mut empty_scope;
            let scope = match _scope {
                Some(scope) => scope,
                None => {
                    empty_scope = Scope::new();
                    &mut empty_scope
                }
            };

            mem::swap(&mut global.source, &mut source);

            let result = if _is_method_call {
                // Method call of script function - map first argument to `this`
                let (first_arg, rest_args) = args.split_first_mut().unwrap();

                self.call_script_fn(
                    scope,
                    global,
                    caches,
                    lib,
                    &mut Some(*first_arg),
                    func,
                    rest_args,
                    true,
                    pos,
                    level,
                )
            } else {
                // Normal call of script function
                let mut backup = ArgBackup::new();

                // The first argument is a reference?
                if is_ref_mut && !args.is_empty() {
                    backup.change_first_arg_to_copy(args);
                }

                let result = self.call_script_fn(
                    scope, global, caches, lib, &mut None, func, args, true, pos, level,
                );

                // Restore the original reference
                backup.restore_first_arg(args);

                result
            };

            // Restore the original source
            mem::swap(&mut global.source, &mut source);

            return Ok((result?, false));
        }

        // Native function call
        let hash = hashes.native;
        self.call_native_fn(
            global, caches, lib, fn_name, hash, args, is_ref_mut, false, pos, level,
        )
    }

    /// Evaluate a list of statements with no `this` pointer.
    /// This is commonly used to evaluate a list of statements in an [`AST`] or a script function body.
    #[inline]
    pub(crate) fn eval_global_statements(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        statements: &[Stmt],
        lib: &[&Module],
        level: usize,
    ) -> RhaiResult {
        self.eval_stmt_block(
            scope, global, caches, lib, &mut None, statements, false, level,
        )
        .or_else(|err| match *err {
            ERR::Return(out, ..) => Ok(out),
            ERR::LoopBreak(..) => {
                unreachable!("no outer loop scope to break out of")
            }
            _ => Err(err),
        })
    }

    /// Call a dot method.
    #[cfg(not(feature = "no_object"))]
    pub(crate) fn make_method_call(
        &self,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        fn_name: &str,
        mut hash: FnCallHashes,
        target: &mut crate::eval::Target,
        (call_args, call_arg_pos): &mut (FnArgsVec<Dynamic>, Position),
        pos: Position,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        let is_ref_mut = target.is_ref();

        let (result, updated) = match fn_name {
            KEYWORD_FN_PTR_CALL if target.is::<FnPtr>() => {
                // FnPtr call
                let fn_ptr = target.read_lock::<FnPtr>().expect("`FnPtr`");
                // Redirect function name
                let fn_name = fn_ptr.fn_name();
                let args_len = call_args.len() + fn_ptr.curry().len();
                // Recalculate hashes
                let new_hash = calc_fn_hash(fn_name, args_len).into();
                // Arguments are passed as-is, adding the curried arguments
                let mut curry = FnArgsVec::with_capacity(fn_ptr.num_curried());
                curry.extend(fn_ptr.curry().iter().cloned());
                let mut args = FnArgsVec::with_capacity(curry.len() + call_args.len());
                args.extend(curry.iter_mut());
                args.extend(call_args.iter_mut());

                // Map it to name(args) in function-call style
                self.exec_fn_call(
                    None, global, caches, lib, fn_name, new_hash, &mut args, false, false, pos,
                    level,
                )
            }
            KEYWORD_FN_PTR_CALL => {
                if !call_args.is_empty() {
                    if !call_args[0].is::<FnPtr>() {
                        let typ = self.map_type_name(call_args[0].type_name());
                        return Err(self.make_type_mismatch_err::<FnPtr>(typ, *call_arg_pos));
                    }
                } else {
                    let typ = self.map_type_name(target.type_name());
                    return Err(self.make_type_mismatch_err::<FnPtr>(typ, pos));
                }

                // FnPtr call on object
                let fn_ptr = call_args.remove(0).cast::<FnPtr>();
                // Redirect function name
                let fn_name = fn_ptr.fn_name();
                let args_len = call_args.len() + fn_ptr.curry().len();
                // Recalculate hash
                let new_hash = FnCallHashes::from_all(
                    #[cfg(not(feature = "no_function"))]
                    calc_fn_hash(fn_name, args_len),
                    calc_fn_hash(fn_name, args_len + 1),
                );
                // Replace the first argument with the object pointer, adding the curried arguments
                let mut curry = FnArgsVec::with_capacity(fn_ptr.num_curried());
                curry.extend(fn_ptr.curry().iter().cloned());
                let mut args = FnArgsVec::with_capacity(curry.len() + call_args.len() + 1);
                args.push(target.as_mut());
                args.extend(curry.iter_mut());
                args.extend(call_args.iter_mut());

                // Map it to name(args) in function-call style
                self.exec_fn_call(
                    None, global, caches, lib, fn_name, new_hash, &mut args, is_ref_mut, true, pos,
                    level,
                )
            }
            KEYWORD_FN_PTR_CURRY => {
                if !target.is::<FnPtr>() {
                    let typ = self.map_type_name(target.type_name());
                    return Err(self.make_type_mismatch_err::<FnPtr>(typ, pos));
                }

                let fn_ptr = target.read_lock::<FnPtr>().expect("`FnPtr`");

                // Curry call
                Ok((
                    if call_args.is_empty() {
                        fn_ptr.clone()
                    } else {
                        FnPtr::new_unchecked(
                            fn_ptr.fn_name_raw().clone(),
                            fn_ptr
                                .curry()
                                .iter()
                                .cloned()
                                .chain(call_args.iter_mut().map(mem::take))
                                .collect(),
                        )
                    }
                    .into(),
                    false,
                ))
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if call_args.is_empty() => {
                return Ok((target.is_shared().into(), false));
            }

            _ => {
                let mut fn_name = fn_name;
                let _redirected;

                // Check if it is a map method call in OOP style
                #[cfg(not(feature = "no_object"))]
                if let Some(map) = target.read_lock::<crate::Map>() {
                    if let Some(val) = map.get(fn_name) {
                        if let Some(fn_ptr) = val.read_lock::<FnPtr>() {
                            // Remap the function name
                            _redirected = fn_ptr.fn_name_raw().clone();
                            fn_name = &_redirected;
                            // Add curried arguments
                            if fn_ptr.is_curried() {
                                call_args.insert_many(0, fn_ptr.curry().iter().cloned());
                            }
                            // Recalculate the hash based on the new function name and new arguments
                            hash = FnCallHashes::from_all(
                                #[cfg(not(feature = "no_function"))]
                                calc_fn_hash(fn_name, call_args.len()),
                                calc_fn_hash(fn_name, call_args.len() + 1),
                            );
                        }
                    }
                };

                // Attached object pointer in front of the arguments
                let mut args = FnArgsVec::with_capacity(call_args.len() + 1);
                args.push(target.as_mut());
                args.extend(call_args.iter_mut());

                self.exec_fn_call(
                    None, global, caches, lib, fn_name, hash, &mut args, is_ref_mut, true, pos,
                    level,
                )
            }
        }?;

        // Propagate the changed value back to the source if necessary
        if updated {
            target
                .propagate_changed_value()
                .map_err(|err| err.fill_position(pos))?;
        }

        Ok((result, updated))
    }

    /// Evaluate an argument.
    #[inline]
    pub(crate) fn get_arg_value(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        arg_expr: &Expr,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, Position)> {
        #[cfg(feature = "debugging")]
        if self.debugger.is_some() {
            if let Some(value) = arg_expr.get_literal_value() {
                #[cfg(feature = "debugging")]
                self.run_debugger(scope, global, lib, this_ptr, arg_expr, level)?;
                return Ok((value, arg_expr.start_position()));
            }
        }

        // Do not match function exit for arguments
        #[cfg(feature = "debugging")]
        let reset_debugger = global.debugger.clear_status_if(|status| {
            matches!(status, crate::eval::DebuggerStatus::FunctionExit(..))
        });

        let result = self.eval_expr(scope, global, caches, lib, this_ptr, arg_expr, level);

        // Restore function exit status
        #[cfg(feature = "debugging")]
        global.debugger.reset_status(reset_debugger);

        Ok((result?, arg_expr.start_position()))
    }

    /// Call a function in normal function-call style.
    pub(crate) fn make_function_call(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        fn_name: &str,
        first_arg: Option<&Expr>,
        args_expr: &[Expr],
        hashes: FnCallHashes,
        capture_scope: bool,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        let mut first_arg = first_arg;
        let mut a_expr = args_expr;
        let mut total_args = if first_arg.is_some() { 1 } else { 0 } + a_expr.len();
        let mut curry = FnArgsVec::new_const();
        let mut name = fn_name;
        let mut hashes = hashes;
        let redirected; // Handle call() - Redirect function call

        match name {
            // Handle call()
            KEYWORD_FN_PTR_CALL if total_args >= 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, arg, level)?;

                if !arg_value.is::<FnPtr>() {
                    let typ = self.map_type_name(arg_value.type_name());
                    return Err(self.make_type_mismatch_err::<FnPtr>(typ, arg_pos));
                }

                let fn_ptr = arg_value.cast::<FnPtr>();
                curry.extend(fn_ptr.curry().iter().cloned());

                // Redirect function name
                redirected = fn_ptr.take_data().0;
                name = &redirected;

                // Shift the arguments
                first_arg = a_expr.get(0);
                if !a_expr.is_empty() {
                    a_expr = &a_expr[1..];
                }
                total_args -= 1;

                // Recalculate hash
                let args_len = total_args + curry.len();
                hashes = if !hashes.is_native_only() {
                    calc_fn_hash(name, args_len).into()
                } else {
                    FnCallHashes::from_native(calc_fn_hash(name, args_len))
                };
            }
            // Handle Fn()
            KEYWORD_FN_PTR if total_args == 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, arg, level)?;

                // Fn - only in function call style
                return arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))
                    .and_then(FnPtr::try_from)
                    .map(Into::into)
                    .map_err(|err| err.fill_position(arg_pos));
            }

            // Handle curry()
            KEYWORD_FN_PTR_CURRY if total_args > 1 => {
                let first = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, first, level)?;

                if !arg_value.is::<FnPtr>() {
                    let typ = self.map_type_name(arg_value.type_name());
                    return Err(self.make_type_mismatch_err::<FnPtr>(typ, arg_pos));
                }

                let (name, fn_curry) = arg_value.cast::<FnPtr>().take_data();

                // Append the new curried arguments to the existing list.
                let fn_curry = a_expr.iter().try_fold(fn_curry, |mut curried, expr| {
                    let (value, ..) =
                        self.get_arg_value(scope, global, caches, lib, this_ptr, expr, level)?;
                    curried.push(value);
                    Ok::<_, RhaiError>(curried)
                })?;

                return Ok(FnPtr::new_unchecked(name, fn_curry).into());
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if total_args == 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, ..) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, arg, level)?;
                return Ok(arg_value.is_shared().into());
            }

            // Handle is_def_fn()
            #[cfg(not(feature = "no_function"))]
            crate::engine::KEYWORD_IS_DEF_FN if total_args == 2 => {
                let first = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, first, level)?;

                let fn_name = arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))?;

                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, &a_expr[0], level)?;

                let num_params = arg_value
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, arg_pos))?;

                return Ok(if num_params < 0 {
                    false
                } else {
                    let hash_script = calc_fn_hash(&fn_name, num_params as usize);
                    self.has_script_fn(Some(global), caches, lib, hash_script)
                }
                .into());
            }

            // Handle is_def_var()
            KEYWORD_IS_DEF_VAR if total_args == 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, arg, level)?;
                let var_name = arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))?;
                return Ok(scope.contains(&var_name).into());
            }

            // Handle eval()
            KEYWORD_EVAL if total_args == 1 => {
                // eval - only in function call style
                let orig_scope_len = scope.len();
                let arg = first_arg.unwrap();
                let (arg_value, pos) =
                    self.get_arg_value(scope, global, caches, lib, this_ptr, arg, level)?;
                let script = &arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, pos))?;
                let result = self.eval_script_expr_in_place(
                    scope,
                    global,
                    caches,
                    lib,
                    script,
                    pos,
                    level + 1,
                );

                // IMPORTANT! If the eval defines new variables in the current scope,
                //            all variable offsets from this point on will be mis-aligned.
                if scope.len() != orig_scope_len {
                    global.always_search_scope = true;
                }

                return result.map_err(|err| {
                    ERR::ErrorInFunctionCall(
                        KEYWORD_EVAL.to_string(),
                        global.source.to_string(),
                        err,
                        pos,
                    )
                    .into()
                });
            }

            _ => (),
        }

        // Normal function call - except for Fn, curry, call and eval (handled above)
        let mut arg_values = FnArgsVec::with_capacity(total_args);
        let mut args = FnArgsVec::with_capacity(total_args + curry.len());
        let mut is_ref_mut = false;

        // Capture parent scope?
        //
        // If so, do it separately because we cannot convert the first argument (if it is a simple
        // variable access) to &mut because `scope` is needed.
        if capture_scope && !scope.is_empty() {
            first_arg
                .iter()
                .map(|&v| v)
                .chain(a_expr.iter())
                .try_for_each(|expr| {
                    self.get_arg_value(scope, global, caches, lib, this_ptr, expr, level)
                        .map(|(value, ..)| arg_values.push(value.flatten()))
                })?;
            args.extend(curry.iter_mut());
            args.extend(arg_values.iter_mut());

            // Use parent scope
            let scope = Some(scope);

            return self
                .exec_fn_call(
                    scope, global, caches, lib, name, hashes, &mut args, is_ref_mut, false, pos,
                    level,
                )
                .map(|(v, ..)| v);
        }

        // Call with blank scope
        if total_args == 0 && curry.is_empty() {
            // No arguments
        } else {
            // If the first argument is a variable, and there is no curried arguments,
            // convert to method-call style in order to leverage potential &mut first argument and
            // avoid cloning the value
            if curry.is_empty() && first_arg.map_or(false, |expr| expr.is_variable_access(false)) {
                let first_expr = first_arg.unwrap();

                #[cfg(feature = "debugging")]
                self.run_debugger(scope, global, lib, this_ptr, first_expr, level)?;

                // func(x, ...) -> x.func(...)
                a_expr.iter().try_for_each(|expr| {
                    self.get_arg_value(scope, global, caches, lib, this_ptr, expr, level)
                        .map(|(value, ..)| arg_values.push(value.flatten()))
                })?;

                let (mut target, _pos) =
                    self.search_namespace(scope, global, lib, this_ptr, first_expr, level)?;

                if target.as_ref().is_read_only() {
                    target = target.into_owned();
                }

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, _pos)?;

                #[cfg(not(feature = "no_closure"))]
                let target_is_shared = target.is_shared();
                #[cfg(feature = "no_closure")]
                let target_is_shared = false;

                if target_is_shared || target.is_temp_value() {
                    arg_values.insert(0, target.take_or_clone().flatten());
                    args.extend(arg_values.iter_mut())
                } else {
                    // Turn it into a method call only if the object is not shared and not a simple value
                    is_ref_mut = true;
                    let obj_ref = target.take_ref().expect("ref");
                    args.push(obj_ref);
                    args.extend(arg_values.iter_mut());
                }
            } else {
                // func(..., ...)
                first_arg
                    .into_iter()
                    .chain(a_expr.iter())
                    .try_for_each(|expr| {
                        self.get_arg_value(scope, global, caches, lib, this_ptr, expr, level)
                            .map(|(value, ..)| arg_values.push(value.flatten()))
                    })?;
                args.extend(curry.iter_mut());
                args.extend(arg_values.iter_mut());
            }
        }

        self.exec_fn_call(
            None, global, caches, lib, name, hashes, &mut args, is_ref_mut, false, pos, level,
        )
        .map(|(v, ..)| v)
    }

    /// Call a namespace-qualified function in normal function-call style.
    #[cfg(not(feature = "no_module"))]
    pub(crate) fn make_qualified_function_call(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        namespace: &crate::ast::Namespace,
        fn_name: &str,
        args_expr: &[Expr],
        hash: u64,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        let mut arg_values = FnArgsVec::with_capacity(args_expr.len());
        let mut args = FnArgsVec::with_capacity(args_expr.len());
        let mut first_arg_value = None;

        if args_expr.is_empty() {
            // No arguments
        } else {
            // See if the first argument is a variable (not namespace-qualified).
            // If so, convert to method-call style in order to leverage potential &mut first argument
            // and avoid cloning the value
            if !args_expr.is_empty() && args_expr[0].is_variable_access(true) {
                #[cfg(feature = "debugging")]
                self.run_debugger(scope, global, lib, this_ptr, &args_expr[0], level)?;

                // func(x, ...) -> x.func(...)
                arg_values.push(Dynamic::UNIT);

                args_expr.iter().skip(1).try_for_each(|expr| {
                    self.get_arg_value(scope, global, caches, lib, this_ptr, expr, level)
                        .map(|(value, ..)| arg_values.push(value.flatten()))
                })?;

                // Get target reference to first argument
                let first_arg = &args_expr[0];
                let (target, _pos) =
                    self.search_scope_only(scope, global, lib, this_ptr, first_arg, level)?;

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, _pos)?;

                #[cfg(not(feature = "no_closure"))]
                let target_is_shared = target.is_shared();
                #[cfg(feature = "no_closure")]
                let target_is_shared = false;

                if target_is_shared || target.is_temp_value() {
                    arg_values[0] = target.take_or_clone().flatten();
                    args.extend(arg_values.iter_mut());
                } else {
                    // Turn it into a method call only if the object is not shared and not a simple value
                    let (first, rest) = arg_values.split_first_mut().unwrap();
                    first_arg_value = Some(first);
                    let obj_ref = target.take_ref().expect("ref");
                    args.push(obj_ref);
                    args.extend(rest.iter_mut());
                }
            } else {
                // func(..., ...) or func(mod::x, ...)
                args_expr.iter().try_for_each(|expr| {
                    self.get_arg_value(scope, global, caches, lib, this_ptr, expr, level)
                        .map(|(value, ..)| arg_values.push(value.flatten()))
                })?;
                args.extend(arg_values.iter_mut());
            }
        }

        // Search for the root namespace
        let module = self
            .search_imports(global, namespace)
            .ok_or_else(|| ERR::ErrorModuleNotFound(namespace.to_string(), namespace.position()))?;

        // First search script-defined functions in namespace (can override built-in)
        let mut func = match module.get_qualified_fn(hash) {
            // Then search native Rust functions
            None => {
                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, pos)?;

                let hash_params = calc_fn_params_hash(args.iter().map(|a| a.type_id()));
                let hash_qualified_fn = combine_hashes(hash, hash_params);

                module.get_qualified_fn(hash_qualified_fn)
            }
            r => r,
        };

        // Check for `Dynamic` parameters.
        //
        // Note - This is done during every function call mismatch without cache,
        //        so hopefully the number of arguments should not be too many
        //        (expected because closures cannot be qualified).
        if func.is_none() && !args.is_empty() {
            let num_args = args.len();
            let max_bitmask = 1usize << usize::min(num_args, MAX_DYNAMIC_PARAMETERS);
            let mut bitmask = 1usize; // Bitmask of which parameter to replace with `Dynamic`

            // Try all permutations with `Dynamic` wildcards
            while bitmask < max_bitmask {
                let hash_params = calc_fn_params_hash(args.iter().enumerate().map(|(i, a)| {
                    let mask = 1usize << (num_args - i - 1);
                    if bitmask & mask != 0 {
                        // Replace with `Dynamic`
                        TypeId::of::<Dynamic>()
                    } else {
                        a.type_id()
                    }
                }));
                let hash_qualified_fn = combine_hashes(hash, hash_params);

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, pos)?;

                if let Some(f) = module.get_qualified_fn(hash_qualified_fn) {
                    func = Some(f);
                    break;
                }

                bitmask += 1;
            }
        }

        // Clone first argument if the function is not a method after-all
        if !func.map(|f| f.is_method()).unwrap_or(true) {
            if let Some(first) = first_arg_value {
                *first = args[0].clone();
                args[0] = first;
            }
        }

        let level = level + 1;

        match func {
            #[cfg(not(feature = "no_function"))]
            Some(f) if f.is_script() => {
                let fn_def = f.get_script_fn_def().expect("script-defined function");
                let new_scope = &mut Scope::new();
                let mut source = module.id_raw().clone();
                mem::swap(&mut global.source, &mut source);

                let result = self.call_script_fn(
                    new_scope, global, caches, lib, &mut None, fn_def, &mut args, true, pos, level,
                );

                global.source = source;

                result
            }

            Some(f) if f.is_plugin_fn() => {
                let context = (self, fn_name, module.id(), &*global, lib, pos, level).into();
                let result = f
                    .get_plugin_fn()
                    .expect("plugin function")
                    .clone()
                    .call(context, &mut args);
                self.check_return_value(result, pos)
            }

            Some(f) if f.is_native() => {
                let func = f.get_native_fn().expect("native function");
                let context = (self, fn_name, module.id(), &*global, lib, pos, level).into();
                let result = func(context, &mut args);
                self.check_return_value(result, pos)
            }

            Some(f) => unreachable!("unknown function type: {:?}", f),

            None => Err(ERR::ErrorFunctionNotFound(
                self.gen_call_signature(namespace, fn_name, &args),
                pos,
            )
            .into()),
        }
    }

    /// Evaluate a text script in place - used primarily for 'eval'.
    pub(crate) fn eval_script_expr_in_place(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        script: &str,
        _pos: Position,
        level: usize,
    ) -> RhaiResult {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, _pos)?;

        let script = script.trim();

        if script.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Compile the script text
        // No optimizations because we only run it once
        let ast = self.compile_with_scope_and_optimization_level(
            &Scope::new(),
            &[script],
            #[cfg(not(feature = "no_optimize"))]
            OptimizationLevel::None,
            #[cfg(feature = "no_optimize")]
            OptimizationLevel::default(),
        )?;

        // If new functions are defined within the eval string, it is an error
        #[cfg(not(feature = "no_function"))]
        if !ast.shared_lib().is_empty() {
            return Err(crate::PERR::WrongFnDefinition.into());
        }

        let statements = ast.statements();
        if statements.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Evaluate the AST
        self.eval_global_statements(scope, global, caches, statements, lib, level)
    }
}
