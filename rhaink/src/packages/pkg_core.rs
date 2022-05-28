use crate::def_package;

def_package! {
    /// Core package containing basic facilities.
    ///
    /// # Contents
    ///
    /// * [`LanguageCorePackage`][super::LanguageCorePackage]
    /// * [`ArithmeticPackage`][super::ArithmeticPackage]
    /// * [`BasicStringPackage`][super::BasicStringPackage]
    /// * [`BasicIteratorPackage`][super::BasicIteratorPackage]
    /// * [`BasicFnPackage`][super::BasicFnPackage]
    /// * [`DebuggingPackage`][super::DebuggingPackage]
    pub CorePackage(lib) {
        lib.standard = true;

        super::LanguageCorePackage::init(lib);
        super::ArithmeticPackage::init(lib);
        super::BasicStringPackage::init(lib);
        super::BasicIteratorPackage::init(lib);
        super::BasicFnPackage::init(lib);
        #[cfg(feature = "debugging")]
        super::DebuggingPackage::init(lib);
    }
}
