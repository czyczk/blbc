use crate::def_package;

def_package! {
    /// Standard package containing all built-in features.
    ///
    /// # Contents
    ///
    /// * [`CorePackage`][super::CorePackage]
    /// * [`BitFieldPackage`][super::BitFieldPackage]
    /// * [`LogicPackage`][super::LogicPackage]
    /// * [`BasicMathPackage`][super::BasicMathPackage]
    /// * [`BasicArrayPackage`][super::BasicArrayPackage]
    /// * [`BasicBlobPackage`][super::BasicBlobPackage]
    /// * [`BasicMapPackage`][super::BasicMapPackage]
    /// * [`BasicTimePackage`][super::BasicTimePackage]
    /// * [`MoreStringPackage`][super::MoreStringPackage]
    pub StandardPackage(lib) {
        lib.standard = true;

        super::CorePackage::init(lib);
        super::BitFieldPackage::init(lib);
        super::LogicPackage::init(lib);
        super::BasicMathPackage::init(lib);
        #[cfg(not(feature = "no_index"))]
        super::BasicArrayPackage::init(lib);
        #[cfg(not(feature = "no_index"))]
        super::BasicBlobPackage::init(lib);
        #[cfg(not(feature = "no_object"))]
        super::BasicMapPackage::init(lib);
        #[cfg(not(feature = "no_std"))]
        super::BasicTimePackage::init(lib);
        super::MoreStringPackage::init(lib);
    }
}
