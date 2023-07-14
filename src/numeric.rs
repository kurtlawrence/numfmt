/// An object is representable as a 64-bit floating point number.
pub trait Numeric {
    /// Perform the conversion.
    fn to_f64(&self) -> f64;

    /// Return if the number is NaN.
    ///
    /// The default implementation converts to `f64` and performs the test.
    #[inline(always)]
    fn is_nan(&self) -> bool {
        self.to_f64().is_nan()
    }

    /// Return if the number is negative.
    ///
    /// The default implementation converts to `f64` and performs the test.
    #[inline(always)]
    fn is_negative(&self) -> bool {
        self.to_f64().is_sign_negative()
    }

    /// Return if the number is infinite.
    ///
    /// The default implementation converts to `f64` and performs the test.
    #[inline(always)]
    fn is_infinite(&self) -> bool {
        self.to_f64().is_infinite()
    }

    /// Return if the number is zero.
    ///
    /// The default implementation converts to `f64` and performs the test.
    #[inline(always)]
    fn is_zero(&self) -> bool {
        self.to_f64() == 0.0
    }
}

macro_rules! prim_impls {
    (float => $($t:ty)*) => {
        $(impl Numeric for $t {
            #[inline(always)]
            fn to_f64(&self) -> f64 {
                (*self).into()
            }

            #[inline(always)]
            fn is_nan(&self) -> bool {
                Self::is_nan(*self)
            }

            #[inline(always)]
            fn is_negative(&self) -> bool {
                Self::is_sign_negative(*self)
            }

            #[inline(always)]
            fn is_infinite(&self) -> bool {
                Self::is_infinite(*self)
            }

            #[inline(always)]
            fn is_zero(&self) -> bool {
                *self == 0.0
            }
        })*
    };
    (uint => $($t:ty)*) => {
        $(impl Numeric for $t {
            #[inline(always)]
            fn to_f64(&self) -> f64 {
                *self as f64
            }

            #[inline(always)]
            fn is_nan(&self) -> bool {
                false
            }

            #[inline(always)]
            fn is_negative(&self) -> bool {
                false
            }

            #[inline(always)]
            fn is_infinite(&self) -> bool {
                false
            }

            #[inline(always)]
            fn is_zero(&self) -> bool {
                *self == 0
            }
        })*
    };
    (int => $($t:ty)*) => {
        $(impl Numeric for $t {
            #[inline(always)]
            fn to_f64(&self) -> f64 {
                *self as f64
            }

            #[inline(always)]
            fn is_nan(&self) -> bool {
                false
            }

            #[inline(always)]
            fn is_negative(&self) -> bool {
                *self < 0
            }

            #[inline(always)]
            fn is_infinite(&self) -> bool {
                false
            }

            #[inline(always)]
            fn is_zero(&self) -> bool {
                *self == 0
            }
        })*
    };
}

prim_impls!(float => f32 f64);
prim_impls!(uint => u8 u16 u32 u64 u128 usize);
prim_impls!(int => i8 i16 i32 i64 i128 isize);
