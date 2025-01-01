#[macro_export]
macro_rules! index_type {
    (
        $(#[$attrs:meta])*
        $vis:vis struct $name:ident {
            $(const $const_name:ident = $const_val:expr;)*
        }
    ) => {
        $(#[$attrs])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $name {
            value: u32,
        }

        $(
            $vis const $const_name: $name = $name::from_u32($const_val);
        )*

        impl $name {
            /// Creates a new index from a given `u32`.
            ///
            /// # Panics
            /// Will panic if `value` exceeds `0xFFFF_FF00`
            #[inline]
            $vis const fn from_u32(value: u32) -> Self {
                assert!(value <= 0xFFFF_FF00);
                Self { value }
            }

            /// Creates a new index from a given `usize`.
            ///
            /// # Panics
            /// Will panic if `value` exceeds `0xFFFF_FF00`
            #[inline]
            $vis const fn from_usize(value: usize) -> Self {
                assert!(value <= 0xFFFF_FF00 as usize);
                Self { value: value as u32 }
            }

            /// Extracts the value as a `u32`
            #[inline]
            $vis const fn as_u32(self) -> u32 {
                self.value
            }

            /// Extracts the value as a `usize`
            #[inline]
            $vis const fn as_usize(self) -> usize {
                self.value as usize
            }
        }

        impl From<u32> for $name {
            #[inline]
            fn from(value: u32) -> Self {
                Self::from_u32(value)
            }
        }

        impl From<usize> for $name {
            #[inline]
            fn from(value: usize) -> Self {
                Self::from_usize(value)
            }
        }

        impl From<$name> for u32 {
            #[inline]
            fn from(v: $name) -> u32 {
                v.as_u32()
            }
        }

        impl From<$name> for usize {
            #[inline]
            fn from(v: $name) -> usize {
                v.as_usize()
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.as_u32())
            }
        }
    }
}