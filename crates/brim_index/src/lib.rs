#[macro_export]
macro_rules! index_type {
    (
        $(#[$attrs:meta])*
        $vis:vis struct $name:ident {
            $(const $const_name:ident = $const_val:expr;)*
        }
    ) => {
        use std::sync::atomic::{AtomicUsize, Ordering};

        $(#[$attrs])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        $vis struct $name {
            value: u32,
        }

        thread_local! {
            static COUNTER: AtomicUsize = AtomicUsize::new(0);
        }

        $(
            $vis const $const_name: $name = $name::from_u32($const_val);
        )*

        impl $name {
            pub const MAX: u32 = u32::MAX;

            #[inline]
            $vis fn new() -> Self {
                let value = COUNTER.with(|counter| counter.fetch_add(1, Ordering::SeqCst));
                Self::from_usize(value)
            }

            #[inline]
            $vis const fn from_u32(value: u32) -> Self {
                assert!(value <= 0xFFFF_FF00);
                Self { value }
            }

            #[inline]
            $vis const fn from_usize(value: usize) -> Self {
                assert!(value <= 0xFFFF_FF00 as usize);
                Self { value: value as u32 }
            }

            #[inline]
            $vis const fn as_u32(self) -> u32 {
                self.value
            }

            #[inline]
            $vis const fn as_usize(self) -> usize {
                self.value as usize
            }

            #[inline]
            $vis const fn dummy() -> Self {
                Self::from_u32(0xFFFF_FF00)
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
