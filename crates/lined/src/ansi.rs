//! The ANSI escape sequences the editor writes, matching `termion`'s output byte for byte.

macro_rules! csi {
    ($($l:expr),*) => { concat!("\x1B[", $($l),*) };
}

pub mod clear {
    use std::fmt;

    /// Clear the entire screen.
    pub struct All;

    impl fmt::Display for All {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(csi!("2J"))
        }
    }

    /// Clear everything after the cursor.
    pub struct AfterCursor;

    impl fmt::Display for AfterCursor {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(csi!("J"))
        }
    }
}

pub mod cursor {
    use std::fmt;

    /// Move the cursor to a one-based `(column, row)` position.
    pub struct Goto(pub u16, pub u16);

    impl fmt::Display for Goto {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "\x1B[{};{}H", self.1, self.0)
        }
    }

    macro_rules! derive_move {
        ($($(#[$attr:meta])* $name:ident => $code:literal,)*) => {$(
            $(#[$attr])*
            pub struct $name(pub u16);

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, concat!("\x1B[{}", $code), self.0)
                }
            }
        )*};
    }

    derive_move! {
        /// Move the cursor up.
        Up => "A",
        /// Move the cursor left.
        Left => "D",
        /// Move the cursor right.
        Right => "C",
    }
}

pub mod color {
    macro_rules! derive_color {
        ($($(#[$attr:meta])* $name:ident => $value:literal,)*) => {$(
            $(#[$attr])*
            pub struct $name;

            // Not every colour is used in both positions.
            #[allow(dead_code)]
            impl $name {
                /// The sequence to set this as the foreground colour.
                pub fn fg_str(&self) -> &'static str {
                    csi!("38;5;", $value, "m")
                }

                /// The sequence to set this as the background colour.
                pub fn bg_str(&self) -> &'static str {
                    csi!("48;5;", $value, "m")
                }
            }
        )*};
    }

    derive_color! {
        /// Black.
        Black => "0",
        /// Red.
        Red => "1",
        /// Green.
        Green => "2",
        /// Yellow.
        Yellow => "3",
        /// White.
        White => "7",
    }

    /// Reset to the default colours.
    pub struct Reset;

    impl Reset {
        /// The sequence to reset the foreground colour.
        pub fn fg_str(&self) -> &'static str {
            csi!("39m")
        }

        /// The sequence to reset the background colour.
        pub fn bg_str(&self) -> &'static str {
            csi!("49m")
        }
    }
}
