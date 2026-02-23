use std::{fmt::Display, ops::Range};

use minecraft_command_types::impl_has_macro_false;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Span {
    #[inline]
    #[must_use]
    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
    }

    #[inline]
    #[must_use]
    pub fn into_range(self) -> Range<usize> {
        self.start..self.end
    }

    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl_has_macro_false!(Span);
