use std::fmt::Display;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Visibility {
    Public,
    #[default]
    None,
}

impl Visibility {
    #[inline]
    #[must_use]
    pub const fn should_display(&self) -> bool {
        !matches!(self, Self::None)
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => f.write_str("pub"),
            Self::None => Ok(()),
        }
    }
}
