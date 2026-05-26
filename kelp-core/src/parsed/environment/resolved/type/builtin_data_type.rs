use strum::{Display, EnumIter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighBuiltinTypeId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedBuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}

#[derive(Display, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
pub enum BuiltinTypeKind {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Score,
    List,
    Compound,
    Data,
    EntitySelector,
    ResourceLocation,
    Coordinates,
}

impl BuiltinTypeKind {
    #[must_use]
    pub fn declaration(self) -> ResolvedBuiltinTypeDeclaration {
        macro_rules! declaration {
            ($name:ident) => {
                declaration!($name<0>)
            };

            ($name:ident<$generic_count:literal>) => {
                ResolvedBuiltinTypeDeclaration {
                    name: stringify!($name).to_owned(),
                    generic_count: $generic_count,
                    kind: self,
                }
            };
        }

        match self {
            Self::Boolean => declaration!(bool),
            Self::Byte => declaration!(byte),
            Self::Short => declaration!(short),
            Self::Integer => declaration!(integer),
            Self::Long => declaration!(long),
            Self::Float => declaration!(float),
            Self::Double => declaration!(double),
            Self::String => declaration!(string),
            Self::Score => declaration!(score<1>),
            Self::List => declaration!(list<1>),
            Self::Compound => declaration!(compound<1>),
            Self::Data => declaration!(data<1>),
            Self::EntitySelector => declaration!(entity_selector),
            Self::ResourceLocation => declaration!(resource_location),
            Self::Coordinates => declaration!(coordinates),
        }
    }
}
