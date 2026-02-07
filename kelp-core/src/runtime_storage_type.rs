use minecraft_command_types::impl_has_macro_false;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum RuntimeStorageType {
    Score,
    Data,
}

impl_has_macro_false!(RuntimeStorageType);
