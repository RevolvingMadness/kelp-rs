use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBT, SNBTString},
};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FieldAccessType {
    #[default]
    Name,
    Index,
}

impl FieldAccessType {
    #[must_use]
    pub fn into_nbt_path_node(self, field: String) -> NbtPathNode {
        match self {
            Self::Name => NbtPathNode::Named(SNBTString(false, field), None),
            Self::Index => {
                let index = field.parse::<usize>().unwrap();

                NbtPathNode::Index(Some(SNBT::macroable_integer(index as i32)))
            }
        }
    }
}
