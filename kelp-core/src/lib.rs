pub mod compile_context;
pub mod data;
pub mod datapack;
pub mod low;
pub mod operator;
pub mod parameter_types_iter;
pub mod parsed;
pub mod pattern_type;
pub mod player_score;
pub mod runtime_storage;
pub mod semantic;
pub mod span;
pub mod trait_ext;
pub mod visibility;

#[macro_export]
macro_rules! make_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub u32);
    };
}
