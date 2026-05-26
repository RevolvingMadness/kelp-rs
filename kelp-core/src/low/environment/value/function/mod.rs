use la_arena::Idx;
use minecraft_command_types::snbt::SNBT;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        data_type::DataType,
        environment::value::function::{
            builtin::BuiltinFunctionDeclaration, regular::RegularFunctionDeclaration,
        },
        text_component::{TextComponent, format_generics},
    },
    parameter_types_iter::{ParameterTypesIter, take_second},
    typed::pattern::TypedPattern,
};

use std::fmt::Write as _;

pub mod builtin;
pub mod regular;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub u32);

impl TextComponent for FunctionId {
    fn into_text_component(self, datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        let (_, _, declaration) = datapack.get_function(self);

        let mut output = format!("fn {}", declaration.name());

        format_generics(
            &mut output,
            declaration.generic_types(),
            &datapack.environment,
        );

        output.push('(');

        for (i, data_type) in declaration.parameter_types().enumerate() {
            if i != 0 {
                output.push_str(", ");
            }

            let _ = write!(output, "{}", data_type.display(&datapack.environment));
        }

        output.push(')');

        let return_type = declaration.return_type();

        if *return_type != DataType::Unit {
            let _ = write!(output, " -> {}", return_type.display(&datapack.environment));
        }

        SNBT::string(output)
    }
}

#[derive(Debug, Clone)]
pub enum FunctionDeclaration {
    Regular(RegularFunctionDeclaration),
    Builtin(BuiltinFunctionDeclaration),
}

impl FunctionDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => &declaration.name,
            Self::Builtin(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_types(&self) -> &[DataType] {
        match self {
            Self::Regular(declaration) => &declaration.generic_types,
            Self::Builtin(declaration) => &declaration.generic_types,
        }
    }

    #[must_use]
    pub fn parameter_types(&self) -> ParameterTypesIter<'_, Idx<TypedPattern>, DataType> {
        match self {
            Self::Regular(declaration) => {
                ParameterTypesIter::Regular(declaration.parameters.iter().map(take_second))
            }
            Self::Builtin(declaration) => {
                ParameterTypesIter::Builtin(declaration.parameters.iter())
            }
        }
    }

    #[must_use]
    pub const fn return_type(&self) -> &DataType {
        match self {
            Self::Regular(declaration) => &declaration.return_type,
            Self::Builtin(declaration) => &declaration.return_type,
        }
    }
}
