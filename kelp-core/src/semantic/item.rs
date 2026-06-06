use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        data_type::SemanticDataType, environment::value::constant::HighConstantId,
        expression::SemanticExpression,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticItem {
    InherentImplementation,
    ModuleDeclaration,
    FunctionDeclaration,
    MinecraftFunctionDeclaration(ResourceLocation, SemanticExpression),
    TypeAliasDeclaration,
    RegularStructDeclaration,
    TupleStructDeclaration,
    ConstantDeclaration {
        id: HighConstantId,
        data_type: SemanticDataType,
        value: SemanticExpression,
    },
    Use,
}

impl SemanticItem {
    pub fn compile(self, datapack: &mut Datapack) {
        match self {
            Self::InherentImplementation => {}
            Self::ModuleDeclaration => {}
            Self::FunctionDeclaration => {}
            Self::MinecraftFunctionDeclaration(id, expression) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    expression
                        .kind
                        .compile_as_statement(datapack, &mut function_ctx);

                    let function_commands = function_ctx.compile();

                    datapack
                        .current_namespace_mut()
                        .current_function_mut()
                        .add_commands(function_commands);

                    datapack.pop_function_from_current_namespace();
                });
            }
            Self::TypeAliasDeclaration => {}
            Self::RegularStructDeclaration => {}
            Self::TupleStructDeclaration => {}
            Self::ConstantDeclaration {
                id,
                data_type,
                value,
            } => {
                let data_type = data_type.resolve(datapack).unwrap();

                let value = value.kind.resolve_constant(datapack);

                datapack.declare_constant(id, data_type, value);
            }
            Self::Use => {}
        }
    }
}
