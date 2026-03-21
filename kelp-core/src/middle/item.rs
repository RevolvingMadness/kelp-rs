use minecraft_command_types::resource_location::ResourceLocation;

use crate::{compile_context::CompileContext, datapack::Datapack, middle::statement::Statement};

#[derive(Debug, Clone)]
pub enum Item {
    ModuleDeclaration,
    MCFNDeclaration(ResourceLocation, Box<Statement>),
    TypeAliasDeclaration,
    StructDeclaration,
}

impl Item {
    pub fn compile(self, datapack: &mut Datapack, _ctx: &mut CompileContext) {
        match self {
            Self::MCFNDeclaration(id, statement) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    statement.compile(datapack, &mut function_ctx);

                    let function_commands = function_ctx.compile();

                    datapack
                        .current_namespace_mut()
                        .current_function_mut()
                        .add_commands(function_commands);

                    datapack.pop_function_from_current_namespace();
                });
            }
            Self::ModuleDeclaration | Self::TypeAliasDeclaration | Self::StructDeclaration => {}
        }
    }
}
