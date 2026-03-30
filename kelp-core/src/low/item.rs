use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    low::expression::unresolved::UnresolvedExpression,
};

#[derive(Debug, Clone)]
pub enum Item {
    ModuleDeclaration,
    MCFNDeclaration(ResourceLocation, UnresolvedExpression),
    TypeAliasDeclaration,
    StructStructDeclaration,
    TupleStructDeclaration,
    Use,
}

impl Item {
    pub fn compile(self, datapack: &mut Datapack, _ctx: &mut CompileContext) {
        match self {
            Self::MCFNDeclaration(id, expression) => {
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
            Self::ModuleDeclaration
            | Self::TypeAliasDeclaration
            | Self::StructStructDeclaration
            | Self::TupleStructDeclaration
            | Self::Use => {}
        }
    }
}
