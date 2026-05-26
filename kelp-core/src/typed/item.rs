use la_arena::Idx;
use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::expression::{TypedExpression, TypedExpressionId},
};

#[derive(Debug, Clone)]
pub enum TypedItem {
    InherentImplementation,
    ModuleDeclaration,
    FunctionDeclaration,
    MinecraftFunctionDeclaration(ResourceLocation, TypedExpressionId),
    TypeAliasDeclaration,
    RegularStructDeclaration,
    TupleStructDeclaration,
    Use,
}

impl TypedItem {
    pub fn compile(
        id: Idx<Self>,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        _ctx: &mut CompileContext,
    ) {
        match arena.get_item(id) {
            Self::InherentImplementation => {}
            Self::ModuleDeclaration => {}
            Self::FunctionDeclaration => {}
            Self::MinecraftFunctionDeclaration(id, expression) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    TypedExpression::compile_as_statement(
                        *expression,
                        arena,
                        datapack,
                        &mut function_ctx,
                    );

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
            Self::Use => {}
        }
    }
}
