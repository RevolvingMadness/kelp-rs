use la_arena::Idx;
use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::expression::{TypedExpression, TypedExpressionId},
};

#[derive(Debug, Clone)]
pub enum Item {
    InherentImplementation,
    ModuleDeclaration,
    FunctionDeclaration,
    MinecraftFunctionDeclaration(ResourceLocation, TypedExpressionId),
    TypeAliasDeclaration,
    RegularStructDeclaration,
    TupleStructDeclaration,
    Use,
}

impl Item {
    pub fn compile(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        _ctx: &mut CompileContext,
    ) {
        match allocator.get_item(id) {
            Self::InherentImplementation => {}
            Self::ModuleDeclaration => {}
            Self::FunctionDeclaration => {}
            Self::MinecraftFunctionDeclaration(id, expression) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    TypedExpression::compile_as_statement(
                        *expression,
                        allocator,
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
