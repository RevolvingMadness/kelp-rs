use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    middle::{
        data_type::DataType, data_type_declaration::DataTypeDeclarationKind, statement::Statement,
    },
};

#[derive(Debug, Clone)]
pub enum Item {
    MCFNDeclaration(ResourceLocation, Box<Statement>),
    TypeAliasDeclaration(String, Vec<String>, DataType),
    StructDeclaration(String, Vec<String>, HashMap<String, DataType>),
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
            Self::TypeAliasDeclaration(name, generics, alias) => {
                datapack.declare_data_type(
                    name.clone(),
                    DataTypeDeclarationKind::Alias {
                        name,
                        generics,
                        alias,
                    },
                );
            }
            Self::StructDeclaration(name, generics, fields) => {
                datapack.declare_data_type(
                    name.clone(),
                    DataTypeDeclarationKind::Struct {
                        name,
                        generics,
                        fields,
                    },
                );
            }
        }
    }
}
