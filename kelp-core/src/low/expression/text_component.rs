use minecraft_command_types::{
    macroable::RegularMacroableExt,
    nbt_path::SNBTCompound,
    snbt::{SNBT, SNBTString},
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{expression::Expression, text_component::TextComponent},
};

impl TextComponent for Expression {
    fn into_text_component(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> SNBT {
        match self {
            Self::Score(player_score) => player_score.into_text_component(datapack, ctx),
            Self::Data(data) => data.into_text_component(datapack, ctx),
            Self::Boolean(value) => value.into_text_component(datapack, ctx),
            Self::Byte(value) => value.into_text_component(datapack, ctx),
            Self::Short(value) => value.into_text_component(datapack, ctx),
            Self::Integer(value) => value.into_text_component(datapack, ctx),
            Self::Long(value) => value.into_text_component(datapack, ctx),
            Self::Float(value) => value.into_text_component(datapack, ctx),
            Self::Double(value) => value.into_text_component(datapack, ctx),
            Self::String(string) => string.into_text_component(datapack, ctx),
            Self::List(list) => list.into_text_component(datapack, ctx),
            Self::Compound(compound) => compound.into_text_component(datapack, ctx),
            Self::Tuple(tuple) => tuple.into_text_component(datapack, ctx),
            Self::Never => SNBT::string('!'),
            Self::Unit => SNBT::string("()"),
            Self::Reference(place) => place
                .resolve(datapack, ctx)
                .into_text_component(datapack, ctx),
            Self::Function(id) => id.into_text_component(datapack, ctx),
            Self::RegularStruct(_id, fields) => {
                // if force_display {
                //     // Maybe display full path?

                //     let (_, visibility, declaration) = datapack.get_regular_struct_type(id);

                //     let mut output = Vec::new();

                //     if visibility.should_display() {
                //         output.push(SNBT::string(format!("{} ", visibility)));
                //     }

                //     output.push(SNBT::string(if declaration.generic_types.is_empty() {
                //         if fields.is_empty() {
                //             format!("{} {{", declaration.name)
                //         } else {
                //             format!("{} {{ ", declaration.name)
                //         }
                //     } else {
                //         declaration.name.clone()
                //     }));

                //     if !declaration.generic_types.is_empty() {
                //         output.push(SNBT::string("<"));

                //         for (i, generic) in declaration.generic_types.iter().enumerate() {
                //             if i != 0 {
                //                 output.push(SNBT::string(", "));
                //             }

                //             output.push(SNBT::string(generic.display(&datapack.environment)));
                //         }

                //         output.push(SNBT::string(if fields.is_empty() { "> {" } else { "> { " }));
                //     }

                //     for (i, (key, value)) in fields.iter().enumerate() {
                //         if i != 0 {
                //             output.push(SNBT::string(", "));
                //         }

                //         output.push(SNBT::string(format!("{}: ", key)));
                //         output.push(value.clone().into_text_component(datapack, ctx, true));
                //     }

                //     if fields.is_empty() {
                //         output.push(SNBT::string("}"));
                //     } else {
                //         output.push(SNBT::string(" }"));
                //     }

                //     SNBT::list(output)
                // } else {
                let mut output = SNBTCompound::new();

                for (field_name, field_value) in fields {
                    output.insert(
                        SNBTString(false, field_name),
                        field_value
                            .into_text_component(datapack, ctx)
                            .regular_macroable(),
                    );
                }

                SNBT::compound(output)
                // }
            }
            Self::TupleStruct(_id, fields) => {
                // if force_display {
                //     // Maybe display full path?

                //     let (_, visibility, declaration) = datapack.get_tuple_struct_type(id);

                //     let mut output = Vec::new();

                //     if visibility.should_display() {
                //         output.push(SNBT::string(format!("{} ", visibility)));
                //     }

                //     output.push(SNBT::string(if declaration.generic_types.is_empty() {
                //         format!("{}(", declaration.name)
                //     } else {
                //         declaration.name.clone()
                //     }));

                //     if !declaration.generic_types.is_empty() {
                //         output.push(SNBT::string("<"));

                //         for (i, generic) in declaration.generic_types.iter().enumerate() {
                //             if i != 0 {
                //                 output.push(SNBT::string(", "));
                //             }

                //             output.push(SNBT::string(generic.display(&datapack.environment)));
                //         }

                //         output.push(SNBT::string(">("));
                //     }

                //     for (i, value) in fields.iter().enumerate() {
                //         if i != 0 {
                //             output.push(SNBT::string(", "));
                //         }

                //         output.push(value.clone().into_text_component(datapack, ctx));
                //     }

                //     output.push(SNBT::string(")"));

                //     SNBT::list(output)
                // } else {
                let mut output = Vec::new();

                for field_value in fields {
                    output.push(
                        field_value
                            .into_text_component(datapack, ctx)
                            .regular_macroable(),
                    );
                }

                SNBT::list(output)
                // }
            }
            Self::ResourceLocation(resource_location) => {
                resource_location.into_text_component(datapack, ctx)
            }
            Self::EntitySelector(selector) => selector.into_text_component(datapack, ctx),
            Self::Coordinates(coordinates) => coordinates.into_text_component(datapack, ctx),
        }
    }
}
