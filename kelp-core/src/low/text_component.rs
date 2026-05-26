use std::{collections::BTreeMap, fmt::Write as _};

use minecraft_command_types::{
    coordinate::Coordinates,
    entity_selector::EntitySelector,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{data_type::DataType, environment::Environment},
};

pub fn format_generics(output: &mut String, generics: &[DataType], environment: &Environment) {
    if generics.is_empty() {
        return;
    }

    output.push('<');

    for (i, data_type) in generics.iter().enumerate() {
        if i != 0 {
            output.push_str(", ");
        }

        let _ = write!(output, "{}", data_type.display(environment));
    }

    output.push('>');
}

pub trait TextComponent {
    #[must_use]
    fn into_text_component(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> SNBT;
}

impl TextComponent for bool {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Byte(i8::from(self))
    }
}

impl TextComponent for i8 {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Byte(self)
    }
}

impl TextComponent for i16 {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Short(self)
    }
}

impl TextComponent for i32 {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Integer(self)
    }
}

impl TextComponent for i64 {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Long(self)
    }
}

impl TextComponent for NotNan<f32> {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Float(self)
    }
}

impl TextComponent for NotNan<f64> {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::Double(self)
    }
}

impl TextComponent for String {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::string(self)
    }
}

impl<T: TextComponent> TextComponent for Vec<T> {
    fn into_text_component(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> SNBT {
        SNBT::list(
            self.into_iter()
                .map(|value| value.into_text_component(datapack, ctx))
                .collect(),
        )
    }
}

impl<V: TextComponent> TextComponent for BTreeMap<String, V> {
    fn into_text_component(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> SNBT {
        SNBT::compound(
            self.into_iter()
                .map(|(key, value)| {
                    (
                        SNBTString(false, key),
                        value.into_text_component(datapack, ctx),
                    )
                })
                .collect(),
        )
    }
}

impl TextComponent for ResourceLocation {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::string(self)
    }
}

impl TextComponent for EntitySelector {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::string(self)
    }
}

impl TextComponent for Coordinates {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        SNBT::string(self)
    }
}
