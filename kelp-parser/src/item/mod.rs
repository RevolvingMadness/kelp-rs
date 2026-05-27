use kelp_core::{
    parsed::item::{ParsedItem, ParsedItemKind},
    visibility::Visibility,
};

use crate::{
    cst::{CSTItem, CSTItemKind, CSTTypeAliasDeclarationItem},
    data_type::generics::lower_generic_names,
    extension_traits::{
        AstNodeExt, LowerableAstNode, ParsableAstNode, RecoverableAstNode, SyntaxTokenExt,
    },
    item::{
        function_declaration::{
            lower_function_declaration_item_kind, try_parse_function_declaration_item_kind,
        },
        implementation::inherent::{
            lower_inherent_implementation_item, try_parse_inherent_implementation_item_kind,
        },
        minecraft_function_declaration::{
            lower_minecraft_function_declaration_item_kind,
            try_parse_minecraft_function_declaration_item_kind,
        },
        module_declaration::{
            lower_module_declaration_item, try_parse_module_declaration_item_kind,
        },
        struct_declaration::try_parse_struct_declaration_item_kind,
        r#use::{lower_use_item, try_parse_use_item_kind},
    },
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod associated;
pub mod function_declaration;
pub mod implementation;
pub mod minecraft_function_declaration;
pub mod module_declaration;
pub mod struct_declaration;
pub mod type_alias_declaration;
pub mod r#use;

impl ParsableAstNode for CSTItemKind {
    fn try_parse(parser: &mut Parser) -> bool {
        let Some(identifier) = parser.peek_identifier() else {
            return false;
        };

        match identifier {
            "impl" => try_parse_inherent_implementation_item_kind(parser),
            "use" => try_parse_use_item_kind(parser),
            "mod" => try_parse_module_declaration_item_kind(parser),
            "recursive" | "runtime" | "fn" => try_parse_function_declaration_item_kind(parser),
            "mcfn" => try_parse_minecraft_function_declaration_item_kind(parser),
            "struct" => try_parse_struct_declaration_item_kind(parser),
            "type" => CSTTypeAliasDeclarationItem::try_parse(parser),
            _ => false,
        }
    }
}

fn recover_item(parser: &mut Parser, error_message: &str) {
    let bytes = &parser.source.as_bytes()[parser.pos..];

    let (not_end, length) = bytes
        .iter()
        .position(|&b| b == b'\n' || b == b';')
        .map_or((false, bytes.len()), |value| (true, value));

    let length = length + usize::from(not_end);

    parser.error_with_len(error_message, length);
    parser.add_token(SyntaxKind::Error, length);
}

impl ParsableAstNode for CSTItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        let parsed_visibility = if parser.try_parse_identifier_kind("pub", SyntaxKind::PubKeyword) {
            Some(parser.expect_whitespace())
        } else {
            None
        };

        let parsed_item_kind = CSTItemKind::try_parse(parser);

        if !parsed_item_kind {
            match parsed_visibility {
                Some(true) => {
                    parser.error("Expected item");
                }
                Some(false) => {}
                None => {
                    return false;
                }
            }
        }

        marker.start_node(parser, SyntaxKind::Item);

        parser.finish_node();

        true
    }

    fn expect(parser: &mut Parser, message: &str) -> bool {
        Self::recover_expect(parser, message)
    }
}

impl RecoverableAstNode for CSTItem {
    fn recover(parser: &mut Parser) {
        parser.bump_until_char(&['{', ';', '\n']);
    }
}

impl LowerableAstNode for CSTItemKind {
    type Lowered = ParsedItemKind;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::InherentImplementationItem(node) => lower_inherent_implementation_item(node, ctx),
            Self::ModuleDeclarationItem(node) => lower_module_declaration_item(node, ctx),
            Self::FunctionDeclarationItem(node) => lower_function_declaration_item_kind(node, ctx),
            Self::MinecraftFunctionDeclarationItem(node) => {
                lower_minecraft_function_declaration_item_kind(node, ctx)
            }
            Self::RegularStructDeclarationItem(node) => {
                let name_token = node.name()?;
                let name_span = name_token.span();
                let name = name_token.text();

                let generic_names = node.generic_names().and_then(lower_generic_names);

                let field_types = node.struct_fields().and_then(|fields| fields.lower(ctx));

                Some(ParsedItemKind::RegularStructDeclaration {
                    name_span,
                    name: name.to_owned(),
                    generic_names: generic_names.unwrap_or_default(),
                    field_types: field_types.unwrap_or_default(),
                })
            }
            Self::TupleStructDeclarationItem(node) => {
                let name_token = node.name()?;
                let name_span = name_token.span();
                let name = name_token.text();

                let generic_names = node.generic_names().and_then(lower_generic_names);

                let field_types = node.tuple_fields().and_then(|fields| fields.lower(ctx));

                Some(ParsedItemKind::TupleStructDeclaration {
                    name_span,
                    name: name.to_owned(),
                    generic_names: generic_names.unwrap_or_default(),
                    field_types: field_types.unwrap_or_default(),
                })
            }
            Self::TypeAliasDeclarationItem(node) => node.lower(ctx),
            Self::UseItem(node) => lower_use_item(node),
        }
    }
}

impl LowerableAstNode for CSTItem {
    type Lowered = ParsedItem;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let visibility = if self.pub_token().is_some() {
            Visibility::Public
        } else {
            Visibility::None
        };

        let kind = self.item_kind()?.lower(ctx)?;

        Some(ParsedItem {
            span: self.span(),
            visibility,
            kind,
        })
    }
}
