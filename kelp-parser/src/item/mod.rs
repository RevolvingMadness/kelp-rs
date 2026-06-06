use kelp_core::{
    parsed::item::{ParsedItem, ParsedItemKind},
    visibility::Visibility,
};

use crate::{
    cst::{
        CSTConstantDeclarationItem, CSTFunctionDeclarationItem, CSTInherentImplementationItem,
        CSTItem, CSTItemKind, CSTModuleDeclarationItem, CSTTypeAliasDeclarationItem, CSTUseItem,
    },
    extension_traits::{
        AstNodeExt, LowerableAstNode, ParsableAstNode, RecoverableAstNode, SyntaxTokenExt,
    },
    item::{
        minecraft_function_declaration::try_parse_minecraft_function_declaration_item_kind,
        struct_declaration::try_parse_struct_declaration_item_kind,
    },
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod associated;
pub mod constant_declaration;
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
            "impl" => CSTInherentImplementationItem::try_parse(parser),
            "use" => CSTUseItem::try_parse(parser),
            "mod" => CSTModuleDeclarationItem::try_parse(parser),
            "recursive" | "runtime" | "fn" => CSTFunctionDeclarationItem::try_parse(parser),
            "mcfn" => try_parse_minecraft_function_declaration_item_kind(parser),
            "struct" => try_parse_struct_declaration_item_kind(parser),
            "type" => CSTTypeAliasDeclarationItem::try_parse(parser),
            "const" => CSTConstantDeclarationItem::try_parse(parser),
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
                    parser.error("expected item");
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

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::InherentImplementationItem(node) => node.lower(ctx),
            Self::ModuleDeclarationItem(node) => node.lower(ctx),
            Self::FunctionDeclarationItem(node) => node.lower(ctx),
            Self::MinecraftFunctionDeclarationItem(node) => node.lower(ctx),
            Self::RegularStructDeclarationItem(node) => {
                let name_token = node.name()?;
                let name_span = name_token.span();
                let name = name_token.text();

                let generics = node.generic_names().and_then(|names| names.lower(ctx));

                let field_types = node
                    .regular_struct_fields()
                    .and_then(|fields| fields.lower(ctx));

                Some(ParsedItemKind::RegularStructDeclaration {
                    name_span,
                    name: name.to_owned(),
                    generics: generics.unwrap_or_default(),
                    field_types: field_types.unwrap_or_default(),
                })
            }
            Self::TupleStructDeclarationItem(node) => {
                let name_token = node.name()?;
                let name_span = name_token.span();
                let name = name_token.text();

                let generics = node.generic_names().and_then(|names| names.lower(ctx));

                let field_types = node
                    .tuple_struct_fields()
                    .and_then(|fields| fields.lower(ctx));

                Some(ParsedItemKind::TupleStructDeclaration {
                    name_span,
                    name: name.to_owned(),
                    generics: generics.unwrap_or_default(),
                    field_types: field_types.unwrap_or_default(),
                })
            }
            Self::TypeAliasDeclarationItem(node) => node.lower(ctx),
            Self::ConstantDeclarationItem(node) => node.lower(ctx),
            Self::UseItem(node) => node.lower(ctx),
        }
    }
}

impl LowerableAstNode for CSTItem {
    type Lowered = ParsedItem;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
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
