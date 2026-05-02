use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::{
    collections::{HashMap, HashSet},
    env, fs,
    path::{Path, PathBuf},
};
use syn::parse_file;
use ungrammar::{Grammar, Rule};

fn main() {
    let grammar_path = PathBuf::from("kelp.ungram");
    if !grammar_path.exists() {
        return;
    }
    let grammar_src = fs::read_to_string(grammar_path).unwrap();
    let grammar: Grammar = grammar_src.parse().unwrap();

    let ast = lower(&grammar);

    let generated_code = generate_rust_code(&ast);

    let out_dir = env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir).join("cst.rs");
    fs::write(out_dir, generated_code).unwrap();

    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=kelp.ungram");
}

#[derive(Default)]
struct AstSrc {
    nodes: Vec<AstNodeSrc>,
    enums: Vec<AstEnumSrc>,
}

struct AstNodeSrc {
    name: String,
    fields: Vec<Field>,
}

struct AstEnumSrc {
    name: String,
    variants: Vec<String>,
}

#[derive(Clone, PartialEq, Eq)]
enum Cardinality {
    Optional,
    Many,
}

#[derive(Clone, PartialEq, Eq)]
enum Field {
    Token {
        name: String,
        label: Option<String>,
        cardinality: Cardinality,
    },
    Node {
        name: String,
        ty: String,
        cardinality: Cardinality,
    },
}

fn lower(grammar: &Grammar) -> AstSrc {
    let mut ast = AstSrc::default();

    for node_id in grammar.iter() {
        let node = &grammar[node_id];
        let name = node.name.clone();

        if let Some(variants) = lower_enum(grammar, &node.rule) {
            ast.enums.push(AstEnumSrc { name, variants });
        } else {
            let mut fields = Vec::new();
            lower_rule(&mut fields, grammar, None, &node.rule, false);

            fields.dedup_by(|a, b| a == b);

            ast.nodes.push(AstNodeSrc { name, fields });
        }
    }

    ast
}

fn lower_rule(
    acc: &mut Vec<Field>,
    grammar: &Grammar,
    label: Option<&String>,
    rule: &Rule,
    in_rep: bool,
) {
    match rule {
        Rule::Node(node_id) => {
            let ty = grammar[*node_id].name.clone();

            let mut found = false;
            for field in acc.iter_mut() {
                if let Field::Node {
                    name: existing_name,
                    ty: existing_ty,
                    cardinality,
                } = field
                {
                    let matches = label.map_or_else(
                        || existing_ty == &ty,
                        |l| *existing_name == *l || existing_name == &format!("{}s", l),
                    );

                    if matches {
                        *cardinality = Cardinality::Many;
                        if label.is_none() && !existing_name.ends_with('s') {
                            *existing_name = format!("{}s", existing_name);
                        }
                        found = true;
                        break;
                    }
                }
            }

            if !found {
                let base_name = label.cloned().unwrap_or_else(|| to_lower_snake_case(&ty));
                let name = if in_rep && label.is_none() {
                    format!("{}s", base_name)
                } else {
                    base_name
                };

                let cardinality = if in_rep {
                    Cardinality::Many
                } else {
                    Cardinality::Optional
                };

                acc.push(Field::Node {
                    name,
                    ty,
                    cardinality,
                });
            }
        }
        Rule::Token(token_id) => {
            let name = grammar[*token_id].name.clone();
            acc.push(Field::Token {
                name,
                label: label.cloned(),
                cardinality: if in_rep {
                    Cardinality::Many
                } else {
                    Cardinality::Optional
                },
            });
        }
        Rule::Rep(inner) => lower_rule(acc, grammar, label, inner, true),
        Rule::Labeled { label: l, rule } => lower_rule(acc, grammar, Some(l), rule, in_rep),
        Rule::Seq(rules) | Rule::Alt(rules) => {
            for r in rules {
                lower_rule(acc, grammar, label, r, in_rep);
            }
        }
        Rule::Opt(inner) => lower_rule(acc, grammar, label, inner, in_rep),
    }
}

fn lower_enum(grammar: &Grammar, rule: &Rule) -> Option<Vec<String>> {
    let Rule::Alt(alternatives) = rule else {
        return None;
    };

    let mut variants = Vec::new();
    for alt in alternatives {
        match alt {
            Rule::Node(it) => variants.push(grammar[*it].name.clone()),
            _ => return None,
        }
    }
    Some(variants)
}

fn generate_rust_code(ast: &AstSrc) -> String {
    let node_defs = ast.nodes.iter().map(generate_node);
    let enum_defs = ast.enums.iter().map(generate_enum);

    let file = quote! {
        use crate::syntax::{SyntaxNode, SyntaxToken, SyntaxKind};
        use rowan::{NodeOrToken, ast::{AstNode, AstChildren, support}};

        #(#node_defs)*
        #(#enum_defs)*
    };

    prettyplease::unparse(&parse_file(&file.to_string()).unwrap())
}

fn generate_node(node: &AstNodeSrc) -> TokenStream {
    let name = format_ident!("CST{}", node.name);
    let kind = format_ident!("{}", node.name);

    let mut token_groups: HashMap<String, (HashSet<String>, Cardinality)> = HashMap::new();
    let mut node_fields = Vec::new();

    for field in &node.fields {
        match field {
            Field::Token {
                name,
                label,
                cardinality,
            } => {
                let method_name = label.clone().unwrap_or_else(|| {
                    let kind_str = token_to_syntax_kind(name);
                    format!("{}_token", to_lower_snake_case(kind_str))
                });

                let entry = token_groups
                    .entry(method_name)
                    .or_insert_with(|| (HashSet::new(), Cardinality::Optional));
                entry.0.insert(name.clone());
                if *cardinality == Cardinality::Many {
                    entry.1 = Cardinality::Many;
                }
            }
            Field::Node { .. } => node_fields.push(field),
        }
    }

    let token_methods = token_groups
        .into_iter()
        .map(|(method_name, (tokens, cardinality))| {
            let method_ident = format_ident!("{}", method_name);
            let kinds: Vec<_> = tokens
                .iter()
                .map(|t| format_ident!("{}", token_to_syntax_kind(t)))
                .collect();

            match cardinality {
                Cardinality::Optional => quote! {
                    #[must_use]
                    pub fn #method_ident(&self) -> Option<SyntaxToken> {
                        self.syntax.children_with_tokens()
                            .filter_map(NodeOrToken::into_token)
                            .find(|it| matches!(it.kind(), #(SyntaxKind::#kinds)|*))
                    }
                },
                Cardinality::Many => quote! {
                    pub fn #method_ident(&self) -> impl Iterator<Item = SyntaxToken> {
                        self.syntax.children_with_tokens()
                            .filter_map(NodeOrToken::into_token)
                            .filter(move |it| matches!(it.kind(), #(SyntaxKind::#kinds)|*))
                    }
                },
            }
        });

    let mut ty_counts: HashMap<String, usize> = HashMap::new();
    for field in &node_fields {
        if let Field::Node { ty, .. } = field {
            *ty_counts.entry(ty.clone()).or_default() += 1;
        }
    }

    let mut ty_indices: HashMap<String, usize> = HashMap::new();

    let node_methods = node_fields.into_iter().map(|field| {
        if let Field::Node {
            name: field_name,
            ty,
            cardinality,
        } = field
        {
            let method_name = format_ident!("{}", field_name);
            let ty_ident = format_ident!("CST{}", ty);

            let count = ty_counts[ty];
            let index = *ty_indices.entry(ty.clone()).or_default();
            ty_indices.insert(ty.clone(), index + 1);

            match cardinality {
                Cardinality::Optional => {
                    if count > 1 {
                        quote! {
                            #[must_use]
                            pub fn #method_name(&self) -> Option<#ty_ident> {
                                support::children(&self.syntax).nth(#index)
                            }
                        }
                    } else {
                        quote! {
                            #[must_use]
                            pub fn #method_name(&self) -> Option<#ty_ident> {
                                support::child(&self.syntax)
                            }
                        }
                    }
                }
                Cardinality::Many => quote! {
                    #[must_use]
                    pub fn #method_name(&self) -> AstChildren<#ty_ident> {
                        support::children(&self.syntax)
                    }
                },
            }
        } else {
            unreachable!()
        }
    });

    quote! {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct #name {
            pub(crate) syntax: SyntaxNode,
        }

        impl AstNode for #name {
            type Language = crate::syntax::KelpLanguage;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::#kind
            }
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
            }
            fn syntax(&self) -> &SyntaxNode { &self.syntax }
        }

        impl #name {
            #(#token_methods)*
            #(#node_methods)*
        }
    }
}

fn generate_enum(enm: &AstEnumSrc) -> TokenStream {
    let name = format_ident!("CST{}", enm.name);

    let variants: Vec<_> = enm
        .variants
        .iter()
        .map(|v| format_ident!("{}", v))
        .collect();

    let cst_variants: Vec<_> = enm
        .variants
        .iter()
        .map(|v| format_ident!("CST{}", v))
        .collect();

    quote! {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum #name {
            #(#variants(#cst_variants)),*
        }

        impl AstNode for #name {
            type Language = crate::syntax::KelpLanguage;

            fn can_cast(kind: SyntaxKind) -> bool {
                #( <#cst_variants as AstNode>::can_cast(kind) )||*
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                let res = match syntax.kind() {
                    #(
                        kind if <#cst_variants as AstNode>::can_cast(kind) => {
                            Self::#variants(<#cst_variants as AstNode>::cast(syntax)?)
                        }
                    )*
                    _ => return None,
                };
                Some(res)
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    #( Self::#variants(it) => it.syntax(), )*
                }
            }
        }
    }
}

fn to_lower_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_');
        }
        prev = true;
        buf.push(c.to_ascii_lowercase());
    }
    buf
}

fn token_to_syntax_kind(token: &str) -> &str {
    match token {
        "@" => "At",
        "~" => "Tilde",
        "^" => "Caret",
        ":" => "Colon",
        "{" => "LeftBrace",
        "}" => "RightBrace",
        "[" => "LeftBracket",
        "]" => "RightBracket",
        "(" => "LeftParenthesis",
        ")" => "RightParenthesis",
        "=" => "Equal",
        "+=" => "PlusEqual",
        "-=" => "MinusEqual",
        "*=" => "StarEqual",
        "/=" => "ForwardSlashEqual",
        "%=" => "PercentEqual",
        "&=" => "AmpersandEqual",
        "|=" => "PipeEqual",
        "<<=" => "LeftArrowLeftArrowEqual",
        ">>=" => "RightArrowRightArrowEqua",
        "><" => "RightArrowLeftArrow",
        "!" => "ExclamationMark",
        "," => "Comma",
        "." => "Period",
        "'" => "SingleQuote",
        "\"" => "DoubleQuote",
        "#" => "Pound",
        "+" => "Plus",
        "-" => "Minus",
        "*" => "Star",
        "/" => "ForwardSlash",
        "\\" => "BackwardSlash",
        "&" => "Ampersand",
        "%" => "Percent",
        "$" => "DollarSign",
        "_" => "Underscore",
        ";" => "Semicolon",
        "<" => "LeftArrow",
        "<<" => "LeftArrowLeftArrow",
        ">" => "RightArrow",
        ">>" => "RightArrowRightArrow",
        "?" => "QuestionMark",
        "|" => "Pipe",
        "`" => "Backtick",
        "&&" => "AmpersandAmpersand",
        "||" => "PipePipe",
        "==" => "EqualEqual",
        "!=" => "ExclamationMarkEqual",
        ">=" => "RightArrowEqual",
        "<=" => "LeftArrowEqual",
        "score" => "ScoreKeyword",
        "entity" => "EntityKeyword",
        "block" => "BlockKeyword",
        "storage" => "StorageKeyword",
        "ident" => "Identifier",
        "tellraw" => "TellrawKeyword",
        "function" => "FunctionKeyword",
        "fractional_value" => "FractionalValue",
        "whole_value" => "WholeValue",
        "if" => "IfKeyword",
        "while" => "WhileKeyword",
        "else" => "ElseKeyword",
        "let" => "LetKeyword",
        "string_literal" => "StringLiteral",
        "character_literal" => "CharacterLiteral",
        "mcfn" => "MCFNKeyword",
        "to" => "ToKeyword",
        "as" => "AsKeyword",
        "entity_selector_variable" => "EntitySelectorVariable",
        "runtime_storage_type" => "RuntimeStorageType",
        "struct" => "StructKeyword",
        "data_type_name" => "DataTypeName",
        "struct_field_name" => "StructFieldName",
        "type_name" => "TypeName",
        "compound_key_name" => "CompoundKeyName",
        "compound_name" => "CompoundName",
        "binding_pattern_name" => "BindingPatternName",
        "named_nbt_path_node_name" => "NamedNBTPathNodeName",
        "type" => "TypeKeyword",
        "typed_compound_data_type_field" => "TypedCompoundDataTypeField",
        "typed_compound_data_type_field_name" => "TypedCompoundDataTypeFieldName",
        "compound_key" => "CompoundKey",
        "return" => "ReturnKeyword",
        "field_name" => "FieldName",
        "break" => "BreakKeyword",
        "continue" => "ContinueKeyword",
        "true" => "TrueKeyword",
        "false" => "FalseKeyword",
        "for" => "ForKeyword",
        "in" => "InKeyword",
        "loop" => "LoopKeyword",
        "append" => "AppendKeyword",
        "remove" => "RemoveKeyword",
        "numerical_suffix" => "NumericExpressionSuffix",
        "stopwatch" => "StopwatchKeyword",
        "create" => "CreateKeyword",
        "query" => "QueryKeyword",
        "restart" => "RestartKeyword",
        "scoreboard_objective" => "ScoreboardObjective",
        "::" => "ColonColon",
        "path_identifier" => "PathIdentifier",
        "mod" => "ModKeyword",
        "module_name" => "ModuleName",
        "use" => "UseKeyword",
        "pub" => "PubKeyword",
        "resource_location" => "ResourceLocationKeyword",
        "entity_selector" => "EntitySelectorKeyword",
        "fn" => "FNKeyword",
        "->" => "MinusRightArrow",
        "const" => "ConstKeyword",
        _ => todo!("{:?}", token),
    }
}
