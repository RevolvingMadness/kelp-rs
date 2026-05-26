use std::fmt::{Display, Write};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    typed::arena::TypedAstArena,
    typed::coordinate::{TypedCoordinates, TypedWorldCoordinate},
};

#[derive(Debug, Clone)]
pub enum WorldCoordinate {
    Relative(Option<ParsedExpressionId>),
    Absolute(ParsedExpressionId),
}

impl Display for WorldCoordinate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Relative(expression) => {
                f.write_char('~')?;

                if expression.is_some() {
                    f.write_str("...")?;
                }

                Ok(())
            }
            Self::Absolute(..) => f.write_str("..."),
        }
    }
}

impl WorldCoordinate {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedWorldCoordinate> {
        match self {
            Self::Relative(expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let expression = ParsedExpression::perform_semantic_analysis(
                            expression,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        Some(expression)
                    }
                    None => None,
                };

                Some(TypedWorldCoordinate::Relative(expression))
            }
            Self::Absolute(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                Some(TypedWorldCoordinate::Absolute(expression))
            }
        }
    }
}

pub type LocalCoordinate = Option<ParsedExpressionId>;

#[derive(Debug, Clone)]
pub enum Coordinates {
    World(WorldCoordinate, WorldCoordinate, WorldCoordinate),
    Local(LocalCoordinate, LocalCoordinate, LocalCoordinate),
}

impl Display for Coordinates {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::World(x, y, z) => {
                x.fmt(f)?;

                f.write_char(' ')?;

                y.fmt(f)?;

                f.write_char(' ')?;

                z.fmt(f)?;

                Ok(())
            }
            Self::Local(x, y, z) => {
                f.write_char('^')?;

                if x.is_some() {
                    f.write_str("...")?;
                }

                f.write_str(" ^")?;

                if y.is_some() {
                    f.write_str("...")?;
                }

                f.write_str(" ^")?;

                if z.is_some() {
                    f.write_str("...")?;
                }

                Ok(())
            }
        }
    }
}

impl Coordinates {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedCoordinates> {
        Some(match self {
            Self::World(x, y, z) => {
                let x = x.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;
                let y = y.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;
                let z = z.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCoordinates::World(x, y, z)
            }
            Self::Local(x, y, z) => {
                let x = match x {
                    Some(x) => {
                        let x_span = parsed_arena.get_expression_span(x);

                        let x = ParsedExpression::perform_semantic_analysis(
                            x,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        let x_type = typed_arena.get_expression_type(x);

                        if !x_type.can_be_represented_as_snbt_float_macro() {
                            return ctx.add_error(
                                x_span,
                                SemanticAnalysisError::CannotBeRepresentedAsFloat(x_type.clone()),
                            );
                        }

                        Some(x)
                    }
                    None => None,
                };

                let y = match y {
                    Some(y) => {
                        let y_span = parsed_arena.get_expression_span(y);

                        let y = ParsedExpression::perform_semantic_analysis(
                            y,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        let y_type = typed_arena.get_expression_type(y);

                        if !y_type.can_be_represented_as_snbt_float_macro() {
                            return ctx.add_error(
                                y_span,
                                SemanticAnalysisError::CannotBeRepresentedAsFloat(y_type.clone()),
                            );
                        }

                        Some(y)
                    }
                    None => None,
                };

                let z = match z {
                    Some(z) => {
                        let z_span = parsed_arena.get_expression_span(z);

                        let z = ParsedExpression::perform_semantic_analysis(
                            z,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        let z_type = typed_arena.get_expression_type(z);

                        if !z_type.can_be_represented_as_snbt_float_macro() {
                            return ctx.add_error(
                                z_span,
                                SemanticAnalysisError::CannotBeRepresentedAsFloat(z_type.clone()),
                            );
                        }

                        Some(z)
                    }
                    None => None,
                };

                TypedCoordinates::Local(x, y, z)
            }
        })
    }
}
