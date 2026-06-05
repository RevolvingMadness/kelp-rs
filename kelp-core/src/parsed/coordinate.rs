use std::fmt::{Display, Write};

use crate::{
    parsed::{
        expression::ParsedExpression,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    semantic::coordinate::{SemanticCoordinates, SemanticWorldCoordinate},
};

#[derive(Debug, Clone)]
pub enum ParsedWorldCoordinate {
    Relative(Option<ParsedExpression>),
    Absolute(ParsedExpression),
}

impl Display for ParsedWorldCoordinate {
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

impl ParsedWorldCoordinate {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticWorldCoordinate> {
        match self {
            Self::Relative(expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    }
                    None => None,
                };

                Some(SemanticWorldCoordinate::Relative(expression))
            }
            Self::Absolute(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                Some(SemanticWorldCoordinate::Absolute(expression))
            }
        }
    }
}

pub type ParsedLocalCoordinate = Option<ParsedExpression>;

#[derive(Debug, Clone)]
pub enum ParsedCoordinates {
    World(
        ParsedWorldCoordinate,
        ParsedWorldCoordinate,
        ParsedWorldCoordinate,
    ),
    Local(
        ParsedLocalCoordinate,
        ParsedLocalCoordinate,
        ParsedLocalCoordinate,
    ),
}

impl Display for ParsedCoordinates {
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

impl ParsedCoordinates {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticCoordinates> {
        Some(match self {
            Self::World(x, y, z) => {
                let x = x.perform_semantic_analysis(ctx)?;
                let y = y.perform_semantic_analysis(ctx)?;
                let z = z.perform_semantic_analysis(ctx)?;

                SemanticCoordinates::World(x, y, z)
            }
            Self::Local(x, y, z) => {
                let x = x.map(|x| x.perform_semantic_analysis(ctx));
                let y = y.map(|y| y.perform_semantic_analysis(ctx));
                let z = z.map(|z| z.perform_semantic_analysis(ctx));

                let x = match x {
                    Some(Some((_, x))) => Some(x),
                    Some(None) => return None,
                    None => None,
                };

                let y = match y {
                    Some(Some((_, y))) => Some(y),
                    Some(None) => return None,
                    None => None,
                };

                let z = match z {
                    Some(Some((_, z))) => Some(z),
                    Some(None) => return None,
                    None => None,
                };

                SemanticCoordinates::Local(x, y, z)
            }
        })
    }
}
