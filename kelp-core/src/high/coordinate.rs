use std::fmt::{Display, Write};

use crate::{
    high::{
        expression::Expression,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::coordinate::{
        Coordinates as MiddleCoordinates, WorldCoordinate as MiddleWorldCoordinate,
    },
};

#[derive(Debug, Clone)]
pub struct WorldCoordinate {
    pub relative: bool,
    pub value: Option<Expression>,
}

impl Display for WorldCoordinate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.relative {
            f.write_str("~")?;
        }

        if self.value.is_some() {
            f.write_str("...")?;
        }

        Ok(())
    }
}

impl WorldCoordinate {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleWorldCoordinate> {
        let value = match self.value {
            Some(value) => {
                let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                if !value.data_type.can_be_represented_as_snbt_float_macro() {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::CannotBeRepresentedAsFloat(value.data_type),
                    );
                }

                Some(value)
            }
            None => None,
        };

        Some(MiddleWorldCoordinate {
            relative: self.relative,
            value,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Coordinates {
    World(WorldCoordinate, WorldCoordinate, WorldCoordinate),
    Local(Option<Expression>, Option<Expression>, Option<Expression>),
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleCoordinates> {
        Some(match self {
            Self::World(x, y, z) => {
                let x = x.perform_semantic_analysis(ctx)?;
                let y = y.perform_semantic_analysis(ctx)?;
                let z = z.perform_semantic_analysis(ctx)?;

                MiddleCoordinates::World(x, y, z)
            }
            Self::Local(x, y, z) => {
                let x = match x {
                    Some(x) => {
                        let (x_span, x) = x.perform_semantic_analysis(ctx)?;

                        if !x.data_type.can_be_represented_as_snbt_float_macro() {
                            return ctx.add_error(
                                x_span,
                                SemanticAnalysisError::CannotBeRepresentedAsFloat(x.data_type),
                            );
                        }

                        Some(x)
                    }
                    None => None,
                };

                let y = match y {
                    Some(y) => {
                        let (y_span, y) = y.perform_semantic_analysis(ctx)?;

                        if !y.data_type.can_be_represented_as_snbt_float_macro() {
                            return ctx.add_error(
                                y_span,
                                SemanticAnalysisError::CannotBeRepresentedAsFloat(y.data_type),
                            );
                        }

                        Some(y)
                    }
                    None => None,
                };

                let z = match z {
                    Some(z) => {
                        let (z_span, z) = z.perform_semantic_analysis(ctx)?;

                        if !z.data_type.can_be_represented_as_snbt_float_macro() {
                            return ctx.add_error(
                                z_span,
                                SemanticAnalysisError::CannotBeRepresentedAsFloat(z.data_type),
                            );
                        }

                        Some(z)
                    }
                    None => None,
                };

                MiddleCoordinates::Local(x, y, z)
            }
        })
    }
}
