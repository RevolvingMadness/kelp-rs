use minecraft_command_types::{
    command::{
        Command,
        enums::{numeric_snbt_type::NumericSNBTType, store_type::StoreType},
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::NbtPath,
    snbt::SNBTString,
};
use ordered_float::NotNan;
use parser_rs::parser_range::ParserRange;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::{
        Expression, ExpressionKind,
        constant::{ConstantExpression, ConstantExpressionKind},
    },
    high::{
        data::GeneratedDataTarget, player_score::GeneratedPlayerScore, snbt_string::HighSNBTString,
    },
    operator::ArithmeticOperator,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug)]
pub enum Place {
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
    Variable(String),
    Tuple(Vec<Place>),
    Dereference(Box<Expression>),
    Field(Box<ConstantExpressionKind>, SNBTString),
    Underscore,
}

impl Place {
    pub fn assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        value: ConstantExpressionKind,
    ) {
        match self {
            Place::Score(score) => {
                value.assign_to_score(datapack, ctx, score);
            }
            Place::Data(target, path) => {
                value.assign_to_data(datapack, ctx, target, path);
            }
            Place::Variable(name) => {
                datapack.assign_variable(&name, value.into_dummy_constant_expression());
            }
            Place::Underscore => {}
            Place::Tuple(places) => {
                if let ConstantExpressionKind::Tuple(values) = value {
                    if places.len() != values.len() {
                        return;
                    }

                    let safe_values: Vec<ConstantExpression> = values
                        .into_iter()
                        .map(|v| match v.kind {
                            ConstantExpressionKind::PlayerScore(_) => {
                                let unique = v.kind.as_score(datapack, ctx, true);
                                ConstantExpressionKind::PlayerScore(unique)
                                    .into_dummy_constant_expression()
                            }
                            ConstantExpressionKind::Data(_, _) => {
                                let (t, p) = v.kind.as_data_force(datapack, ctx);
                                ConstantExpressionKind::Data(t, p).into_dummy_constant_expression()
                            }
                            _ => v,
                        })
                        .collect();

                    for (place, val) in places.into_iter().zip(safe_values) {
                        place.assign(datapack, ctx, val.kind);
                    }
                } else {
                    unreachable!()
                }
            }
            Place::Dereference(expression) => {
                expression
                    .dereference(datapack)
                    .as_place(datapack, ctx)
                    .assign(datapack, ctx, value);
            }
            Place::Field(expression, field) => {
                if expression.is_lvalue() {
                    expression
                        .access_field(field.1)
                        .as_place()
                        .assign(datapack, ctx, value);
                } else {
                    let mut new_container = *expression.clone();

                    match &mut new_container {
                        ConstantExpressionKind::Struct(_, _, fields) => {
                            fields.insert(field.1, value.into_dummy_constant_expression());
                        }
                        ConstantExpressionKind::Compound(fields) => {
                            fields.insert(
                                HighSNBTString {
                                    span: ParserRange::dummy(),
                                    snbt_string: SNBTString(false, field.1),
                                },
                                value.into_dummy_constant_expression(),
                            );
                        }
                        _ => unreachable!("Cannot assign field to {:?}", new_container),
                    }

                    expression.as_place().assign(datapack, ctx, new_container);
                }
            }
        }
    }

    pub fn augmented_assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ConstantExpressionKind,
    ) {
        match self {
            Place::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Place::Data(target, path) => {
                let unique_score = datapack.get_unique_score();

                ConstantExpressionKind::Data(target.clone(), path.clone()).assign_to_score(
                    datapack,
                    ctx,
                    unique_score.clone(),
                );

                unique_score
                    .clone()
                    .assign_augmented(datapack, ctx, operator, value);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Data(
                            target.target,
                            path,
                            NumericSNBTType::Integer,
                            NotNan::new(1.0).unwrap(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Scoreboard(
                                ScoreboardCommand::Players(PlayersScoreboardCommand::Get(
                                    unique_score.score,
                                )),
                            )))),
                        ),
                    )),
                );
            }
            Place::Variable(name) => {
                let (_, variable_value) = datapack.get_variable(&name).unwrap();

                if variable_value.kind.is_lvalue() {
                    variable_value
                        .kind
                        .compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_kind = variable_value
                        .kind
                        .perform_arithmetic(datapack, ctx, operator, value);

                    datapack.get_variable_mut(&name).unwrap().kind = new_kind;
                }
            }
            Place::Field(expression, field) => {
                let field_value = expression.clone().access_field(field.1.clone());

                if field_value.is_lvalue() {
                    field_value.compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_field_value =
                        field_value.perform_arithmetic(datapack, ctx, operator, value);

                    let mut new_container = *expression.clone();

                    match &mut new_container {
                        ConstantExpressionKind::Struct(_, _, fields) => {
                            fields
                                .insert(field.1, new_field_value.into_dummy_constant_expression());
                        }
                        ConstantExpressionKind::Compound(fields) => {
                            fields.insert(
                                HighSNBTString {
                                    span: ParserRange::dummy(),
                                    snbt_string: SNBTString(false, field.1),
                                },
                                new_field_value.into_dummy_constant_expression(),
                            );
                        }
                        _ => unreachable!("Cannot assign field to {:?}", new_container),
                    }

                    expression.as_place().assign(datapack, ctx, new_container);
                }
            }
            Place::Tuple(_) | Place::Underscore => {
                unreachable!()
            }
            Place::Dereference(expression) => {
                expression
                    .dereference(datapack)
                    .as_place(datapack, ctx)
                    .augmented_assign(datapack, ctx, operator, value);
            }
        }
    }
}

#[derive(Debug)]
pub enum PlaceTypeKind {
    Score,
    Data(DataTypeKind),
    Tuple(Vec<PlaceType>, DataTypeKind),
    Variable(DataTypeKind),
    Underscore,
}

impl PlaceTypeKind {
    pub fn with_span(self, span: ParserRange) -> PlaceType {
        PlaceType { span, kind: self }
    }
}

#[derive(Debug)]
pub struct PlaceType {
    pub span: ParserRange,
    pub kind: PlaceTypeKind,
}

impl PlaceType {
    pub fn perform_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value: Expression,
        value_type: &DataTypeKind,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Score => {
                if !value_type.is_score_value() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToScore(value_type.clone()),
                        ),
                    });
                }
            }
            PlaceTypeKind::Data(data_type) => {
                if !value_type.equals(&data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: data_type.clone(),
                                actual: value_type.clone(),
                            },
                        ),
                    });
                }
            }
            PlaceTypeKind::Tuple(place_types, _) => {
                if let ExpressionKind::Tuple(expressions) = value.kind {
                    assert!(expressions.len() == place_types.len());

                    return place_types
                        .into_iter()
                        .zip(expressions)
                        .map(|(place_type, value)| {
                            let value_type = value.kind.infer_data_type(ctx)?;

                            place_type.perform_assignment_semantic_analysis(ctx, value, &value_type)
                        })
                        .all_some();
                } else {
                    unreachable!()
                }
            }
            PlaceTypeKind::Variable(data_type) => {
                data_type.perform_equality_semantic_analysis(ctx, value_type, &value)?;
            }
            PlaceTypeKind::Underscore => {}
        }

        Some(())
    }

    pub fn perform_augmented_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        operator: &ArithmeticOperator,
        value: Expression,
        value_type: &DataTypeKind,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Score => {
                if !value_type.is_score_value() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToScore(value_type.clone()),
                        ),
                    });
                }
            }
            PlaceTypeKind::Data(data_type) => {
                if !value_type.can_perform_augmented_assignment(operator, &data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::InvalidAugmentedAssignmentType(
                                *operator,
                                data_type.clone(),
                                value_type.clone(),
                            ),
                        ),
                    });
                }
            }
            PlaceTypeKind::Tuple(_, data_type) => {
                return ctx.add_info(SemanticAnalysisInfo {
                    span: self.span,
                    kind: SemanticAnalysisInfoKind::Error(
                        SemanticAnalysisError::CannotPerformAugmentedAssignment(data_type),
                    ),
                });
            }
            PlaceTypeKind::Variable(data_type) => {
                if !data_type.can_perform_augmented_assignment(operator, value_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformAugmentedAssignment(data_type),
                        ),
                    });
                }
            }
            PlaceTypeKind::Underscore => {}
        }

        Some(())
    }
}
