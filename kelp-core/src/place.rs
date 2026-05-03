use minecraft_command_types::{
    command::{
        Command,
        enums::{numeric_snbt_type::NumericSNBTType, store_type::StoreType},
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::NbtPath,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    high::semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    low::{
        data_type::DataType,
        environment::value::{
            ValueDeclaration, ValueDeclarationKind, ValueId, variable::VariableId,
        },
        expression::resolved::ResolvedExpression,
    },
    operator::ArithmeticOperator,
    player_score::GeneratedPlayerScore,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum Place {
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
    Value(ValueId),
    Tuple(Vec<Self>),
    Underscore,
}

impl Place {
    pub fn dereference(self, datapack: &mut Datapack) -> Option<Self> {
        Some(match self {
            Self::Score(score) => Self::Score(score),
            Self::Data(target, path) => Self::Data(target, path),
            Self::Value(id) => {
                let declaration = datapack.get_value(id);

                match &declaration.kind {
                    ValueDeclarationKind::Variable(_) => datapack
                        .get_variable_value(VariableId(id.0))
                        .1
                        .clone()
                        .as_place()?,
                    ValueDeclarationKind::Function(_) => return None,
                }
            }
            Self::Tuple(_) | Self::Underscore => return None,
        })
    }

    pub fn assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Score(score) => {
                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target, path) => {
                value.assign_to_data(datapack, ctx, target, path);
            }
            Self::Value(id) => {
                let ValueDeclaration {
                    kind: ValueDeclarationKind::Variable(_),
                    ..
                } = datapack.get_value(id)
                else {
                    unreachable!("The expression '{:?}' cannot be assigned to", self);
                };

                datapack.set_variable(VariableId(id.0), value);
            }
            Self::Underscore => {}
            Self::Tuple(places) => {
                let ResolvedExpression::Tuple(expressions) = value else {
                    unreachable!();
                };

                assert!(places.len() == expressions.len());

                for (place, expression) in places.into_iter().zip(expressions) {
                    place.assign(datapack, ctx, expression);
                }
            }
        }
    }

    pub fn augmented_assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Self::Data(target, path) => {
                let unique_score = datapack.get_unique_score();

                ResolvedExpression::Data(Box::new((target.clone(), path.clone()))).assign_to_score(
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
            Self::Value(id) => {
                let ValueDeclaration {
                    kind: ValueDeclarationKind::Variable(_),
                    ..
                } = datapack.get_value(id)
                else {
                    return;
                };

                let id = VariableId(id.0);

                let variable_value = datapack.get_variable_value(id).1.clone();

                if variable_value.is_lvalue() {
                    variable_value.compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        variable_value.perform_arithmetic(datapack, ctx, operator, value);

                    datapack.set_variable(id, new_value);
                }
            }
            Self::Tuple(_) | Self::Underscore => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug)]
pub enum PlaceTypeKind {
    Score(DataType),
    Data(DataType),
    Tuple(Vec<PlaceType>),
    Value,
    Index(Box<PlaceType>),
    FieldAccess(Box<PlaceType>, Span, String),
    Dereference(Box<PlaceType>),
    Underscore,
}

#[derive(Debug)]
pub struct PlaceType {
    pub span: Span,
    pub data_type: DataType,
    pub kind: PlaceTypeKind,
}

impl PlaceType {
    #[inline]
    pub fn perform_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &DataType,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Score(_) => {
                if !value_type.is_score_compatible() {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::TypeIsNotScoreCompatible(value_type.clone()),
                    );
                }
            }
            PlaceTypeKind::Data(_) | PlaceTypeKind::Underscore => {}
            PlaceTypeKind::Tuple(place_types) => {
                let DataType::Tuple(data_types) = value_type else {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: self.data_type,
                            actual: value_type.clone(),
                        },
                    );
                };

                if data_types.len() != place_types.len() {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: self.data_type,
                            actual: value_type.clone(),
                        },
                    );
                }

                place_types
                    .into_iter()
                    .zip(data_types)
                    .map(|(place_type, data_type)| {
                        place_type.perform_assignment_semantic_analysis(ctx, value_span, data_type)
                    })
                    .run_all_succeeded()?;
            }
            PlaceTypeKind::Value => {
                value_type.assert_equals(ctx, value_span, &self.data_type)?;

                self.data_type
                    .assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;
            }
            PlaceTypeKind::Index(place_type) => {
                let index_result = place_type
                    .data_type
                    .get_index_result_semantic_analysis(ctx, place_type.span)?;

                index_result.assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;
            }
            PlaceTypeKind::Dereference(place_type) => match place_type.data_type {
                DataType::Score(_) => {
                    if !value_type.is_score_compatible() {
                        return ctx.add_error(
                            value_span,
                            SemanticAnalysisError::TypeIsNotScoreCompatible(value_type.clone()),
                        );
                    }
                }

                DataType::Data(_) => {}

                DataType::Reference(inner_type) => {
                    value_type.assert_equals(ctx, value_span, &inner_type)?;

                    inner_type.assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;
                }

                place_data_type => {
                    return ctx.add_error(
                        place_type.span,
                        SemanticAnalysisError::CannotBeDereferenced(place_data_type),
                    );
                }
            },
            PlaceTypeKind::FieldAccess(target, _, field) => {
                let field_result = target.data_type.get_field_result_semantic_analysis(
                    ctx,
                    target.span,
                    &field,
                )?;

                field_result.assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;
            }
        }

        Some(())
    }

    #[inline]
    pub fn perform_augmented_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        operator: ArithmeticOperator,
        value_span: Span,
        value_type: &DataType,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Data(data_type) | PlaceTypeKind::Score(data_type) => {
                data_type.assert_can_perform_augmented_assignment(
                    ctx, operator, value_span, value_type,
                )?;
            }
            PlaceTypeKind::Value => {
                self.data_type
                    .assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;

                self.data_type.assert_can_perform_augmented_assignment(
                    ctx, operator, value_span, value_type,
                )?;
            }
            PlaceTypeKind::Tuple(_) | PlaceTypeKind::Underscore => {
                return ctx.add_error(
                    self.span,
                    SemanticAnalysisError::CannotPerformAugmentedAssignment(self.data_type),
                );
            }
            PlaceTypeKind::Index(target) => {
                let index_result = target
                    .data_type
                    .get_index_result_semantic_analysis(ctx, target.span)?;

                index_result.assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;

                if !index_result.can_perform_augmented_assignment(operator, value_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            index_result,
                            value_type.clone(),
                        ),
                    );
                }
            }
            PlaceTypeKind::Dereference(place_type) => {
                place_type
                    .data_type
                    .assert_can_perform_augmented_assignment(
                        ctx, operator, value_span, value_type,
                    )?;
            }
            PlaceTypeKind::FieldAccess(target, field_span, field) => {
                let field_result = target
                    .data_type
                    .get_field_result_semantic_analysis(ctx, field_span, &field)?;

                field_result.assert_runtime_value_mutation_in_runtime_loop(ctx, self.span)?;

                field_result.assert_can_perform_augmented_assignment(
                    ctx, operator, value_span, value_type,
                )?;
            }
        }

        Some(())
    }
}
