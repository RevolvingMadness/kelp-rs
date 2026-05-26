use la_arena::Idx;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{environment::value::variable::VariableId, expression::place::PlaceExpression},
    parsed::semantic_analysis::SemanticAnalysisContext,
    span::Span,
    typed::{
        arena::{Typed, TypedAstArena},
        data::TypedData,
        data_type::SemanticDataType,
        environment::value::HighValueId,
        expression::{TypedExpression, TypedExpressionId},
        player_score::TypedPlayerScore,
    },
};

pub type TypedPlaceExpressionId = Idx<Typed<TypedPlaceExpression>>;

#[derive(Debug, Clone)]
pub enum TypedPlaceExpression {
    Value(HighValueId, Vec<SemanticDataType>),
    Score(TypedPlayerScore),
    Data(Box<TypedData>),
    FieldAccess(TypedPlaceExpressionId, String),
    Index(TypedPlaceExpressionId, TypedExpressionId),
    Dereference(TypedPlaceExpressionId),
}

impl TypedPlaceExpression {
    #[must_use]
    pub fn resolve(
        id: TypedPlaceExpressionId,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> PlaceExpression {
        match arena.get_place_expression_value(id) {
            Self::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(*id, generic_types)
                    .unwrap();

                PlaceExpression::Variable(VariableId(id.0))
            }
            Self::Score(score) => {
                let score = score.clone().compile(arena, datapack, ctx);

                PlaceExpression::Score(score)
            }
            Self::Data(data) => {
                let data = data.clone().compile(arena, datapack, ctx);

                PlaceExpression::Data(data)
            }
            Self::FieldAccess(place, field) => {
                let access_type = arena
                    .get_place_expression_type(*place)
                    .clone()
                    .resolve(datapack)
                    .unwrap()
                    .get_field_access_type(&datapack.environment)
                    .unwrap();

                let place = Self::resolve(*place, arena, datapack, ctx);

                PlaceExpression::FieldAccess(Box::new(place), access_type, field.clone())
            }
            Self::Index(place, index) => {
                let place = Self::resolve(*place, arena, datapack, ctx);
                let index = TypedExpression::resolve(*index, arena, datapack, ctx);

                PlaceExpression::Index(Box::new(place), index)
            }
            Self::Dereference(place) => Self::resolve(*place, arena, datapack, ctx)
                .resolve(datapack, ctx)
                .dereference_place()
                .unwrap(),
        }
    }

    pub fn perform_assignment_semantic_analysis(
        id: TypedPlaceExpressionId,
        arena: &TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &SemanticDataType,
    ) -> Option<()> {
        match arena.get_place_expression_value(id) {
            Self::Value(..) => {
                let data_type = arena.get_place_expression_type(id);

                value_type.assert_equals(ctx, value_span, data_type)
            }
            Self::Score(..) => value_type.assert_score_compatible(ctx, value_span),
            Self::Data(..) => value_type.assert_data_compatible(ctx, value_span),
            Self::Index(place, _) | Self::FieldAccess(place, _) | Self::Dereference(place) => {
                let data_type = arena.get_place_expression_type(*place);

                match data_type {
                    SemanticDataType::Score(..) => {
                        value_type.assert_score_compatible(ctx, value_span)
                    }
                    SemanticDataType::Data(..) => {
                        value_type.assert_data_compatible(ctx, value_span)
                    }
                    _ => {
                        let data_type = arena.get_place_expression_type(id);

                        value_type.assert_equals(ctx, value_span, data_type)
                    }
                }
            }
        }
    }
}
