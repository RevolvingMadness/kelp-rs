use la_arena::Idx;

use crate::{
    ast_allocator::low::{LowAstAllocator, Typed},
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::{
        environment::resolved::value::HighValueId, semantic_analysis::SemanticAnalysisContext,
    },
    span::Span,
    typed::{
        data::TypedData,
        data_type::unresolved::UnresolvedDataType,
        environment::value::variable::VariableId,
        expression::{
            place::ResolvedPlaceExpression,
            typed::{TypedExpression, TypedExpressionId},
        },
        player_score::TypedPlayerScore,
    },
};

pub type UnresolvedPlaceExpressionId = Idx<Typed<UnresolvedPlaceExpression>>;

#[derive(Debug, Clone)]
pub enum UnresolvedPlaceExpression {
    Value(HighValueId, Vec<UnresolvedDataType>),
    Score(TypedPlayerScore),
    Data(Box<TypedData>),
    FieldAccess(UnresolvedPlaceExpressionId, String),
    Index(UnresolvedPlaceExpressionId, TypedExpressionId),
    Dereference(UnresolvedPlaceExpressionId),
}

impl UnresolvedPlaceExpression {
    #[must_use]
    pub fn resolve(
        id: UnresolvedPlaceExpressionId,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedPlaceExpression {
        match allocator.get_place_expression_value(id) {
            Self::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(*id, generic_types)
                    .unwrap();

                ResolvedPlaceExpression::Variable(VariableId(id.0))
            }
            Self::Score(score) => {
                let score = score.clone().compile(allocator, datapack, ctx);

                ResolvedPlaceExpression::Score(score)
            }
            Self::Data(data) => {
                let data = data.clone().compile(allocator, datapack, ctx);

                ResolvedPlaceExpression::Data(data)
            }
            Self::FieldAccess(place, field) => {
                let access_type = allocator
                    .get_place_expression_type(*place)
                    .clone()
                    .resolve(datapack)
                    .unwrap()
                    .get_field_access_type(&datapack.environment)
                    .unwrap();

                let place = Self::resolve(*place, allocator, datapack, ctx);

                ResolvedPlaceExpression::FieldAccess(Box::new(place), access_type, field.clone())
            }
            Self::Index(place, index) => {
                let place = Self::resolve(*place, allocator, datapack, ctx);
                let index = TypedExpression::resolve(*index, allocator, datapack, ctx);

                ResolvedPlaceExpression::Index(Box::new(place), index)
            }
            Self::Dereference(place) => Self::resolve(*place, allocator, datapack, ctx)
                .resolve(datapack, ctx)
                .dereference_place()
                .unwrap(),
        }
    }

    pub fn perform_assignment_semantic_analysis(
        id: UnresolvedPlaceExpressionId,
        allocator: &LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &UnresolvedDataType,
    ) -> Option<()> {
        match allocator.get_place_expression_value(id) {
            Self::Value(..) => {
                let data_type = allocator.get_place_expression_type(id);

                value_type.assert_equals(ctx, value_span, data_type)
            }
            Self::Score(..) => value_type.assert_score_compatible(ctx, value_span),
            Self::Data(..) => value_type.assert_data_compatible(ctx, value_span),
            Self::Index(place, _) | Self::FieldAccess(place, _) | Self::Dereference(place) => {
                let data_type = allocator.get_place_expression_type(*place);

                match data_type {
                    UnresolvedDataType::Score(..) => {
                        value_type.assert_score_compatible(ctx, value_span)
                    }
                    UnresolvedDataType::Data(..) => {
                        value_type.assert_data_compatible(ctx, value_span)
                    }
                    _ => {
                        let data_type = allocator.get_place_expression_type(id);

                        value_type.assert_equals(ctx, value_span, data_type)
                    }
                }
            }
        }
    }
}
