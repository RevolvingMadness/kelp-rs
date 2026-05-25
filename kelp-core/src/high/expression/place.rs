use la_arena::Idx;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    high::{environment::resolved::value::HighValueId, semantic_analysis::SemanticAnalysisContext},
    low::{
        data::Data,
        data_type::unresolved::UnresolvedDataType,
        environment::value::variable::VariableId,
        expression::{place::ResolvedPlaceExpression, unresolved::UnresolvedExpression},
        player_score::PlayerScore,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub enum UnresolvedPlaceExpression {
    Value(HighValueId, Vec<UnresolvedDataType>),
    Score(PlayerScore),
    Data(Box<Data>),
    FieldAccess(Idx<Self>, String),
    Index(Idx<Self>, Idx<UnresolvedExpression>),
    Dereference(Idx<Self>),
}

impl UnresolvedPlaceExpression {
    #[must_use]
    pub fn resolve(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedPlaceExpression {
        match allocator.get_place_expression(id) {
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
                let index = UnresolvedExpression::resolve(*index, allocator, datapack, ctx);

                ResolvedPlaceExpression::Index(Box::new(place), index)
            }
            Self::Dereference(place) => Self::resolve(*place, allocator, datapack, ctx)
                .resolve(datapack, ctx)
                .dereference_place()
                .unwrap(),
        }
    }

    pub fn perform_assignment_semantic_analysis(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &UnresolvedDataType,
    ) -> Option<()> {
        match allocator.get_place_expression(id) {
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
