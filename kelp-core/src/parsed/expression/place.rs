use la_arena::Idx;

use crate::{
    ast_allocator::low::{LowAstAllocator, Typed},
    compile_context::CompileContext,
    datapack::Datapack,
    low::environment::value::variable::VariableId,
    parsed::semantic_analysis::SemanticAnalysisContext,
    span::Span,
    typed::{
        data::TypedData,
        data_type::SemanticDataType,
        environment::value::HighValueId,
        expression::{TypedExpression, TypedExpressionId, place::TypedPlaceExpression},
        player_score::TypedPlayerScore,
    },
};

pub type ParsedPlaceExpressionId = Idx<Typed<ParsedPlaceExpression>>;

#[derive(Debug, Clone)]
pub enum ParsedPlaceExpression {
    Value(HighValueId, Vec<SemanticDataType>),
    Score(TypedPlayerScore),
    Data(Box<TypedData>),
    FieldAccess(ParsedPlaceExpressionId, String),
    Index(ParsedPlaceExpressionId, TypedExpressionId),
    Dereference(ParsedPlaceExpressionId),
}

impl ParsedPlaceExpression {
    #[must_use]
    pub fn resolve(
        id: ParsedPlaceExpressionId,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> TypedPlaceExpression {
        match allocator.get_place_expression_value(id) {
            Self::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(*id, generic_types)
                    .unwrap();

                TypedPlaceExpression::Variable(VariableId(id.0))
            }
            Self::Score(score) => {
                let score = score.clone().compile(allocator, datapack, ctx);

                TypedPlaceExpression::Score(score)
            }
            Self::Data(data) => {
                let data = data.clone().compile(allocator, datapack, ctx);

                TypedPlaceExpression::Data(data)
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

                TypedPlaceExpression::FieldAccess(Box::new(place), access_type, field.clone())
            }
            Self::Index(place, index) => {
                let place = Self::resolve(*place, allocator, datapack, ctx);
                let index = TypedExpression::resolve(*index, allocator, datapack, ctx);

                TypedPlaceExpression::Index(Box::new(place), index)
            }
            Self::Dereference(place) => Self::resolve(*place, allocator, datapack, ctx)
                .resolve(datapack, ctx)
                .dereference_place()
                .unwrap(),
        }
    }

    pub fn perform_assignment_semantic_analysis(
        id: ParsedPlaceExpressionId,
        allocator: &LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &SemanticDataType,
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
                    SemanticDataType::Score(..) => {
                        value_type.assert_score_compatible(ctx, value_span)
                    }
                    SemanticDataType::Data(..) => {
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
