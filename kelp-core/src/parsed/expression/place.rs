use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::semantic_analysis::SemanticAnalysisContext,
    span::Span,
    semantic::{
        data::Data,
        expression::{place::ResolvedPlaceExpression, unresolved::SemanticExpression},
        player_score::PlayerScore,
    },
};
use crate::low::environment::value::variable::VariableId;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::HighValueId;

#[derive(Debug, Clone)]
pub enum UnresolvedPlaceExpressionKind {
    Value(HighValueId, Vec<SemanticDataType>),
    Score(PlayerScore),
    Data(Box<Data>),
    FieldAccess(Box<SemanticPlaceExpression>, String),
    Index(Box<SemanticPlaceExpression>, Box<SemanticExpression>),
    Dereference(Box<SemanticPlaceExpression>),
}

impl UnresolvedPlaceExpressionKind {
    #[inline]
    #[must_use]
    pub const fn with(self, data_type: SemanticDataType) -> SemanticPlaceExpression {
        SemanticPlaceExpression {
            kind: self,
            data_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticPlaceExpression {
    pub kind: UnresolvedPlaceExpressionKind,
    pub data_type: SemanticDataType,
}

impl SemanticPlaceExpression {
    #[must_use]
    pub fn resolve(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedPlaceExpression {
        match self.kind {
            UnresolvedPlaceExpressionKind::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(id, &generic_types)
                    .unwrap();

                ResolvedPlaceExpression::Variable(VariableId(id.0))
            }
            UnresolvedPlaceExpressionKind::Score(score) => {
                let score = score.compile(datapack, ctx);

                ResolvedPlaceExpression::Score(score)
            }
            UnresolvedPlaceExpressionKind::Data(data) => {
                let data = data.compile(datapack, ctx);

                ResolvedPlaceExpression::Data(data)
            }
            UnresolvedPlaceExpressionKind::FieldAccess(place, field) => {
                let access_type = place
                    .data_type
                    .clone()
                    .resolve(datapack)
                    .unwrap()
                    .get_field_access_type(&datapack.environment)
                    .unwrap();

                let place = place.resolve(datapack, ctx);

                ResolvedPlaceExpression::FieldAccess(Box::new(place), access_type, field)
            }
            UnresolvedPlaceExpressionKind::Index(place, index) => {
                let place = place.resolve(datapack, ctx);
                let index = index.kind.resolve(datapack, ctx);

                ResolvedPlaceExpression::Index(Box::new(place), index)
            }
            UnresolvedPlaceExpressionKind::Dereference(place) => place
                .resolve(datapack, ctx)
                .resolve(datapack, ctx)
                .dereference_place()
                .unwrap(),
        }
    }

    pub fn perform_assignment_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &SemanticDataType,
    ) -> Option<()> {
        match &self.kind {
            UnresolvedPlaceExpressionKind::Value(..) => {
                value_type.assert_equals(ctx, value_span, &self.data_type)
            }
            UnresolvedPlaceExpressionKind::Score(..) => {
                value_type.assert_score_compatible(ctx, value_span)
            }
            UnresolvedPlaceExpressionKind::Data(..) => {
                value_type.assert_data_compatible(ctx, value_span)
            }
            UnresolvedPlaceExpressionKind::Index(place, _)
            | UnresolvedPlaceExpressionKind::FieldAccess(place, _)
            | UnresolvedPlaceExpressionKind::Dereference(place) => match &place.data_type {
                SemanticDataType::Score(..) => {
                    value_type.assert_score_compatible(ctx, value_span)
                }
                SemanticDataType::Data(..) => value_type.assert_data_compatible(ctx, value_span),
                _ => value_type.assert_equals(ctx, value_span, &self.data_type),
            },
        }
    }
}
