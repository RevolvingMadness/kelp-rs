use crate::low::environment::value::variable::VariableId;
use crate::low::expression::place::PlaceExpression;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::HighValueId;
use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::semantic_analysis::SemanticAnalysisContext,
    semantic::{
        data::SemanticData, expression::SemanticExpression, player_score::SemanticPlayerScore,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub enum ParsedPlaceExpressionKind {
    Value(HighValueId, Vec<SemanticDataType>),
    Score(SemanticPlayerScore),
    Data(Box<SemanticData>),
    FieldAccess(Box<ParsedPlaceExpression>, String),
    Index(Box<ParsedPlaceExpression>, Box<SemanticExpression>),
    Dereference(Box<ParsedPlaceExpression>),
}

impl ParsedPlaceExpressionKind {
    #[inline]
    #[must_use]
    pub const fn with(self, data_type: SemanticDataType) -> ParsedPlaceExpression {
        ParsedPlaceExpression {
            kind: self,
            data_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPlaceExpression {
    pub kind: ParsedPlaceExpressionKind,
    pub data_type: SemanticDataType,
}

impl ParsedPlaceExpression {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> PlaceExpression {
        match self.kind {
            ParsedPlaceExpressionKind::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(id, &generic_types)
                    .unwrap();

                PlaceExpression::Variable(VariableId(id.0))
            }
            ParsedPlaceExpressionKind::Score(score) => {
                let score = score.compile(datapack, ctx);

                PlaceExpression::Score(score)
            }
            ParsedPlaceExpressionKind::Data(data) => {
                let data = data.compile(datapack, ctx);

                PlaceExpression::Data(data)
            }
            ParsedPlaceExpressionKind::FieldAccess(place, field) => {
                let access_type = place
                    .data_type
                    .clone()
                    .resolve(datapack)
                    .unwrap()
                    .get_field_access_type(&datapack.environment)
                    .unwrap();

                let place = place.resolve(datapack, ctx);

                PlaceExpression::FieldAccess(Box::new(place), access_type, field)
            }
            ParsedPlaceExpressionKind::Index(place, index) => {
                let place = place.resolve(datapack, ctx);
                let index = index.kind.resolve(datapack, ctx);

                PlaceExpression::Index(Box::new(place), index)
            }
            ParsedPlaceExpressionKind::Dereference(place) => place
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
            ParsedPlaceExpressionKind::Value(..) => {
                value_type.assert_equals(ctx, value_span, &self.data_type)
            }
            ParsedPlaceExpressionKind::Score(..) => {
                value_type.assert_score_compatible(ctx, value_span)
            }
            ParsedPlaceExpressionKind::Data(..) => {
                value_type.assert_data_compatible(ctx, value_span)
            }
            ParsedPlaceExpressionKind::Index(place, _)
            | ParsedPlaceExpressionKind::FieldAccess(place, _)
            | ParsedPlaceExpressionKind::Dereference(place) => match &place.data_type {
                SemanticDataType::Score(..) => value_type.assert_score_compatible(ctx, value_span),
                SemanticDataType::Data(..) => value_type.assert_data_compatible(ctx, value_span),
                _ => value_type.assert_equals(ctx, value_span, &self.data_type),
            },
        }
    }
}
