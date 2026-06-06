use crate::{
    parsed::{
        environment::r#type::{ParsedTypeDeclaration, ParsedTypeDeclarationKind},
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    semantic::environment::{r#type::HighVisibleTypeId, value::HighVisibleValueId},
    span::Span,
};

pub struct ResolvedPath {
    pub type_result: Result<HighVisibleTypeId, SemanticAnalysisError>,
    pub value_result: Result<HighVisibleValueId, SemanticAnalysisError>,
    pub name_span: Span,
    pub name: String,
}

impl ResolvedPath {
    #[must_use]
    pub fn get_type_id(&self, ctx: &mut SemanticAnalysisContext) -> Option<HighVisibleTypeId> {
        let type_id = match &self.type_result {
            Ok(id) => *id,
            Err(error) => return ctx.add_error(error.clone()),
        };

        Some(type_id)
    }

    #[must_use]
    pub fn get_value_id(&self, ctx: &mut SemanticAnalysisContext) -> Option<HighVisibleValueId> {
        let value_id = match &self.value_result {
            Ok(id) => *id,
            Err(error) => return ctx.add_error(error.clone()),
        };

        Some(value_id)
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPathSegment {
    pub name: String,
    pub name_span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedPath {
    pub span: Span,
    pub segments: Vec<ParsedPathSegment>,
}

impl ParsedPath {
    #[must_use]
    pub fn perform_semantic_analysis(
        mut self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<ResolvedPath> {
        assert!(!self.segments.is_empty());

        if self.segments.len() == 1 {
            let segment = self.segments.remove(0);

            let type_result = ctx
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get_type_id(&segment.name))
                .map(|id| HighVisibleTypeId(id.0))
                .ok_or_else(|| SemanticAnalysisError::UnknownType {
                    span: segment.name_span,
                    name: segment.name.clone(),
                });

            let value_result = ctx
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get_value_id(&segment.name))
                .map(|id| HighVisibleValueId(id.0))
                .ok_or_else(|| SemanticAnalysisError::UnknownValue {
                    span: segment.name_span,
                    name: segment.name.clone(),
                });

            return Some(ResolvedPath {
                type_result,
                value_result,
                name_span: segment.name_span,
                name: segment.name,
            });
        }

        let mut segments = self.segments.into_iter();

        let first_segment = segments.next().unwrap();
        let last_segment = segments.next_back().unwrap();

        let mut current_type_id = ctx
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_type_id(&first_segment.name))
            .map(|id| HighVisibleTypeId(id.0))
            .unwrap();

        let mut current_span = first_segment.name_span;

        for segment in segments {
            let declaration = ctx.semantic_environment.get_type(current_type_id);

            let next_type_id = match declaration.get_visible_type_id(
                &ctx.parsed_environment,
                &ctx.semantic_environment,
                &ctx.current_module_path,
                current_type_id,
                current_span,
                &segment.name,
                segment.name_span,
            ) {
                Ok(id) => id,
                Err(error) => return ctx.add_error(error),
            };

            current_type_id = next_type_id;
            current_span = segment.name_span;
        }

        let declaration = ctx.parsed_environment.get_type(current_type_id);

        let type_kind = declaration.kind.get_type_kind();

        let ParsedTypeDeclaration {
            kind: ParsedTypeDeclarationKind::Module(declaration),
            ..
        } = declaration
        else {
            return ctx.add_error(SemanticAnalysisError::NotAModule {
                span: current_span,
                type_kind,
            });
        };

        let type_result = declaration.get_visible_type_id(
            &ctx.parsed_environment,
            &ctx.current_module_path,
            &last_segment.name,
            last_segment.name_span,
        );

        let value_result = declaration.get_visible_value_id(
            &ctx.parsed_environment,
            &ctx.current_module_path,
            &last_segment.name,
            last_segment.name_span,
        );

        Some(ResolvedPath {
            type_result,
            value_result,
            name_span: last_segment.name_span,
            name: last_segment.name,
        })
    }
}
