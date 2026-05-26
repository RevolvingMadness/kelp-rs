use kelp_core::{parsed::arena::ParsedAstArena, span::Span};
use strum::Display;
use thiserror::Error;

#[derive(Display, Debug, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
pub enum LowerDataType {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
}

#[derive(Debug, Clone, Error)]
pub enum LowerError {
    #[error("Unknown runtime storage type")]
    UnknownRuntimeStorageType,
    #[error("An expression sigil is not allowed here")]
    ExpressionSigilNotAllowed,
    #[error("This value is too small to fit in the type `{}`", .0)]
    ValueTooSmall(LowerDataType),
    #[error("This value is too big to fit in the type `{}`", .0)]
    ValueTooBig(LowerDataType),
}

#[derive(Debug, Clone)]
pub enum LowerInfoKind {
    // Warning(LowerWarning),
    Error(LowerError),
}

#[derive(Debug, Clone)]
pub struct LowerInfo {
    pub span: Span,
    pub kind: LowerInfoKind,
}

pub struct LowerContext {
    pub infos: Vec<LowerInfo>,
    pub max_infos: usize,
    pub arena: ParsedAstArena,
}

impl LowerContext {
    #[must_use]
    pub fn new(max_infos: usize) -> Self {
        Self {
            infos: Vec::new(),
            max_infos,
            arena: ParsedAstArena::default(),
        }
    }

    pub fn add_info<T>(&mut self, info: LowerInfo) -> Option<T> {
        if self.infos.len() >= self.max_infos {
            return None;
        }

        self.infos.push(info);

        None
    }

    #[inline]
    #[must_use]
    pub fn add_error<T>(&mut self, span: Span, error: LowerError) -> Option<T> {
        self.add_info(LowerInfo {
            span,
            kind: LowerInfoKind::Error(error),
        })
    }

    #[inline]
    pub fn add_error_unit(&mut self, span: Span, error: LowerError) {
        let _ = self.add_error::<()>(span, error);
    }
}
