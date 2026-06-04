use kelp_core::{parsed::semantic_analysis::info::diagnostic::Diagnostic, span::Span};
use strum::Display;

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

#[derive(Debug, Clone)]
pub enum LowerError {
    InvalidRuntimeStorageType {
        span: Span,
    },
    ExpressionSigilNotAllowed {
        span: Span,
    },
    ValueTooSmall {
        value: String,
        type_span: Span,
        data_type: LowerDataType,
    },
    ValueTooBig {
        value: String,
        type_span: Span,
        data_type: LowerDataType,
    },
}

impl LowerError {
    #[must_use]
    pub fn into_diagnostic(self) -> Diagnostic {
        match self {
            Self::InvalidRuntimeStorageType { span } => {
                Diagnostic::error("invalid runtime storage type").with_primary_no_label(span)
            }
            Self::ExpressionSigilNotAllowed { span } => {
                Diagnostic::error("expression sigils are not allowed here")
                    .with_primary_no_label(span)
            }
            Self::ValueTooSmall {
                value,
                type_span,
                data_type,
            } => Diagnostic::error(format!("value `{}` too small for `{}`", value, data_type))
                .with_primary_no_label(type_span),
            Self::ValueTooBig {
                value,
                type_span,
                data_type,
            } => Diagnostic::error(format!("value `{}` too big for `{}`", value, data_type))
                .with_primary_no_label(type_span),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LowerInfo {
    // Warning(LowerWarning),
    Error(LowerError),
}

pub struct LowerContext {
    pub infos: Vec<LowerInfo>,
    pub max_infos: usize,
}

impl LowerContext {
    #[must_use]
    pub const fn new(max_infos: usize) -> Self {
        Self {
            infos: Vec::new(),
            max_infos,
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
    pub fn add_error<T>(&mut self, error: LowerError) -> Option<T> {
        self.add_info(LowerInfo::Error(error))
    }

    #[inline]
    pub fn add_error_unit(&mut self, error: LowerError) {
        let _ = self.add_error::<()>(error);
    }
}
