use crate::span::Span;

#[derive(Debug, Clone, Copy)]
pub enum LabelType {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
pub struct DiagnosticLabel {
    pub span: Span,
    pub message: Option<String>,
    pub type_: LabelType,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub labels: Vec<DiagnosticLabel>,
    pub notes: Vec<String>,
    pub help: Option<String>,
}

impl Diagnostic {
    #[must_use]
    pub fn error(message: impl Into<String>) -> Self {
        let message = message.into();

        Self {
            message,
            labels: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }

    #[must_use]
    fn inner_with_label(mut self, span: Span, message: Option<String>, type_: LabelType) -> Self {
        self.labels.push(DiagnosticLabel {
            span,
            message,
            type_,
        });

        self
    }

    #[must_use]
    pub fn with_label(self, span: Span, message: impl Into<String>, type_: LabelType) -> Self {
        let message = message.into();

        self.inner_with_label(span, Some(message), type_)
    }

    #[inline]
    #[must_use]
    pub fn with_primary_label(self, span: Span, message: impl Into<String>) -> Self {
        self.with_label(span, message, LabelType::Primary)
    }

    #[inline]
    #[must_use]
    pub fn with_secondary_label(self, span: Span, message: impl Into<String>) -> Self {
        self.with_label(span, message, LabelType::Secondary)
    }

    #[inline]
    #[must_use]
    pub fn with_optional_secondary_label(
        self,
        span: Option<Span>,
        message: impl Into<String>,
    ) -> Self {
        let Some(span) = span else {
            return self;
        };

        self.with_label(span, message, LabelType::Secondary)
    }

    #[inline]
    #[must_use]
    pub fn with_no_label(self, span: Span, type_: LabelType) -> Self {
        self.inner_with_label(span, None, type_)
    }

    #[inline]
    #[must_use]
    pub fn with_primary_no_label(self, span: Span) -> Self {
        self.with_no_label(span, LabelType::Primary)
    }

    #[must_use]
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());

        self
    }

    #[must_use]
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());

        self
    }
}
