
use crate::{basegen::{FileSpan, Program}, diagnostic::Diagnostic, parser::Span, arch::Intrinsic};

// pub(crate) struct Eval<Impl> {
// 
// }

pub(crate) fn eval<I: Intrinsic>(program: Program<I>) -> Result<(), EvalError> {

    todo!()

}

pub(crate) struct EvalError {
    kind: EvalErrorKind,
    file_span: FileSpan,
}

impl EvalError {
    pub(crate) fn unspanned(kind: EvalErrorKind) -> Self {
        Self { kind, file_span: FileSpan::default() }
    }
}

pub(crate) enum EvalErrorKind {}

pub(crate) fn format_error(value: EvalError) -> Diagnostic {
    // let diag = match value.kind {};
    let diag = Diagnostic::error("eval-error");
    if value.file_span.span != Span::default() {
        diag.file(&value.file_span.file)
            .pos(value.file_span.span.to_pos())
    } else {
        diag
    }
}

