
use std::{path::PathBuf, borrow::Cow};
use crate::{codegen::Program, parser::{Span, Type}, diagnostic::Diagnostic};

pub(crate) fn typegen<I>(program: &mut Program<I>, file_path: &PathBuf, types: Vec<Type>) -> Result<(), TypegenError> {

    // add the primitive types to the programs type list
    
    program.types.insert(Cow::Borrowed("int"), Type::Int);

    Ok(())

}

pub(crate) struct TypegenError {
    pub(crate) kind: TypegenErrorKind,
    pub(crate) span: Span,
}

impl TypegenError {
    pub(crate) fn spanned(kind: TypegenErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

pub(crate) enum TypegenErrorKind {
}

pub(crate) fn format_error(value: TypegenError) -> Diagnostic {
    todo!()
}

