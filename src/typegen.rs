
use std::{path::PathBuf, borrow::Cow, collections::HashMap};
use crate::{codegen::{FunWithMetadata, Symbols, Program}, parser::{Span, Type, ParsedSignature, FnSignature, IdentStr}, diagnostic::Diagnostic, typecheck::{Entity, types_to_entities}};

pub(crate) fn typegen<I>(mut program: Symbols<I>) -> Result<Program<I>, TypegenError> {

    // add the primitive types to the programs type list
    
    program.types.insert(Cow::Borrowed("int"), Type::Int);

    let mut funs = HashMap::with_capacity(program.funs.len());
    for (name, fun) in program.funs.into_iter() {
        let types = eval_types_signature(fun.signature)?;
        let val = FunWithMetadata {
            file_name: fun.file_name,
            body: fun.body,
            signature: types
        };
        funs.insert(name, val);
    }

    Ok(Program {
        funs,
        types: program.types,
    })

}

fn eval_types_signature(signature: ParsedSignature) -> Result<FnSignature, TypegenError> {

    Ok(FnSignature {
        takes: eval_types(signature.takes)?,
        returns: eval_types(signature.returns)?,
    })

}

fn eval_types(types: Vec<IdentStr>) -> Result<Vec<Entity>, TypegenError> {

    let mut result = Vec::new();

    for item in types {

        match &item[..] {
            "int" => result.push(Type::Int),
            _other => todo!("unimplemented type literal")
        }

    }
    
    Ok(types_to_entities(result))

}

#[derive(Debug)]
pub(crate) struct TypegenError {
    pub(crate) kind: TypegenErrorKind,
    pub(crate) span: Span,
}

impl TypegenError {
    pub(crate) fn spanned(kind: TypegenErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug)]
pub(crate) enum TypegenErrorKind {
}

pub(crate) fn format_error(value: TypegenError) -> Diagnostic {
    todo!()
}

