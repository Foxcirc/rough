

use std::collections::HashMap;

use crate::{parser::{Span, Literal, Op}, diagnostic::Diagnostic, parse_modules};

pub(crate) fn codegen(state: &mut State, source_file: parse_modules::SourceFile) -> Result<(), CodegenError> {

    let current_span_id = state.content.len();
    let errors = Vec::new();

    for fun in source_file.items.funs {

        state.funs.insert(fun.name.inside(&source_file.content).to_string(), state.bytecode.len()); // fun starts at current position

        state.bytecode.push(Instruction::comment(format!("fn {}", fun.name.inside(&source_file.content))));

        codegen_block(&mut state.bytecode, fun.body, None, current_span_id);

        state.bytecode.push(Instruction::Return);

    }

    state.content.push(source_file.content);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(CodegenError { errors })
    }

}

fn codegen_block(dest: &mut Vec<Instruction<Span>>, block: Vec<Op>, loop_escape: Option<usize>, current_span_id: usize) {

    for op in block {

        match op {
            Op::Push { value } => dest.push(Instruction::Push { value }),
            Op::Call { name }  => dest.push(Instruction::Call { what: name.with(current_span_id) }), // todo: make span only have an id after this (two different span types, so you cant forget calling with)

            Op::Copy  => dest.push(Instruction::Copy),
            Op::Over  => dest.push(Instruction::Over),
            Op::Swap  => dest.push(Instruction::Swap),
            Op::Rot3  => dest.push(Instruction::Rot3),
            Op::Rot4  => dest.push(Instruction::Rot3),
            Op::Drop  => dest.push(Instruction::Drop),

            Op::Read  => dest.push(Instruction::Read),
            Op::Write => dest.push(Instruction::Write),

            Op::Move  => todo!(),
            Op::Addr  => todo!(),
            Op::Type  => todo!(),
            Op::Size  => todo!(),
            Op::Dot   => todo!(),

            Op::Add => dest.push(Instruction::Add),
            Op::Sub => dest.push(Instruction::Sub),
            Op::Mul => dest.push(Instruction::Mul),
            Op::Dvm => dest.push(Instruction::Dvm),

            Op::Not => dest.push(Instruction::Not),
            Op::And => dest.push(Instruction::And),
            Op::Or  => dest.push(Instruction::Or),
            Op::Xor => dest.push(Instruction::Xor),

            Op::Eq  => dest.push(Instruction::Eq),
            Op::Gt  => dest.push(Instruction::Gt),
            Op::Gte => dest.push(Instruction::Gte),
            Op::Lt  => dest.push(Instruction::Lt),
            Op::Lte => dest.push(Instruction::Lte),

            Op::If { block } => {

                let start = dest.len();
                dest.push(Instruction::Placeholder);
                codegen_block(dest, block, loop_escape, current_span_id);
                // this is for a potential `else` block to use
                dest.push(Instruction::IfBlank);
                let end = dest.len();

                // jump after the `if`
                dest[start] = Instruction::Bne { to: end };

            },
            Op::Elif { block: _block } => todo!(),
            Op::Else { block } => {

                // check that this `else` is coming directly after an `if`
                assert!(matches!(dest.last(), Some(Instruction::IfBlank)), "todo: not after if");

                let start = dest.len() - 1;
                codegen_block(dest, block, loop_escape, current_span_id);
                let end = dest.len();

                // skip the `else` body
                dest[start] = Instruction::Bra { to: end }

            },
            Op::Loop { block } => {

                let escape = dest.len() - 1;
                let start = dest.len();
                codegen_block(dest, block, Some(escape), current_span_id);
                // jump back to the `loop` beginning
                dest.push(Instruction::Bra { to: start });
                let end = dest.len();

                // patch up the `break` bra instructions
                for item in dest[start..end].iter_mut() {
                    if matches!(item, Instruction::Bra { to } if *to == escape) {
                        *item = Instruction::Bra { to: end };
                    }
                }

            },
            Op::For  { block: _block } => todo!(),
            Op::Break => {

                let idx = loop_escape.expect("todo: not in a loop");
                dest.push(Instruction::Bra { to: idx })

            },
        };

    }

}

pub(crate) fn crossref(state: State) -> Result<Bytecode<Position>, CodegenError> {

    let mut errors = Vec::new();
    let mut bytecode = Bytecode::new();

    for instruction in state.bytecode {

        let result = match instruction {

            Instruction::Placeholder => todo!("Placeholder instruction during crossref"),

            Instruction::Push { value } => Instruction::Push { value },
            
            Instruction::Call { what }  => {
                let name = what.inside(&state.content[what.id]);
                if let Some(pos) = state.funs.get(name) {
                    Instruction::Call { what: *pos }
                } else {
                    errors.push(CodegenErrorKind::UnknownFn { name: name.to_string() });
                    continue
                }
            },

            Instruction::Return => Instruction::Return,

            Instruction::Drop => Instruction::Drop,
            Instruction::Copy => Instruction::Copy,
            Instruction::Over => Instruction::Over,
            Instruction::Swap => Instruction::Swap,
            Instruction::Rot3 => Instruction::Rot3,
            Instruction::Rot4 => Instruction::Rot4,

            Instruction::Read  => Instruction::Read,
            Instruction::Write => Instruction::Write,

            Instruction::Add => Instruction::Add,
            Instruction::Sub => Instruction::Sub,
            Instruction::Mul => Instruction::Mul,
            Instruction::Dvm => Instruction::Dvm,

            Instruction::Not => Instruction::Not,
            Instruction::And => Instruction::And,
            Instruction::Or  => Instruction::Or,
            Instruction::Xor => Instruction::Xor,
            Instruction::Eq  => Instruction::Eq,
            Instruction::Gt  => Instruction::Gt,
            Instruction::Gte => Instruction::Gte,
            Instruction::Lt  => Instruction::Lt,
            Instruction::Lte => Instruction::Lte,

            Instruction::Bne { to } => Instruction::Bne { to },
            Instruction::Bra { to } => Instruction::Bra { to },

            Instruction::IfBlank => Instruction::IfBlank,

            Instruction::Comment { text } => Instruction::Comment { text },

        };

        bytecode.push(result);

    }

    if errors.is_empty() {
        Ok(bytecode)
    } else {
        Err(CodegenError { errors })
    }

}

#[derive(Debug)]
pub(crate) enum Instruction<P> {

    Comment { text: String },
    Placeholder,

    Push { value: Literal },

    Call { what: P },
    Return,

    Drop,
    Copy,
    Over,
    Swap,
    Rot3,
    Rot4,

    Read,
    Write,
    // Move,

    // Addr,
    // Type,
    // Size,

    // Access,

    Add,
    Sub,
    Mul,
    Dvm,

    Not,
    And,
    Or,
    Xor,

    Eq,
    Gt,
    Gte,
    Lt,
    Lte,

    Bne { to: usize },
    Bra { to: usize },

    IfBlank,

}

impl<P> Instruction<P> {
    pub(crate) fn comment<S: ToString>(value: S) -> Self {
        Self::Comment { text: value.to_string() }
    }
}

#[derive(Default)]
pub(crate) struct State {
    pub(crate) content: Vec<String>, // every Span has an id that corresponds to an index into this list
    pub(crate) funs: HashMap<String, Position>,
    pub(crate) bytecode: Bytecode<Span>,
}

pub(crate) type Bytecode<P> = Vec<Instruction<P>>;
pub(crate) type Position = usize;

pub(crate) struct CodegenError {
    pub(crate) errors: Vec<CodegenErrorKind>,
}

pub(crate) enum CodegenErrorKind {
    UnknownFn { name: String },
}

pub(crate) fn format_error(value: CodegenError) -> Vec<Diagnostic> {

    let mut diags = Vec::new();

    for error in value.errors {
        let diag = match error {
            CodegenErrorKind::UnknownFn { name } => Diagnostic::error("unknown function").note(name),
        };
        diags.push(diag);
    }

    diags

}

pub(crate) mod amd64 {

}

