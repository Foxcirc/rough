

use std::fmt;
use crate::{parser::{Literal, Op}, diagnostic::Diagnostic, parse_modules, arch::Intrinsic};

pub(crate) fn codegen<I: Intrinsic>(state: &mut State<I>, source_file: parse_modules::SourceFile) -> Result<(), CodegenError> {

    let errors = Vec::new();

    for fun in source_file.items.funs {

        state.bytecode.push(Instruction::FnLabel { label: Label::new(fun.name) });

        codegen_block(state, fun.body, None);

        state.bytecode.push(Instruction::Return);

    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(CodegenError { errors })
    }

}

fn codegen_block<I: Intrinsic>(state: &mut State<I>, block: Vec<Op>, loop_escape: Option<BrLabel>) {

    for op in block {

        match op {

            Op::Push { value } => state.bytecode.push(Instruction::Push { value }),

            Op::Call { name }  => {
                let inrinsic = I::generate(&name);
                let result = match inrinsic {
                    Some(val) => Instruction::Intrinsic(val),
                    None => Instruction::Call { to: Label::new(name) }
                };
                state.bytecode.push(result);
            },

            Op::Copy  => state.bytecode.push(Instruction::Copy),
            Op::Over  => state.bytecode.push(Instruction::Over),
            Op::Swap  => state.bytecode.push(Instruction::Swap),
            Op::Rot3  => state.bytecode.push(Instruction::Rot3),
            Op::Rot4  => state.bytecode.push(Instruction::Rot4),
            Op::Drop  => state.bytecode.push(Instruction::Drop),

            Op::Read  => state.bytecode.push(Instruction::Read),
            Op::Write => state.bytecode.push(Instruction::Write),

            Op::Move  => todo!(),
            Op::Addr  => todo!(),
            Op::Type  => todo!(),
            Op::Size  => todo!(),
            Op::Dot   => todo!(),

            Op::Add => state.bytecode.push(Instruction::Add),
            Op::Sub => state.bytecode.push(Instruction::Sub),
            Op::Mul => state.bytecode.push(Instruction::Mul),
            Op::Dvm => state.bytecode.push(Instruction::Dvm),

            Op::Not => state.bytecode.push(Instruction::Not),
            Op::And => state.bytecode.push(Instruction::And),
            Op::Or  => state.bytecode.push(Instruction::Or),
            Op::Xor => state.bytecode.push(Instruction::Xor),

            Op::Eq  => state.bytecode.push(Instruction::Eq),
            Op::Gt  => state.bytecode.push(Instruction::Gt),
            Op::Gte => state.bytecode.push(Instruction::Gte),
            Op::Lt  => state.bytecode.push(Instruction::Lt),
            Op::Lte => state.bytecode.push(Instruction::Lte),

            Op::If { block } => {

                let end = state.next_label();
                state.bytecode.push(Instruction::Bne { to: end });
                codegen_block(state, block, loop_escape);
                state.bytecode.push(Instruction::BrLabel { label: end });

            },
            Op::Elif { block: _block } => todo!(),
            Op::Else { block } => {

                // check that this `else` is coming directly after an `if`
                assert!(matches!(state.bytecode.last(), Some(Instruction::BrLabel { .. })), "todo: not after if"); // todo: harden check

                let end = state.next_label();
                state.bytecode.insert(state.bytecode.len() - 1, Instruction::Bra { to: end });
                codegen_block(state, block, loop_escape);
                state.bytecode.push(Instruction::BrLabel { label: end });

            },
            Op::Loop { block } => {

                let start = state.next_label();
                let escape = state.next_label();
                state.bytecode.push(Instruction::BrLabel { label: start });
                codegen_block(state, block, Some(escape));
                state.bytecode.push(Instruction::Bra { to: start });

            },
            Op::For  { block: _block } => todo!(),
            Op::Break => {

                let escape = loop_escape.expect("todo: not in a loop");
                state.bytecode.push(Instruction::Bra { to: escape })

            },
        };

    }

}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Instruction<I> {

    FnLabel { label: FnLabel },
    BrLabel { label: BrLabel },

    Push { value: Literal },

    Call { to: FnLabel },
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

    Bne { to: BrLabel }, // todo: make it all tuple variants
    Bra { to: BrLabel },

    Intrinsic(I),

}

#[derive(Clone, Copy, PartialEq, Eq)] // todo: remove eq?
pub(crate) struct Label<T> {
    pub(crate) inner: T,
}

impl<T> Label<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self { inner }
    }
}

pub(crate) type FnLabel = Label<String>;
pub(crate) type BrLabel = Label<usize>;

impl<T: fmt::Debug> fmt::Debug for Label<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Label({:?})", self.inner)
    }
}

pub(crate) struct State<I> {
    pub(crate) counter: usize,
    pub(crate) bytecode: Vec<Instruction<I>>,
}

impl<I> Default for State<I> {
    fn default() -> Self {
        Self {
            counter: 0,
            bytecode: Vec::with_capacity(128),
        }
    }
}

impl<I> State<I> {
    fn next_label(&mut self) -> BrLabel {
        self.counter += 1;
        Label::new(self.counter)
    }
}

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

