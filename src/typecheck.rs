
use std::{convert::identity, iter};

use crate::{codegen::{InstrKind, Producer, Bytecode, FileSpan, Label, BytecodeSlice, Program, FunWithMetadata}, parser::{Type, literal_type, Span, Signature, ParsedSignature, TypeLiteral}, arch::Intrinsic, diagnostic::Diagnostic};

pub(crate) fn typecheck<I: Intrinsic>(program: &Program<I>) -> Result<(), TypeError> {

    let tc_state = TcState {
        program
    };

    for fun in program.funs.values() {
        typecheck_fun(&tc_state, &fun)?;
    }

    Ok(())

}

fn typecheck_fun<I: Intrinsic>(tc_state: &TcState<I>, fun: &FunWithMetadata<I>) -> Result<(), TypeError> {

    let mut state = BlockState {
        body: &fun.body,
        file_name: &fun.file_name,
        stack: Vec::new(),
        ip: 0,
    };

    // todo: push signature onto the stack

    let takes = translate_type_lit(&fun.signature.takes)?;
    state.stack.extend(takes);

    typecheck_block(tc_state, &mut state, &fun.body)?;

    // todo: actually verify signature

    let returns = translate_type_lit(&fun.signature.returns)?;
    if state.stack != returns {
        return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: returns, got: state.stack }))
    }

    Ok(())

}

fn translate_type_lit(types: &Vec<TypeLiteral>) -> Result<Vec<Type>, TypeError> {

    let mut result = Vec::new();

    for item in types {
        match &item.name[..] {
            "int" => result.push(Type::Int),
            other => todo!("invalid type literal in signature: {}", other),
        }
    }

    Ok(result)

}

fn typecheck_block<I: Intrinsic>(tc_state: &TcState<I>, block_state: &mut BlockState<I>, block: &BytecodeSlice<I>) -> Result<(), TypeError> {

    while let Some(instr) = block.get(block_state.ip) {

        match typecheck_instr(tc_state, block_state, &instr.kind) {  
            Ok(()) => (),
            Err(mut err) => {
                err.file_span = FileSpan {
                    span: instr.span,
                    file: block_state.file_name.to_string(),
                };
                return Err(err)
            }
        }

        block_state.ip += 1;

    }

    Ok(())

}

fn typecheck_instr<I: Intrinsic>(tc_state: &TcState<I>, block_state: &mut BlockState<I>, instr_kind: &InstrKind<I>) -> Result<(), TypeError> {

    match instr_kind {

        InstrKind::Push { value } => {
            block_state.stack.push(literal_type(value))
        },

        InstrKind::Call { to } => { // todo: use fn signature
            let inner = match tc_state.program.funs.get(to) {
                Some(val) => val,
                None => return Err(TypeError::unspanned(TypeErrorKind::UnknownFn { name: to.to_string() }))
            };
            todo!("typecheck signature");
            // let fun = &bytecode[position].kind;
            // if let InstrKind::FnLabel { label, signature } = fun {
            //     let elems = split_signature_dynamic(stack, signature.takes.len());
            //     for (lhs, rhs) in iter::zip(signature, &elems) {
            //         if Some(lhs) != rhs { return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: signature.takes.to_vec(), got: elems.into_iter().filter_map(identity).collect() })) }
            //     }
            //     stack.extend(signature.returns);
            // } else {
            //     unreachable!()
            // }
        },

        InstrKind::Return => (),

        InstrKind::Drop => {
            if let Some(val) = block_state.stack.pop() {
                drop(val)
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any], got: block_state.stack.to_vec() }))
            }
        },
        InstrKind::Copy => {
            if let Some(val) = block_state.stack.pop() {
                block_state.stack.push(val.clone());
                block_state.stack.push(val.clone());
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any], got: block_state.stack.to_vec() }))
            }
        },
        InstrKind::Over => {
            let got = split_signature::<2>(&mut block_state.stack);
            if let [Some(first), Some(second)] = got {
                block_state.stack.push(first.clone());
                block_state.stack.push(second.clone());
                block_state.stack.push(first.clone());
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 2], got: block_state.stack.to_vec() }))
            }
        },
        InstrKind::Swap => {
            let got = split_signature::<2>(&mut block_state.stack);
            if let [Some(first), Some(second)] = got {
                block_state.stack.push(second);
                block_state.stack.push(first);
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 2], got: block_state.stack.to_vec() }))
            }
        },
        InstrKind::Rot3 => {
            let len = block_state.stack.len();
            let elems = match block_state.stack.get_mut(len - 4..) {
                Some(val) => val,
                None => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 3], got: block_state.stack.to_vec() }))
            };
            elems.rotate_left(1);
        },
        InstrKind::Rot4 => {
            let len = block_state.stack.len();
            let elems = match block_state.stack.get_mut(len - 5..) {
                Some(val) => val,
                None => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 4], got: block_state.stack.to_vec() }))
            };
            elems.rotate_left(1);
        },

        InstrKind::Read => {
            match block_state.stack.pop() {
                Some(Type::Ptr { inner }) => {
                    block_state.stack.push(*inner);
                },
                other => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Ptr { inner: Box::new(Type::Any) }], got: other.into_iter().collect() }))
            }
        },
        InstrKind::Write => {
            let got = split_signature::<2>(&mut block_state.stack);
            match got { // todo: rewrtie thi
                [Some(Type::Ptr { ref inner }), Some(ref value)] => {
                    if inner.as_ref() != value {
                        return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Ptr { inner: Box::new(value.clone()) }, Type::Any], got: got.into_iter().filter_map(identity).collect() }))
                    };
                    block_state.stack.push(value.clone());
                },
                other => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Ptr { inner: Box::new(Type::Any) }, Type::Any], got: other.into_iter().filter_map(identity).collect() }))
            }
        },
        // Move,

        // Addr,
        // Type,
        // Size,

        // Access,

        InstrKind::Add => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Int);
        },
        InstrKind::Sub => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Int);
        },
        InstrKind::Mul => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Int);
        },
        InstrKind::Dvm => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Int);
        },

        InstrKind::Not => {
            let got = split_signature::<1>(&mut block_state.stack);
            verify_signature([Type::Bool], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::And => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Bool, Type::Bool], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::Or  => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Bool, Type::Bool], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::Xor => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Bool, Type::Bool], got)?;
            block_state.stack.push(Type::Bool);
        },
                
        InstrKind::Eq  => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::Gt  => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::Gte => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::Lt  => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Bool);
        },
        InstrKind::Lte => {
            let got = split_signature::<2>(&mut block_state.stack);
            verify_signature([Type::Int, Type::Int], got)?;
            block_state.stack.push(Type::Bool);
        },

        InstrKind::Bne { to } => {

            let got = split_signature::<1>(&mut block_state.stack);
            verify_signature([Type::Bool], got)?;

            let if_label = find_label(block_state.body, *to, Producer::If).expect("invalid br-label");
            let else_bra = &block_state.body[if_label - 1]; // the bra from the `else` should be there

            if let InstrKind::Bra { to: else_to } = &else_bra.kind { // todo: harden check (producer == Else)
                // `if/else` block
                let else_label = find_label(block_state.body, *else_to, Producer::Else).expect("invalid br-label");
                let mut if_state = block_state.clone_and_reset_ip();
                let mut else_state = block_state.clone_and_reset_ip();
                typecheck_block(tc_state, &mut if_state, &block_state.body[block_state.ip + 1 .. if_label])?;
                typecheck_block(tc_state, &mut else_state, &block_state.body[if_label .. else_label])?;
                if if_state.stack != else_state.stack {
                    return Err(TypeError::unspanned(TypeErrorKind::BranchesNotEqual));
                }
                block_state.stack = if_state.stack;
                let end_position = find_label(block_state.body, *else_to, Producer::Else).expect("invalid br-label");
                block_state.ip = end_position; // remember: ip will be incremented again when we return here
            } else {
                // `if` block
                let mut if_state = block_state.clone_and_reset_ip();
                typecheck_block(tc_state, &mut if_state, &block_state.body[block_state.ip + 1 .. if_label])?;
                if block_state.stack != if_state.stack {
                    return Err(TypeError::unspanned(TypeErrorKind::BranchesNotEmpty));
                }
                block_state.ip = if_label; // remember: ip will be incremented again when we return here
            }

        },

        InstrKind::Label { label, producer: Producer::Loop } => {

            let loop_bra = find_instr_kind(block_state.body, InstrKind::Bra { to: *label }).expect("cannot find loop-bra");

            let mut loop_state = block_state.clone_and_reset_ip();
            typecheck_block(tc_state, &mut loop_state, &block_state.body[block_state.ip + 1 .. loop_bra])?;
            if block_state.stack != loop_state.stack {
                return Err(TypeError::unspanned(TypeErrorKind::BranchesNotEmpty));
            }
            block_state.ip = loop_bra; // remember: ip will be incremented again when we return here

        },

        InstrKind::Intrinsic(intrinsic) => {
            I::signature(intrinsic);
        },

        InstrKind::Bra { .. } => (),
        InstrKind::Label { .. } => (),

    };

    Ok(())

}

struct TcState<'b, I> {
    program: &'b Program<I>
}

struct BlockState<'b, I> {
    pub body: &'b Bytecode<I>,
    pub file_name: &'b str,
    pub stack: Vec<Type>,
    pub ip: usize,
}

impl<I> BlockState<'_, I> {
    pub(crate) fn clone_and_reset_ip(&self) -> Self {
        Self {
            body: self.body,
            file_name: self.file_name,
            stack: Clone::clone(&self.stack),
            ip: 0
        }
    }
}

pub(crate) fn split_signature<const N: usize>(vec: &mut Vec<Type>) -> [Option<Type>; N] {
    const NONE: Option<Type> = None;
    let mut res = [NONE; N];
    let start = if vec.len() < N { vec.len() as isize - 1 } else { vec.len() as isize - N as isize };
    for (idx, item) in vec.drain((start.max(0) as usize)..).rev().enumerate() {
        res[idx] = Some(item);
    }
    res
}

pub(crate) fn split_signature_dynamic(vec: &mut Vec<Type>, n: usize) -> Vec<Option<Type>> {
    const NONE: Option<Type> = None;
    let mut res = vec![NONE; n];
    let start = if vec.len() < n { vec.len() - 1 } else { vec.len() - n };
    for (idx, item) in vec.drain(start..).rev().enumerate() {
        res[idx] = Some(item);
    }
    res
}

pub(crate) fn verify_signature<const N: usize>(want: [Type; N], got: [Option<Type>; N]) -> Result<(), TypeError> {
    for (lhs, rhs) in iter::zip(&want, &got) {
        let expect_any   = lhs == &Type::Any && rhs.is_none();
        let expect_match = lhs != &Type::Any && Some(lhs) != rhs.as_ref();
        if expect_any || expect_match {
            return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: want.to_vec(), got: got.into_iter().filter_map(identity).collect() }))
        }
    }
    Ok(())
}

fn find_instr_kind<I: Intrinsic>(bytecode: &BytecodeSlice<I>, instr_kind: InstrKind<I>) -> Option<usize> {
    bytecode.iter().position(|item| item.kind == instr_kind)
}

fn find_label<I: Intrinsic>(bytecode: &BytecodeSlice<I>, label: Label, producer: Producer) -> Option<usize> {
    find_instr_kind(bytecode, InstrKind::Label { label, producer })
}

pub(crate) struct TypeError {
    kind: TypeErrorKind,
    file_span: FileSpan,
}

impl TypeError {
    pub(crate) fn unspanned(kind: TypeErrorKind) -> Self {
        Self { kind, file_span: FileSpan::default() }
    }
}

pub(crate) enum TypeErrorKind {
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Mismatch { want: Vec<Type>, got: Vec<Type> }
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    let diag = match &value.kind {
        TypeErrorKind::UnknownFn { name } => Diagnostic::error("unknown function").code(name),
        TypeErrorKind::BranchesNotEmpty => {
            Diagnostic::error("branch changes stack")
                .note("this branch may not change the types on the stack")
                .note("use `if/else` instead")
        },
        TypeErrorKind::BranchesNotEqual => {
            Diagnostic::error("branches not equal")
                .note("both branches have to evaluate to the same types")
        },
        TypeErrorKind::Mismatch { want, got } => {
            Diagnostic::error("type mismatch")
                .note(format!("want: {:?}", want))
                .note(format!("got: {:?}", got))
        }
    };
    if value.file_span.span != Span::default() {
        diag.file(&value.file_span.file)
            .pos(value.file_span.span.to_pos())
    } else {
        diag
    }
}

