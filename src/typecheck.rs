
use std::{convert::identity, iter, collections::HashMap};

use crate::{codegen::{InstrKind, Producer, Instr, Bytecode, FileSpan}, parser::{Type, literal_type, Span}, arch::Intrinsic, diagnostic::Diagnostic};

pub(crate) fn typecheck<I: Intrinsic>(funs: &HashMap<String, Bytecode<I>>) -> Result<(), TypeError> {

    let main = match funs.get("main") {
        Some(val) => val,
        None => return Err(TypeError::unspanned(TypeErrorKind::MissingMain))
    };

    let mut stack = Vec::new();
    eval_fn(&funs, &mut stack, main, 0, InstrKind::Return)?;

    if !stack.is_empty() {
        return Err(TypeError::unspanned(TypeErrorKind::InvalidMain { got: stack }))
    }

    Ok(())

}

fn eval_fn<I: Intrinsic>(funs: &HashMap<String, Bytecode<I>>, stack: &mut Vec<Type>, body: &Bytecode<I>, start: usize, end: InstrKind<I>) -> Result<(), TypeError> {

    let file_name = "{unknown}"; // todo: make every function have a file name as associated
    // metadata
    let mut ip = start;

    loop {

        let instr = &body[ip];

        if &instr.kind == &end {
            return Ok(())
        }

        match eval_instr(funs, body, stack, &mut ip, &instr.kind) {
            Ok(()) => (),
            Err(mut err) => {
                err.file_span = FileSpan {
                    span: instr.span,
                    file: file_name.to_string(),
                };
                return Err(err)
            }
        };

        ip += 1;

    }

}

pub(crate) fn eval_instr<I: Intrinsic>(funs: &HashMap<String, Bytecode<I>>, body: &Bytecode<I>, stack: &mut Vec<Type>, ip: &mut usize, instr_kind: &InstrKind<I>) -> Result<(), TypeError> {

    match instr_kind {

        InstrKind::Push { value } => {
            stack.push(literal_type(value))
        },

        InstrKind::Call { to } => { // todo: use fn signature
            let body = match funs.get(to) {
                Some(val) => val,
                None => return Err(TypeError::unspanned(TypeErrorKind::UnknownFn { name: to.to_string() }))
            };
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
            eval_fn(funs, stack, body, 0, InstrKind::Return)?;
        },
        InstrKind::Return => (),

        InstrKind::Drop => {
            if let Some(val) = stack.pop() {
                drop(val)
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any], got: stack.to_vec() }))
            }
        },
        InstrKind::Copy => {
            if let Some(val) = stack.pop() {
                stack.push(val.clone());
                stack.push(val.clone());
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any], got: stack.to_vec() }))
            }
        },
        InstrKind::Over => {
            let got = split_signature::<2>(stack);
            if let [Some(first), Some(second)] = got {
                stack.push(first.clone());
                stack.push(second.clone());
                stack.push(first.clone());
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 2], got: stack.to_vec() }))
            }
        },
        InstrKind::Swap => {
            let got = split_signature::<2>(stack);
            if let [Some(first), Some(second)] = got {
                stack.push(second);
                stack.push(first);
            } else {
                return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 2], got: stack.to_vec() }))
            }
        },
        InstrKind::Rot3 => {
            let len = stack.len();
            let elems = match stack.get_mut(len - 4..) {
                Some(val) => val,
                None => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 3], got: stack.to_vec() }))
            };
            elems.rotate_left(1);
        },
        InstrKind::Rot4 => {
            let len = stack.len();
            let elems = match stack.get_mut(len - 5..) {
                Some(val) => val,
                None => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Any; 4], got: stack.to_vec() }))
            };
            elems.rotate_left(1);
        },

        InstrKind::Read => {
            match stack.pop() {
                Some(Type::Ptr { inner }) => {
                    stack.push(*inner);
                },
                other => return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Ptr { inner: Box::new(Type::Any) }], got: other.into_iter().collect() }))
            }
        },
        InstrKind::Write => {
            let got = split_signature::<2>(stack);
            match got { // todo: rewrtie thi
                [Some(Type::Ptr { ref inner }), Some(ref value)] => {
                    if inner.as_ref() != value {
                        return Err(TypeError::unspanned(TypeErrorKind::Mismatch { want: vec![Type::Ptr { inner: Box::new(value.clone()) }, Type::Any], got: got.into_iter().filter_map(identity).collect() }))
                    };
                    stack.push(value.clone());
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
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Int);
        },
        InstrKind::Sub => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Int);
        },
        InstrKind::Mul => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Int);
        },
        InstrKind::Dvm => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Int);
        },

        InstrKind::Not => {
            let got = split_signature::<1>(stack);
            verify_signature([Type::Bool], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::And => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Bool, Type::Bool], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::Or  => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Bool, Type::Bool], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::Xor => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Bool, Type::Bool], got)?;
            stack.push(Type::Bool);
        },
                
        InstrKind::Eq  => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::Gt  => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::Gte => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::Lt  => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Bool);
        },
        InstrKind::Lte => {
            let got = split_signature::<2>(stack);
            verify_signature([Type::Int, Type::Int], got)?;
            stack.push(Type::Bool);
        },

        InstrKind::Bne { to } => {

            let got = split_signature::<1>(stack);
            verify_signature([Type::Bool], got)?;

            let position = find_label(body, to).expect("invalid br-label");
            let else_bra = &body[position - 1]; // the bra from the `else` should be there

            if let InstrKind::Bra { to: else_to } = &else_bra.kind {
                // `if/else` block
                let mut if_stack = stack.clone();
                let mut else_stack = stack.clone();
                eval_fn(funs, &mut if_stack, body, *ip + 1, InstrKind::Label { label: *to, producer: Producer::If })?;
                eval_fn(funs, &mut else_stack, body, position, InstrKind::Label { label: *else_to, producer: Producer::Else })?;
                if if_stack != else_stack {
                    return Err(TypeError::unspanned(TypeErrorKind::BranchesNotEqual));
                }
                *stack = if_stack;
                let end_position = find_label(body, else_to).expect("invalid br-label");
                *ip = end_position; // remember: ip will be incremented again when we return here
            } else {
                // `if` block
                let mut if_stack = stack.clone();
                eval_fn(funs, &mut if_stack, body, *ip + 1, InstrKind::Label { label: *to, producer: Producer::If })?;
                if stack != &mut if_stack {
                    return Err(TypeError::unspanned(TypeErrorKind::BranchesNotEmpty));
                }
                *ip = position; // remember: ip will be incremented again when we return here
            }

        },

        InstrKind::Label { label, producer: Producer::Loop } => {

            let mut loop_stack = stack.clone();
            eval_fn(funs, &mut loop_stack, body, *ip + 1, InstrKind::Bra { to: *label })?;
            if stack != &mut loop_stack {
                return Err(TypeError::unspanned(TypeErrorKind::BranchesNotEmpty));
            }
            let pos = body.iter().position(|item| { // todo: wtf
                if let InstrKind::Bra { to: this } = &item.kind {
                    this == label
                } else {
                    false
                }
            }).expect("invalid loop-bra");
            *ip = pos; // remember: ip will be incremented again when we return here

        },

        InstrKind::Intrinsic(intrinsic) => {
            I::signature(intrinsic);
        },

        InstrKind::Bra { .. } => (),
        InstrKind::Label { .. } => (),

    };

    Ok(())

}

pub(crate) fn split_signature<const N: usize>(vec: &mut Vec<Type>) -> [Option<Type>; N] {
    const NONE: Option<Type> = None;
    let mut res = [NONE; N];
    let start = if vec.len() < N { vec.len() - 1 } else { vec.len() - N };
    for (idx, item) in vec.drain(start..).rev().enumerate() {
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

fn find_label<I: Intrinsic>(bytecode: &Vec<Instr<I>>, target: &usize) -> Option<usize> {
    bytecode.iter().position(|item| {
        if let InstrKind::Label { label, .. } = &item.kind {
            label == target
        } else {
            false
        }
    })
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
    MissingMain,
    InvalidMain { got: Vec<Type> },
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Mismatch { want: Vec<Type>, got: Vec<Type> }
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    let diag = match &value.kind {
        TypeErrorKind::MissingMain => Diagnostic::error("missing `main` function"),
        TypeErrorKind::InvalidMain { got } => Diagnostic::error("invalid `main` function").note(format!("got {:?}", got)),
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

