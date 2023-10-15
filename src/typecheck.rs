
use std::{convert::identity, iter};

use crate::{codegen::{Instruction, BrLabel, FnLabel}, parser::{Type, literal_type}, arch::Intrinsic, diagnostic::Diagnostic};

pub(crate) fn typecheck<I: Intrinsic>(bytecode: Vec<Instruction<I>>) -> Result<(), TypeError> {

    let main = match find_fn_label(&bytecode, &FnLabel::new("main".to_string())) {
        Some(val) => val,
        None => return Err(TypeError::MissingMain)
    };

    let mut stack = Vec::new();
    eval_fn(&bytecode, &mut stack, main, Instruction::Return)?;

    if !stack.is_empty() {
        return Err(TypeError::InvalidMain { got: stack })
    }

    Ok(())

}

fn eval_fn<I: Intrinsic>(bytecode: &Vec<Instruction<I>>, stack: &mut Vec<Type>, start: usize, end: Instruction<I>) -> Result<(), TypeError> {

    let mut ip = start;

    loop {

        let instruction = &bytecode[ip];

        if instruction == &end {
            return Ok(())
        }

        match instruction {

            Instruction::FnLabel { .. } => (),
            Instruction::BrLabel { .. } => (),

            Instruction::Push { value } => {
                stack.push(literal_type(value))
            },

            Instruction::Call { to } => { // todo: use fn signature
                let position = match find_fn_label(&bytecode, to) {
                    Some(val) => val,
                    None => return Err(TypeError::UnknownFn { name: to.inner.clone() })
                };
                eval_fn(bytecode, stack, position, Instruction::Return)?;
            },
            Instruction::Return => (),

            Instruction::Drop => {
                if let Some(val) = stack.pop() {
                    drop(val)
                } else {
                    return Err(TypeError::Mismatch { want: vec![Type::Any], got: stack.to_vec() })
                }
            },
            Instruction::Copy => {
                if let Some(val) = stack.pop() {
                    stack.push(val.clone());
                    stack.push(val.clone());
                } else {
                    return Err(TypeError::Mismatch { want: vec![Type::Any], got: stack.to_vec() })
                }
            },
            Instruction::Over => {
                let got = split_signature::<2>(stack);
                if let [Some(first), Some(second)] = got {
                    stack.push(first.clone());
                    stack.push(second.clone());
                    stack.push(first.clone());
                } else {
                    return Err(TypeError::Mismatch { want: vec![Type::Any; 2], got: stack.to_vec() })
                }
            },
            Instruction::Swap => {
                let got = split_signature::<2>(stack);
                if let [Some(first), Some(second)] = got {
                    stack.push(second);
                    stack.push(first);
                } else {
                    return Err(TypeError::Mismatch { want: vec![Type::Any; 2], got: stack.to_vec() })
                }
            },
            Instruction::Rot3 => {
                let len = stack.len();
                let elems = match stack.get_mut(len - 4..) {
                    Some(val) => val,
                    None => return Err(TypeError::Mismatch { want: vec![Type::Any; 3], got: stack.to_vec() })
                };
                elems.rotate_left(1);
            },
            Instruction::Rot4 => {
                let len = stack.len();
                let elems = match stack.get_mut(len - 5..) {
                    Some(val) => val,
                    None => return Err(TypeError::Mismatch { want: vec![Type::Any; 4], got: stack.to_vec() })
                };
                elems.rotate_left(1);
            },

            Instruction::Read => {
                match stack.pop() {
                    Some(Type::Ptr { inner }) => {
                        stack.push(*inner);
                    },
                    other => return Err(TypeError::Mismatch { want: vec![Type::Ptr { inner: Box::new(Type::Any) }], got: other.into_iter().collect() })
                }
            },
            Instruction::Write => {
                let got = split_signature::<2>(stack);
                match got { // todo: rewrtie thi
                    [Some(Type::Ptr { ref inner }), Some(ref value)] => {
                        if inner.as_ref() != value {
                            return Err(TypeError::Mismatch { want: vec![Type::Ptr { inner: Box::new(value.clone()) }, Type::Any], got: got.into_iter().filter_map(identity).collect() })
                        };
                        stack.push(value.clone());
                    },
                    other => return Err(TypeError::Mismatch { want: vec![Type::Ptr { inner: Box::new(Type::Any) }, Type::Any], got: other.into_iter().filter_map(identity).collect() })
                }
            },
            // Move,

            // Addr,
            // Type,
            // Size,

            // Access,

            Instruction::Add => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Int);
            },
            Instruction::Sub => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Int);
            },
            Instruction::Mul => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Int);
            },
            Instruction::Dvm => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Int);
            },

            Instruction::Not => {
                let got = split_signature::<1>(stack);
                verify_signature([Type::Bool], got)?;
                stack.push(Type::Bool);
            },
            Instruction::And => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Bool, Type::Bool], got)?;
                stack.push(Type::Bool);
            },
            Instruction::Or  => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Bool, Type::Bool], got)?;
                stack.push(Type::Bool);
            },
            Instruction::Xor => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Bool, Type::Bool], got)?;
                stack.push(Type::Bool);
            },
                    

            Instruction::Eq  => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Bool);
            },
            Instruction::Gt  => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Bool);
            },
            Instruction::Gte => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Bool);
            },
            Instruction::Lt  => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Bool);
            },
            Instruction::Lte => {
                let got = split_signature::<2>(stack);
                verify_signature([Type::Int, Type::Int], got)?;
                stack.push(Type::Bool);
            },

            Instruction::Bne { to } => {

                let got = split_signature::<1>(stack);
                verify_signature([Type::Bool], got)?;

                let position = find_br_label(bytecode, to).expect("invalid br-label");
                let else_bra = &bytecode[position - 1]; // the bra from the `else` should be there

                if let Instruction::Bra { to: else_to } = else_bra {
                    // `if/else` block
                    let mut if_stack = stack.clone();
                    let mut else_stack = stack.clone();
                    eval_fn(bytecode, &mut if_stack, ip + 1, Instruction::BrLabel { label: *to })?;
                    eval_fn(bytecode, &mut else_stack, position, Instruction::BrLabel { label: *else_to })?;
                    if if_stack != else_stack {
                        return Err(TypeError::BranchesNotEqual);
                    }
                    *stack = if_stack;
                    let end_position = find_br_label(bytecode, else_to).expect("invalid br-label");
                    ip = end_position;
                } else {
                    // `if` block
                    let mut if_stack = stack.clone();
                    eval_fn(bytecode, &mut if_stack, ip + 1, Instruction::BrLabel { label: *to })?;
                    if stack != &mut if_stack {
                        return Err(TypeError::BranchesNotEmpty);
                    }
                    ip = position;
                }

            },

            Instruction::Bra { to } => {

            

            },

            Instruction::Intrinsic(intrinsic) => {
                I::signature(intrinsic);
            },

        }

        ip += 1;

    }

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

pub(crate) fn verify_signature<const N: usize>(want: [Type; N], got: [Option<Type>; N]) -> Result<(), TypeError> {
    for (lhs, rhs) in iter::zip(&want, &got) {
        let expect_any   = lhs == &Type::Any && rhs.is_none();
        let expect_match = lhs != &Type::Any && Some(lhs) != rhs.as_ref();
        if expect_any || expect_match {
            return Err(TypeError::Mismatch { want: want.to_vec(), got: got.into_iter().filter_map(identity).collect() })
        }
    }
    Ok(())
}

fn find_fn_label<I: Intrinsic>(bytecode: &Vec<Instruction<I>>, target: &FnLabel) -> Option<usize> {
    bytecode.iter().position(|item| {
        if let Instruction::FnLabel { label } = item {
            label == target
        } else {
            false
        }
    })
}

fn find_br_label<I: Intrinsic>(bytecode: &Vec<Instruction<I>>, target: &BrLabel) -> Option<usize> {
    bytecode.iter().position(|item| {
        if let Instruction::BrLabel { label } = item {
            label == target
        } else {
            false
        }
    })
}

pub(crate) enum TypeError {
    MissingMain,
    InvalidMain { got: Vec<Type> },
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Mismatch { want: Vec<Type>, got: Vec<Type> }
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    match value {
        TypeError::MissingMain => Diagnostic::error("missing `main` function"),
        TypeError::InvalidMain { got } => Diagnostic::error("invalid `main` function").note(format!("got {:?}", got)),
        TypeError::UnknownFn { name } => Diagnostic::error("unknown function").code(name),
        TypeError::BranchesNotEmpty => {
            Diagnostic::error("branch changes stack")
                .note("this branch may not change the types on the stack")
                .note("use `if/else` instead")
        },
        TypeError::BranchesNotEqual => {
            Diagnostic::error("branches not equal")
                .note("both branches have to evaluate to the same types")
        },
        TypeError::Mismatch { want, got } => {
            Diagnostic::error("type mismatch")
                .note(format!("want: {:?}", want))
                .note(format!("got: {:?}", got))
        }
    }
}

