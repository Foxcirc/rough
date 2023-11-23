
use crate::{basegen::{FileSpan, BaseProgram, Program, FunWithMetadata, InstrKind, InstrLiteral}, parser::{Span, TranslationUnit, Type, Literal}, arch::Intrinsic, diagnostic::Diagnostic};

pub(crate) fn typecheck<I: Intrinsic>(base_program: TranslationUnit<BaseProgram<I>>) -> Result<TranslationUnit<Program<I>>, TypeError> {

    let mut part = TranslationUnit {
        inner: Program { funs: Default::default(), types: Default::default() },
        arena: base_program.arena,
    };

    for (_fun_name, fun) in base_program.inner.funs {

        let mut state = State {
            stack: Vec::new(),
        };
        typecheck_fun(&mut state, fun)?;

    }

    Ok(part)

}

fn typecheck_fun<I: Intrinsic>(state: &mut State<I>, fun: FunWithMetadata<I>) -> Result<(), TypeError> {

    // todo: push signature onto the stack
    
    for instr in fun.body {

        match instr.kind {

            InstrKind::Label { label, producer } => todo!(),

            InstrKind::Push { value } => state.stack.push(Item::literal(value)),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => todo!(),

            InstrKind::Drop => todo!(),
            InstrKind::Copy => todo!(),
            InstrKind::Over => todo!(),
            InstrKind::Swap => todo!(),
            InstrKind::Rot3 => todo!(),
            InstrKind::Rot4 => todo!(),

            InstrKind::Read  => todo!(),
            InstrKind::Move  => todo!(),
            InstrKind::Write => todo!(),

            // InstrKind::Addr => todo!(),
            // InstrKind::Type => todo!(),
            // InstrKind::Size => todo!(),

            // InstrKind::Access => todo!(),

            InstrKind::Arrow => todo!(),

            InstrKind::Add => todo!(),
            InstrKind::Sub => todo!(),
            InstrKind::Mul => todo!(),
            InstrKind::Dvm => todo!(),

            InstrKind::Not => todo!(),
            InstrKind::And => todo!(),
            InstrKind::Or  => todo!(),
            InstrKind::Xor => todo!(),

            InstrKind::Eq  => todo!(),
            InstrKind::Gt  => todo!(),
            InstrKind::Gte => todo!(),
            InstrKind::Lt  => todo!(),
            InstrKind::Lte => todo!(),

            InstrKind::Bne { to } => todo!(),
            InstrKind::Bra { to } => todo!(),

            InstrKind::Intrinsic(intrinsic) => todo!(),

        }

    }

    Ok(())

}

struct State<I> {
    pub stack: Vec<Item<I>>,
}

pub(crate) enum Item<I> {
    Comptime { t: Type, v: Value<I> },
    Runtime { t: Type }
}

impl<I> Item<I> {

    pub fn literal(lit: InstrLiteral<I>) -> Self {
        match lit {
            InstrLiteral::Int   (..) => Self::Comptime { t: Type::Int, v: lit },
            InstrLiteral::Bool  (..) => Self::Comptime { t: Type::Bool, v: lit },
            InstrLiteral::Str   (..) => todo!(),
            InstrLiteral::Tuple (..) => todo!(),
        }
    }

}

pub(crate) type Value<I> = InstrLiteral<I>;

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
    Mismatch { want: Vec<()>, got: Vec<()> },
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    let diag = match value.kind {
        TypeErrorKind::UnknownFn { name } => Diagnostic::error("unknown word").code(name),
        TypeErrorKind::BranchesNotEmpty => {
            Diagnostic::error("branch changes stack")
                .note("this branch may not change the types on the stack")
                .note("consider adding an `else` block")
        },
        TypeErrorKind::BranchesNotEqual => {
            Diagnostic::error("branches not equal")
                .note("both branches have to evaluate to the same types")
        },
        TypeErrorKind::Mismatch { want, got } => {
            Diagnostic::error("type mismatch")
                .note(format!("want {:?}", want.into_iter().map(|val| val).collect::<Vec<_>>()))
                .note(format!("got {:?}", got .into_iter().map(|val| val).collect::<Vec<_>>()))
        }
    };
    if value.file_span.span != Span::default() {
        diag.file(&value.file_span.file)
            .pos(value.file_span.span.to_pos())
    } else {
        diag
    }
}

