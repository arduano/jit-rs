use crate::types::IntrinsicValueType;

pub struct Type {
    pub ty: IntrinsicValueType,
    pub name: String,
}

pub struct VariableDeclare {
    pub name: String,
    pub type_: Type,
}

pub struct Function {
    pub name: String,
    pub args: Vec<VariableDeclare>,
    pub ret_type: Type,
    pub body: Vec<BodyStatement>,
}

#[derive(Debug, Clone)]
pub struct BodyStatement {
    pub kind: BodyStatementKind,
}

#[derive(Debug, Clone)]
pub enum BodyStatementKind {
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryMathOp(BinaryMathOp),
    ComparisonOp(ComparisonOp),
    SingleOp(SingleOp),
    ReadVar(String),
}

#[derive(Debug, Clone)]
pub struct BinaryMathOp {
    pub op: BinaryMathOpKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryMathOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub struct ComparisonOp {
    pub op: ComparisonOpKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Copy, Clone)]
pub enum ComparisonOpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub struct SingleOp {
    pub op: SingleOpKind,
    pub expr: Box<Expression>,
}

#[derive(Debug, Copy, Clone)]
pub enum SingleOpKind {
    Neg,
}

//
// Helpers
//

impl Expression {
    pub fn read_var(name: impl Into<String>) -> Self {
        Expression::ReadVar(name.into())
    }

    pub fn new_math_op(op: BinaryMathOpKind, lhs: Expression, rhs: Expression) -> Self {
        Expression::BinaryMathOp(BinaryMathOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn new_cmp_op(op: ComparisonOpKind, lhs: Expression, rhs: Expression) -> Self {
        Expression::ComparisonOp(ComparisonOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn new_single_op(op: SingleOpKind, expr: Expression) -> Self {
        Expression::SingleOp(SingleOp {
            op,
            expr: Box::new(expr),
        })
    }

    pub fn add(lhs: Expression, rhs: Expression) -> Self {
        Self::new_math_op(BinaryMathOpKind::Add, lhs, rhs)
    }

    pub fn sub(lhs: Expression, rhs: Expression) -> Self {
        Self::new_math_op(BinaryMathOpKind::Sub, lhs, rhs)
    }

    pub fn mul(lhs: Expression, rhs: Expression) -> Self {
        Self::new_math_op(BinaryMathOpKind::Mul, lhs, rhs)
    }

    pub fn div(lhs: Expression, rhs: Expression) -> Self {
        Self::new_math_op(BinaryMathOpKind::Div, lhs, rhs)
    }

    pub fn neg(expr: Expression) -> Self {
        Self::new_single_op(SingleOpKind::Neg, expr)
    }

    pub fn eq(lhs: Expression, rhs: Expression) -> Self {
        Self::new_cmp_op(ComparisonOpKind::Eq, lhs, rhs)
    }

    pub fn ne(lhs: Expression, rhs: Expression) -> Self {
        Self::new_cmp_op(ComparisonOpKind::Ne, lhs, rhs)
    }

    pub fn lt(lhs: Expression, rhs: Expression) -> Self {
        Self::new_cmp_op(ComparisonOpKind::Lt, lhs, rhs)
    }

    pub fn le(lhs: Expression, rhs: Expression) -> Self {
        Self::new_cmp_op(ComparisonOpKind::Le, lhs, rhs)
    }

    pub fn gt(lhs: Expression, rhs: Expression) -> Self {
        Self::new_cmp_op(ComparisonOpKind::Gt, lhs, rhs)
    }

    pub fn ge(lhs: Expression, rhs: Expression) -> Self {
        Self::new_cmp_op(ComparisonOpKind::Ge, lhs, rhs)
    }
}

impl Type {
    pub fn new(ty: IntrinsicValueType) -> Self {
        Self {
            ty,
            name: ty.name().to_string(),
        }
    }
}

impl From<IntrinsicValueType> for Type {
    fn from(ty: IntrinsicValueType) -> Self {
        Self::new(ty)
    }
}

impl VariableDeclare {
    pub fn new(name: impl Into<String>, ty: impl Into<Type>) -> Self {
        Self {
            name: name.into(),
            type_: ty.into(),
        }
    }
}
