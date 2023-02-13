use crate::{
    common::{IntBits, NumberKind},
    mir::{misc::mir_extend_num_to_vector, MirVectorBinaryOp},
    tree_parser::{
        TreeBinaryOpKind, TreeBinaryOpList, TreeExpression, TreeUnaryOp, TreeUnaryOpKind,
    },
};

use super::{
    mir_deref_expr, mir_parse_expression, ExprLocation, MirBinaryOp, MirExpression,
    MirExpressionContext, MirExpressionKind, MirIntrinsicBinaryOp, MirIntrinsicUnaryOp,
    MirIntrinsicVectorBinaryOp, MirType, MirUnaryOp,
};

#[derive(Debug, Clone)]
struct TreeBinaryOpSlice {
    list: TreeBinaryOpList,
}

impl TreeBinaryOpSlice {
    fn advance(&mut self) -> (TreeExpression, TreeBinaryOpKind) {
        let (expr, op) = self.list.pairs.remove(0);
        (expr, op)
    }

    fn is_empty(&self) -> bool {
        self.list.pairs.is_empty()
    }

    fn get_last(self) -> TreeExpression {
        *self.list.last
    }
}

// FIXME: Don't erquire an owned list
pub fn mir_parse_binary_expr_list(
    expr: TreeBinaryOpList,
    ctx: &mut MirExpressionContext,
) -> Result<MirExpression, ()> {
    let mut slice = TreeBinaryOpSlice { list: expr };

    let (left, op) = slice.advance();
    let left = mir_parse_expression(&left, ctx, ExprLocation::Other)?;
    mir_parse_binary_expr_slice(left, op, slice, ctx)
}

fn mir_parse_binary_expr_slice(
    left: MirExpression,
    op1: TreeBinaryOpKind,
    mut expr: TreeBinaryOpSlice,
    ctx: &mut MirExpressionContext,
) -> Result<MirExpression, ()> {
    if expr.is_empty() {
        let right = mir_parse_expression(&expr.get_last(), ctx, ExprLocation::Other)?;
        return seal_mir_binary_expr(left, op1, right);
    }

    let (right, op2) = expr.advance();
    let right = mir_parse_expression(&right, ctx, ExprLocation::Other)?;

    let op1_prec = mir_op_precedence(op1);
    let op2_prec = mir_op_precedence(op2);

    let expr = if op1_prec >= op2_prec {
        let sealed = seal_mir_binary_expr(left, op1, right)?;
        mir_parse_binary_expr_slice(sealed, op2, expr, ctx)?
    } else {
        let right = mir_parse_binary_expr_slice(right, op2, expr, ctx)?;
        seal_mir_binary_expr(left, op1, right)?
    };

    Ok(expr)
}

fn mir_op_precedence(op: TreeBinaryOpKind) -> u32 {
    match op {
        TreeBinaryOpKind::Eq => 10,
        TreeBinaryOpKind::Neq => 10,
        TreeBinaryOpKind::Lt => 10,
        TreeBinaryOpKind::Gt => 10,
        TreeBinaryOpKind::Lte => 10,
        TreeBinaryOpKind::Gte => 10,

        TreeBinaryOpKind::Add => 20,
        TreeBinaryOpKind::Sub => 20,

        TreeBinaryOpKind::Mul => 30,
        TreeBinaryOpKind::Div => 30,

        TreeBinaryOpKind::Mod => 40,

        TreeBinaryOpKind::BinaryAnd => 50,
        TreeBinaryOpKind::BinaryOr => 50,
        TreeBinaryOpKind::BinaryXor => 50,
    }
}

fn seal_mir_binary_expr(
    left: MirExpression,
    op: TreeBinaryOpKind,
    right: MirExpression,
) -> Result<MirExpression, ()> {
    use MirIntrinsicBinaryOp as Op;
    use MirIntrinsicVectorBinaryOp as VOp;

    let (op, ty) = match op {
        TreeBinaryOpKind::Lt => (
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatLt
                } else if is_uint_type(&left.ty) {
                    MirIntrinsicBinaryOp::UIntLt
                } else if is_sint_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntLt
                } else {
                    return Err(());
                }
            },
            MirType::Bool,
        ),
        TreeBinaryOpKind::Gt => (
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatGt
                } else if is_uint_type(&left.ty) {
                    MirIntrinsicBinaryOp::UIntGt
                } else if is_sint_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntGt
                } else {
                    return Err(());
                }
            },
            MirType::Bool,
        ),
        TreeBinaryOpKind::Lte => (
            {
                if &left.ty != &right.ty {
                    return Err(());
                } else {
                    if is_float_type(&left.ty) {
                        MirIntrinsicBinaryOp::FloatLte
                    } else if is_uint_type(&left.ty) {
                        MirIntrinsicBinaryOp::UIntLte
                    } else if is_sint_type(&left.ty) {
                        MirIntrinsicBinaryOp::IntLte
                    } else {
                        return Err(());
                    }
                }
            },
            MirType::Bool,
        ),
        TreeBinaryOpKind::Gte => (
            {
                if &left.ty != &right.ty {
                    return Err(());
                } else {
                    if is_float_type(&left.ty) {
                        MirIntrinsicBinaryOp::FloatGte
                    } else if is_uint_type(&left.ty) {
                        MirIntrinsicBinaryOp::UIntGte
                    } else if is_sint_type(&left.ty) {
                        MirIntrinsicBinaryOp::IntGte
                    } else {
                        return Err(());
                    }
                }
            },
            MirType::Bool,
        ),
        TreeBinaryOpKind::Eq => (
            {
                if &left.ty != &right.ty {
                    return Err(());
                } else {
                    if is_float_type(&left.ty) {
                        MirIntrinsicBinaryOp::FloatEq
                    } else if is_int_type(&left.ty) {
                        MirIntrinsicBinaryOp::IntEq
                    } else {
                        return Err(());
                    }
                }
            },
            MirType::Bool,
        ),
        TreeBinaryOpKind::Neq => (
            {
                if &left.ty != &right.ty {
                    return Err(());
                } else {
                    if is_float_type(&left.ty) {
                        MirIntrinsicBinaryOp::FloatNeq
                    } else if is_int_type(&left.ty) {
                        MirIntrinsicBinaryOp::IntNeq
                    } else {
                        return Err(());
                    }
                }
            },
            MirType::Bool,
        ),
        TreeBinaryOpKind::Add => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: Some(Op::FloatAdd),
                    uint: Some(Op::IntAdd),
                    sint: Some(Op::IntAdd),
                    vec_float: Some(VOp::FloatAdd),
                    vec_uint: Some(VOp::IntAdd),
                    vec_sint: Some(VOp::IntAdd),
                },
            )
        }
        TreeBinaryOpKind::Sub => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: Some(Op::FloatSub),
                    uint: Some(Op::IntSub),
                    sint: Some(Op::IntSub),
                    vec_float: Some(VOp::FloatSub),
                    vec_uint: Some(VOp::IntSub),
                    vec_sint: Some(VOp::IntSub),
                },
            )
        }
        TreeBinaryOpKind::Mul => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: Some(Op::FloatMul),
                    uint: Some(Op::IntMul),
                    sint: Some(Op::IntMul),
                    vec_float: Some(VOp::FloatMul),
                    vec_uint: Some(VOp::IntMul),
                    vec_sint: Some(VOp::IntMul),
                },
            )
        }
        TreeBinaryOpKind::Div => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: Some(Op::FloatDiv),
                    uint: Some(Op::UIntDiv),
                    sint: Some(Op::IntDiv),
                    vec_float: Some(VOp::FloatDiv),
                    vec_uint: Some(VOp::UIntDiv),
                    vec_sint: Some(VOp::IntDiv),
                },
            )
        }
        TreeBinaryOpKind::Mod => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: Some(Op::FloatRem),
                    uint: Some(Op::UIntRem),
                    sint: Some(Op::IntRem),
                    vec_float: Some(VOp::FloatRem),
                    vec_uint: Some(VOp::UIntRem),
                    vec_sint: Some(VOp::IntRem),
                },
            )
        }
        TreeBinaryOpKind::BinaryAnd => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: None,
                    uint: Some(Op::IntAnd),
                    sint: Some(Op::IntAnd),
                    vec_float: None,
                    vec_uint: Some(VOp::IntAnd),
                    vec_sint: Some(VOp::IntAnd),
                },
            )
        }
        TreeBinaryOpKind::BinaryOr => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: None,
                    uint: Some(Op::IntOr),
                    sint: Some(Op::IntOr),
                    vec_float: None,
                    vec_uint: Some(VOp::IntOr),
                    vec_sint: Some(VOp::IntOr),
                },
            )
        }
        TreeBinaryOpKind::BinaryXor => {
            return seal_mir_arithmetic_expr(
                (left, op, right),
                ArithmeticOperations {
                    float: None,
                    uint: Some(Op::IntXor),
                    sint: Some(Op::IntXor),
                    vec_float: None,
                    vec_uint: Some(VOp::IntXor),
                    vec_sint: Some(VOp::IntXor),
                },
            )
        }
    };

    Ok(MirExpression {
        ty,
        kind: MirExpressionKind::BinaryOp(Box::new(MirBinaryOp {
            op,
            lhs: left,
            rhs: right,
        })),
    })
}

type MirExprData = (MirExpression, TreeBinaryOpKind, MirExpression);

struct ArithmeticOperations {
    float: Option<MirIntrinsicBinaryOp>,
    uint: Option<MirIntrinsicBinaryOp>,
    sint: Option<MirIntrinsicBinaryOp>,
    vec_float: Option<MirIntrinsicVectorBinaryOp>,
    vec_uint: Option<MirIntrinsicVectorBinaryOp>,
    vec_sint: Option<MirIntrinsicVectorBinaryOp>,
}

fn seal_mir_arithmetic_expr(
    data: MirExprData,
    ops: ArithmeticOperations,
) -> Result<MirExpression, ()> {
    let (left, tree_op, right) = data;

    match (&left.ty, &right.ty) {
        // Unit operations
        (&MirType::Num(lty), &MirType::Num(rty)) => {
            if lty != rty {
                panic!(
                    "Operator {:?} not allowed between {:?} and {:?}",
                    tree_op, left.ty, right.ty
                )
            }

            let basic_op = match lty {
                NumberKind::Float(_) => ops.float,
                NumberKind::SignedInt(_) => ops.sint,
                NumberKind::UnsignedInt(_) => ops.uint,
            };

            if let Some(op) = basic_op {
                return Ok(MirExpression {
                    ty: left.ty.clone(),
                    kind: MirExpressionKind::BinaryOp(Box::new(MirBinaryOp {
                        op,
                        lhs: left,
                        rhs: right,
                    })),
                });
            }
        }
        (&MirType::Vector(lty, lwidth), &MirType::Vector(rty, rwidth)) => {
            if lty != rty || lwidth != rwidth {
                panic!(
                    "Operator {:?} not allowed between {:?} and {:?}",
                    tree_op, left.ty, right.ty
                )
            }

            let vector_op = match lty {
                NumberKind::SignedInt(IntBits::BitsSize)
                | NumberKind::UnsignedInt(IntBits::BitsSize) => None,

                NumberKind::Float(_) => ops.vec_float,
                NumberKind::SignedInt(_) => ops.vec_sint,
                NumberKind::UnsignedInt(_) => ops.vec_uint,
            };

            if let Some(op) = vector_op {
                return Ok(MirExpression {
                    ty: left.ty.clone(),
                    kind: MirExpressionKind::VectorBinaryOp(Box::new(MirVectorBinaryOp {
                        op,
                        lhs: left,
                        rhs: right,
                        scalar_ty: lty,
                        width: lwidth,
                    })),
                });
            }
        }

        (&MirType::Num(lty), &MirType::Vector(rty, rwidth)) => {
            if lty != rty {
                panic!(
                    "Operator {:?} not allowed between {:?} and {:?}",
                    tree_op, left.ty, right.ty
                )
            }

            let left = mir_extend_num_to_vector(left, rwidth)?;
            return seal_mir_arithmetic_expr((left, tree_op, right), ops);
        }
        (&MirType::Vector(lty, lwidth), &MirType::Num(rty)) => {
            if lty != rty {
                panic!(
                    "Operator {:?} not allowed between {:?} and {:?}",
                    tree_op, left.ty, right.ty
                )
            }

            let right = mir_extend_num_to_vector(right, lwidth)?;
            return seal_mir_arithmetic_expr((left, tree_op, right), ops);
        }

        _ => {}
    }

    panic!(
        "Operator {:?} not allowed between {:?} and {:?}",
        tree_op, left.ty, right.ty
    );
    #[allow(unreachable_code)]
    Err(())
}

pub fn mir_parse_unary_expr(
    unary: &TreeUnaryOp,
    ctx: &mut MirExpressionContext,
    pos: ExprLocation,
) -> Result<MirExpression, ()> {
    let op = &unary.op;

    let expr_pos = if op == &TreeUnaryOpKind::Ref {
        ExprLocation::NoDeref
    } else {
        ExprLocation::Other
    };

    let expr = mir_parse_expression(&unary.expr, ctx, expr_pos)?;

    let (op, ty) = match op {
        TreeUnaryOpKind::Neg => {
            if is_float_type(&expr.ty) {
                (MirIntrinsicUnaryOp::FloatNeg, expr.ty.clone())
            } else if is_int_type(&expr.ty) {
                (MirIntrinsicUnaryOp::IntNeg, expr.ty.clone())
            } else {
                return Err(());
            }
        }
        TreeUnaryOpKind::Deref => {
            if is_ptr_type(&expr.ty) {
                if pos == ExprLocation::NoDeref {
                    // This is an edge case where the expression is unaffected
                    return Ok(expr);
                } else {
                    return Ok(mir_deref_expr(expr, ctx));
                }
            } else {
                return Err(());
            }
        }
        TreeUnaryOpKind::Ref => {
            if is_ptr_type(&expr.ty) {
                return Ok(expr);
            } else {
                return Err(());
            }
        }
    };

    Ok(MirExpression {
        ty,
        kind: MirExpressionKind::UnaryOp(Box::new(MirUnaryOp { op, operand: expr })),
    })
}

fn is_float_type(ty: &MirType) -> bool {
    match &ty {
        MirType::Num(NumberKind::Float(_)) => true,
        _ => false,
    }
}

fn is_uint_type(ty: &MirType) -> bool {
    match &ty {
        MirType::Num(NumberKind::UnsignedInt(_)) => true,
        _ => false,
    }
}

fn is_sint_type(ty: &MirType) -> bool {
    match &ty {
        MirType::Num(NumberKind::SignedInt(_)) => true,
        _ => false,
    }
}

fn is_int_type(ty: &MirType) -> bool {
    is_uint_type(ty) || is_sint_type(ty)
}

fn is_ptr_type(ty: &MirType) -> bool {
    match &ty {
        MirType::Ptr(_) => true,
        _ => false,
    }
}
