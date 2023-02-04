use crate::tree_parser::{TreeBinaryOpKind, TreeBinaryOpList, TreeExpression};

use super::{
    mir_parse_expression, MirBinaryOp, MirExpression, MirExpressionContext, MirExpressionKind,
    MirIntrinsicBinaryOp, MirIntrinsicType, MirType, MirTypeKind,
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
    let left = mir_parse_expression(&left, ctx, false)?;
    mir_parse_binary_expr_slice(left, op, slice, ctx)
}

fn mir_parse_binary_expr_slice(
    left: MirExpression,
    op1: TreeBinaryOpKind,
    mut expr: TreeBinaryOpSlice,
    ctx: &mut MirExpressionContext,
) -> Result<MirExpression, ()> {
    if expr.is_empty() {
        let right = mir_parse_expression(&expr.get_last(), ctx, false)?;
        return seal_mir_parse_binary_expr(left, op1, right);
    }

    let (right, op2) = expr.advance();
    let right = mir_parse_expression(&right, ctx, false)?;

    let op1_prec = mir_op_precedence(op1);
    let op2_prec = mir_op_precedence(op2);

    let expr = if op1_prec < op2_prec {
        let sealed = seal_mir_parse_binary_expr(left, op1, right)?;
        mir_parse_binary_expr_slice(sealed, op2, expr, ctx)?
    } else {
        let right = mir_parse_binary_expr_slice(right, op2, expr, ctx)?;
        seal_mir_parse_binary_expr(left, op1, right)?
    };

    Ok(expr)
}

fn mir_op_precedence(op: TreeBinaryOpKind) -> u32 {
    match op {
        TreeBinaryOpKind::Add => 1,
        TreeBinaryOpKind::Sub => 1,
        TreeBinaryOpKind::Mul => 2,
        TreeBinaryOpKind::Div => 2,
        TreeBinaryOpKind::Mod => 2,
        TreeBinaryOpKind::Lt => 3,
        TreeBinaryOpKind::Gt => 3,
        TreeBinaryOpKind::Eq => 4,
        TreeBinaryOpKind::Neq => 4,
        TreeBinaryOpKind::Lte => 3,
        TreeBinaryOpKind::Gte => 3,
        TreeBinaryOpKind::And => 5,
        TreeBinaryOpKind::Or => 6,
    }
}

fn seal_mir_parse_binary_expr(
    left: MirExpression,
    op: TreeBinaryOpKind,
    right: MirExpression,
) -> Result<MirExpression, ()> {
    let op = match op {
        TreeBinaryOpKind::Lt => {
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
            }
        }
        TreeBinaryOpKind::Gt => {
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
            }
        }
        TreeBinaryOpKind::Lte => {
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
        }
        TreeBinaryOpKind::Gte => {
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
        }
        TreeBinaryOpKind::Eq => {
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
        }
        TreeBinaryOpKind::Neq => {
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
        }
        TreeBinaryOpKind::Add => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatAdd
                } else if is_int_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntAdd
                } else {
                    return Err(());
                }
            }
        }
        TreeBinaryOpKind::Sub => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatSub
                } else if is_int_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntSub
                } else {
                    return Err(());
                }
            }
        }
        TreeBinaryOpKind::Mul => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatMul
                } else if is_int_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntMul
                } else {
                    return Err(());
                }
            }
        }
        TreeBinaryOpKind::Div => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatDiv
                } else if is_uint_type(&left.ty) {
                    MirIntrinsicBinaryOp::UIntDiv
                } else if is_sint_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntDiv
                } else {
                    return Err(());
                }
            }
        }
        TreeBinaryOpKind::Mod => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_float_type(&left.ty) {
                    MirIntrinsicBinaryOp::FloatRem
                } else if is_uint_type(&left.ty) {
                    MirIntrinsicBinaryOp::UIntRem
                } else if is_sint_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntRem
                } else {
                    return Err(());
                }
            }
        }
        TreeBinaryOpKind::And => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_int_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntAnd
                } else {
                    return Err(());
                }
            }
        }
        TreeBinaryOpKind::Or => {
            if &left.ty != &right.ty {
                return Err(());
            } else {
                if is_int_type(&left.ty) {
                    MirIntrinsicBinaryOp::IntOr
                } else {
                    return Err(());
                }
            }
        }
    };

    Ok(MirExpression {
        ty: left.ty.clone(),
        kind: MirExpressionKind::BinaryOp(Box::new(MirBinaryOp {
            op,
            lhs: left,
            rhs: right,
        })),
    })
}

fn is_float_type(ty: &MirType) -> bool {
    use MirIntrinsicType as IT;

    match ty.kind {
        MirTypeKind::Intrinsic(ty) => match ty {
            IT::F32 | IT::F64 => true,
            _ => false,
        },
    }
}

fn is_uint_type(ty: &MirType) -> bool {
    use MirIntrinsicType as IT;

    match ty.kind {
        MirTypeKind::Intrinsic(ty) => match ty {
            IT::U8 | IT::U16 | IT::U32 | IT::U64 => true,
            _ => false,
        },
    }
}

fn is_sint_type(ty: &MirType) -> bool {
    use MirIntrinsicType as IT;

    match ty.kind {
        MirTypeKind::Intrinsic(ty) => match ty {
            IT::I8 | IT::I16 | IT::I32 | IT::I64 => true,
            _ => false,
        },
    }
}

fn is_int_type(ty: &MirType) -> bool {
    is_uint_type(ty) || is_sint_type(ty)
}
