use std::ops::Deref;

use crate::tree_parser::{TreeExpression, TreeStaticFnCall, TreeType};

use super::{
    mir_parse_expression, mir_parse_type, ExprLocation, MirExpression, MirExpressionContext,
    MirExpressionKind, MirIntrinsicOp, MirType, MirTypeContext,
};

pub fn mir_try_parse_intrinsic_fn(
    f: &TreeStaticFnCall,
    ctx: &mut MirExpressionContext,
) -> Result<Option<MirExpression>, ()> {
    Ok(Some(match f.name.deref() {
        "load_vec" => {
            let ty = parse_1_type_arg(&f.types, ctx.ty)?;
            let (ty, width) = match ty {
                MirType::Vector(ty, width) => (ty, width),
                _ => panic!("Expected a vector type"),
            };

            let expr = parse_1_value_arg(ctx, &f.args, MirType::Ptr(Box::new(MirType::Num(ty))))?;

            MirExpression {
                kind: MirExpressionKind::IntrinsicOp(Box::new(MirIntrinsicOp::LoadVector {
                    ptr: expr,
                    scalar_ty: ty,
                    width: width,
                })),
                ty: MirType::Vector(ty, width),
            }
        }
        "store_vec" => {
            let ty = parse_1_type_arg(&f.types, ctx.ty)?;
            let (ty, width) = match ty {
                MirType::Vector(ty, width) => (ty, width),
                _ => panic!("Expected a vector type"),
            };

            let (ptr, value) = parse_2_value_arg(
                ctx,
                &f.args,
                (
                    MirType::Ptr(Box::new(MirType::Num(ty))),
                    MirType::Vector(ty, width),
                ),
            )?;

            MirExpression {
                kind: MirExpressionKind::IntrinsicOp(Box::new(MirIntrinsicOp::StoreVector {
                    ptr,
                    value,
                    scalar_ty: ty,
                    width,
                })),
                ty: MirType::Void,
            }
        }
        "extend" => {
            let vec_ty = parse_1_type_arg(&f.types, ctx.ty)?;
            let (ty, width) = match vec_ty {
                MirType::Vector(ty, width) => (ty, width),
                _ => panic!("Expected a vector type"),
            };

            let expr = parse_1_value_arg(ctx, &f.args, MirType::Num(ty))?;

            MirExpression {
                kind: MirExpressionKind::IntrinsicOp(Box::new(MirIntrinsicOp::ExtendToVector {
                    unit_ty: ty,
                    unit: expr,
                    width,
                })),
                ty: vec_ty,
            }
        }
        "unreachable" => MirExpression {
            kind: MirExpressionKind::IntrinsicOp(Box::new(MirIntrinsicOp::Unreachable)),
            ty: MirType::Never,
        },
        "zeroed" => {
            let ty = parse_1_type_arg(&f.types, ctx.ty)?;
            MirExpression {
                kind: MirExpressionKind::IntrinsicOp(Box::new(MirIntrinsicOp::Zeroed {
                    ty: ty.clone(),
                })),
                ty,
            }
        }
        _ => return Ok(None),
    }))
}

fn parse_1_type_arg(args: &[TreeType], ctx: &MirTypeContext) -> Result<MirType, ()> {
    if args.len() != 1 {
        panic!("Expected 1 type argument");
    }

    let ty = &args[0];
    return mir_parse_type(ty, ctx);
}

fn parse_1_value_arg(
    ctx: &mut MirExpressionContext,
    args: &[TreeExpression],
    expected_ty: MirType,
) -> Result<MirExpression, ()> {
    if args.len() != 1 {
        panic!("Expected 1 argument");
    }

    let expr = mir_parse_expression(&args[0], ctx, ExprLocation::Other)?;

    if expr.ty != expected_ty {
        panic!("Expected type {:?}", expected_ty);
    }

    Ok(expr)
}

fn parse_2_value_arg(
    ctx: &mut MirExpressionContext,
    args: &[TreeExpression],
    expected_tys: (MirType, MirType),
) -> Result<(MirExpression, MirExpression), ()> {
    if args.len() != 2 {
        panic!("Expected 2 arguments");
    }

    let expr1 = mir_parse_expression(&args[0], ctx, ExprLocation::Other)?;
    let expr2 = mir_parse_expression(&args[1], ctx, ExprLocation::Other)?;

    if expr1.ty != expected_tys.0 {
        panic!("Expected type {:?}", expected_tys.0);
    }
    if expr2.ty != expected_tys.1 {
        panic!("Expected type {:?}", expected_tys.1);
    }

    Ok((expr1, expr2))
}
