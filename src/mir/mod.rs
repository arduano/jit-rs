mod binary_expr;
mod blocks;
mod intrinsics;
mod structures;
mod variables;

pub use intrinsics::*;
pub use structures::*;

use crate::{
    macro_builder::{JitTokenFloatBits, JitTokenIntegerBits, JitTokenNumberKind},
    mir::binary_expr::mir_parse_binary_expr_list,
    tree_parser::{
        TreeBody, TreeExpression, TreeExpressionKind, TreeFunction, TreeModule, TreeType,
    },
};

use self::{blocks::MirBlockBuilder, variables::VariableStorage};

struct MirFunctionContext<'a> {
    functions: &'a [MirFunctionDeclaration],
}

pub struct MirExpressionContext<'a> {
    functions: &'a [MirFunctionDeclaration],
    blocks: MirBlockBuilder,
    variables: VariableStorage,
}

pub fn mir_parse_module(module: &TreeModule) -> Result<MirModule, ()> {
    let function_decls = module
        .functions
        .iter()
        .map(mir_parse_function_decl)
        .collect::<Result<Vec<_>, _>>()?;

    // FIXME: Process duplicate functions

    let fn_context = MirFunctionContext {
        functions: &function_decls,
    };

    let functions = module
        .functions
        .iter()
        .map(|function| mir_parse_function(function, &fn_context))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(MirModule { functions })
}

pub fn mir_parse_function_decl(function: &TreeFunction) -> Result<MirFunctionDeclaration, ()> {
    Ok(MirFunctionDeclaration {
        ret_type: match function.ret_type {
            Some(ref ty) => mir_parse_type(ty)?,
            None => mir_get_void_type(),
        },
        name: function.name.clone(),
        args: function
            .args
            .iter()
            .map(|arg| {
                Ok(MirFunctionArg {
                    name: arg.name.clone(),
                    ty: mir_parse_type(&arg.ty)?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn mir_parse_function(
    function: &TreeFunction,
    ctx: &MirFunctionContext,
) -> Result<MirFunction, ()> {
    let decl = ctx
        .functions
        .iter()
        .find(|decl| decl.name == function.name)
        .unwrap();

    let mut ctx = MirExpressionContext {
        functions: ctx.functions,
        blocks: MirBlockBuilder::new(),
        variables: VariableStorage::new(),
    };

    ctx.variables.push_scope();

    let initial = ctx.blocks.add_block();
    ctx.blocks.select_block(initial);

    // Insert the args as variables
    for (i, arg) in function.args.iter().enumerate() {
        let ty = mir_parse_type(&arg.ty)?;
        let var_decl = ctx.variables.declare(arg.name.clone(), ty.clone());

        let read = MirExpression {
            kind: MirExpressionKind::ReadArg(MirReadArg { index: i as u32 }),
            ty: ty.clone(),
        };

        let assign = MirStatement {
            kind: MirStatementKind::VarAssign(MirVariableAssign {
                var: var_decl.as_var(),
                value: read,
            }),
        };
        ctx.blocks.add_statement(assign);
    }

    let final_expr = mir_parse_body(&function.body, &mut ctx)?;
    ctx.variables.pop_scope();

    if !mir_is_empty_type(&final_expr.ty) {
        let ret = MirStatement {
            kind: MirStatementKind::Return(final_expr),
        };
        ctx.blocks.add_statement(ret);
    }

    Ok(MirFunction {
        variables: ctx.variables.get_variables(),
        decl: decl.clone(),
        blocks: ctx.blocks.get_blocks(),
    })
}

fn mir_parse_type(ty: &TreeType) -> Result<MirType, ()> {
    fn intrinsic(ty: MirIntrinsicType) -> MirType {
        MirType {
            kind: MirTypeKind::Intrinsic(ty),
        }
    }

    Ok(match ty.name.as_ref() {
        "u8" => intrinsic(MirIntrinsicType::U8),
        "u16" => intrinsic(MirIntrinsicType::U16),
        "u32" => intrinsic(MirIntrinsicType::U32),
        "u64" => intrinsic(MirIntrinsicType::U64),
        "i8" => intrinsic(MirIntrinsicType::I8),
        "i16" => intrinsic(MirIntrinsicType::I16),
        "i32" => intrinsic(MirIntrinsicType::I32),
        "i64" => intrinsic(MirIntrinsicType::I64),
        "f32" => intrinsic(MirIntrinsicType::F32),
        "f64" => intrinsic(MirIntrinsicType::F64),
        "bool" => intrinsic(MirIntrinsicType::Bool),
        _ => {
            panic!("Unsupported type: {}", ty.name);
        }
    })
}

fn mir_get_void_type() -> MirType {
    MirType {
        kind: MirTypeKind::Intrinsic(MirIntrinsicType::Void),
    }
}

fn mir_parse_body(body: &TreeBody, ctx: &mut MirExpressionContext) -> Result<MirExpression, ()> {
    for (i, expr) in body.body.iter().enumerate() {
        let is_last = i == body.body.len() - 1;
        let expr = mir_parse_expression(expr, ctx, true)?;

        if is_last {
            return Ok(expr);
        }
    }

    unreachable!()
}

fn mir_parse_expression(
    expr: &TreeExpression,
    ctx: &mut MirExpressionContext,
    is_root: bool,
) -> Result<MirExpression, ()> {
    let mut value = match &expr.kind {
        TreeExpressionKind::IfStatement(statement) => {
            let cond = mir_parse_expression(&statement.cond, ctx, false)?;
            let start_block = ctx.blocks.current_block().unwrap();

            let then_block = ctx.blocks.add_block();
            let else_block = ctx.blocks.add_block();

            ctx.blocks.select_block(then_block);
            let then_value = mir_parse_body(&statement.then, ctx)?;
            let then_block_end = ctx.blocks.current_block().unwrap();

            ctx.blocks.select_block(else_block);
            let else_value = mir_parse_body(&statement.else_, ctx)?;
            let else_block_end = ctx.blocks.current_block().unwrap();

            if then_value.ty != else_value.ty {
                panic!("If statement branches must return the same type");
            }

            let value = if !mir_is_empty_type(&then_value.ty) {
                let ty = then_value.ty.clone();

                // We don't do phi, so just jump around the blocks with a variable
                ctx.blocks.select_block(start_block);

                let var_decl = ctx.variables.declare_nameless(ty.clone());

                ctx.blocks.select_block(then_block_end);
                let assign = MirStatement {
                    kind: MirStatementKind::VarAssign(MirVariableAssign {
                        var: var_decl.as_var(),
                        value: then_value,
                    }),
                };
                ctx.blocks.add_statement(assign);

                ctx.blocks.select_block(else_block_end);
                let assign = MirStatement {
                    kind: MirStatementKind::VarAssign(MirVariableAssign {
                        var: var_decl.as_var(),
                        value: else_value,
                    }),
                };
                ctx.blocks.add_statement(assign);

                MirExpression {
                    ty,
                    kind: MirExpressionKind::ReadVariable(MirReadVariable {
                        var: var_decl.as_var(),
                    }),
                }
            } else {
                MirExpression {
                    ty: then_value.ty,
                    kind: MirExpressionKind::NoValue,
                }
            };

            let end_block = ctx.blocks.add_block();

            // Append all the jumps
            ctx.blocks.select_block(start_block);
            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::ConditionalJump(MirConditionalJump {
                    condition: cond,
                    then_index: then_block.index,
                    else_index: else_block.index,
                }),
            });

            ctx.blocks.select_block(then_block_end);
            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::Jump(MirJump {
                    index: end_block.index,
                }),
            });

            ctx.blocks.select_block(else_block_end);
            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::Jump(MirJump {
                    index: end_block.index,
                }),
            });

            ctx.blocks.select_block(end_block);
            value
        }
        TreeExpressionKind::LetStatement(statement) => {
            if !is_root {
                panic!("Let statements can only be used at the root of an expression");
            } else {
                let value = mir_parse_expression(&statement.value, ctx, false)?;

                let assign = MirVariableAssign {
                    var: ctx
                        .variables
                        .declare(statement.name.clone(), value.ty.clone())
                        .as_var(),
                    value,
                };
                let assign = MirStatement {
                    kind: MirStatementKind::VarAssign(assign),
                };
                ctx.blocks.add_statement(assign);

                mir_make_empty_expr()
            }
        }
        TreeExpressionKind::BinaryOpList(list) => mir_parse_binary_expr_list(list.clone(), ctx)?,
        TreeExpressionKind::UnaryOp(_) => todo!(),
        TreeExpressionKind::Group(_) => todo!(),
        TreeExpressionKind::ReadVar(read) => {
            let var = ctx.variables.get(&read.name).unwrap();
            MirExpression {
                kind: MirExpressionKind::ReadVariable(MirReadVariable { var: var.as_var() }),
                ty: var.ty.clone(),
            }
        }
        TreeExpressionKind::VarAssign(statement) => {
            if !is_root {
                panic!("Var assign statements can only be used at the root of an expression");
            } else {
                let value = mir_parse_expression(&statement.value, ctx, false)?;

                let assign = MirVariableAssign {
                    var: ctx.variables.get(&statement.name).unwrap().as_var(),
                    value,
                };
                let assign = MirStatement {
                    kind: MirStatementKind::VarAssign(assign),
                };
                ctx.blocks.add_statement(assign);

                mir_make_empty_expr()
            }
        }
        TreeExpressionKind::Number(num) => {
            use JitTokenFloatBits as FB;
            use JitTokenIntegerBits as IB;
            use JitTokenNumberKind::*;
            use MirIntrinsicType as Ty;
            use MirLiteral as Lit;

            let value = &num.value;
            let (literal, ty) = match num.ty {
                UnsignedInt(IB::Bits8) => (Lit::U8(value.parse().unwrap()), Ty::U8),
                UnsignedInt(IB::Bits16) => (Lit::U16(value.parse().unwrap()), Ty::U16),
                UnsignedInt(IB::Bits32) => (Lit::U32(value.parse().unwrap()), Ty::U32),
                UnsignedInt(IB::Bits64) => (Lit::U64(value.parse().unwrap()), Ty::U64),
                SignedInt(IB::Bits8) => (Lit::I8(value.parse().unwrap()), Ty::I8),
                SignedInt(IB::Bits16) => (Lit::I16(value.parse().unwrap()), Ty::I16),
                SignedInt(IB::Bits32) => (Lit::I32(value.parse().unwrap()), Ty::I32),
                SignedInt(IB::Bits64) => (Lit::I64(value.parse().unwrap()), Ty::I64),
                Float(FB::Bits32) => (Lit::F32(value.parse().unwrap()), Ty::F32),
                Float(FB::Bits64) => (Lit::F64(value.parse().unwrap()), Ty::F64),
            };

            MirExpression {
                kind: MirExpressionKind::Literal(literal),
                ty: MirType {
                    kind: MirTypeKind::Intrinsic(ty),
                },
            }
        }
        TreeExpressionKind::Bool(bool) => MirExpression {
            kind: MirExpressionKind::Literal(MirLiteral::Bool(bool.value)),
            ty: MirType {
                kind: MirTypeKind::Intrinsic(MirIntrinsicType::Bool),
            },
        },
        TreeExpressionKind::ReturnStatement(ret) => {
            let value = mir_parse_expression(&ret.value, ctx, false)?;
            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::Return(value),
            });
            mir_make_empty_expr()
        }
    };

    if expr.has_semi {
        value.ty = mir_get_void_type();
    }

    Ok(value)
}

fn mir_is_empty_type(ty: &MirType) -> bool {
    match &ty.kind {
        MirTypeKind::Intrinsic(MirIntrinsicType::Void) => true,
        MirTypeKind::Intrinsic(MirIntrinsicType::Never) => true,
        _ => false,
    }
}

fn mir_make_empty_expr() -> MirExpression {
    MirExpression {
        kind: MirExpressionKind::NoValue,
        ty: mir_get_void_type(),
    }
}
