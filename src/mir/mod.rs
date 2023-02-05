mod binary_expr;
mod blocks;
mod expression;
mod intrinsics;
mod statement;
mod structures;
mod types;
mod variables;

pub use expression::*;
pub use intrinsics::*;
pub use statement::*;
pub use structures::*;
pub use types::*;

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

        let assign = MirStatement::set_variable(var_decl, read);
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

    Ok(match ty {
        TreeType::Base(ty) => match ty.name.as_ref() {
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
        },
        TreeType::Ptr(ty) => MirType {
            kind: MirTypeKind::Intrinsic(MirIntrinsicType::Ptr(Box::new(mir_parse_type(&ty)?))),
        },
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
        let expr = mir_parse_expression(expr, ctx, ExprLocation::Root)?;

        if is_last {
            return Ok(expr);
        }
    }

    unreachable!()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprLocation {
    Root,
    PtrAssign,
    Other,
}

fn mir_parse_expression(
    expr: &TreeExpression,
    ctx: &mut MirExpressionContext,
    pos: ExprLocation,
) -> Result<MirExpression, ()> {
    let mut value = match &expr.kind {
        TreeExpressionKind::IfStatement(statement) => {
            let cond = mir_parse_expression(&statement.cond, ctx, ExprLocation::Other)?;
            mir_insert_condition(cond, &statement.then, statement.else_.as_ref(), ctx)?
        }
        TreeExpressionKind::LetStatement(statement) => {
            if pos != ExprLocation::Root {
                panic!("Let statements can only be used at the root of an expression");
            } else {
                let value = mir_parse_expression(&statement.value, ctx, ExprLocation::Other)?;

                let var_decl = ctx
                    .variables
                    .declare(statement.name.clone(), value.ty.clone());

                let assign = MirStatement::set_variable(var_decl, value);
                ctx.blocks.add_statement(assign);

                mir_make_empty_expr()
            }
        }
        TreeExpressionKind::BinaryOpList(list) => mir_parse_binary_expr_list(list.clone(), ctx)?,
        TreeExpressionKind::UnaryOp(_) => todo!(),
        TreeExpressionKind::Group(_) => todo!(),
        TreeExpressionKind::VarRead(read) => {
            let var = ctx.variables.get(&read.name).unwrap();
            let expr = MirExpression {
                kind: MirExpressionKind::GetVariablePtr(MirGetVariablePtr { var: var.as_var() }),
                ty: var.ty.as_ptr().clone(),
            };

            if pos == ExprLocation::PtrAssign {
                expr
            } else {
                mir_deref_expr(expr, ctx)
            }
        }
        TreeExpressionKind::PtrAssign(statement) => {
            let ptr = mir_parse_expression(&statement.ptr, ctx, ExprLocation::PtrAssign)?;
            let value = mir_parse_expression(&statement.value, ctx, ExprLocation::Other)?;

            let assign = MirPtrAssign { ptr, value };
            let assign = MirStatement {
                kind: MirStatementKind::PtrAssign(assign),
            };
            ctx.blocks.add_statement(assign);

            mir_make_empty_expr()
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
            let value = mir_parse_expression(&ret.value, ctx, ExprLocation::Other)?;
            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::Return(value),
            });
            mir_make_empty_expr()
        }
        TreeExpressionKind::WhileStatement(statement) => {
            let loop_start = ctx.blocks.add_block();
            let end_block = ctx.blocks.add_block();
            let loop_body = ctx.blocks.add_block();

            let jump = MirStatement {
                kind: MirStatementKind::Jump(MirJump {
                    index: loop_start.index,
                }),
            };
            ctx.blocks.add_statement(jump);

            ctx.blocks.select_block(loop_start);
            let cond = mir_parse_expression(&statement.cond, ctx, ExprLocation::Other)?;

            let jump = MirStatement {
                kind: MirStatementKind::ConditionalJump(MirConditionalJump {
                    condition: cond,
                    then_index: loop_body.index,
                    else_index: end_block.index,
                }),
            };
            ctx.blocks.add_statement(jump);

            ctx.blocks.select_block(loop_body);
            mir_parse_body(&statement.body, ctx)?;

            let jump = MirStatement {
                kind: MirStatementKind::Jump(MirJump {
                    index: loop_start.index,
                }),
            };
            ctx.blocks.add_statement(jump);

            ctx.blocks.select_block(end_block);

            mir_make_empty_expr()
        }
        TreeExpressionKind::Parenthesized(expr) => mir_parse_expression(&expr.inner, ctx, pos)?,
        TreeExpressionKind::IndexOp(index) => {
            let ptr = mir_parse_expression(&index.ptr, ctx, ExprLocation::Other)?;
            let index = mir_parse_expression(&index.index, ctx, ExprLocation::Other)?;

            match &index.ty.kind {
                MirTypeKind::Intrinsic(MirIntrinsicType::U32) => {}
                _ => panic!("Unexpected value type for index operation"),
            }

            match &ptr.ty.kind {
                MirTypeKind::Intrinsic(MirIntrinsicType::Ptr(_)) => {}
                _ => panic!("Unexpected value type for index operation"),
            }

            let value = MirExpression {
                ty: ptr.ty.clone(),
                kind: MirExpressionKind::IndexPtr(Box::new(MirIndexPtr { value: ptr, index })),
            };

            if pos == ExprLocation::PtrAssign {
                value
            } else {
                mir_deref_expr(value, ctx)
            }
        }
        TreeExpressionKind::VoidValue(expr) => {
            let mut value = mir_parse_expression(&expr, ctx, ExprLocation::Other)?;
            value.ty = mir_get_void_type();

            value
        }
    };

    Ok(value)
}

fn mir_deref_expr(expr: MirExpression, ctx: &mut MirExpressionContext) -> MirExpression {
    MirExpression {
        ty: expr.ty.deref_ptr().clone(),
        kind: MirExpressionKind::PtrDeref(Box::new(MirPtrDeref { ptr: expr })),
    }
}

fn mir_insert_condition(
    cond: MirExpression,
    then_tree: &TreeBody,
    else_tree: Option<&TreeBody>,
    ctx: &mut MirExpressionContext,
) -> Result<MirExpression, ()> {
    let start_block = ctx.blocks.current_block().unwrap();

    let then_block = ctx.blocks.add_block();
    let else_block = ctx.blocks.add_block();

    ctx.blocks.select_block(then_block);
    let then_value = mir_parse_body(then_tree, ctx)?;
    let then_block_end = ctx.blocks.current_block().unwrap();

    ctx.blocks.select_block(else_block);
    let else_value = if let Some(else_tree) = else_tree {
        mir_parse_body(else_tree, ctx)?
    } else {
        mir_make_empty_expr()
    };
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
        let assign = MirStatement::set_variable(var_decl.clone(), then_value);
        ctx.blocks.add_statement(assign);

        ctx.blocks.select_block(else_block_end);
        let assign = MirStatement::set_variable(var_decl.clone(), else_value);
        ctx.blocks.add_statement(assign);

        MirExpression {
            ty,
            kind: MirExpressionKind::GetVariablePtr(MirGetVariablePtr {
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
