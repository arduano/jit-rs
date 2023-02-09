mod assertions;
mod blocks;
mod cast;
mod expression;
mod intrinsics;
mod misc;
mod operators;
mod statement;
mod structures;
mod ty;
mod variables;

pub use expression::*;
pub use intrinsics::*;
pub use statement::*;
pub use structures::*;
pub use ty::*;

use crate::{
    common::{FloatBits, IntBits, NumberKind},
    mir::operators::mir_parse_binary_expr_list,
    tree_parser::{
        TreeBody, TreeExpression, TreeExpressionKind, TreeFunction, TreeModule, TreeType,
    },
};

use self::{
    assertions::{mir_assert_is_boolean, mir_assert_types_equal},
    blocks::MirBlockBuilder,
    cast::{mir_cast_to_number, mir_cast_to_vector},
    misc::{mir_is_empty_type, mir_make_empty_expr},
    operators::mir_parse_unary_expr,
    variables::VariableStorage,
};

struct MirFunctionContext<'a> {
    functions: &'a [MirFunctionDeclaration],
    return_ty: MirType,
}

pub struct MirExpressionContext<'a> {
    fn_ctx: &'a MirFunctionContext<'a>,
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

    let functions = module
        .functions
        .iter()
        .map(|function| {
            mir_parse_function(
                function,
                &MirFunctionContext {
                    functions: &function_decls,
                    return_ty: match function.ret_type {
                        Some(ref ty) => mir_parse_type(ty)?,
                        None => MirType::Void,
                    },
                },
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(MirModule { functions })
}

pub fn mir_parse_function_decl(function: &TreeFunction) -> Result<MirFunctionDeclaration, ()> {
    Ok(MirFunctionDeclaration {
        ret_type: match function.ret_type {
            Some(ref ty) => mir_parse_type(ty)?,
            None => MirType::Void,
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
        fn_ctx: ctx,
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

    mir_assert_types_equal(&final_expr.ty, &ctx.fn_ctx.return_ty)?;

    let return_val = if !mir_is_empty_type(&final_expr.ty) {
        Some(final_expr)
    } else {
        None
    };

    let ret = MirStatement {
        kind: MirStatementKind::Return(return_val),
    };
    ctx.blocks.add_statement(ret);

    Ok(MirFunction {
        variables: ctx.variables.get_variables(),
        decl: decl.clone(),
        blocks: ctx.blocks.get_blocks(),
    })
}

fn mir_parse_type(ty: &TreeType) -> Result<MirType, ()> {
    fn number(ty: NumberKind) -> MirType {
        MirType::Num(ty)
    }

    fn float(bits: FloatBits) -> MirType {
        number(NumberKind::Float(bits))
    }

    fn uint(bits: IntBits) -> MirType {
        number(NumberKind::UnsignedInt(bits))
    }

    fn sint(bits: IntBits) -> MirType {
        number(NumberKind::SignedInt(bits))
    }

    Ok(match ty {
        TreeType::Base(ty) => match ty.name.as_ref() {
            "u8" => uint(IntBits::Bits8),
            "u16" => uint(IntBits::Bits16),
            "u32" => uint(IntBits::Bits32),
            "u64" => uint(IntBits::Bits64),
            "usize" => uint(IntBits::BitsSize),
            "i8" => sint(IntBits::Bits8),
            "i16" => sint(IntBits::Bits16),
            "i32" => sint(IntBits::Bits32),
            "i64" => sint(IntBits::Bits64),
            "isize" => sint(IntBits::BitsSize),
            "f32" => float(FloatBits::Bits32),
            "f64" => float(FloatBits::Bits64),
            "bool" => MirType::Bool,
            _ => {
                panic!("Unsupported type: {}", ty.name);
            }
        },

        TreeType::Ptr(ty) => MirType::Ptr(Box::new(mir_parse_type(&ty)?)),
        TreeType::ConstArray(ty, size) => {
            let ty = mir_parse_type(&ty)?;

            MirType::ConstArray(Box::new(ty), *size)
        }
        TreeType::Vector(ty, width) => {
            let ty = mir_parse_type(&ty)?;

            let ty = match ty {
                MirType::Num(ty) => ty,
                _ => {
                    panic!("Unsupported vector type: {:?}", ty);
                }
            };

            MirType::Vector(ty, *width)
        }
    })
}

fn mir_parse_body(body: &TreeBody, ctx: &mut MirExpressionContext) -> Result<MirExpression, ()> {
    if body.body.is_empty() {
        return Ok(MirExpression {
            kind: MirExpressionKind::NoValue,
            ty: MirType::Void,
        });
    }

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
pub enum ExprLocation {
    Root,
    NoDeref,
    Other,
}

fn mir_parse_expression(
    expr: &TreeExpression,
    ctx: &mut MirExpressionContext,
    pos: ExprLocation,
) -> Result<MirExpression, ()> {
    let value = match &expr.kind {
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
        TreeExpressionKind::UnaryOp(unary) => mir_parse_unary_expr(unary, ctx, pos)?,
        TreeExpressionKind::Group(inner) => mir_parse_body(inner, ctx)?,
        TreeExpressionKind::VarRead(read) => {
            let var = ctx.variables.get(&read.name).unwrap();
            let expr = MirExpression {
                kind: MirExpressionKind::GetVariablePtr(MirGetVariablePtr { var: var.as_var() }),
                ty: var.ty.as_ptr().clone(),
            };

            if pos == ExprLocation::NoDeref {
                expr
            } else {
                mir_deref_expr(expr, ctx)
            }
        }
        TreeExpressionKind::PtrAssign(statement) => {
            let ptr = mir_parse_expression(&statement.ptr, ctx, ExprLocation::NoDeref)?;
            let value = mir_parse_expression(&statement.value, ctx, ExprLocation::Other)?;

            dbg!(&statement.ptr);

            if let Some(ptr_ty) = ptr.ty.try_deref_ptr() {
                mir_assert_types_equal(&ptr_ty, &value.ty)?;
            } else {
                panic!("Cannot assign to non-pointer");
            }

            let assign = MirPtrAssign { ptr, value };
            let assign = MirStatement {
                kind: MirStatementKind::PtrAssign(assign),
            };
            ctx.blocks.add_statement(assign);

            mir_make_empty_expr()
        }
        TreeExpressionKind::Number(num) => {
            use FloatBits as FB;
            use IntBits as IB;
            use MirLiteral as Lit;
            use NumberKind::*;

            let value = &num.value;
            let literal = match num.ty {
                UnsignedInt(IB::Bits8) => Lit::U8(value.parse().unwrap()),
                UnsignedInt(IB::Bits16) => Lit::U16(value.parse().unwrap()),
                UnsignedInt(IB::Bits32) => Lit::U32(value.parse().unwrap()),
                UnsignedInt(IB::Bits64) => Lit::U64(value.parse().unwrap()),
                UnsignedInt(IB::BitsSize) => Lit::USize(value.parse().unwrap()),
                SignedInt(IB::Bits8) => Lit::I8(value.parse().unwrap()),
                SignedInt(IB::Bits16) => Lit::I16(value.parse().unwrap()),
                SignedInt(IB::Bits32) => Lit::I32(value.parse().unwrap()),
                SignedInt(IB::Bits64) => Lit::I64(value.parse().unwrap()),
                SignedInt(IB::BitsSize) => Lit::ISize(value.parse().unwrap()),
                Float(FB::Bits32) => Lit::F32(value.parse().unwrap()),
                Float(FB::Bits64) => Lit::F64(value.parse().unwrap()),
            };

            MirExpression {
                kind: MirExpressionKind::Literal(literal),
                ty: MirType::Num(num.ty),
            }
        }
        TreeExpressionKind::Bool(bool) => MirExpression {
            kind: MirExpressionKind::Literal(MirLiteral::Bool(bool.value)),
            ty: MirType::Bool,
        },
        TreeExpressionKind::ReturnStatement(ret) => {
            let value = mir_parse_expression(&ret.value, ctx, ExprLocation::Other)?;
            mir_assert_types_equal(&value.ty, &ctx.fn_ctx.return_ty)?;

            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::Return(Some(value)),
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
            mir_assert_is_boolean(&cond.ty)?;

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
            let ptr = mir_parse_expression(&index.ptr, ctx, ExprLocation::NoDeref)?;
            let index = mir_parse_expression(&index.index, ctx, ExprLocation::Other)?;

            match &index.ty {
                MirType::Num(NumberKind::UnsignedInt(IntBits::BitsSize)) => {}
                _ => panic!("Unexpected value type for index operation"),
            }

            // TODO: Error if this isn't a ptr. We can only index ptr types.
            let ty_inside_ptr = ptr.ty.deref_ptr().clone();

            let value = match &ty_inside_ptr {
                MirType::Ptr(ty) => {
                    // From: parent ptr -> ptr -> value
                    // To: (derefed -> ) indexed ptr -> value

                    let index_ty = (**ty).clone();
                    let ptr = mir_deref_expr(ptr, ctx);
                    MirExpression {
                        ty: ty_inside_ptr.clone(),
                        kind: MirExpressionKind::IndexPtr(Box::new(MirIndexPtr {
                            value: ptr,
                            index,
                            index_ty,
                        })),
                    }
                }
                MirType::ConstArray(ty, _) => {
                    // From: parent ptr -> [array -> value]
                    // To: indexed ptr -> value

                    let index_ty = (**ty).clone();
                    let new_ty = ty.as_ptr();

                    MirExpression {
                        ty: new_ty,
                        kind: MirExpressionKind::IndexPtr(Box::new(MirIndexPtr {
                            value: ptr,
                            index,
                            index_ty,
                        })),
                    }
                }
                MirType::Vector(ty, _) => {
                    // From: parent ptr -> [array -> value]
                    // To: indexed ptr -> value

                    let ty = MirType::Num(*ty);

                    let new_ty = ty.as_ptr();

                    MirExpression {
                        ty: new_ty,
                        kind: MirExpressionKind::IndexPtr(Box::new(MirIndexPtr {
                            value: ptr,
                            index,
                            index_ty: ty,
                        })),
                    }
                }
                _ => panic!("Unexpected value type for index operation"),
            };

            if pos == ExprLocation::NoDeref {
                value
            } else {
                mir_deref_expr(value, ctx)
            }
        }
        TreeExpressionKind::Cast(cast) => {
            let value = mir_parse_expression(&cast.value, ctx, ExprLocation::Other)?;
            let new_ty = mir_parse_type(&cast.new_ty)?;

            match &new_ty {
                &MirType::Num(ty) => mir_cast_to_number(value, ty)?,
                &MirType::Vector(ty, width) => mir_cast_to_vector(value, ty, width)?,
                MirType::Ptr(_) => match &new_ty {
                    MirType::Ptr(_) => MirExpression {
                        kind: MirExpressionKind::PtrCast(Box::new(value)),
                        ty: new_ty,
                    },
                    _ => panic!("Unexpected value type for cast operation"),
                },
                _ => panic!("Unexpected value type for cast operation"),
            }
        }
        TreeExpressionKind::VoidValue(expr) => {
            // Pass pos across, because void is just a marker expression
            let mut value = mir_parse_expression(&expr, ctx, pos)?;
            value.ty = MirType::Void;

            value
        }
    };

    Ok(value)
}

fn mir_deref_expr(expr: MirExpression, _ctx: &mut MirExpressionContext) -> MirExpression {
    let ty = expr.ty.deref_ptr().clone();
    MirExpression {
        ty: ty.clone(),
        kind: MirExpressionKind::PtrDeref(Box::new(MirPtrDeref {
            ptr: expr,
            underlying_ty: ty.clone(),
        })),
    }
}

fn mir_insert_condition(
    cond: MirExpression,
    then_tree: &TreeBody,
    else_tree: Option<&TreeBody>,
    ctx: &mut MirExpressionContext,
) -> Result<MirExpression, ()> {
    mir_assert_is_boolean(&cond.ty)?;

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

    mir_assert_types_equal(&then_value.ty, &else_value.ty)?;

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
