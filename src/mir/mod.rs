mod assertions;
mod blocks;
mod calls;
mod cast;
mod expression;
mod functions;
mod intrinsic_fns;
mod intrinsics;
mod misc;
mod operators;
mod statement;
mod structures;
mod ty;
mod variables;

use std::{collections::HashMap, ops::Deref};

pub use expression::*;
pub use functions::*;
pub use intrinsics::*;
pub use statement::*;
pub use structures::*;
pub use ty::*;

use crate::{
    common::{FloatBits, IntBits, NumberKind},
    mir::operators::mir_parse_binary_expr_list,
    tree_parser::{
        TreeBody, TreeExpression, TreeExpressionKind, TreeFunction, TreeModule, TreeStruct,
        TreeType, TreeTypeMarker,
    },
};

use self::{
    assertions::{mir_assert_is_boolean, mir_assert_types_equal},
    blocks::{MirBlockBuilder, MirBlockId},
    calls::mir_parse_static_function_call,
    cast::{mir_cast_to_number, mir_cast_to_vector},
    intrinsic_fns::mir_try_parse_intrinsic_fn,
    misc::{mir_is_empty_type, mir_make_empty_expr, mir_make_never_expr},
    operators::mir_parse_unary_expr,
    variables::VariableStorage,
};

pub struct MirTypeContext<'a> {
    structs: &'a [MirStructDeclaration],
}

struct MirFunctionContext<'a> {
    functions: &'a [MirFunctionDeclaration],
    structs: &'a [MirStruct],
    ty: &'a MirTypeContext<'a>,
    return_ty: MirType,
}

struct LoopContext {
    break_block: MirBlockId,
}

pub struct MirExpressionContext<'a> {
    fn_ctx: &'a MirFunctionContext<'a>,
    functions: &'a [MirFunctionDeclaration],
    structs: &'a [MirStruct],
    ty: &'a MirTypeContext<'a>,
    blocks: MirBlockBuilder,
    variables: VariableStorage,
    loop_stack: Vec<LoopContext>,
}

pub fn mir_parse_module(module: &TreeModule) -> Result<MirModule, ()> {
    // FIXME: Process duplicate functions and structs

    let struct_decls = module
        .structs
        .iter()
        .map(mir_parse_struct_decl)
        .collect::<Result<Vec<_>, _>>()?;

    let ty_ctx = MirTypeContext {
        structs: &struct_decls,
    };

    let function_decls = module
        .functions
        .iter()
        .map(|f| mir_parse_function_decl(f, &ty_ctx))
        .collect::<Result<Vec<_>, _>>()?;

    let structs = module
        .structs
        .iter()
        .map(|struc| mir_parse_struct(struc, &ty_ctx))
        .collect::<Result<Vec<_>, _>>()?;

    let functions = module
        .functions
        .iter()
        .map(|function| {
            mir_parse_function(
                function,
                &MirFunctionContext {
                    functions: &function_decls,
                    structs: &structs,
                    ty: &ty_ctx,
                    return_ty: match function.ret_type {
                        Some(ref ty) => mir_parse_type(ty, &ty_ctx)?,
                        None => MirType::Void,
                    },
                },
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(MirModule { functions, structs })
}

pub fn mir_parse_struct_decl(struc: &TreeStruct) -> Result<MirStructDeclaration, ()> {
    let mut field_indexes = HashMap::new();

    for (i, field) in struc.fields.iter().enumerate() {
        field_indexes.insert(field.name.clone(), i);
    }

    Ok(MirStructDeclaration {
        name: struc.name.clone(),
        field_indexes,
    })
}

fn mir_parse_struct(struc: &TreeStruct, ctx: &MirTypeContext) -> Result<MirStruct, ()> {
    let decl = ctx
        .structs
        .iter()
        .find(|decl| decl.name == struc.name)
        .unwrap()
        .clone();

    let mut fields = Vec::<MirType>::new();

    for (_i, field) in struc.fields.iter().enumerate() {
        fields.push(mir_parse_type(&field.ty, ctx)?);
    }

    Ok(MirStruct {
        name: struc.name.clone(),
        fields,
        decl,
    })
}

fn mir_parse_function_decl(
    function: &TreeFunction,
    ty_ctx: &MirTypeContext,
) -> Result<MirFunctionDeclaration, ()> {
    let name = mir_parse_fn_name_with_markers(&function.name, ty_ctx)?;

    if name.markers.len() != 0 && function.public {
        panic!("Public function with markers is not allowed");
    }

    Ok(MirFunctionDeclaration {
        public: function.public,
        ret_type: match function.ret_type {
            Some(ref ty) => mir_parse_type(ty, ty_ctx)?,
            None => MirType::Void,
        },
        name,
        args: function
            .args
            .iter()
            .map(|arg| {
                Ok(MirFunctionArg {
                    name: arg.name.clone(),
                    ty: mir_parse_type(&arg.ty, ty_ctx)?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn mir_parse_function(
    function: &TreeFunction,
    ctx: &MirFunctionContext,
) -> Result<MirFunction, ()> {
    let name = mir_parse_fn_name_with_markers(&function.name, ctx.ty)?;

    let decl = ctx.functions.iter().find(|decl| decl.name == name).unwrap();

    let mut ctx = MirExpressionContext {
        fn_ctx: ctx,
        functions: ctx.functions,
        structs: ctx.structs,
        ty: ctx.ty,
        blocks: MirBlockBuilder::new(),
        variables: VariableStorage::new(),
        loop_stack: Vec::new(),
    };

    ctx.variables.push_scope();

    let initial = ctx.blocks.add_block();
    ctx.blocks.select_block(initial);

    // Insert the args as variables
    for (i, arg) in function.args.iter().enumerate() {
        let ty = mir_parse_type(&arg.ty, ctx.ty)?;
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

    if !final_expr.ty.is_never() {
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
    }

    Ok(MirFunction {
        variables: ctx.variables.get_variables(),
        decl: decl.clone(),
        blocks: ctx.blocks.get_blocks(),
    })
}

fn mir_parse_type(ty: &TreeType, ctx: &MirTypeContext) -> Result<MirType, ()> {
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
                let found_struct = ctx
                    .structs
                    .iter()
                    .find(|struc| struc.name == ty.name)
                    .cloned();

                if let Some(struc) = found_struct {
                    MirType::Struct(struc.name)
                } else {
                    panic!("Unsupported type: {}", ty.name);
                }
            }
        },

        TreeType::Ptr(ty) => MirType::Ptr(Box::new(mir_parse_type(&ty, ctx)?)),
        TreeType::ConstArray(ty, size) => {
            let ty = mir_parse_type(&ty, ctx)?;

            MirType::ConstArray(Box::new(ty), *size)
        }
        TreeType::Vector(ty, width) => {
            let ty = mir_parse_type(&ty, ctx)?;

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

fn mir_parse_type_marker(
    marker: &TreeTypeMarker,
    ctx: &MirTypeContext,
) -> Result<MirTypeMarker, ()> {
    Ok(match marker {
        TreeTypeMarker::Type(ty) => MirTypeMarker::Type(mir_parse_type(ty, ctx)?),
        TreeTypeMarker::NumLiteral(num) => MirTypeMarker::Literal(mir_parse_num_literal(num)),
        TreeTypeMarker::BoolLiteral(bool) => MirTypeMarker::Literal(mir_parse_bool_literal(bool)),
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

        if is_last || expr.ty.is_never() {
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
            let var = ctx
                .variables
                .get(&read.name)
                .expect(format!("Unknown variable: {}", read.name).as_ref());
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
            let lit = mir_parse_num_literal(num);

            MirExpression {
                kind: MirExpressionKind::Literal(lit),
                ty: MirType::Num(num.ty),
            }
        }
        TreeExpressionKind::Bool(bool) => MirExpression {
            kind: MirExpressionKind::Literal(mir_parse_bool_literal(bool)),
            ty: MirType::Bool,
        },
        TreeExpressionKind::ReturnStatement(ret) => {
            let value = mir_parse_expression(&ret.value, ctx, ExprLocation::Other)?;
            mir_assert_types_equal(&value.ty, &ctx.fn_ctx.return_ty)?;

            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::Return(Some(value)),
            });
            mir_make_never_expr()
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

            ctx.loop_stack.push(LoopContext {
                break_block: end_block,
            });
            let expr = mir_parse_body(&statement.body, ctx)?;
            ctx.loop_stack.pop();

            if !expr.ty.is_never() {
                let jump = MirStatement {
                    kind: MirStatementKind::Jump(MirJump {
                        index: loop_start.index,
                    }),
                };
                ctx.blocks.add_statement(jump);
            }

            ctx.blocks.select_block(end_block);

            mir_make_empty_expr()
        }
        TreeExpressionKind::LoopStatement(statement) => {
            let loop_body = ctx.blocks.add_block();
            let end_block = ctx.blocks.add_block();

            let jump = MirStatement {
                kind: MirStatementKind::Jump(MirJump {
                    index: loop_body.index,
                }),
            };
            ctx.blocks.add_statement(jump);

            ctx.blocks.select_block(loop_body);

            ctx.loop_stack.push(LoopContext {
                break_block: end_block,
            });
            let expr = mir_parse_body(&statement.body, ctx)?;
            ctx.loop_stack.pop();

            if !expr.ty.is_never() {
                let jump = MirStatement {
                    kind: MirStatementKind::Jump(MirJump {
                        index: loop_body.index,
                    }),
                };
                ctx.blocks.add_statement(jump);
            }

            ctx.blocks.select_block(end_block);

            mir_make_empty_expr()
        }
        TreeExpressionKind::BreakStatement(_) => {
            let loop_ctx = ctx.loop_stack.last().unwrap();
            let jump = MirStatement {
                kind: MirStatementKind::Jump(MirJump {
                    index: loop_ctx.break_block.index,
                }),
            };
            ctx.blocks.add_statement(jump);

            mir_make_never_expr()
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
            let new_ty = mir_parse_type(&cast.new_ty, ctx.ty)?;

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
            let value = mir_parse_expression(&expr, ctx, pos)?;

            let never = value.ty.is_never();

            ctx.blocks.add_statement(MirStatement {
                kind: MirStatementKind::VoidExpr(value),
            });

            if never {
                mir_make_never_expr()
            } else {
                mir_make_empty_expr()
            }
        }
        TreeExpressionKind::StaticFnCall(call) => {
            if let Some(expr) = mir_parse_static_function_call(call, ctx)? {
                expr
            } else if let Some(expr) = mir_try_parse_intrinsic_fn(call, ctx)? {
                expr
            } else {
                panic!("Unknown function call");
            }
        }
        TreeExpressionKind::IndexField(index) => {
            let ptr = mir_parse_expression(&index.ptr, ctx, ExprLocation::NoDeref)?;

            // TODO: Error if this isn't a ptr. We can only index ptr types.
            let ty_inside_ptr = ptr.ty.deref_ptr().clone();

            let (name, should_deref) = match &ty_inside_ptr {
                MirType::Struct(name) => (name, false),
                MirType::Ptr(ty) => match ty.deref() {
                    MirType::Struct(name) => (name, true),
                    _ => panic!("No field {} on type {:?}", index.field_name, ty_inside_ptr),
                },
                _ => panic!("No field {} on type {:?}", index.field_name, ty_inside_ptr),
            };

            let ptr = if should_deref {
                mir_deref_expr(ptr, ctx)
            } else {
                ptr
            };

            // From: parent ptr -> [value -> field]
            // To: indexed ptr -> field

            let struc = ctx.structs.iter().find(|s| s.name == *name).unwrap();

            let ind = struc.decl.field_indexes.get(&index.field_name);
            let Some(&ind) = ind else {
                panic!("No field {} on type {:?}", index.field_name, ty_inside_ptr);
            };

            let field_ty = struc.fields[ind].clone();
            let result_ty = field_ty.as_ptr();

            let value = MirExpression {
                ty: result_ty,
                kind: MirExpressionKind::IndexStruct(Box::new(MirIndexStruct {
                    value: ptr,
                    struct_name: name.clone(),
                    index: ind as u32,
                    index_ty: field_ty,
                })),
            };

            if pos == ExprLocation::NoDeref {
                value
            } else {
                mir_deref_expr(value, ctx)
            }
        }
        TreeExpressionKind::StructInit(init) => {
            let struc = ctx.structs.iter().find(|s| s.name == init.name);

            let Some(struc) = struc else {
                panic!("No struct named {}", init.name);
            };

            // Ensure that the fields match
            for field in &init.fields {
                if !struc.decl.field_indexes.contains_key(&field.name) {
                    panic!("No field named {} on struct {}", field.name, init.name);
                }
            }

            for field in struc.decl.field_indexes.keys() {
                if !init.fields.iter().any(|f| &f.name == field) {
                    panic!("Missing field {} on struct {}", field, init.name);
                }
            }

            let mut fields = Vec::new();
            for i in 0..struc.fields.len() {
                // Find the field name at that index
                let field = struc
                    .decl
                    .field_indexes
                    .iter()
                    .find(|f| f.1 == &i)
                    .unwrap()
                    .0;

                // Get the value for the field
                let expr = init
                    .fields
                    .iter()
                    .find(|f| &f.name == field)
                    .unwrap()
                    .value
                    .clone();

                let value = mir_parse_expression(&expr, ctx, ExprLocation::Other)?;

                fields.push(value);
            }

            let ty = MirType::Struct(init.name.clone());

            MirExpression {
                ty: ty.clone(),
                kind: MirExpressionKind::StructInit(MirStructInit {
                    name: init.name.clone(),
                    fields,
                }),
            }
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

    let then_ty = then_value.ty.clone();
    let else_ty = else_value.ty.clone();

    let result_ty = if then_value.ty.is_never() && else_value.ty.is_never() {
        &MirType::Never
    } else if then_value.ty.is_never() {
        &else_ty
    } else if else_value.ty.is_never() {
        &then_ty
    } else {
        mir_assert_types_equal(&then_value.ty, &else_value.ty)?;
        &then_ty
    };

    let value = if !mir_is_empty_type(&result_ty) {
        let ty = then_ty.clone();

        // We don't do phi, so just jump around the blocks with a variable
        ctx.blocks.select_block(start_block);

        let var_decl = ctx.variables.declare_nameless(ty.clone());

        if !then_ty.is_never() {
            ctx.blocks.select_block(then_block_end);
            let assign = MirStatement::set_variable(var_decl.clone(), then_value);
            ctx.blocks.add_statement(assign);
        }

        if !else_ty.is_never() {
            ctx.blocks.select_block(else_block_end);
            let assign = MirStatement::set_variable(var_decl.clone(), else_value);
            ctx.blocks.add_statement(assign);
        }

        MirExpression {
            ty,
            kind: MirExpressionKind::GetVariablePtr(MirGetVariablePtr {
                var: var_decl.as_var(),
            }),
        }
    } else {
        MirExpression {
            ty: result_ty.clone(),
            kind: MirExpressionKind::NoValue,
        }
    };

    // Append all the jumps
    ctx.blocks.select_block(start_block);
    ctx.blocks.add_statement(MirStatement {
        kind: MirStatementKind::ConditionalJump(MirConditionalJump {
            condition: cond,
            then_index: then_block.index,
            else_index: else_block.index,
        }),
    });

    // If the result type is never, we don't need to do any jumps
    if result_ty.is_never() {
        return Ok(value);
    }

    let end_block = ctx.blocks.add_block();

    if !then_ty.is_never() {
        ctx.blocks.select_block(then_block_end);
        ctx.blocks.add_statement(MirStatement {
            kind: MirStatementKind::Jump(MirJump {
                index: end_block.index,
            }),
        });
    }

    if !else_ty.is_never() {
        ctx.blocks.select_block(else_block_end);
        ctx.blocks.add_statement(MirStatement {
            kind: MirStatementKind::Jump(MirJump {
                index: end_block.index,
            }),
        });
    }

    ctx.blocks.select_block(end_block);

    Ok(value)
}
