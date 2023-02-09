use crate::tree_parser::TreeStaticFnCall;

use super::{
    mir_parse_expression, ExprLocation, MirExpression, MirExpressionContext, MirFunctionCall,
};

pub fn mir_parse_static_function_call(
    call: &TreeStaticFnCall,
    ctx: &mut MirExpressionContext,
) -> Result<Option<MirExpression>, ()> {
    let found_fn = ctx.functions.iter().find(|f| f.name == call.name);

    let Some(found_fn) = found_fn else {
        return Ok(None);
    };

    if call.args.len() != found_fn.args.len() {
        panic!("Wrong number of arguments");
    }

    let mut args = Vec::new();
    for (arg, arg_def) in call.args.iter().zip(found_fn.args.iter()) {
        let arg = mir_parse_expression(arg, ctx, ExprLocation::Other)?;

        if arg.ty != arg_def.ty {
            panic!("Expected type {:?} but got {:?}", arg_def.ty, arg.ty);
        }

        args.push(arg);
    }

    Ok(Some(MirExpression {
        kind: super::MirExpressionKind::FunctionCall(MirFunctionCall {
            args,
            is_void: found_fn.ret_type == super::MirType::Void
                || found_fn.ret_type == super::MirType::Never,
            name: found_fn.name.clone(),
        }),
        ty: found_fn.ret_type.clone(),
    }))
}
