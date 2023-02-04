mod cursor;
mod error;
mod types;

pub use types::*;

use crate::macro_builder::{JitToken, JitTokenKind, JitTokenTree};

use self::{cursor::ParseCursor, error::ParseError};

macro_rules! parse_lit {
    ($cursor:ident, $kind:ident) => {
        if matches!(
            $cursor.next(),
            Some(JitToken {
                kind: JitTokenKind::$kind,
                ..
            })
        ) {
            Ok(())
        } else {
            Err(ParseError::Failed)
        }
    };
}

macro_rules! peek_lit {
    ($cursor:ident, $kind:ident) => {
        if matches!(
            $cursor.peek(0),
            Some(JitToken {
                kind: JitTokenKind::$kind,
                ..
            })
        ) {
            true
        } else {
            false
        }
    };
}

macro_rules! parse_ident {
    ($cursor:ident $(,)?) => {
        if let Some(JitToken {
            kind: JitTokenKind::Ident(ident),
            ..
        }) = $cursor.next()
        {
            Ok(ident.clone())
        } else {
            Err(ParseError::Failed)
        }
    };
}

macro_rules! parse_group {
    ($cursor:ident, $kind:ident) => {
        if let Some(JitToken {
            kind: JitTokenKind::$kind(tree),
            ..
        }) = $cursor.next()
        {
            Ok(ParseCursor::new(&tree))
        } else {
            Err(ParseError::Failed)
        }
    };
}

macro_rules! try_parse {
    ($cursor:ident, $parser:ident( $($arg:tt)* )) => {
        {
            let forked = $cursor.fork();
            if let Ok(result) = $parser($cursor, $($arg)*) {
                Some(result)
            } else {
                $cursor.reset(&forked);
                None
            }
        }
    };
    ($cursor:ident, $parser:ident!( $($arg:tt)* )) => {
        {
            let forked = $cursor.fork();
            if let Ok(result) = $parser!($cursor, $($arg)*) {
                Some(result)
            } else {
                $cursor.reset(&forked);
                None
            }
        }
    };
}

pub fn parse_tokens_to_tree(tokens: &JitTokenTree) -> Result<TreeModule, ParseError> {
    let mut cursor = ParseCursor::new(tokens);
    parse_module(&mut cursor)
}

fn parse_module(cursor: &mut ParseCursor) -> Result<TreeModule, ParseError> {
    let mut functions = Vec::new();
    while !cursor.is_empty() {
        let function = parse_function(cursor)?;
        functions.push(function);
    }

    Ok(TreeModule { functions })
}

fn parse_function(cursor: &mut ParseCursor) -> Result<TreeFunction, ParseError> {
    dbg!(cursor.peek(0));

    let _is_pub = try_parse!(cursor, parse_lit!(Pub)).is_some();

    parse_lit!(cursor, Fn)?;
    let name = parse_ident!(cursor)?;

    let mut args_cursor = parse_group!(cursor, Parenthesized)?;
    let mut args = Vec::new();

    while !args_cursor.is_empty() {
        let name = parse_ident!(args_cursor)?;
        parse_lit!(args_cursor, Colon)?;
        let ty = parse_type(&mut args_cursor)?;

        args.push(TreeVariableDeclare { name, ty });

        if !args_cursor.is_empty() {
            parse_lit!(args_cursor, Comma)?;
        }
    }

    // Parse return arrow
    let ret_type = if peek_lit!(cursor, Arrow) {
        parse_lit!(cursor, Arrow)?;
        Some(parse_type(cursor)?)
    } else {
        None
    };

    let mut body_cursor = parse_group!(cursor, Braced)?;
    let body = parse_body(&mut body_cursor)?;

    Ok(TreeFunction {
        name,
        body,
        args,
        ret_type,
    })
}

fn parse_type(cursor: &mut ParseCursor) -> Result<TreeType, ParseError> {
    Ok(TreeType {
        name: parse_ident!(cursor)?.clone(),
    })
}

fn parse_body(cursor: &mut ParseCursor) -> Result<TreeBody, ParseError> {
    let mut body = Vec::new();

    while !cursor.is_empty() {
        let expr = parse_expression(cursor, true)?;
        body.push(expr);
    }

    Ok(TreeBody { body })
}

fn parse_expression(cursor: &mut ParseCursor, is_line: bool) -> Result<TreeExpression, ParseError> {
    let expr = if peek_lit!(cursor, If) {
        TreeExpressionKind::IfStatement(parse_if_expression(cursor)?)
    } else if peek_lit!(cursor, Let) {
        TreeExpressionKind::LetStatement(parse_let_expression(cursor)?)
    } else if peek_lit!(cursor, Return) {
        TreeExpressionKind::ReturnStatement(parse_return_expression(cursor)?)
    } else if let Some(num) = try_parse!(cursor, parse_number_literal()) {
        TreeExpressionKind::Number(num)
    } else if let Some(num) = try_parse!(cursor, parse_bool_literal()) {
        TreeExpressionKind::Bool(num)
    } else if let Some(op) = try_parse!(cursor, parse_unary_operator()) {
        let expr = parse_expression(cursor, false)?;
        TreeExpressionKind::UnaryOp(TreeUnaryOp {
            op,
            expr: Box::new(expr),
        })
    } else if let Some(name) = try_parse!(cursor, parse_ident!()) {
        if let Some(()) = try_parse!(cursor, parse_lit!(Equal)) {
            let value = parse_expression(cursor, false)?;
            TreeExpressionKind::VarAssign(TreeVarAssign {
                name,
                value: Box::new(value),
            })
        } else {
            TreeExpressionKind::ReadVar(TreeVarRead { name })
        }
    } else {
        return Err(ParseError::Failed);
    };

    let expr = if let Some(binary_op) = try_parse!(cursor, parse_binary_operator()) {
        let expression = TreeExpression {
            has_semi: false,
            kind: expr,
        };
        TreeExpressionKind::BinaryOpList(parse_binary_expression(cursor, expression, binary_op)?)
    } else {
        expr
    };

    if is_line && peek_lit!(cursor, Semicolon) {
        parse_lit!(cursor, Semicolon)?;
        Ok(TreeExpression {
            has_semi: true,
            kind: expr,
        })
    } else {
        Ok(TreeExpression {
            has_semi: false,
            kind: expr,
        })
    }
}

fn parse_if_expression(cursor: &mut ParseCursor) -> Result<TreeIfStatement, ParseError> {
    parse_lit!(cursor, If)?;

    let cond = parse_expression(cursor, false)?;

    let mut expr_braces = parse_group!(cursor, Braced)?;
    let then = parse_body(&mut expr_braces)?;

    let else_ = if peek_lit!(cursor, Else) {
        parse_lit!(cursor, Else)?;
        let mut expr_braces = parse_group!(cursor, Braced)?;
        parse_body(&mut expr_braces)?
    } else {
        TreeBody { body: Vec::new() }
    };

    Ok(TreeIfStatement {
        cond: Box::new(cond),
        then,
        else_,
    })
}

fn parse_let_expression(cursor: &mut ParseCursor) -> Result<TreeLetStatement, ParseError> {
    parse_lit!(cursor, Let)?;
    let name = parse_ident!(cursor)?;
    parse_lit!(cursor, Equal)?;
    let value = parse_expression(cursor, false)?;

    Ok(TreeLetStatement {
        name,
        value: Box::new(value),
    })
}

fn parse_return_expression(cursor: &mut ParseCursor) -> Result<TreeReturnStatement, ParseError> {
    parse_lit!(cursor, Return)?;
    let value = parse_expression(cursor, false)?;

    Ok(TreeReturnStatement {
        value: Box::new(value),
    })
}

fn parse_binary_expression(
    cursor: &mut ParseCursor,
    first: TreeExpression,
    first_op: TreeBinaryOpKind,
) -> Result<TreeBinaryOpList, ParseError> {
    let mut pairs = vec![(first, first_op)];
    let mut last = parse_expression(cursor, false)?;

    loop {
        let op = try_parse!(cursor, parse_binary_operator());
        if op.is_none() {
            break;
        }

        let op = op.unwrap();
        let right = parse_expression(cursor, false)?;

        pairs.push((last, op));
        last = right;
    }

    Ok(TreeBinaryOpList {
        last: Box::new(last),
        pairs,
    })
}

fn parse_binary_operator(cursor: &mut ParseCursor) -> Result<TreeBinaryOpKind, ParseError> {
    match cursor.peek(0) {
        Some(token) => match token.kind {
            JitTokenKind::Plus => {
                cursor.next();
                Ok(TreeBinaryOpKind::Add)
            }
            JitTokenKind::Minus => {
                cursor.next();
                Ok(TreeBinaryOpKind::Sub)
            }
            JitTokenKind::Star => {
                cursor.next();
                Ok(TreeBinaryOpKind::Mul)
            }
            JitTokenKind::Slash => {
                cursor.next();
                Ok(TreeBinaryOpKind::Div)
            }
            JitTokenKind::LeftBracket => {
                cursor.next();
                Ok(TreeBinaryOpKind::Lt)
            }
            JitTokenKind::RightBracket => {
                cursor.next();
                Ok(TreeBinaryOpKind::Gt)
            }
            _ => Err(ParseError::Failed),
        },
        None => Err(ParseError::Failed),
    }
}

fn parse_unary_operator(cursor: &mut ParseCursor) -> Result<TreeUnaryOpKind, ParseError> {
    match cursor.peek(0) {
        Some(token) => match token.kind {
            JitTokenKind::Minus => {
                cursor.next();
                Ok(TreeUnaryOpKind::Neg)
            }
            _ => Err(ParseError::Failed),
        },
        None => Err(ParseError::Failed),
    }
}

fn parse_number_literal(cursor: &mut ParseCursor) -> Result<TreeNumberLiteral, ParseError> {
    match cursor.peek(0) {
        Some(token) => match &token.kind {
            JitTokenKind::Number(kind, str) => {
                cursor.next();
                Ok(TreeNumberLiteral {
                    value: str.clone(),
                    ty: kind.clone(),
                })
            }
            _ => Err(ParseError::Failed),
        },
        None => Err(ParseError::Failed),
    }
}

fn parse_bool_literal(cursor: &mut ParseCursor) -> Result<TreeBoolLiteral, ParseError> {
    match cursor.peek(0) {
        Some(token) => match &token.kind {
            JitTokenKind::Bool(value) => {
                cursor.next();
                Ok(TreeBoolLiteral { value: *value })
            }
            _ => Err(ParseError::Failed),
        },
        None => Err(ParseError::Failed),
    }
}
