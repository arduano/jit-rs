use crate::{
    macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind},
    tree_parser::{
        macros::{get_required_val, ident_or_error, pass_val},
        parser::{ParseCursor, ParseResult, TreeParseItem},
    },
};

use super::*;

#[derive(Debug, Clone)]
pub struct TreeExpression {
    pub kind: TreeExpressionKind,
}

#[derive(Debug, Clone)]
pub enum TreeExpressionKind {
    IfStatement(TreeIfStatement),
    WhileStatement(TreeWhileStatement),
    LetStatement(TreeLetStatement),
    ReturnStatement(TreeReturnStatement),
    BinaryOpList(TreeBinaryOpList),
    IndexOp(TreeIndexOp),
    UnaryOp(TreeUnaryOp),
    Group(TreeBody),
    VarRead(TreeVarRead),
    PtrAssign(TreePtrAssign),
    Number(TreeNumberLiteral),
    Bool(TreeBoolLiteral),
    Parenthesized(TreeParenthesizedExpr),
    VoidValue(Box<TreeExpression>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ExprLocation {
    Root,
    Other,
}

impl TreeExpression {
    pub fn parse<'a>(mut cursor: ParseCursor<'a>, pos: ExprLocation) -> ParseResult<'a, Self> {
        let expr_kind = if let Some(expr) =
            pass_val!(cursor, TreeIfStatement::parse(cursor.clone()))
        {
            TreeExpressionKind::IfStatement(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeLetStatement::parse(cursor.clone())) {
            TreeExpressionKind::LetStatement(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeWhileStatement::parse(cursor.clone())) {
            TreeExpressionKind::WhileStatement(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeVarRead::parse(cursor.clone())) {
            TreeExpressionKind::VarRead(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeUnaryOp::parse(cursor.clone())) {
            TreeExpressionKind::UnaryOp(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeNumberLiteral::parse(cursor.clone())) {
            TreeExpressionKind::Number(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeBoolLiteral::parse(cursor.clone())) {
            TreeExpressionKind::Bool(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeParenthesizedExpr::parse(cursor.clone())) {
            TreeExpressionKind::Parenthesized(expr)
        } else if let Some(expr) = pass_val!(cursor, TreeReturnStatement::parse(cursor.clone())) {
            TreeExpressionKind::ReturnStatement(expr)
        } else {
            dbg!(cursor.peek(0));
            return ParseResult::error("couldn't parse expression");
        };

        let mut expr = TreeExpression { kind: expr_kind };

        loop {
            if pos == ExprLocation::Root {
                if cursor.parse_next_basic(JitBasicToken::Semicolon) {
                    expr = TreeExpression {
                        kind: TreeExpressionKind::VoidValue(Box::new(expr)),
                    };
                    break;
                }
            }

            if TreeBinaryOpList::could_match(&cursor) {
                expr = TreeExpression {
                    kind: TreeExpressionKind::BinaryOpList(get_required_val!(
                        cursor,
                        TreeBinaryOpList::parse(cursor, expr)
                    )),
                };
                continue;
            }
            if TreePtrAssign::could_match(&cursor) {
                expr = TreeExpression {
                    kind: TreeExpressionKind::PtrAssign(get_required_val!(
                        cursor,
                        TreePtrAssign::parse(cursor, expr)
                    )),
                };
                continue;
            }
            if TreeIndexOp::could_match(&cursor) {
                expr = TreeExpression {
                    kind: TreeExpressionKind::IndexOp(get_required_val!(
                        cursor,
                        TreeIndexOp::parse(cursor, expr)
                    )),
                };
                continue;
            }

            break;
        }

        ParseResult::Ok(cursor, expr)
    }
}

#[derive(Debug, Clone)]
pub struct TreeLetStatement {
    pub name: Cow<'static, str>,
    pub value: Box<TreeExpression>,
}

impl TreeLetStatement {
    const KIND: &'static str = "let statement";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::Let) {
            return ParseResult::no_match(Self::KIND);
        }

        let name = ident_or_error!(cursor, "expected variable name");

        if !cursor.parse_next_basic(JitBasicToken::Equal) {
            return ParseResult::error("expected '=' after variable name");
        }

        let value = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::Other));

        ParseResult::Ok(
            cursor,
            Self {
                name: name.clone(),
                value: Box::new(value),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeReturnStatement {
    pub value: Box<TreeExpression>,
}

impl TreeReturnStatement {
    const KIND: &'static str = "return statement";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::Return) {
            return ParseResult::no_match(Self::KIND);
        }

        let value = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::Other));

        ParseResult::Ok(
            cursor,
            Self {
                value: Box::new(value),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeUnaryOp {
    pub op: TreeUnaryOpKind,
    pub expr: Box<TreeExpression>,
}

impl TreeUnaryOp {
    const KIND: &'static str = "unary operator";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if let Some(op) = pass_val!(cursor, TreeUnaryOpKind::parse(cursor.clone())) {
            let expr = get_required_val!(
                cursor,
                TreeExpression::parse(cursor.clone(), ExprLocation::Other)
            );

            ParseResult::Ok(
                cursor,
                Self {
                    op,
                    expr: Box::new(expr),
                },
            )
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TreeVarRead {
    pub name: Cow<'static, str>,
}

impl TreeVarRead {
    const KIND: &'static str = "variable read";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if let Some(JitToken {
            kind: JitTokenKind::Ident(name),
            span,
        }) = cursor.next()
        {
            ParseResult::Ok(cursor, Self { name: name.clone() })
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TreeParenthesizedExpr {
    pub inner: Box<TreeExpression>,
}

impl TreeParenthesizedExpr {
    const KIND: &'static str = "parenthesized expression";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if let Ok(mut inner_cursor) = cursor.parse_next_group(JitGroupKind::Parentheses) {
            let inner = get_required_val!(
                inner_cursor,
                TreeExpression::parse(inner_cursor, ExprLocation::Other)
            );

            if !inner_cursor.is_empty() {
                return ParseResult::error("expected end of parentheses");
            }

            ParseResult::Ok(
                cursor,
                Self {
                    inner: Box::new(inner),
                },
            )
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}
