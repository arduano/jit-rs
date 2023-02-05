macro_rules! get_required_val {
    ($cursor:ident, $val:expr) => {
        match $val.no_match_into_error() {
            ParseResult::Ok(new_cursor, val) => {
                $cursor = new_cursor;
                val
            }
            ParseResult::NoMatch { .. } => unreachable!(),
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    };
}
pub(super) use get_required_val;

macro_rules! pass_val {
    ($val:expr) => {
        match $val {
            Ok(cursor) => cursor,
            Err(err) => return ParseResult::Error(err),
        }
    };
    ($cursor:ident, $val:expr) => {{
        match $val {
            ParseResult::Ok(new_cursor, val) => {
                $cursor = new_cursor;
                Some(val)
            }
            ParseResult::NoMatch { .. } => None,
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}
pub(super) use pass_val;

macro_rules! ident_or_error {
    ($cursor:ident, $err:expr) => {
        if let Some(JitToken {
            kind: JitTokenKind::Ident(name),
            span,
        }) = $cursor.next()
        {
            name
        } else {
            return ParseResult::error($err);
        }
    };
}
pub(super) use ident_or_error;
