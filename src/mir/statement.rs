use super::{variables::MirVariableDecl, MirExpression, MirExpressionKind, MirGetVariablePtr};

#[derive(Debug, Clone)]
pub struct MirStatement {
    pub kind: MirStatementKind,
}

impl MirStatement {
    pub fn set_variable(var: MirVariableDecl, value: MirExpression) -> Self {
        let ptr = MirExpression {
            kind: MirExpressionKind::GetVariablePtr(MirGetVariablePtr { var: var.as_var() }),
            ty: var.ty,
        };

        // FIXME: Check the types match

        Self {
            kind: MirStatementKind::PtrAssign(MirPtrAssign { ptr, value }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MirStatementKind {
    PtrAssign(MirPtrAssign),
    Jump(MirJump),
    ConditionalJump(MirConditionalJump),
    Return(Option<MirExpression>),
}

#[derive(Debug, Clone)]
pub struct MirPtrAssign {
    pub ptr: MirExpression,
    pub value: MirExpression,
}

#[derive(Debug, Clone)]
pub struct MirJump {
    pub index: usize,
}
#[derive(Debug, Clone)]
pub struct MirConditionalJump {
    pub condition: MirExpression,
    pub then_index: usize,
    pub else_index: usize,
}
