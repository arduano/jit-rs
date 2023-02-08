use super::{MirExpression, MirExpressionKind, MirType, MirVectorExtend};

pub fn mir_extend_num_to_vector(num: MirExpression, width: u32) -> Result<MirExpression, ()> {
    let num_type = match num.ty {
        MirType::Num(num_type) => num_type,
        _ => return Err(()),
    };

    let kind = MirExpressionKind::VectorExtend(MirVectorExtend {
        unit: Box::new(num),
        width,
        unit_ty: num_type,
    });

    let new_vec_type = MirType::Vector(num_type, width);

    Ok(MirExpression {
        kind,
        ty: new_vec_type,
    })
}

pub fn mir_is_empty_type(ty: &MirType) -> bool {
    match &ty {
        MirType::Void => true,
        MirType::Never => true,
        _ => false,
    }
}

pub fn mir_make_empty_expr() -> MirExpression {
    MirExpression {
        kind: MirExpressionKind::NoValue,
        ty: MirType::Void,
    }
}
