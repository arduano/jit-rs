use super::{MirExpression, MirExpressionKind, MirType, MirTypeKind, MirVectorExtend};

pub fn mir_extend_num_to_vector(num: MirExpression, width: u32) -> Result<MirExpression, ()> {
    let num_type = match num.ty.kind {
        MirTypeKind::Num(num_type) => num_type,
        _ => return Err(()),
    };

    let kind = MirExpressionKind::VectorExtend(MirVectorExtend {
        unit: Box::new(num),
        width,
        unit_ty: num_type,
    });

    let new_vec_type = MirType {
        kind: MirTypeKind::Vector(num_type, width),
    };

    Ok(MirExpression {
        kind,
        ty: new_vec_type,
    })
}
