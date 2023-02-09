use crate::common::NumberKind;

use super::{MirCastNumber, MirCastVector, MirExpression, MirExpressionKind, MirType};

pub fn mir_cast_to_number(num: MirExpression, to: NumberKind) -> Result<MirExpression, ()> {
    let num_type = match num.ty {
        MirType::Num(num_type) => num_type,
        _ => panic!("Cannot cast non-number to number"),
    };

    let kind = MirExpressionKind::CastNumber(MirCastNumber {
        number: Box::new(num),
        from: num_type,
        to,
    });

    let new_num_type = MirType::Num(to);

    Ok(MirExpression {
        kind,
        ty: new_num_type,
    })
}

pub fn mir_cast_to_vector(
    num: MirExpression,
    to: NumberKind,
    width: u32,
) -> Result<MirExpression, ()> {
    match num.ty {
        MirType::Vector(num_type, width2) => {
            if width != width2 {
                panic!("Cannot cast vector with different width");
            }

            let kind = MirExpressionKind::CastVector(MirCastVector {
                vector: Box::new(num),
                from: num_type,
                to,
            });

            let new_vec_type = MirType::Vector(to, width);

            Ok(MirExpression {
                kind,
                ty: new_vec_type,
            })
        }
        _ => panic!("Invalid cast"),
    }
}
