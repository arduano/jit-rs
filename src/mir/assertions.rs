use super::MirType;

pub fn mir_assert_types_equal(left: &MirType, right: &MirType) -> Result<(), ()> {
    if left == right {
        Ok(())
    } else {
        panic!("Types do not match, expected {:?} got {:?}", left, right);
    }
}

pub fn mir_assert_is_boolean(ty: &MirType) -> Result<(), ()> {
    mir_assert_types_equal(&MirType::Bool, &ty)?;
    Ok(())
}
