use std::borrow::Cow;

use crate::tree_parser::TreeFunctionNameWithMarkers;

use super::{
    mir_parse_type_marker, MirBlock, MirType, MirTypeContext, MirTypeMarker, MirVariableDeclare,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirFunctionNameWithMarkers {
    pub name: Cow<'static, str>,
    pub markers: Vec<MirTypeMarker>,
}

impl MirFunctionNameWithMarkers {
    pub fn as_string<'a>(&'a self) -> Cow<'a, str> {
        if self.markers.is_empty() {
            self.name.as_ref().into()
        } else {
            struct Formatter<'a>(&'a MirFunctionNameWithMarkers);
            impl std::fmt::Display for Formatter<'_> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.0.name)?;
                    for marker in &self.0.markers {
                        write!(f, "$")?;
                        marker.format_as_mangled_unique(f)?;
                    }
                    Ok(())
                }
            }

            Cow::Owned(format!("{}", Formatter(self)))
        }
    }
}

#[derive(Debug, Clone)]
pub struct MirFunctionDeclaration {
    pub public: bool,
    pub name: MirFunctionNameWithMarkers,
    pub args: Vec<MirFunctionArg>,
    pub ret_type: MirType,
}

#[derive(Debug, Clone)]
pub struct MirFunctionBody {
    pub decl: MirFunctionDeclaration,
    pub variables: Vec<MirVariableDeclare>,
    pub blocks: Vec<MirBlock>,
}

#[derive(Debug, Clone)]
pub struct MirFunction {
    pub decl: MirFunctionDeclaration,
    pub variables: Vec<MirVariableDeclare>,
    pub blocks: Vec<MirBlock>,
}

#[derive(Debug, Clone)]
pub struct MirFunctionArg {
    pub name: Cow<'static, str>,
    pub ty: MirType,
}

pub fn mir_parse_fn_name_with_markers(
    name: &TreeFunctionNameWithMarkers,
    ctx: &MirTypeContext,
) -> Result<MirFunctionNameWithMarkers, ()> {
    Ok(MirFunctionNameWithMarkers {
        name: name.name.clone(),
        markers: name
            .ty_markers
            .iter()
            .map(|m| mir_parse_type_marker(m, ctx))
            .collect::<Result<_, _>>()?,
    })
}
