use std::{borrow::Cow, collections::HashMap};

use super::{MirType, MirVariable, MirVariableDeclare};

#[derive(Debug, Clone)]
pub struct MirVariableDecl {
    pub name: Cow<'static, str>,
    pub ty: MirType,
    pub index: usize,
}

impl MirVariableDecl {
    pub fn as_var(&self) -> MirVariable {
        MirVariable { index: self.index }
    }
}

pub struct VariableStorage {
    all_vars: Vec<MirVariableDeclare>,
    vars: Vec<HashMap<Cow<'static, str>, MirVariableDecl>>,
}

impl VariableStorage {
    pub fn new() -> Self {
        Self {
            all_vars: Vec::new(),
            vars: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }

    pub fn declare(&mut self, name: Cow<'static, str>, ty: MirType) -> MirVariableDecl {
        let index = self.all_vars.len();

        let var = MirVariableDecl {
            name,
            ty: ty.clone(),
            index,
        };

        self.vars
            .last_mut()
            .unwrap()
            .insert(var.name.clone(), var.clone());

        self.all_vars
            .push(MirVariableDeclare { ty: var.ty.clone() });

        var
    }

    pub fn declare_nameless(&mut self, ty: MirType) -> MirVariableDecl {
        let index = self.all_vars.len();

        self.all_vars.push(MirVariableDeclare { ty: ty.clone() });

        MirVariableDecl {
            name: Cow::from(format!("__var_{}", index)),
            ty,
            index,
        }
    }

    pub fn get(&self, name: &Cow<'static, str>) -> Option<MirVariableDecl> {
        for scope in self.vars.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var.clone());
            }
        }

        None
    }

    pub fn get_variables(self) -> Vec<MirVariableDeclare> {
        self.all_vars
    }
}
