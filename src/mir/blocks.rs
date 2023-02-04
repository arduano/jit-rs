use super::{MirBlock, MirStatement};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MirBlockId {
    pub index: usize,
}

pub struct MirBlockBuilder {
    current_block: Option<usize>,
    blocks: Vec<MirBlock>,
}

impl MirBlockBuilder {
    pub fn new() -> Self {
        Self {
            current_block: None,
            blocks: Vec::new(),
        }
    }

    pub fn current_block(&self) -> Option<MirBlockId> {
        self.current_block.map(|index| MirBlockId { index })
    }

    pub fn add_block(&mut self) -> MirBlockId {
        let index = self.blocks.len();
        self.blocks.push(MirBlock {
            statements: Vec::new(),
        });
        MirBlockId { index }
    }

    pub fn select_block(&mut self, block_id: MirBlockId) -> &mut Self {
        self.current_block = Some(block_id.index);
        self
    }

    pub fn add_statement(&mut self, statement: MirStatement) -> &mut Self {
        self.blocks[self.current_block.unwrap()]
            .statements
            .push(statement);
        self
    }

    pub fn get_blocks(self) -> Vec<MirBlock> {
        self.blocks
    }
}
