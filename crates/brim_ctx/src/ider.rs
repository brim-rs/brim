use brim_ast::expr::Expr;
use brim_ast::item::{Block, Item};
use brim_ast::NodeId;
use brim_ast::stmts::{Let, Stmt};
use crate::walker::AstWalker;

pub struct IdAssigner {
    current_id: usize,
}

impl IdAssigner {
    pub fn new() -> Self {
        Self { current_id: 0 }
    }

    fn next_id(&mut self) -> NodeId {
        let id = self.current_id;
        self.current_id += 1;
        NodeId::from_usize(id)
    }
}

impl AstWalker for IdAssigner {
    fn visit_item(&mut self, item: &mut Item) {
        item.id = self.next_id(); 
        self.walk_item(item);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        stmt.id = self.next_id(); 
        self.walk_stmt(stmt);    
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        expr.id = self.next_id();
        self.walk_expr(expr);     
    }

    fn visit_let(&mut self, let_stmt: &mut Let) {
        let_stmt.id = self.next_id();
        self.walk_let(let_stmt);   
    }

    fn visit_block(&mut self, block: &mut Block) {
        block.id = self.next_id(); 
        self.walk_block(block);   
    }
}
