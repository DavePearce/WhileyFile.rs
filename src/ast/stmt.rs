use crate::ast::*;


// =============================================================================
// Statements
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Stmt(pub usize);

impl Stmt {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Stmt::is(&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Stmt(index)
    }

    /// Determine whether a given term is a declaration or not.
    pub fn is(t: &Node) -> bool {
        match t {
	    Node::AssertStmt(_) => true,
	    Node::BlockStmt(_) => true,
	    Node::SkipStmt(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Stmt {
    fn into(self) -> usize { self.0 }
}

// =============================================================================
// Assert
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct AssertStmt(pub Expr);

impl From<AssertStmt> for Node {
    fn from(s: AssertStmt) -> Self { Node::AssertStmt(s) }
}

// =============================================================================
// Block
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct BlockStmt(pub Vec<Stmt>);

impl From<BlockStmt> for Node {
    fn from(s: BlockStmt) -> Self { Node::BlockStmt(s) }
}

// =============================================================================
// Skip
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct SkipStmt();

impl From<SkipStmt> for Node {
    fn from(s: SkipStmt) -> Self { Node::SkipStmt(s) }
}
