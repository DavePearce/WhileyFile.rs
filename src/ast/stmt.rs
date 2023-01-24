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
	    Node::AssignStmt(_) => true,
	    Node::AssumeStmt(_) => true,
	    Node::BlockStmt(_) => true,
	    Node::IfElseStmt(_) => true,
	    Node::ReturnStmt(_) => true,
	    Node::SkipStmt(_) => true,
	    Node::VarDeclStmt(_) => true,
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
pub struct Assert(pub Expr);

impl From<Assert> for Node {
    fn from(s: Assert) -> Self { Node::AssertStmt(s) }
}

impl TryFromRef<Node> for Assert {
    fn try_from_ref(r:&Node) -> Option<&Self> {
        match r {
            Node::AssertStmt(s) => Some(&s),
            _ => None
        }
    }
}

// =============================================================================
// Assignment
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Assignment(pub LVal, pub Expr);

impl From<Assignment> for Node {
    fn from(s: Assignment) -> Self { Node::AssignStmt(s) }
}

// =============================================================================
// Assume
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Assume(pub Expr);

impl From<Assume> for Node {
    fn from(s: Assume) -> Self { Node::AssumeStmt(s) }
}

// =============================================================================
// Block
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Block(pub Vec<Stmt>);

impl From<Block> for Node {
    fn from(s: Block) -> Self { Node::BlockStmt(s) }
}

// =============================================================================
// IfElse
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct IfElse(pub Expr, pub Stmt, pub Option<Stmt>);

impl From<IfElse> for Node {
    fn from(s: IfElse) -> Self { Node::IfElseStmt(s) }
}

// =============================================================================
// Return
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Return(pub Option<Expr>);

impl From<Return> for Node {
    fn from(s: Return) -> Self { Node::ReturnStmt(s) }
}

// =============================================================================
// Skip
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Skip();

impl From<Skip> for Node {
    fn from(s: Skip) -> Self { Node::SkipStmt(s) }
}

// =============================================================================
// Variable Declarations
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct VarDecl(pub Type, pub Name, pub Option<Expr>);

impl From<VarDecl> for Node {
    fn from(s: VarDecl) -> Self { Node::VarDeclStmt(s) }
}
