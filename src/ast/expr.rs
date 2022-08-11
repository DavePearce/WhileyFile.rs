use crate::ast::*;
use crate::lexer::{Token,TokenType};

// =============================================================================
// Expressions
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Expr(pub usize);

impl Expr {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Expr::is(&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Expr(index)
    }

    /// Determine whether a given term is a declaration or not.
    pub fn is(t: &Node) -> bool {
        match t {
	    Node::ArrayAccessExpr(_) => true,
	    Node::ArrayInitialiserExpr(_) => true,
	    Node::ArrayLengthExpr(_) => true,
	    Node::BoolExpr(_) => true,
	    Node::BinaryExpr(_) => true,
	    Node::IntExpr(_) => true,
	    Node::VarAccessExpr(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Expr {
    fn into(self) -> usize { self.0 }
}

// =============================================================================
// Literals
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Bool(pub bool);

impl From<Bool> for Node {
    fn from(s: Bool) -> Self { Node::BoolExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Int(pub i32);

impl From<Int> for Node {
    fn from(s: Int) -> Self { Node::IntExpr(s) }
}

// =============================================================================
// Variables
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct VarAccess(pub Name);

impl From<VarAccess> for Node {
    fn from(s: VarAccess) -> Self { Node::VarAccessExpr(s) }
}

// =============================================================================
// Binary Expressions
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Binary(pub BinOp, pub Expr, pub Expr);

impl From<Binary> for Node {
    fn from(s: Binary) -> Self { Node::BinaryExpr(s) }
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum BinOp {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
    LogicalAnd,
    LogicalOr,
    LogicalXor,
}

impl BinOp {
    /// Attempt to construct a binary operator from an arbitrary
    /// token.  Obviously, this is not guaranteed to succeed!
    pub fn from(token: &Token) -> Option<Self> {
	let bop = match token.kind {
            // Equality
            TokenType::EqualEqual => BinOp::Equals,
            TokenType::ShreakEquals => BinOp::NotEquals,
            // Arithmetic
	    TokenType::LeftAngle => BinOp::LessThan,
            TokenType::LeftAngleEquals => BinOp::LessThanOrEquals,
            TokenType::RightAngle => BinOp::GreaterThan,
            TokenType::RightAngleEquals => BinOp::GreaterThanOrEquals,
            // Logical
            TokenType::AmpersandAmpersand => BinOp::LogicalAnd,
            TokenType::BarBar => BinOp::LogicalOr,
            // No match
	    _ => { return None; }
	};
        Some(bop)
    }
}

// =============================================================================
// Arrays
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayAccess(pub Expr, pub Expr);

impl From<ArrayAccess> for Node {
    fn from(s: ArrayAccess) -> Self { Node::ArrayAccessExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayInitialiser(pub Vec<Expr>);

impl From<ArrayInitialiser> for Node {
    fn from(s: ArrayInitialiser) -> Self { Node::ArrayInitialiserExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayLength(pub Expr);

impl From<ArrayLength> for Node {
    fn from(s: ArrayLength) -> Self { Node::ArrayLengthExpr(s) }
}
