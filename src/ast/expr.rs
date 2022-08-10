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
	    Node::ArrayLengthExpr(_) => true,
	    Node::BoolExpr(_) => true,
	    Node::BinaryExpr(_) => true,
	    Node::IntExpr(_) => true,
	    Node::VarExpr(_) => true,
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
pub struct BoolExpr(pub bool);

impl From<BoolExpr> for Node {
    fn from(s: BoolExpr) -> Self { Node::BoolExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct IntExpr(pub i32);

impl From<IntExpr> for Node {
    fn from(s: IntExpr) -> Self { Node::IntExpr(s) }
}

// =============================================================================
// Variables
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct VarExpr(pub Name);

impl From<VarExpr> for Node {
    fn from(s: VarExpr) -> Self { Node::VarExpr(s) }
}

// =============================================================================
// Binary Expressions
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct BinaryExpr(pub BinOp, pub Expr, pub Expr);

impl From<BinaryExpr> for Node {
    fn from(s: BinaryExpr) -> Self { Node::BinaryExpr(s) }
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
pub struct ArrayLengthExpr(pub Expr);

impl From<ArrayLengthExpr> for Node {
    fn from(s: ArrayLengthExpr) -> Self { Node::ArrayLengthExpr(s) }
}
