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
	    Node::BoolLiteral(_) => true,
	    Node::CharLiteral(_) => true,
	    Node::BinaryExpr(_) => true,
	    Node::IntLiteral(_) => true,
	    Node::InvokeExpr(_) => true,
	    Node::VarAccessExpr(_) => true,
	    Node::StringLiteral(_) => true,
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
pub struct BoolLiteral(pub bool);

impl From<BoolLiteral> for Node {
    fn from(s: BoolLiteral) -> Self { Node::BoolLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct CharLiteral(pub char);

impl From<CharLiteral> for Node {
    fn from(s: CharLiteral) -> Self { Node::CharLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct IntLiteral(pub i32);

impl From<IntLiteral> for Node {
    fn from(s: IntLiteral) -> Self { Node::IntLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct StringLiteral(pub String);

impl From<StringLiteral> for Node {
    fn from(s: StringLiteral) -> Self { Node::StringLiteral(s) }
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
// Invoke
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Invoke(pub Expr, pub Vec<Expr>);

impl From<Invoke> for Node {
    fn from(s: Invoke) -> Self { Node::InvokeExpr(s) }
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
