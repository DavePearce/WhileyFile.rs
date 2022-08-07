use crate::ast::{Expr,Name,Parameter,Stmt,Type};
use crate::lexer::{Token,TokenType};

// =============================================================================
// Declarations
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct TypeDecl {
    modifiers: Vec<Modifier>,
    name: Name,
    pattern: Type
}
impl TypeDecl {
    pub fn new(modifiers: Vec<Modifier>, name: Name, pattern: Type) -> Self {
        TypeDecl{modifiers,name,pattern}
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct FunctionDecl {
    modifiers: Vec<Modifier>,
    name: Name,
    parameters: Vec<Parameter>,
    returns: Vec<Parameter>,
    clauses: Vec<Clause>,
    body:Stmt
}
impl FunctionDecl {
    pub fn new(modifiers: Vec<Modifier>, name: Name, parameters: Vec<Parameter>,
               returns: Vec<Parameter>, clauses: Vec<Clause>, body: Stmt) -> Self {
        FunctionDecl{modifiers,name,parameters,returns,clauses,body}
    }
}

#[derive(Clone,Debug,PartialEq)]
    pub struct MethodDecl {
    modifiers: Vec<Modifier>,
    name: Name,
    parameters: Vec<Parameter>,
    returns: Vec<Parameter>,
    clauses: Vec<Clause>,
    body:Stmt
}
impl MethodDecl {
    pub fn new(modifiers: Vec<Modifier>, name: Name, parameters: Vec<Parameter>,
               returns: Vec<Parameter>, clauses: Vec<Clause>, body: Stmt) -> Self {
        MethodDecl{modifiers,name,parameters,returns,clauses,body}
    }
}

/// A clause represents part of the specification given to a function
/// or method.  For example, a function's precondition is made up from
/// `requires` clauses, etc.
#[derive(Clone,Debug,PartialEq)]
pub enum Clause {
    Requires(Expr),
    Ensures(Expr),
    Where(Expr)
}

// =============================================================================
// Statements
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct AssertStmt(pub Expr);

#[derive(Clone,Debug,PartialEq)]
pub struct BlockStmt(pub Vec<Stmt>);

#[derive(Clone,Debug,PartialEq)]
pub struct SkipStmt();

// =============================================================================
// Expressions
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct BoolExpr(pub bool);

#[derive(Clone,Debug,PartialEq)]
pub struct BinaryExpr(pub BinOp, pub Expr, pub Expr);

#[derive(Clone,Debug,PartialEq)]
pub struct IntExpr(pub i32);

#[derive(Clone,Debug,PartialEq)]
pub struct VarExpr(pub Name);

// =============================================================================
// Binary Operators
// =============================================================================
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
// Types
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayType(pub Type);

#[derive(Clone,Debug,PartialEq)]
pub struct BoolType();

#[derive(Clone,Debug,PartialEq)]
pub struct IntType(pub bool, pub u8);

#[derive(Clone,Debug,PartialEq)]
pub struct NullType();

#[derive(Clone,Debug,PartialEq)]
pub struct RecordType(pub Vec<(Type,Name)>);

#[derive(Clone,Debug,PartialEq)]
pub struct ReferenceType(pub Type);

#[derive(Clone,Debug,PartialEq)]
pub struct VoidType();

#[derive(Clone,Debug,PartialEq)]
pub struct NominalType(pub Name);

// =============================================================================
// Modifiers
// =============================================================================

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Modifier {
    Export,
    Final,
    Native,
    Public,
    Private
}

// =============================================================================
// Misc
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct LineComment(pub String);

#[derive(Clone,Debug,PartialEq)]
pub struct BlockComment(pub String);
