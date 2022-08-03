use crate::ast::{Expr,Name,Parameter,Stmt,Type};

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
pub struct EqualsExpr(pub Expr, pub Expr);

#[derive(Clone,Debug,PartialEq)]
pub struct NotEqualsExpr(pub Expr, pub Expr);

#[derive(Clone,Debug,PartialEq)]
pub struct LessThanExpr(pub Expr, pub Expr);

#[derive(Clone,Debug,PartialEq)]
pub struct IntExpr(pub i32);

#[derive(Clone,Debug,PartialEq)]
pub struct VarExpr(pub Name);

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
