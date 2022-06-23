use crate::ast::{Expr,Name,Parameter,Stmt,Type};

// =============================================================================
// Declarations
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct TypeDecl {
    name: Name,
    pattern: Type
}

impl TypeDecl {
    pub fn new(name: Name, pattern: Type) -> Self {
        TypeDecl{name,pattern}
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct FunctionDecl {
    name: Name,
    parameters: Vec<Parameter>,
    returns: Vec<Parameter>,
    body:Stmt
}

impl FunctionDecl {
    pub fn new(name: Name, parameters: Vec<Parameter>,
               returns: Vec<Parameter>, body: Stmt) -> Self {
        FunctionDecl{name,parameters,returns,body}
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct MethodDecl {
    name: Name,
    parameters: Vec<Parameter>,
    returns: Vec<Parameter>,
    body:Stmt
}

impl MethodDecl {
    pub fn new(name: Name, parameters: Vec<Parameter>,
               returns: Vec<Parameter>, body: Stmt) -> Self {
        MethodDecl{name,parameters,returns,body}
    }
}

// =============================================================================
// Statements
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct AssertStmt {
    operand: Expr
}

impl AssertStmt {
    pub fn new(operand: Expr) -> Self { AssertStmt{operand} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct BlockStmt {
    stmts: Vec<Stmt>
}

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> Self { BlockStmt{stmts} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct SkipStmt {}

impl SkipStmt {
    pub fn new() -> Self { SkipStmt{} }
}


// =============================================================================
// Expressions
// =============================================================================
