use crate::ast::{Name,Parameter,Stmt,Type};

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
    pub fn new(name: Name, parameters: Vec<Parameter>, returns: Vec<Parameter>, body: Stmt) -> Self {
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
    pub fn new(name: Name, parameters: Vec<Parameter>, returns: Vec<Parameter>, body: Stmt) -> Self {
        MethodDecl{name,parameters,returns,body}
    }
}

// =============================================================================
// Statements
// =============================================================================

// =============================================================================
// Expressions
// =============================================================================
