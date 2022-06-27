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

#[derive(Clone,Debug,PartialEq)]
pub struct BoolExpr { value: bool }
impl BoolExpr {
    pub fn new(value: bool) -> Self { BoolExpr{value} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct EqualsExpr { lhs: Expr, rhs: Expr }
impl EqualsExpr {
    pub fn new(lhs: Expr, rhs: Expr) -> Self { EqualsExpr{lhs,rhs} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct NotEqualsExpr { lhs: Expr, rhs: Expr }
impl NotEqualsExpr {
    pub fn new(lhs: Expr, rhs: Expr) -> Self { NotEqualsExpr{lhs,rhs} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct LessThanExpr { pub lhs: Expr, pub rhs: Expr }
impl LessThanExpr {
    pub fn new(lhs: Expr, rhs: Expr) -> Self { LessThanExpr{lhs,rhs} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct IntExpr { value: i32 }
impl IntExpr {
    pub fn new(value: i32) -> Self { IntExpr{value} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct VarExpr { name: Name }
impl VarExpr {
    pub fn new(name: Name) -> Self { VarExpr{name} }
}

// =============================================================================
// Expressions
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayType(pub Type);

#[derive(Clone,Debug,PartialEq)]
pub struct BoolType();

#[derive(Clone,Debug,PartialEq)]
pub struct IntType(pub bool, pub u8); // { pub signed: bool, pub width: u8 }

#[derive(Clone,Debug,PartialEq)]
pub struct NullType();

#[derive(Clone,Debug,PartialEq)]
pub struct RecordType(pub Vec<(Type,Name)>);

#[derive(Clone,Debug,PartialEq)]
pub struct ReferenceType(pub Type);

#[derive(Clone,Debug,PartialEq)]
pub struct VoidType();
