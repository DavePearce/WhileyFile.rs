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
pub struct ArrayType { pub element: Type }
impl ArrayType {
    pub fn new(element: Type) -> Self { ArrayType{element} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct BoolType { }
impl BoolType {
    pub fn new() -> Self { BoolType{} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct IntType { pub signed: bool, pub width: u8 }
impl IntType {
    pub fn new(signed: bool, width: u8) -> Self { IntType{signed,width} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct NullType { }
impl NullType {
    pub fn new() -> Self { NullType{} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct RecordType { pub fields: Vec<(Type,Name)> }
impl RecordType {
    pub fn new(fields: Vec<(Type,Name)>) -> Self { RecordType{fields} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct ReferenceType { pub element: Type }
impl ReferenceType {
    pub fn new(element: Type) -> Self { ReferenceType{element} }
}

#[derive(Clone,Debug,PartialEq)]
pub struct VoidType { }
impl VoidType {
    pub fn new() -> Self { VoidType{} }
}
