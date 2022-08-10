use std::convert::From;
use crate::ast::*;

// =============================================================================
// Declaration
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Decl { pub index: usize }

/// Represents a parameter declaration in the source of a given method.
#[derive(Clone,Debug,PartialEq)]
pub struct Parameter {
    pub declared : Type,
    pub name : Name
}

impl Decl {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Decl::is(&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Decl{index}
    }

    /// Determine whether a given term is a declaration or not.
    pub fn is(t: &Node) -> bool {
        match t {
	    Node::FunctionDecl(_) => true,
	    Node::MethodDecl(_) => true,
            Node::TypeDecl(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Decl {
    fn into(self) -> usize { self.index }
}

// =============================================================================
// Type Declaration
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

impl From<TypeDecl> for Node {
    fn from(d: TypeDecl) -> Self { Node::TypeDecl(d) }
}

// =============================================================================
// Function Declaration
// =============================================================================

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

impl From<FunctionDecl> for Node {
    fn from(d: FunctionDecl) -> Self { Node::FunctionDecl(d) }
}

// =============================================================================
// Method Declaration
// =============================================================================

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

impl From<MethodDecl> for Node {
    fn from(d: MethodDecl) -> Self { Node::MethodDecl(d) }
}

// =============================================================================
// Misc
// =============================================================================

/// A clause represents part of the specification given to a function
/// or method.  For example, a function's precondition is made up from
/// `requires` clauses, etc.
#[derive(Clone,Debug,PartialEq)]
pub enum Clause {
    Requires(Expr),
    Ensures(Expr),
    Where(Expr)
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Modifier {
    Export,
    Final,
    Native,
    Public,
    Private
}
