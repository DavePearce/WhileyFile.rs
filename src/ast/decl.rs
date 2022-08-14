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
    pub declared : types::Type,
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
pub struct Type {
    modifiers: Vec<Modifier>,
    name: Name,
    pattern: types::Type
}

impl Type {
    pub fn new(modifiers: Vec<Modifier>, name: Name, pattern: types::Type) -> Self {
        Type{modifiers,name,pattern}
    }
}

impl From<Type> for Node {
    fn from(d: Type) -> Self { Node::TypeDecl(d) }
}

// =============================================================================
// Function Declaration
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Function {
    modifiers: Vec<Modifier>,
    name: Name,
    parameters: Vec<Parameter>,
    returns: Vec<Parameter>,
    clauses: Vec<Clause>,
    body:Stmt
}

impl Function {
    pub fn new(modifiers: Vec<Modifier>, name: Name, parameters: Vec<Parameter>,
               returns: Vec<Parameter>, clauses: Vec<Clause>, body: Stmt) -> Self {
        Function{modifiers,name,parameters,returns,clauses,body}
    }
}

impl From<Function> for Node {
    fn from(d: Function) -> Self { Node::FunctionDecl(d) }
}

// =============================================================================
// Method Declaration
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
    pub struct Method {
    modifiers: Vec<Modifier>,
    name: Name,
    parameters: Vec<Parameter>,
    returns: Vec<Parameter>,
    clauses: Vec<Clause>,
    body:Stmt
}
impl Method {
    pub fn new(modifiers: Vec<Modifier>, name: Name, parameters: Vec<Parameter>,
               returns: Vec<Parameter>, clauses: Vec<Clause>, body: Stmt) -> Self {
        Method{modifiers,name,parameters,returns,clauses,body}
    }
}

impl From<Method> for Node {
    fn from(d: Method) -> Self { Node::MethodDecl(d) }
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
