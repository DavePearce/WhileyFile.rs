use crate::ast::*;

// =============================================================================
// Types
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Type(pub usize);

impl Type {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Type::is(ast,&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Type(index)
    }

    /// Determine whether a given term is a type (or not).
    pub fn is(ast: &AbstractSyntaxTree, t: &Node) -> bool {
        match t {
            Node::ArrayType(_) => true,
            Node::BoolType(_) => true,
            Node::FunctionType(_) => true,
            Node::IntType(_) => true,
            Node::NominalType(_) => true,
            Node::NullType(_) => true,
            Node::ReferenceType(_) => true,
            Node::RecordType(_) => true,
            Node::VoidType(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Type {
    fn into(self) -> usize { self.0 }
}

// =============================================================================
// Array
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Array(pub Type);

impl From<Array> for Node {
    fn from(s: Array) -> Self { Node::ArrayType(s) }
}

// =============================================================================
// Bool
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Bool();

impl From<Bool> for Node {
    fn from(s: Bool) -> Self { Node::BoolType(s) }
}

// =============================================================================
// Function
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Function(pub Vec<Type>, pub Vec<Type>);

impl From<Function> for Node {
    fn from(s: Function) -> Self { Node::FunctionType(s) }
}

// =============================================================================
// Int
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Int(pub bool, pub u8);

impl From<Int> for Node {
    fn from(s: Int) -> Self { Node::IntType(s) }
}

// =============================================================================
// Nominal
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Nominal(pub Name);

impl From<Nominal> for Node {
    fn from(s: Nominal) -> Self { Node::NominalType(s) }
}

// =============================================================================
// Null
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Null();

impl From<Null> for Node {
    fn from(s: Null) -> Self { Node::NullType(s) }
}

// =============================================================================
// Record
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Record(pub Vec<(Type,Name)>);

impl From<Record> for Node {
    fn from(s: Record) -> Self { Node::RecordType(s) }
}

// =============================================================================
// Reference
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Reference(pub Type);

impl From<Reference> for Node {
    fn from(s: Reference) -> Self { Node::ReferenceType(s) }
}

// =============================================================================
// Void
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Void();

impl From<Void> for Node {
    fn from(s: Void) -> Self { Node::VoidType(s) }
}
