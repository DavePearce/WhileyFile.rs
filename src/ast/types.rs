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
            Node::UnionType(_) => true,
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

impl Into<Types> for Array {
    fn into(self) -> Types { Types::ArrayType(self) }
}

// =============================================================================
// Bool
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Bool();

impl From<Bool> for Node {
    fn from(s: Bool) -> Self { Node::BoolType(s) }
}

impl Into<Types> for Bool {
    fn into(self) -> Types { Types::BoolType(self) }
}

// =============================================================================
// Function
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Function(pub Vec<Type>, pub Vec<Type>);

impl From<Function> for Node {
    fn from(s: Function) -> Self { Node::FunctionType(s) }
}

impl Into<Types> for Function {
    fn into(self) -> Types { Types::FunctionType(self) }
}

// =============================================================================
// Int
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Int(pub bool, pub u8);

impl From<Int> for Node {
    fn from(s: Int) -> Self { Node::IntType(s) }
}

impl Into<Types> for Int {
    fn into(self) -> Types { Types::IntType(self) }
}

// =============================================================================
// Nominal
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Nominal(pub Name);

impl From<Nominal> for Node {
    fn from(s: Nominal) -> Self { Node::NominalType(s) }
}

impl Into<Types> for Nominal {
    fn into(self) -> Types { Types::NominalType(self) }
}

// =============================================================================
// Null
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Null();

impl From<Null> for Node {
    fn from(s: Null) -> Self { Node::NullType(s) }
}

impl Into<Types> for Null {
    fn into(self) -> Types { Types::NullType(self) }
}

// =============================================================================
// Record
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Record(pub Vec<(Type,Name)>);

impl From<Record> for Node {
    fn from(s: Record) -> Self { Node::RecordType(s) }
}

impl Into<Types> for Record {
    fn into(self) -> Types { Types::RecordType(self) }
}

// =============================================================================
// Reference
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Reference(pub Type);

impl From<Reference> for Node {
    fn from(s: Reference) -> Self { Node::ReferenceType(s) }
}

impl Into<Types> for Reference {
    fn into(self) -> Types { Types::ReferenceType(self) }
}

// =============================================================================
// Union
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Union(pub Type, pub Type);

impl From<Union> for Node {
    fn from(s: Union) -> Self { Node::UnionType(s) }
}

impl Into<Types> for Union {
    fn into(self) -> Types { Types::UnionType(self) }
}

// =============================================================================
// Void
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Void();

impl From<Void> for Node {
    fn from(s: Void) -> Self { Node::VoidType(s) }
}

impl Into<Types> for Void {
    fn into(self) -> Types { Types::VoidType(self) }
}
