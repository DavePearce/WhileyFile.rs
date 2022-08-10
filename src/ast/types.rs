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
            Node::BoolType(_) => true,
            Node::IntType(_) => true,
            Node::NominalType(_) => true,
            Node::NullType(_) => true,
            Node::VoidType(_) => true,
            Node::ArrayType(_) => true,
            Node::ReferenceType(_) => true,
            Node::RecordType(_) => true,
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
pub struct ArrayType(pub Type);

impl From<ArrayType> for Node {
    fn from(s: ArrayType) -> Self { Node::ArrayType(s) }
}

// =============================================================================
// Bool
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct BoolType();

impl From<BoolType> for Node {
    fn from(s: BoolType) -> Self { Node::BoolType(s) }
}

// =============================================================================
// Int
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct IntType(pub bool, pub u8);

impl From<IntType> for Node {
    fn from(s: IntType) -> Self { Node::IntType(s) }
}

// =============================================================================
// Nominal
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct NominalType(pub Name);

impl From<NominalType> for Node {
    fn from(s: NominalType) -> Self { Node::NominalType(s) }
}

// =============================================================================
// Null
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct NullType();

impl From<NullType> for Node {
    fn from(s: NullType) -> Self { Node::NullType(s) }
}

// =============================================================================
// Record
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct RecordType(pub Vec<(Type,Name)>);

impl From<RecordType> for Node {
    fn from(s: RecordType) -> Self { Node::RecordType(s) }
}

// =============================================================================
// Reference
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct ReferenceType(pub Type);

impl From<ReferenceType> for Node {
    fn from(s: ReferenceType) -> Self { Node::ReferenceType(s) }
}

// =============================================================================
// Void
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct VoidType();

impl From<VoidType> for Node {
    fn from(s: VoidType) -> Self { Node::VoidType(s) }
}
