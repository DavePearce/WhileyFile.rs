use std::fmt;
use std::convert::From;
use syntactic_heap::SyntacticHeap;
use syntactic_heap::Ref;
pub use crate::nodes::*;

// =============================================================================
// Abstract Syntax Tree
// =============================================================================

pub type AbstractSyntaxTree = SyntacticHeap<Node>;

// =============================================================================
// Terms
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub enum Node {
    // Base
    Utf8(String),
    LineComment(LineComment),
    BlockComment(BlockComment),
    // Declarations
    TypeDecl(TypeDecl),
    FunctionDecl(FunctionDecl),
    MethodDecl(MethodDecl),
    // Statements
    AssertStmt(AssertStmt),
    BlockStmt(BlockStmt),
    SkipStmt(SkipStmt),
    // Expressions
    BoolExpr(BoolExpr),
    EqualsExpr(EqualsExpr),
    NotEqualsExpr(NotEqualsExpr),
    LessThanExpr(LessThanExpr),
    IntExpr(IntExpr),
    VarExpr(VarExpr),
    // Types
    ArrayType(ArrayType),
    BoolType(BoolType),
    IntType(IntType),
    NullType(NullType),
    RecordType(RecordType),
    ReferenceType(ReferenceType),
    VoidType(VoidType)
}

// =============================================================================
// Declarations
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

impl From<FunctionDecl> for Node {
    fn from(d: FunctionDecl) -> Self { Node::FunctionDecl(d) }
}

impl From<MethodDecl> for Node {
    fn from(d: MethodDecl) -> Self { Node::MethodDecl(d) }
}

impl From<TypeDecl> for Node {
    fn from(d: TypeDecl) -> Self { Node::TypeDecl(d) }
}

// =============================================================================
// Statements
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Stmt(pub usize);

impl Stmt {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Stmt::is(&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Stmt(index)
    }

    /// Determine whether a given term is a declaration or not.
    pub fn is(t: &Node) -> bool {
        match t {
	    Node::AssertStmt(_) => true,
	    Node::BlockStmt(_) => true,
	    Node::SkipStmt(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Stmt {
    fn into(self) -> usize { self.0 }
}

impl From<AssertStmt> for Node {
    fn from(s: AssertStmt) -> Self { Node::AssertStmt(s) }
}

impl From<BlockStmt> for Node {
    fn from(s: BlockStmt) -> Self { Node::BlockStmt(s) }
}

impl From<SkipStmt> for Node {
    fn from(s: SkipStmt) -> Self { Node::SkipStmt(s) }
}

// =============================================================================
// Expressions
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Expr(pub usize);

impl Expr {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Expr::is(&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Expr(index)
    }

    /// Determine whether a given term is a declaration or not.
    pub fn is(t: &Node) -> bool {
        match t {
	    Node::BoolExpr(_) => true,
	    Node::EqualsExpr(_) => true,
	    Node::LessThanExpr(_) => true,
	    Node::NotEqualsExpr(_) => true,
	    Node::IntExpr(_) => true,
	    Node::VarExpr(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Expr {
    fn into(self) -> usize { self.0 }
}

impl From<BoolExpr> for Node {
    fn from(s: BoolExpr) -> Self { Node::BoolExpr(s) }
}

impl From<EqualsExpr> for Node {
    fn from(s: EqualsExpr) -> Self { Node::EqualsExpr(s) }
}

impl From<NotEqualsExpr> for Node {
    fn from(s: NotEqualsExpr) -> Self { Node::NotEqualsExpr(s) }
}

impl From<LessThanExpr> for Node {
    fn from(s: LessThanExpr) -> Self { Node::LessThanExpr(s) }
}

impl From<IntExpr> for Node {
    fn from(s: IntExpr) -> Self { Node::IntExpr(s) }
}

impl From<VarExpr> for Node {
    fn from(s: VarExpr) -> Self { Node::VarExpr(s) }
}

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

impl From<ArrayType> for Node {
    fn from(s: ArrayType) -> Self { Node::ArrayType(s) }
}

impl From<BoolType> for Node {
    fn from(s: BoolType) -> Self { Node::BoolType(s) }
}

impl From<IntType> for Node {
    fn from(s: IntType) -> Self { Node::IntType(s) }
}

impl From<NullType> for Node {
    fn from(s: NullType) -> Self { Node::NullType(s) }
}

impl From<RecordType> for Node {
    fn from(s: RecordType) -> Self { Node::RecordType(s) }
}

impl From<ReferenceType> for Node {
    fn from(s: ReferenceType) -> Self { Node::ReferenceType(s) }
}

impl From<VoidType> for Node {
    fn from(s: VoidType) -> Self { Node::VoidType(s) }
}

// =============================================================================
// Names
// =============================================================================

#[derive(Clone,Copy,Debug,Hash,PartialEq,Eq)]
pub struct Name(pub usize);

impl Name {
    pub fn new(ast: &mut AbstractSyntaxTree, s : &str) -> Self {
	let node = Node::Utf8(s.to_string());
        // Create new node
        let index = ast.push(node).raw_index();
        // Done
        Name(index)
    }
}

// =============================================================================
// Misc
// =============================================================================

impl From<LineComment> for Node {
    fn from(s: LineComment) -> Self { Node::LineComment(s) }
}

impl From<BlockComment> for Node {
    fn from(s: BlockComment) -> Self { Node::BlockComment(s) }
}

// =============================================================================
// Conversions
// =============================================================================

impl From<Ref<'_,Node>> for Decl {
    fn from(r: Ref<'_,Node>) -> Decl {
	Decl{index:r.raw_index()}
    }
}

impl From<Ref<'_,Node>> for Stmt {
    fn from(r: Ref<'_,Node>) -> Stmt {
	Stmt(r.raw_index())
    }
}

impl From<Ref<'_,Node>> for Type {
    fn from(r: Ref<'_,Node>) -> Type {
	Type(r.raw_index())
    }
}

impl From<Ref<'_,Node>> for Name {
    fn from(r: Ref<'_,Node>) -> Name {
	Name(r.raw_index())
    }
}

// =============================================================================
// Debug
// =============================================================================

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::TypeDecl(d) => {
                write!(f,"TypeDecl({:?})",d)
            }
            Node::ArrayType(t) => {
                write!(f,"ArrayType({:?})",t.0)
            }
            // Default for those without children
            _ => write!(f,"{:?}",self)
        }
    }
}

fn to_string<T:fmt::Display>(items : &[T]) -> String {
    let mut s = String::new();
    let mut f = true;
    s.push('[');
    for item in items {
	if !f { s.push(','); }
	f = false;
	s.push_str(&item.to_string());
    }
    s.push(']');
    return s;
}
