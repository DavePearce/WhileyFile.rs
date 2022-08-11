// Hidden
mod decl;
pub mod expr;
mod stmt;
mod types;

use std::fmt;
use std::convert::From;
use syntactic_heap::SyntacticHeap;
use syntactic_heap::Ref;
// Reexport everything
pub use self::decl::*;
pub use self::expr::{BinOp,Expr};
pub use self::stmt::*;
pub use self::types::*;

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
    ReturnStmt(ReturnStmt),
    SkipStmt(SkipStmt),
    VarDeclStmt(VarDeclStmt),
    // Expressions
    ArrayAccessExpr(expr::ArrayAccessExpr),
    ArrayInitialiserExpr(expr::ArrayInitialiser),
    ArrayLengthExpr(expr::ArrayLengthExpr),
    BoolExpr(expr::Bool),
    BinaryExpr(expr::BinaryExpr),
    IntExpr(expr::IntExpr),
    VarExpr(expr::VarExpr),
    // Types
    ArrayType(ArrayType),
    BoolType(BoolType),
    IntType(IntType),
    NominalType(NominalType),
    NullType(NullType),
    RecordType(RecordType),
    ReferenceType(ReferenceType),
    VoidType(VoidType)
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
// Misc
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct LineComment(pub String);

impl From<LineComment> for Node {
    fn from(s: LineComment) -> Self { Node::LineComment(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct BlockComment(pub String);

impl From<BlockComment> for Node {
    fn from(s: BlockComment) -> Self { Node::BlockComment(s) }
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
