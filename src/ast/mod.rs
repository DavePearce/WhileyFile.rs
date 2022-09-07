// Hidden
pub mod decl;
pub mod expr;
pub mod stmt;
pub mod types;
pub mod comment;

use std::fmt;
use std::convert::From;
use syntactic_heap::SyntacticHeap;
use syntactic_heap::Ref;
// Reexport everything
pub use self::decl::{Decl};
pub use self::expr::{BinOp,Expr};
pub use self::stmt::{Stmt};
pub use self::types::{Type};
pub use self::comment::*;

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
    LineComment(comment::Line),
    BlockComment(comment::Block),
    // Declarations
    TypeDecl(decl::Type),
    FunctionDecl(decl::Function),
    MethodDecl(decl::Method),
    // Statements
    AssertStmt(stmt::Assert),
    AssumeStmt(stmt::Assume),
    BlockStmt(stmt::Block),
    ReturnStmt(stmt::Return),
    SkipStmt(stmt::Skip),
    VarDeclStmt(stmt::VarDecl),
    // Expressions
    ArrayAccessExpr(expr::ArrayAccess),
    ArrayInitialiserExpr(expr::ArrayInitialiser),
    ArrayLengthExpr(expr::ArrayLength),
    BinaryExpr(expr::Binary),
    InvokeExpr(expr::Invoke),
    VarAccessExpr(expr::VarAccess),
    // Literals
    BoolLiteral(expr::BoolLiteral),
    CharLiteral(expr::CharLiteral),
    IntLiteral(expr::IntLiteral),
    StringLiteral(expr::StringLiteral),
    // Types
    ArrayType(types::Array),
    BoolType(types::Bool),
    IntType(types::Int),
    NominalType(types::Nominal),
    NullType(types::Null),
    RecordType(types::Record),
    ReferenceType(types::Reference),
    VoidType(types::Void)
}

// =============================================================================
// Names
// =============================================================================

#[derive(Clone,Copy,Debug,Hash,PartialEq,Eq)]
pub struct Name(pub usize);

impl Name {
    pub fn new(ast: &mut AbstractSyntaxTree, s : String) -> Self {
	let node = Node::Utf8(s);
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
