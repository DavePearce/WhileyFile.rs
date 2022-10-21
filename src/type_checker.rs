use std::collections::HashMap;
use syntactic_heap::SyntacticHeap;
use syntactic_heap::Ref;

use crate::Error;
use crate::ast::*;

// =================================================================
// Error
// =================================================================

pub type Result<T> = std::result::Result<T, Error>;

// =================================================================
// Typing
// =================================================================

pub type Typing = SyntacticHeap<Types>;

// =================================================================
// Type Checker
// =================================================================

pub type Env = HashMap<String, Type>;

pub struct TypeChecker<'a> {
    globals: Env,
    typing: Typing,
    ast: &'a AbstractSyntaxTree
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a AbstractSyntaxTree) -> Self {
        let globals : Env = HashMap::new();
        let typing = Typing::new();
	TypeChecker{globals,typing,ast}
    }

    pub fn check_all(&mut self) {
        self.check(self.ast.len()-1);
    }

    pub fn check<T:Into<usize>>(&mut self, term: T) -> Result<Type> {
        // Extract node corresponding to term
        let n = self.ast.get(term.into());
        // Dispatch
        match n {
            // Declarations
            Node::TypeDecl(t) => self.check_decl_type(&t),
            Node::FunctionDecl(t) => self.check_decl_function(&t),
            Node::MethodDecl(t) => self.check_decl_method(&t),
            // Statements
            Node::AssertStmt(s) => self.check_stmt_assert(&s),
            Node::AssignStmt(s) => self.check_stmt_assignment(&s),
            Node::AssumeStmt(s) => self.check_stmt_assume(&s),
            Node::BlockStmt(s) => self.check_stmt_block(&s),
            Node::IfElseStmt(s) => self.check_stmt_ifelse(&s),
            Node::ReturnStmt(s) => self.check_stmt_return(&s),
            Node::SkipStmt(s) => self.check_stmt_skip(&s),
            Node::VarDeclStmt(s) => self.check_stmt_vardecl(&s),
            // // Expressions
            // ArrayAccessExpr(expr::ArrayAccess),
            // ArrayGeneratorExpr(expr::ArrayGenerator),
            // ArrayInitialiserExpr(expr::ArrayInitialiser),
            // ArrayLengthExpr(expr::ArrayLength),
            // BinaryExpr(expr::Binary),
            // UnaryExpr(expr::Unary),
            // InvokeExpr(expr::Invoke),
            // IsTypeExpr(expr::IsType),
            // RangeExpr(expr::Range),
            // QuantifierExpr(expr::Quantifier),
            // VarAccessExpr(expr::VarAccess),
            // // Literals
            Node::BoolLiteral(e) => self.check_expr_bool(&e),
            // CharLiteral(expr::CharLiteral),
            Node::IntLiteral(e) => {
                todo!("integer literal");
            }
            // LambdaLiteral(expr::LambdaLiteral),
            // NullLiteral(expr::NullLiteral),
            // StringLiteral(expr::StringLiteral),
            // // Types
            // ArrayType(types::Array),
            // BoolType(types::Bool),
            // FunctionType(types::Function),
            // IntType(types::Int),
            // NominalType(types::Nominal),
            // NullType(types::Null),
            // RecordType(types::Record),
            // ReferenceType(types::Reference),
            // UnionType(types::Union),
            // VoidType(types::Void)
            _ => {
                panic!("GOT HERE");
            }
        }
    }

    // =============================================================================
    // Declarations
    // =============================================================================

    pub fn check_decl_type(&mut self, d: &decl::Type) -> Result<Type> {
        todo!("check_decl_type");
    }

    pub fn check_decl_function(&mut self, d: &decl::Function) -> Result<Type> {
        for p in &d.parameters {
            self.check(p.declared);
        }
        for r in &d.returns {
            self.check(r.declared);
        }
        for c in &d.clauses {
            self.check_clause(c);
        }
        self.check(d.body);
	//
	Ok(self.type_of(types::Void()))
    }

    pub fn check_decl_method(&mut self, d: &decl::Method) -> Result<Type> {
        todo!("check_decl_method");
    }

    // =============================================================================
    // Specification
    // =============================================================================

    pub fn check_clause(&mut self, d: &decl::Clause) -> Result<Type> {
        todo!("check_clause");
    }

    // =============================================================================
    // Statements
    // =============================================================================

    pub fn check_stmt_assert(&mut self, d: &stmt::Assert) -> Result<Type> {
	// Determine type for asserted expression
	self.check(d.0);
	// TODO: sort this out!
	Ok(self.type_of(types::Void()))
    }

    pub fn check_stmt_assignment(&mut self, d: &stmt::Assignment) -> Result<Type> {
        todo!("check_stmt_assign");
    }

    pub fn check_stmt_assume(&mut self, d: &stmt::Assume) -> Result<Type> {
        todo!("check_stmt_assume");
    }

    pub fn check_stmt_block(&mut self, d: &stmt::Block) -> Result<Type> {
	// Check each statement in a row
	for s in &d.0 {
	    self.check(*s)?;
	}
	Ok(self.type_of(types::Void()))
    }

    pub fn check_stmt_ifelse(&mut self, d: &stmt::IfElse) -> Result<Type> {
        todo!("check_stmt_ifelse");
    }

    pub fn check_stmt_return(&mut self, d: &stmt::Return) -> Result<Type> {
        todo!("check_stmt_return");
    }

    pub fn check_stmt_skip(&mut self, d: &stmt::Skip) -> Result<Type> {
	// Do nothing!
	Ok(self.type_of(types::Void()))
    }

    pub fn check_stmt_vardecl(&mut self, d: &stmt::VarDecl) -> Result<Type> {
        todo!("check_stmt_vardecl");
    }

    // =============================================================================
    // Literals
    // =============================================================================

    pub fn check_expr_bool(&mut self, d: &expr::BoolLiteral) -> Result<Type> {
	// Construct boolean type.
	Ok(self.type_of(types::Bool()))
    }

    fn type_of<T:Into<Types>>(&mut self, t: T) -> Type {
	// FIXME: this is where we want to manage the creation of
	// types carefully, such that we don't create any duplicate
	// types.  In particular, ideally, physical equality implies
	// semantics equality.
	//
        // Create new node
        let index = self.typing.push(t.into()).raw_index();
        // Done
        Type(index)
    }

}
