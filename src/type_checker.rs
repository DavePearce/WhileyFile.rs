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

    pub fn check<T:Into<usize>>(&mut self, term: T) {
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
            // BoolLiteral(expr::BoolLiteral),
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

    pub fn check_decl_type(&mut self, d: &decl::Type) {
        todo!("check_decl_type");
    }

    pub fn check_decl_function(&mut self, d: &decl::Function) {
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
    }

    pub fn check_decl_method(&mut self, d: &decl::Method) {
        todo!("check_decl_method");
    }

    // =============================================================================
    // Specification
    // =============================================================================

    pub fn check_clause(&mut self, d: &decl::Clause) {
        todo!("check_clause");
    }

    // =============================================================================
    // Statements
    // =============================================================================

    pub fn check_stmt_assert(&mut self, d: &stmt::Assert) {
        todo!("check_stmt_assert");
    }

    pub fn check_stmt_assignment(&mut self, d: &stmt::Assignment) {
        todo!("check_stmt_assign");
    }

    pub fn check_stmt_assume(&mut self, d: &stmt::Assume) {
        todo!("check_stmt_assume");
    }

    pub fn check_stmt_block(&mut self, d: &stmt::Block) {
        todo!("check_stmt_block");
    }

    pub fn check_stmt_ifelse(&mut self, d: &stmt::IfElse) {
        todo!("check_stmt_ifelse");
    }

    pub fn check_stmt_return(&mut self, d: &stmt::Return) {
        todo!("check_stmt_return");
    }

    pub fn check_stmt_skip(&mut self, d: &stmt::Skip) {
        todo!("check_stmt_skip");
    }

    pub fn check_stmt_vardecl(&mut self, d: &stmt::VarDecl) {
        todo!("check_stmt_vardecl");
    }
}
