use std::collections::HashMap;
use crate::Error;
use crate::ast::*;

// =================================================================
// Error
// =================================================================

pub type Result<T> = std::result::Result<T, Error>;

// =================================================================
// Type Checker
// =================================================================

pub type Env = HashMap<String, Type>;

pub struct TypeChecker<'a> {
    globals: Env,
    ast: &'a mut AbstractSyntaxTree
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a mut AbstractSyntaxTree) -> Self {
        let globals : Env = HashMap::new();
	TypeChecker{globals,ast}
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
            // BlockStmt(stmt::Block),
            // IfElseStmt(stmt::IfElse),
            // ReturnStmt(stmt::Return),
            // SkipStmt(stmt::Skip),
            // VarDeclStmt(stmt::VarDecl),
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

    pub fn check_decl_type(&self, d: &decl::Type) {
        todo!("check_decl_type");
    }

    pub fn check_decl_function(&self, d: &decl::Function) {
        todo!("check_decl_function");
    }

    pub fn check_decl_method(&self, d: &decl::Method) {
        todo!("check_decl_method");
    }

    // =============================================================================
    // Statements
    // =============================================================================

    pub fn check_stmt_assert(&self, d: &stmt::Assert) {
        todo!("check_stmt_assert");
    }

    pub fn check_stmt_assignment(&self, d: &stmt::Assignment) {
        todo!("check_stmt_assign");
    }

    pub fn check_stmt_assume(&self, d: &stmt::Assume) {
        todo!("check_stmt_assume");
    }
}
