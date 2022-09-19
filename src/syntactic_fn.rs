use std::collections::HashMap;
use syntactic_heap::SyntacticHeap;
use syntactic_heap::Ref;

use crate::Error;
use crate::ast::*;

pub trait SyntacticFn<V> {
    // =============================================================================
    // Declarations
    // =============================================================================

    fn call_decl_type(&self, d: &decl::Type) -> V;
    fn call_decl_function(&self, d: &decl::Function) -> V;
    fn call_decl_method(&self, d: &decl::Method) -> V;

    // =============================================================================
    // Specification
    // =============================================================================

    fn call_clause(&self, d: &decl::Clause) -> V;

    // =============================================================================
    // Statements
    // =============================================================================

    fn call_stmt_assert(&self, d: &stmt::Assert) -> V;
    fn call_stmt_assignment(&self, d: &stmt::Assignment) -> V;
    fn call_stmt_assume(&self, d: &stmt::Assume) -> V;
    fn call_stmt_block(&self, d: &stmt::Block) -> V;
    fn call_stmt_ifelse(&self, d: &stmt::IfElse) -> V;
    fn call_stmt_return(&self, d: &stmt::Return) -> V;
    fn call_stmt_skip(&self, d: &stmt::Skip) -> V;
    fn call_stmt_vardecl(&self, d: &stmt::VarDecl) -> V;
}

// ===========================================================================
// Syntactic Function
// ===========================================================================

pub struct CachingFn<'a,F,V>
// FIXME: usize broken here
where F : SyntacticFn<V> {
    /// Dispatch function.
    func: F,
    /// Cache of intermediate values.
    cache: HashMap<usize, V>,
    /// Syntax tree over which this function is being applied.
    ast: &'a AbstractSyntaxTree
}

impl<'a,F,V> CachingFn<'a,F,V>
where V : Copy, F : SyntacticFn<V> {

    pub fn new(func: F, ast: &'a AbstractSyntaxTree) -> Self {
        let cache = HashMap::new();
	CachingFn{func,cache,ast}
    }

    pub fn call<I:Into<usize>>(&mut self, term: I) -> V {
        // Extract node index.
        let index = term.into();
        // Check cache
        match self.cache.get(&index) {
            Some(v) => *v,
            None => {
                // Extract node corresponding to term
                let n = self.ast.get(index);
                // Dispatch
                let v = self.dispatch(n);
                // Cache result
                self.cache.insert(index,v);
                // Done
                v
            }
        }
    }

    /// Dispatch to the appropriate method depending on the node type.
    fn dispatch(&self, n: &Node) -> V {
        match n {
            // Declarations
            Node::TypeDecl(t) => self.func.call_decl_type(&t),
            Node::FunctionDecl(t) => self.func.call_decl_function(&t),
            Node::MethodDecl(t) => self.func.call_decl_method(&t),
            // Statements
            Node::AssertStmt(s) => self.func.call_stmt_assert(&s),
            Node::AssignStmt(s) => self.func.call_stmt_assignment(&s),
            Node::AssumeStmt(s) => self.func.call_stmt_assume(&s),
            Node::BlockStmt(s) => self.func.call_stmt_block(&s),
            Node::IfElseStmt(s) => self.func.call_stmt_ifelse(&s),
            Node::ReturnStmt(s) => self.func.call_stmt_return(&s),
            Node::SkipStmt(s) => self.func.call_stmt_skip(&s),
            Node::VarDeclStmt(s) => self.func.call_stmt_vardecl(&s),
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
            // Node::IntLiteral(e) => {
            //     todo!("integer literal");
            // }
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
                unreachable!();
            }
        }
    }
}
