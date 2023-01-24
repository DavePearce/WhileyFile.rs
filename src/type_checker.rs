use std::collections::HashMap;
use std::marker::{PhantomData};
use syntactic_heap::SyntacticHeap;

use crate::{Error,ErrorCode};
use crate::ast::*;
use crate::lexer::{Span,Token};
use decl::Clause::*;
use crate::ast::BinOp::*;
use crate::util::{Constraint,TypeConstraints};

// =================================================================
// Error
// =================================================================

pub type Result<T> = std::result::Result<T, Error>;

// =================================================================
// Syntactic Heap Ref
// =================================================================

pub struct AstRef<'a,T> {
    dummy: PhantomData<T>,
    parent: &'a SyntacticHeap<Node>,
    index: usize
}

impl<'a,T> AstRef<'a,T>
where for<'b> &'b T:TryFrom<&'b Node>
{
    pub fn new<S:Into<usize>>(parent: &'a SyntacticHeap<Node>, index: S) -> Self {
        Self{dummy: PhantomData, parent, index: index.into()}
    }

    pub fn as_ref(&self) -> &T {
        // Extract Node
        let n = self.parent.get(self.index);
        // Check whether cast successful or not
        match <&T>::try_from(n) {
            Ok(r) => &r,
            Err(_) => { panic!("invalid conversion"); }
        }
    }
}

// =================================================================
// Typing Environment
// =================================================================

pub struct Env {
    bindings: HashMap<String, Type>,
    typing: TypeConstraints
}

impl Env {
    pub fn new() -> Self {
        let bindings = HashMap::new();
        let typing = TypeConstraints::new();
        Self{bindings,typing}
    }
    pub fn bind(&mut self, name: String, t: Type) {
        todo!("");
    }
}

// =================================================================
// Type Checker
// =================================================================


pub struct TypeChecker<'a> {
    ast: &'a AbstractSyntaxTree
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a AbstractSyntaxTree) -> Self {
        //let globals : Env = HashMap::new();
	TypeChecker{ast}
    }

    pub fn check_all(&mut self) -> Result<Env> {
        let mut env = Env::new();
        env = self.check(env, self.ast.len()-1)?;
        println!("Constraints: {:?}",env.typing);
        Ok(env)
    }

    pub fn check<T:Into<usize>>(&mut self, env: Env, term: T) -> Result<Env> {
        let index = term.into();
        // Extract node corresponding to term
        let n = self.ast.get(index);
        // Dispatch
        match n {
            // Declarations
            Node::TypeDecl(d) => self.check_decl_type(env, &d),
            Node::FunctionDecl(d) => self.check_decl_function(env, &d),
            Node::MethodDecl(d) => self.check_decl_method(env, &d),
            // Statements
            Node::AssertStmt(s) => self.check_stmt_assert(env, AstRef::new(&self.ast,index)),
            // Node::AssignStmt(s) => self.check_stmt_assignment(env, &s),
            //Node::AssumeStmt(s) => self.check_stmt_assume(env, &s),
            Node::BlockStmt(s) => self.check_stmt_block(env, &s),
            // Node::IfElseStmt(s) => self.check_stmt_ifelse(env, &s),
            // Node::ReturnStmt(s) => self.check_stmt_return(env, &s),
            // Node::SkipStmt(s) => self.check_stmt_skip(env, &s),
            // Node::VarDeclStmt(s) => self.check_stmt_vardecl(env, &s),
            // // Expressions
            // Arrayaccessexpr(expr::ArrayAccess),
            // ArrayGeneratorExpr(expr::ArrayGenerator),
            // ArrayInitialiserExpr(expr::ArrayInitialiser),
            // ArrayLengthExpr(expr::ArrayLength),
            Node::BinaryExpr(e) => self.check_expr_binary(env, &e),
            // UnaryExpr(expr::Unary),
            // InvokeExpr(expr::Invoke),
            // IsTypeExpr(expr::IsType),
            // RangeExpr(expr::Range),
            // QuantifierExpr(expr::Quantifier),
            Node::VarAccessExpr(e) => self.check_expr_var(env, &e),
            // // Literals
            Node::BoolLiteral(e) => self.check_expr_bool(env, AstRef::new(&self.ast,index)),
            // CharLiteral(expr::CharLiteral),
            Node::IntLiteral(e) => self.check_expr_int(env, &e),
            // LambdaLiteral(expr::LambdaLiteral),
            // NullLiteral(expr::NullLiteral),
            // StringLiteral(expr::StringLiteral),
            // // Types
            Node::ArrayType(t) => self.check_type_array(env, &t),
            Node::BoolType(t) => self.check_type_bool(env, &t),
            // FunctionType(types::Function),
            Node::IntType(t) => self.check_type_int(env, &t),
            // NominalType(types::Nominal),
            Node::NullType(t) => self.check_type_null(env, &t),
            Node::RecordType(t) => self.check_type_record(env, &t),
            Node::ReferenceType(t) => self.check_type_reference(env, &t),
            // UnionType(types::Union),
            // VoidType(types::Void)
            _ => {
                panic!("Unimplemented AST type ({:?})",n);
            }
        }
    }

    // =============================================================================
    // Declarations
    // =============================================================================

    pub fn check_decl_type(&mut self, mut env: Env, d: &decl::Type) -> Result<Env> {
        env = self.check(env, d.pattern.declared)?;
        //
	Ok(env)
    }

    pub fn check_decl_function(&mut self, mut env: Env, d: &decl::Function) -> Result<Env> {
        env = self.check_decl_parameters(env, &d.parameters)?;
        env = self.check_decl_parameters(env, &d.returns)?;
        env = self.check_clauses(env,&d.clauses)?;
        env = self.check(env,d.body)?;
	//
	Ok(env)
    }

    pub fn check_decl_method(&mut self, mut env: Env, d: &decl::Method) -> Result<Env> {
        env = self.check_decl_parameters(env, &d.parameters)?;
        env = self.check_decl_parameters(env, &d.returns)?;
        env = self.check_clauses(env,&d.clauses)?;
        env = self.check(env,d.body)?;
	//
	Ok(env)
    }

    pub fn check_decl_parameters(&mut self, env: Env, parameters: &[decl::Parameter]) -> Result<Env> {
        let mut t : Type;
        for p in parameters {
            // env = self.check(env,p.declared)?;
            // let name = self.get_name(p.name)?;
            // env.bind(name.clone(),t);

            // Can we get the parser to do the binding for us already?
            todo!("");
        }
        Ok(env)
    }

    // =============================================================================
    // Specification
    // =============================================================================

    pub fn check_clauses(&mut self, mut env: Env, clauses: &[decl::Clause]) -> Result<Env> {
        for c in clauses {
            env = self.check_clause(env, c)?;
        }
        Ok(env)
    }

    pub fn check_clause(&mut self, env: Env, c: &decl::Clause) -> Result<Env> {
        // Extract node corresponding to term
        match c {
            Requires(e) => self.check(env,*e),
            Ensures(e) => self.check(env,*e),
            Where(e) => self.check(env,*e)
        }
    }

    // =============================================================================
    // Statements
    // =============================================================================

    pub fn check_stmt_assert(&mut self, mut env: Env, r: AstRef<stmt::Assert>) -> Result<Env> {
	// Determine type for asserted expression
	env = self.check(env,r.as_ref().0)?;
	// Check expression is boolean
        let t = env.typing.type_of(types::Bool());
	// Construct boolean type constraint.
        env.typing.add(Constraint::UpperBound(t,r.index));
	//
	Ok(env)
    }

    pub fn check_stmt_block(&mut self, mut env: Env, d: &stmt::Block) -> Result<Env> {
	// Check each statement in a row
	for s in &d.0 {
	    env = self.check(env,*s)?;
	}
	// Done.
	Ok(env)
    }

    pub fn check_stmt_skip(&mut self, env: Env, _d: &stmt::Skip) -> Result<Env> {
	// Do nothing!
	Ok(env)
    }

    // =============================================================================
    // Expressions
    // =============================================================================

    pub fn check_expr_binary(&mut self, env: Env, e: &expr::Binary) -> Result<Env> {
        todo!();
        // // Type check left-hand side
        // let (env,lhs) = self.check(env,e.1)?;
        // // Type check right-hand side
        // let (env,rhs) = self.check(env,e.2)?;
        // match e.0 {
        //     LessThan|LessThanOrEquals|GreaterThan|GreaterThanOrEquals => {
        //         let (s,w) = self.check_int_type(lhs)?;
        //         self.check_type(rhs, types::Int(s,w))?;
        //         Ok((env,self.type_of(types::Bool())))
        //     }
        //     _ => {
        //         todo!("implement me!");
        //     }
        // }
    }

    pub fn check_expr_bool(&mut self, mut env: Env, r: AstRef<expr::BoolLiteral>) -> Result<Env> {
        let t = env.typing.type_of(types::Bool());
	// Construct boolean type constraint.
        env.typing.add(Constraint::LowerBound(r.index,t));
        // Done
	Ok(env)
    }

    pub fn check_expr_int(&mut self, env: Env, _e: &expr::IntLiteral) -> Result<Env> {
	// Construct integer type.
        todo!("Add integer constraint");
        // FIXME: this is of course broken
	Ok(env)
    }

    pub fn check_expr_var(&mut self, env: Env, e: &expr::VarAccess) -> Result<Env> {
        // Extract node corresponding to term
        let n = self.get_name(e.0)?;
        // Find it in the environment
        let t = match env.bindings.get(n) {
            Some(t) => *t,
            _ => {
                // Determine span (somehow)
	        let span = Span::new(Token::Star,0..1);
	        // Return error
	        return Err(Error::new(span,ErrorCode::ExpectedType))
            }
        };
        todo!("add variable constraint");
        Ok(env)
    }

    // =============================================================================
    // Types
    // =============================================================================

    pub fn check_type_array(&mut self, env: Env, t: &types::Array) -> Result<Env> {
        self.check(env, t.0)
    }

    pub fn check_type_bool(&mut self, env: Env, _t: &types::Bool) -> Result<Env> {
        Ok(env)
    }

    pub fn check_type_int(&mut self, env: Env, _t: &types::Int) -> Result<Env> {
        Ok(env)
    }

    pub fn check_type_null(&mut self, env: Env, _t: &types::Null) -> Result<Env> {
        Ok(env)
    }

    pub fn check_type_record(&mut self, mut env: Env, t: &types::Record) -> Result<Env> {
        for &(t,n) in &t.0 {
            env = self.check(env,t)?;
        }
        Ok(env)
    }

    pub fn check_type_reference(&mut self, env: Env, t: &types::Reference) -> Result<Env> {
        self.check(env, t.0)
    }

    // =============================================================================
    // Helpers
    // =============================================================================

    fn get_name(&self, n: Name) -> Result<&String> {
        match self.ast.get(n.0) {
            Node::Utf8(s) => Ok(&s),
            node => {
                panic!("invalid node encountered (found {}, expecting Utf8)",node);
            }
        }

    }
}
