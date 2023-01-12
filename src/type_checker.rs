use std::collections::HashMap;
use syntactic_heap::SyntacticHeap;

use crate::{Error,ErrorCode};
use crate::ast::*;
use crate::lexer::{Span,Token};
use decl::Clause::*;

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
    // globals: Env,
    typing: Typing,
    ast: &'a AbstractSyntaxTree
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a AbstractSyntaxTree) -> Self {
        //let globals : Env = HashMap::new();
        let typing = Typing::new();
	TypeChecker{typing,ast}
    }

    pub fn check_all(&mut self) -> Result<(Env,Type)> {
        let env = Env::new();
        self.check(env, self.ast.len()-1)
    }

    pub fn check<T:Into<usize>>(&mut self, env: Env, term: T) -> Result<(Env,Type)> {
        // Extract node corresponding to term
        let n = self.ast.get(term.into());
        // Dispatch
        match n {
            // Declarations
            Node::TypeDecl(d) => self.check_decl_type(env, &d),
            Node::FunctionDecl(d) => self.check_decl_function(env, &d),
            Node::MethodDecl(d) => self.check_decl_method(env, &d),
            // Statements
            Node::AssertStmt(s) => self.check_stmt_assert(env, &s),
            Node::AssignStmt(s) => self.check_stmt_assignment(env, &s),
            Node::AssumeStmt(s) => self.check_stmt_assume(env, &s),
            Node::BlockStmt(s) => self.check_stmt_block(env, &s),
            Node::IfElseStmt(s) => self.check_stmt_ifelse(env, &s),
            Node::ReturnStmt(s) => self.check_stmt_return(env, &s),
            Node::SkipStmt(s) => self.check_stmt_skip(env, &s),
            Node::VarDeclStmt(s) => self.check_stmt_vardecl(env, &s),
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
            // VarAccessExpr(expr::VarAccess),
            // // Literals
            Node::BoolLiteral(e) => self.check_expr_bool(env, &e),
            // CharLiteral(expr::CharLiteral),
            Node::IntLiteral(_) => {
                todo!("integer literal");
            }
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

    pub fn check_decl_type(&mut self, mut env: Env, d: &decl::Type) -> Result<(Env,Type)> {
        (env,_) = self.check(env, d.pattern.declared)?;
        //
	Ok((env,self.type_of(types::Void())))
    }

    pub fn check_decl_function(&mut self, mut env: Env, d: &decl::Function) -> Result<(Env,Type)> {
        for p in &d.parameters {
            (env,_) = self.check(env,p.declared)?;
        }
        for r in &d.returns {
            (env,_) = self.check(env,r.declared)?;
        }
        for c in &d.clauses {
            (env,_) = self.check_clause(env, c)?;
        }
        (env,_) = self.check(env,d.body)?;
	//
	Ok((env,self.type_of(types::Void())))
    }

    pub fn check_decl_method(&mut self, mut env: Env, d: &decl::Method) -> Result<(Env,Type)> {
        for p in &d.parameters {
            (env,_) = self.check(env,p.declared)?;
        }
        for r in &d.returns {
            (env,_) = self.check(env,r.declared)?;
        }
        for c in &d.clauses {
            (env,_) = self.check_clause(env, c)?;
        }
        (env,_) = self.check(env,d.body)?;
	//
	Ok((env,self.type_of(types::Void())))
    }

    // =============================================================================
    // Specification
    // =============================================================================

    pub fn check_clause(&mut self, env: Env, c: &decl::Clause) -> Result<(Env,Type)> {
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

    pub fn check_stmt_assert(&mut self, env: Env, d: &stmt::Assert) -> Result<(Env,Type)> {
	// Determine type for asserted expression
	let (nenv,t) = self.check(env,d.0)?;
	// Check expression is boolean
	self.check_type(t,types::Bool())?;
	// TODO: sort this out!
	Ok((nenv,self.type_of(types::Void())))
    }

    pub fn check_stmt_assignment(&mut self, env: Env, _d: &stmt::Assignment) -> Result<(Env,Type)> {
        todo!("check_stmt_assign");
    }

    pub fn check_stmt_assume(&mut self, env: Env, d: &stmt::Assume) -> Result<(Env,Type)> {
	// Determine type for assumed expression
	let (nenv,t) = self.check(env,d.0)?;
	// Check expression is boolean
	self.check_type(t,types::Bool())?;
	// TODO: sort this out!
	Ok((nenv,self.type_of(types::Void())))
    }

    pub fn check_stmt_block(&mut self, mut env: Env, d: &stmt::Block) -> Result<(Env,Type)> {
	// Check each statement in a row
	for s in &d.0 {
	    (env,_) = self.check(env,*s)?;
	}
	// Done.
	Ok((env,self.type_of(types::Void())))
    }

    pub fn check_stmt_ifelse(&mut self, env: Env, _d: &stmt::IfElse) -> Result<(Env,Type)> {
        todo!("check_stmt_ifelse");
    }

    pub fn check_stmt_return(&mut self, env: Env, _d: &stmt::Return) -> Result<(Env,Type)> {
        todo!("check_stmt_return");
    }

    pub fn check_stmt_skip(&mut self, env: Env, _d: &stmt::Skip) -> Result<(Env,Type)> {
	// Do nothing!
	Ok((env,self.type_of(types::Void())))
    }

    pub fn check_stmt_vardecl(&mut self, env: Env, _d: &stmt::VarDecl) -> Result<(Env,Type)> {
        todo!("check_stmt_vardecl");
    }

    // =============================================================================
    // Expressions
    // =============================================================================

    pub fn check_expr_binary(&mut self, env: Env, e: &expr::Binary) -> Result<(Env,Type)> {
        // Type check left-hand side
        let (env,lhs) = self.check(env,e.1)?;
        // Type check right-hand side
        let (env,rhs) = self.check(env,e.2)?;
        match e.0 {
            BinOp::LessThan => { todo!("Implement BinOp::LessThan"); }
            BinOp::LessThanOrEquals => {
                let (s,w) = self.check_int_type(lhs)?;
                self.check_type(rhs, types::Int(s,w))?;
                Ok((env,self.type_of(types::Bool())))
            }
            _ => {
                todo!("implement me!");
            }
        }
    }

    pub fn check_expr_bool(&mut self, env: Env, _e: &expr::BoolLiteral) -> Result<(Env,Type)> {
	// Construct boolean type.
	Ok((env,self.type_of(types::Bool())))
    }

    // =============================================================================
    // Types
    // =============================================================================

    pub fn check_type_array(&mut self, env: Env, t: &types::Array) -> Result<(Env,Type)> {
        let (nenv,elem) = self.check(env, t.0)?;
        Ok((nenv,self.type_of(types::Array(elem))))
    }

    pub fn check_type_bool(&mut self, env: Env, _t: &types::Bool) -> Result<(Env,Type)> {
        Ok((env,self.type_of(types::Bool())))
    }

    pub fn check_type_int(&mut self, env: Env, t: &types::Int) -> Result<(Env,Type)> {
        Ok((env,self.type_of(types::Int(t.0,t.1))))
    }

    pub fn check_type_null(&mut self, env: Env, _t: &types::Null) -> Result<(Env,Type)> {
        Ok((env,self.type_of(types::Null())))
    }

    pub fn check_type_record(&mut self, mut env: Env, t: &types::Record) -> Result<(Env,Type)> {
        let mut fields = Vec::new();
        //
        for &(t,n) in &t.0 {
            let ith : Type;
            (env,ith) = self.check(env,t)?;
            // FIXME: there is a major bug here, since n is not
            // allocated within the types heap.
            fields.push((ith,n));
        }
        Ok((env,self.type_of(types::Record(fields))))
    }

    pub fn check_type_reference(&mut self, env: Env, t: &types::Reference) -> Result<(Env,Type)> {
        let (nenv,elem) = self.check(env, t.0)?;
        Ok((nenv,self.type_of(types::Reference(elem))))
    }

    // =============================================================================
    // Helpers
    // =============================================================================

    /// Obtain a type of the given kind.  This may require allocating
    /// such a type on the heap, or it may reuse an existing (and
    /// matching) type.
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

    /// Check one type is an instance of another.
    fn check_type<T:Into<Types>>(&self, t1: Type, t2: T) -> Result<()> {
	// FIXM: this is rather ugly :)
	if *self.typing.get(t1.0) == t2.into() {
	    Ok(())
	} else {
	    // Determine span (somehow)
	    let span = Span::new(Token::Star,0..1);
	    // Return error
	    Err(Error::new(span,ErrorCode::ExpectedType))
	}
    }

    /// Check an arbitrary type is some form of integer type and (if
    /// so) return the details of that.
    fn check_int_type(&self, t1: Type) -> Result<(bool,u8)> {
        let n = self.typing.get(t1.0);
        match n {
            // Match any integer type
            Types::IntType(types::Int(s,w)) => Ok((*s,*w)),
            // Otherwise, fail.
            _ => {
                // Determine span (somehow)
	        let span = Span::new(Token::Star,0..1);
	        // Return error
	        Err(Error::new(span,ErrorCode::ExpectedType))
            }
        }
    }
}
