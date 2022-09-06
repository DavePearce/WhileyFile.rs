use std::collections::HashMap;
use crate::{Error,ErrorCode,Result};
use crate::lexer::{Lexer,Span,Token};
use crate::ast::*;

/// The parsing environment maps raw strings to on-tree names.
type Env = HashMap<String, Name>;

/// Defines the set of tokens which are considered to identify logical
/// connectives (e.g. `&&`, `||`, etc).
pub const LOGICAL_CONNECTIVES : &'static [Token] = &[
    Token::AmpersandAmpersand,
    Token::BarBar
];

/// Defines the set of tokens which are considered to identify
/// arithmetic comparators (e.g. `<`, `<=`, `==`, etc).
pub const ARITHMETIC_COMPARATORS : &'static [Token] = &[
    Token::EqualsEquals,
    Token::ShreakEquals,
    Token::LeftAngle,
    Token::LeftAngleEquals,
    Token::RightAngle,
    Token::RightAngleEquals
];

/// Defines the set of tokens which are considered to identify
/// arithmetic operators (e.g. `+`, `-`, `*`, etc).
pub const ARITHMETIC_OPERATORS : &'static [Token] = &[
    Token::Minus,
    Token::Percent,
    Token::Plus,
    Token::RightSlash,
    Token::Star
];

pub const BINARY_CONNECTIVES : &'static [ &'static [Token] ] = &[
    &ARITHMETIC_OPERATORS,
    &ARITHMETIC_COMPARATORS,
    &LOGICAL_CONNECTIVES,
];

// =========================================================================
// Parser
// =========================================================================

pub struct Parser<'a> {
    /// Provides access to our token stream.
    lexer: Lexer,
    /// Provides access to the ast
    ast: &'a mut AbstractSyntaxTree,
    /// Provides name cache
    env: Env
}

impl<'a> Parser<'a> {

    pub fn new(input: &str, ast: &'a mut AbstractSyntaxTree) -> Self {
	let env : Env = HashMap::new();
	Self { lexer: Lexer::new(input), ast, env }
    }

    // =========================================================================
    // Statements
    // =========================================================================

    fn parse_stmt(&mut self) -> Result<Stmt> {
        // Skip any leading whitespace
        self.skip_whitespace();
    	// Dispatch on lookahead
    	match self.lexer.peek().kind {
    	    Token::Assert => self.parse_stmt_assert(),
    	    Token::Skip => self.parse_stmt_skip(),
            _ => panic!("GOT HERE")
        }
    }

    pub fn parse_stmt_assert(&mut self) -> Result<Stmt> {
	let tok = self.lexer.snap(Token::Assert)?;
	let expr = self.parse_expr()?;
	let stmt = Stmt::new(self.ast,Node::from(stmt::Assert(expr)));
        self.finalise(stmt,tok)
    }

    pub fn parse_stmt_skip(&mut self) -> Result<Stmt> {
    	// "skip"
    	let tok = self.lexer.snap(Token::Skip)?;
    	// Done
    	let stmt = Stmt::new(self.ast,Node::from(stmt::Skip()));
        self.finalise(stmt,tok)
    }

    // =========================================================================
    // Expressions
    // =========================================================================

    pub fn parse_expr(&mut self) -> Result<Expr> {
        // self.parse_expr_binary(3)
        self.parse_expr_term()
    }

    pub fn parse_expr_term(&mut self) -> Result<Expr> {
        // Skip whitespace
        self.skip_whitespace();
        //
	let lookahead = self.lexer.peek();
	//
        match lookahead.kind {
	    Token::Integer => self.parse_literal_int(),
	    Token::LeftBrace => self.parse_expr_bracketed(),
	    _ => {
		return Err(Error::new(lookahead,ErrorCode::UnexpectedToken));
	    }
	}
    }

    pub fn parse_literal_int(&mut self) -> Result<Expr> {
        let tok = self.lexer.snap(Token::Integer)?;
        // Extract characters making up literal
        let content = self.lexer.get_str(tok);
        // Convert content into a integer
        let val : i32 = content.parse().unwrap();
        let expr = Expr::new(self.ast,Node::from(expr::IntLiteral(val)));
        self.finalise(expr,tok)
    }

    pub fn parse_expr_bracketed(&mut self) -> Result<Expr> {
	self.lexer.snap(Token::LeftBrace)?;
	let expr = self.parse_expr();
	self.lexer.snap(Token::RightBrace)?;
        expr
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /// Finalise a given node after its creation.  Amongst other
    /// things, this registers its source mappiung (i.e. all
    /// characters from a given token upto the current position).
    fn finalise<T:Copy+Into<usize>>(&mut self, node:T, span: Span<Token>) -> Result<T> {
        // end = self.lexer.offset();
        // Extract slice
        // let slice = &self.lexer.input[token.start .. end];
        // // Register the mapping
        // (self.mapper)(node.into(),slice);
        //
        Ok(node)
    }

    fn skip_whitespace(&mut self) {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            Token::Gap|Token::NewLine => {
                self.lexer.snap(lookahead.kind).unwrap();
                self.skip_whitespace()
            }
            _ => {
                // Do nothing!
            }
        }
    }

    fn binop_from_token(token: Token) -> Option<BinOp> {
	let bop = match token {
            // // Equality
            Token::EqualsEquals => BinOp::Equals,
            Token::ShreakEquals => BinOp::NotEquals,
            // // Comparison
	    Token::LeftAngle => BinOp::LessThan,
            Token::LeftAngleEquals => BinOp::LessThanOrEquals,
            Token::RightAngle => BinOp::GreaterThan,
            Token::RightAngleEquals => BinOp::GreaterThanOrEquals,
            // Arithmetic
            Token::Minus => BinOp::Subtract,
	    Token::Percent => BinOp::Remainder,
	    Token::Plus => BinOp::Add,
            Token::RightSlash => BinOp::Divide,
            Token::Star => BinOp::Multiply,
            // // Logical
            Token::AmpersandAmpersand => BinOp::LogicalAnd,
            Token::BarBar => BinOp::LogicalOr,
            // No match
	    _ => { return None; }
	};
        Some(bop)
    }
}
