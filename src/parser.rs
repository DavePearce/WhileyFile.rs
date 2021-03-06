use std::collections::HashMap;
use crate::{Error,ErrorCode,Result};
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::ast::*;

/// The parsing environment maps raw strings to on-tree names.
type Env = HashMap<String, Name>;

// =================================================================
// Parser
// =================================================================

/// Response for turning a stream of tokens into an Abstract Syntax
/// Tree and/or producing error messages along the way.
pub struct Parser<'a, 'b, F>
where F : FnMut(usize,&'a str) {
    /// Provides access to our token stream.
    lexer: Lexer<'a>,
    /// Provides access to the ast
    ast: &'b mut AbstractSyntaxTree,
    /// Provides name cache
    env: Env,
    /// Provides mechanism for source maps
    mapper : F
}

impl<'a,'b,'c, F> Parser<'a,'b,F>
where 'a :'b, 'a:'c, F : FnMut(usize,&'a str) {

    pub fn new(input: &'a str, ast: &'b mut AbstractSyntaxTree, mapper : F) -> Self {
	let env : Env = HashMap::new();
	Self { lexer: Lexer::new(input), ast, env, mapper }
    }

    // =========================================================================
    // Accessors / Mutators
    // =========================================================================

    // =========================================================================
    // Declarations
    // =========================================================================

    /// Parse all declarations
    pub fn parse(&'c mut self) -> Result<'a, ()> {
        let mut decls = Vec::new();
        self.skip_whitespace();
        while !self.lexer.is_eof() {
            decls.push(self.parse_decl()?);
            self.skip_whitespace();
        }
        // Done
        Ok(())
    }

    /// Parse an arbitrary declaration
    pub fn parse_decl(&'c mut self) -> Result<'a,Decl> {
	let lookahead = self.lexer.peek();
	// Attempt to parse declaration
	match lookahead.kind {
	    TokenType::Type => {
		self.parse_decl_type()
	    }
            TokenType::Function => {
                self.parse_decl_function()
            }
	    _ => {
                // Temporary (for now).
                Err(Error::new(lookahead,ErrorCode::UnexpectedEof))
	    }
	}
    }

    pub fn parse_decl_function(&'c mut self) -> Result<'a,Decl> {
	// "function"
	self.snap(TokenType::Function)?;
	let name = self.parse_identifier()?;
	let params = self.parse_decl_parameters()?;
        let returns = if self.lookahead(TokenType::MinusGreater) {
	    self.gap_snap(TokenType::MinusGreater)?;
            self.parse_decl_parameters()?
        } else {
            vec![]
        };
        self.gap_snap(TokenType::Colon)?;
        self.match_line_end()?;
	let body = self.parse_stmt_block(&"")?;
	// Construct node
        let n = Node::from(FunctionDecl::new(name,params,returns,body));
        // Done
        Ok(Decl::new(self.ast,n))
    }

    /// Parse a _property declaration_ in a Whiley source file.  A
    /// simple example to illustrate is:
    ///
    /// ```Whiley
    /// property contains(int[] xs, int x) -> bool
    /// requires x >= 0:
    ///    return some { i in 0..|xs| | xs[i] == x }
    /// ```
    ///
    /// Properties are permitted to have `requires` clauses, but not
    /// `ensures` clauses.  Their body is also constrained to admin
    /// only non-looping statements.
    pub fn parse_decl_property(&'c mut self) -> Result<'a,Decl> {
        todo![];
    }

    /// Parse a type declaration in a Whiley source file.  A simple
    /// example to illustrate is:
    ///
    /// ```Whiley
    /// type nat is (int x) where x >= 0
    /// ```
    ///
    /// Here, we are defining a *constrained type* called `nat` which
    /// represents the set of natural numbers (i.e the non-negative
    /// integers). Type declarations may also have modifiers, such as
    /// `public` and `private`.
    pub fn parse_decl_type(&'c mut self) -> Result<'a,Decl> {
	// "type"
	let start = self.snap(TokenType::Type)?;
	let name = self.parse_identifier()?;
	// "is"
	self.gap_snap(TokenType::Is)?;
	let typ_e = self.parse_type()?;
	// Determine declaration end
	let end = self.lexer.offset();
        self.match_line_end()?;
	// Extract corresponding (sub)slice
	let slice = &self.lexer.input[start.start .. end];
	// Apply source map
	//let attr = (self.mapper)(slice);
	// Done
	Ok(Decl::new(self.ast,Node::from(TypeDecl::new(name,typ_e))))
    }

    /// Parse a list of parameter declarations
    pub fn parse_decl_parameters(&mut self) -> Result<'a,Vec<Parameter>> {
    	let mut params : Vec<Parameter> = vec![];
        // Skip any preceeding gap
        self.skip_gap();
    	// "("
    	self.snap(TokenType::LeftBrace)?;
    	// Keep going until a right brace
    	while self.snap(TokenType::RightBrace).is_err() {
    	    // Check if first time or not
    	    if !params.is_empty() {
    		// Not first time, so match comma
    		self.snap(TokenType::Comma)?;
    	    }
    	    // Type
    	    let f_type = self.parse_type()?;
    	    // Identifier
    	    let f_name = self.parse_identifier()?;
    	    //
    	    params.push(Parameter{declared:f_type,name:f_name});
    	}
    	// Done
    	Ok(params)
    }

    // =========================================================================
    // Statements
    // =========================================================================

    /// Parse a block of one or more statements at a given indentation
    /// level.  All statements in the block must have a strictly
    /// greater indentation (i.e. must be the given indentation +
    /// more).  There must be at least one statement to form a block.
    pub fn parse_stmt_block(&mut self, indent : &'a str) -> Result<'a,Stmt> {
    	let mut stmts : Vec<Stmt> = Vec::new();
        // Determine indentation level for this block
    	let nindent = self.determine_block_indent(indent)?;
    	// Parse remaining statements at same indent
        while self.lexer.peek().content == nindent {
            // Attempt to parse statement
            match self.parse_stmt(nindent)? {
                Some(stmt) => stmts.push(stmt),
                None => {}
            }
        }
        // Done
        Ok(Stmt::new(self.ast,Node::from(BlockStmt(stmts))))
    }

    /// Determine (and check) the indentation level for a new block.
    /// As such, the original indentation must be a prefix of the new
    /// indentation.  Also, we have to manage comments in this
    /// process.
    pub fn determine_block_indent(&mut self, indent : &'a str) -> Result<'a,&'a str> {
        let lookahead = self.lexer.peek();
        match lookahead.kind {
            TokenType::BlockComment|TokenType::LineComment => {
                // Ignore comments!
                self.match_line_end();
                self.determine_block_indent(indent)
            }
            TokenType::Gap => {
                let nindent = lookahead.content;
                // Sanity check indentation
                if !nindent.starts_with(indent) || indent.len() == nindent.len() {
    	            // Parent indent not a strict prefix of current indent.
                    return Err(Error::new(lookahead,ErrorCode::InvalidBlockIndent));
                }
                Ok(nindent)
            }
            _ => {
                // Nothing else is the start of a valid statement.
                Err(Error::new(lookahead,ErrorCode::InvalidBlockIndent))
            }
        }
    }

    /// Parse an arbitrary statement at a given level of indentation.
    /// Observe this is not guaranteed to produce a statement since,
    /// for example, we might just have an empty line (or a line only
    /// with a comment).
    pub fn parse_stmt(&mut self, indent : &'a str) -> Result<'a,Option<Stmt>> {
        // Parse indentation
        let nindent = self.snap(TokenType::Gap)?;
        // Sanity check indentation
        if nindent.content != indent {
    	    // Parent indent not a strict prefix of current indent.
            return Err(Error::new(nindent,ErrorCode::InvalidBlockIndent));
        }
        // Continue
    	let lookahead = self.lexer.peek();
    	//
    	match lookahead.kind {
            // Match whitespace
            TokenType::LineComment | TokenType::BlockComment | TokenType::NewLine => {
                self.match_line_end();
                Ok(None)
            }
            // Match everything else!
    	    _ => Ok(Some(self.parse_unit_stmt()?))
    	}
    }

    /// Parse a unit statement.  This one which does not contain other
    /// statements, and is terminated with a ";".
    pub fn parse_unit_stmt(&mut self) -> Result<'a,Stmt> {
    	let lookahead = self.lexer.peek();
    	//
    	let stmt = match lookahead.kind {
    	    TokenType::Assert => self.parse_stmt_assert(),
    	    TokenType::Skip => self.parse_stmt_skip(),
    	    _ => {
    		return Err(Error::new(lookahead,ErrorCode::UnexpectedToken));
    	    }
    	};
        // Match line end
        self.match_line_end()?;
    	// Done
    	stmt
    }

    pub fn parse_stmt_assert(&mut self) -> Result<'a,Stmt> {
    	// "assert"
    	self.snap(TokenType::Assert)?;
    	// Expr
    	let expr = self.parse_expr()?;
    	// Done
    	Ok(Stmt::new(self.ast,Node::from(AssertStmt(expr))))
    }

    pub fn parse_stmt_skip(&mut self) -> Result<'a,Stmt> {
    	// "skip"
    	self.snap(TokenType::Skip)?;
    	// Done
    	Ok(Stmt::new(self.ast,Node::from(SkipStmt())))
    }

    // =========================================================================
    // Expressions
    // =========================================================================

    pub fn parse_expr(&mut self) -> Result<'a,Expr> {
        // Parse term
    	let lhs = self.parse_expr_term()?;
        // Skip remaining whitespace (on this line)
        self.skip_linespace();
	// Check for binary expression
    	let lookahead = self.lexer.peek();
	//
	match lookahead.kind {
	    TokenType::LeftAngle => {
		self.lexer.next();
		let rhs = self.parse_expr_term()?;
		Ok(Expr::new(self.ast,Node::from(LessThanExpr(lhs,rhs))))
	    }
	    _ => {
		Ok(lhs)
	    }
	}
    }

    pub fn parse_expr_term(&mut self) -> Result<'a,Expr> {
        // Skip whitespace
        self.skip_whitespace();
        //
    	let lookahead = self.lexer.peek();
    	//
    	let expr = match lookahead.kind {
    	    TokenType::False => {
    		self.lexer.next();
    		Expr::new(self.ast,Node::from(BoolExpr(false)))
    	    }
	    TokenType::Identifier => {
		let n = self.parse_identifier();
		Expr::new(self.ast,Node::from(VarExpr(n.unwrap())))
	    }
    	    TokenType::Integer => {
    	    	self.lexer.next();
		Expr::new(self.ast,Node::from(IntExpr(lookahead.as_int())))
    	    }
    	    TokenType::LeftBrace => {
    	    	return self.parse_expr_bracketed()
    	    }
    	    TokenType::True => {
    		self.lexer.next();
    		Expr::new(self.ast,Node::from(BoolExpr(true)))
    	    }
    	    _ => {
    		return Err(Error::new(lookahead,ErrorCode::UnexpectedToken));
    	    }
    	};
    	//
    	Ok(expr)
    }

    pub fn parse_expr_bracketed(&mut self) -> Result<'a,Expr> {
    	// "("
    	self.snap(TokenType::LeftBrace)?;
    	// Expr
    	let expr = self.parse_expr();
    	// ")"
    	self.snap(TokenType::RightBrace)?;
    	//
    	expr
    }

    // =========================================================================
    // Types
    // =========================================================================

    pub fn parse_type(&mut self) -> Result<'a,Type> {
        self.skip_gap();
	self.parse_type_compound()
    }

    pub fn parse_type_compound(&mut self) -> Result<'a,Type> {
	let lookahead = self.lexer.peek();
	// Attemp to distinguish
	match lookahead.kind {
	    TokenType::EOF => {
		// Something went wrong
		Err(Error::new(lookahead,ErrorCode::UnexpectedEof))
	    }
	    TokenType::Ampersand => {
	    	// Looks like a reference type
	    	self.parse_type_ref()
	    }
	    TokenType::LeftCurly => {
	    	// Looks like a record type
	    	self.parse_type_record()
	    }
	    _ => {
	    	// Could be an array type
	    	self.parse_type_array()
	    }
	}
    }

    /// Parse a reference type, such as `&i32`, `&(i32[])`, `&&u16`,
    /// etc.
    pub fn parse_type_ref(&mut self) -> Result<'a,Type> {
    	let mut n = 1;
    	// "&"
    	self.snap(TokenType::Ampersand)?;
    	// Check for nested references
    	while self.snap(TokenType::Ampersand).is_ok() {
    	    n = n + 1;
    	}
    	// Type
    	let mut t = self.parse_type_bracketed()?;
    	// Unwind references
    	for _i in 0..n {
            t = Type::new(self.ast,Node::from(ReferenceType(t)));
    	}
    	// Done
    	Ok(t)
    }

    /// Parse a record type, such as `{ i32 f }`, `{ bool f, u64 f }`,
    /// `{ &bool f, u64[] f }`, etc.
    pub fn parse_type_record(&mut self) -> Result<'a,Type> {
    	let mut fields : Vec<(Type,Name)> = vec![];
    	// "{"
    	self.snap(TokenType::LeftCurly)?;
    	// Keep going until a right brace
    	while self.snap(TokenType::RightCurly).is_err() {
    	    // Check if first time or not
    	    if !fields.is_empty() {
    		// Not first time, so match comma
    		self.snap(TokenType::Comma)?;
    	    }
    	    // Type
    	    let f_type = self.parse_type()?;
    	    // Identifier
    	    let f_name = self.parse_identifier()?;
    	    //
    	    fields.push((f_type,f_name));
    	}
    	// Done
    	Ok(Type::new(self.ast,Node::from(RecordType(fields))))
    }

    /// Parse an array type, such as `i32[]`, `bool[][]`, etc.
    pub fn parse_type_array(&'c mut self) -> Result<'a,Type> {
    	// Type
    	let mut t = self.parse_type_bracketed()?;
    	// ([])*
    	while self.snap(TokenType::LeftSquare).is_ok() {
    	    self.snap(TokenType::RightSquare)?;
            t = Type::new(self.ast,Node::from(ArrayType(t)));
    	}
    	//
    	Ok(t)
    }

    /// Parse a type which may (or may not) be bracketed.  For
    /// example, in `(&int)[]` the type `&int` is bracketed.
    pub fn parse_type_bracketed(&'c mut self) -> Result<'a,Type> {
    	// Try and match bracket!
    	if self.snap(TokenType::LeftBrace).is_ok() {
    	    // Bingo!
    	    let typ_e = self.parse_type()?;
    	    // Must match closing brace
    	    self.snap(TokenType::RightBrace)?;
    	    // Done
    	    Ok(typ_e)
    	} else {
    	    self.parse_type_base()
    	}
    }

    pub fn parse_type_base(&'c mut self) -> Result<'a,Type> {
	let lookahead = self.lexer.peek();
	// Look at what we've got!
	let typ_e : Type = match lookahead.kind {
	    TokenType::Null => {
		Type::new(self.ast,Node::from(NullType()))
	    }
	    //
	    TokenType::Bool => {
                Type::new(self.ast,Node::from(BoolType()))
	    }
	    //
	    TokenType::I8 => {
                Type::new(self.ast,Node::from(IntType(true,8)))
	    }
	    TokenType::I16 => {
                Type::new(self.ast,Node::from(IntType(true,16)))
	    }
	    TokenType::I32 => {
                Type::new(self.ast,Node::from(IntType(true,32)))
	    }
	    TokenType::I64 => {
                Type::new(self.ast,Node::from(IntType(true,64)))
	    }
	    //
	    TokenType::U8 => {
                Type::new(self.ast,Node::from(IntType(false,8)))
	    }
	    TokenType::U16 => {
                Type::new(self.ast,Node::from(IntType(false,16)))
	    }
	    TokenType::U32 => {
                Type::new(self.ast,Node::from(IntType(false,32)))
	    }
	    TokenType::U64 => {
                Type::new(self.ast,Node::from(IntType(false,64)))
	    }
	    //
	    TokenType::Void => {
                Type::new(self.ast,Node::from(VoidType()))
	    }
	    _ => {
    		return Err(Error::new(lookahead, ErrorCode::UnexpectedToken));
	    }
	};
	// Move over it
	self.lexer.next();
	//
	Ok(typ_e)
    }

    // =========================================================================
    // Misc
    // =========================================================================

    pub fn parse_identifier(&mut self) -> Result<'a,Name> {
        self.skip_gap();
	let tok = self.snap(TokenType::Identifier)?;
	// FIXME: should employ cache!
	Ok(Name::new(self.ast,&tok.content))
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    // fn source_attr(&self, first : Token<'a>, last: Token<'a>) -> Attributes {
    // 	let start = first.start;
    // 	let end = last.end();
    // 	Attributes{start,end}
    // }

    /// If the next token is a gap, just skip over it.
    fn skip_gap(&mut self) {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            TokenType::Gap => {
                self.snap(TokenType::Gap).unwrap();
            }
            _ => {
                // Do nothing
            }
        }
    }

    /// Match various forms of whitespace which can occur on a single
    /// line, including gaps and comments (but not newlines).
    fn skip_linespace(&mut self) -> Result<'a,()> {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            TokenType::EOF => {
                Ok(())
            }
            TokenType::Gap => {
                self.snap(lookahead.kind)?;
                self.skip_linespace()
            }
            TokenType::LineComment => {
                let tok = self.snap(lookahead.kind)?;
                let comment = tok.content.to_string();
                self.ast.push(Node::from(LineComment(comment)));
                self.skip_linespace()
            }
            TokenType::BlockComment => {
                let tok = self.snap(lookahead.kind)?;
                let comment = tok.content.to_string();
                self.ast.push(Node::from(BlockComment(comment)));
                self.skip_linespace()
            }
            _ => {
                // Do nothing!
                Ok(())
            }
        }
    }

    /// Match various forms of whitespace, including gaps, newlines
    /// and comments.
    fn skip_whitespace(&mut self) -> Result<'a,()> {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            TokenType::EOF => {
                Ok(())
            }
            TokenType::Gap => {
                self.snap(lookahead.kind)?;
                self.skip_whitespace()
            }
            TokenType::NewLine => {
                self.snap(lookahead.kind)?;
                self.skip_whitespace()
            }
            TokenType::LineComment => {
                let tok = self.snap(lookahead.kind)?;
                let comment = tok.content.to_string();
                self.ast.push(Node::from(LineComment(comment)));
                self.skip_whitespace()
            }
            TokenType::BlockComment => {
                let tok = self.snap(lookahead.kind)?;
                let comment = tok.content.to_string();
                self.ast.push(Node::from(BlockComment(comment)));
                self.skip_whitespace()
            }
            _ => {
                // Do nothing!
                Ok(())
            }
        }
    }

    /// Match the end of a line which is used, for example, to signal
    /// the end of the current statement.
    fn match_line_end(&mut self) -> Result<'a,()> {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            TokenType::EOF => {
                Ok(())
            }
            TokenType::Gap => {
                self.snap(lookahead.kind)?;
                self.match_line_end()
            }
            TokenType::NewLine => {
                self.snap(lookahead.kind)?;
                Ok(())
            }
            TokenType::LineComment => {
                let tok = self.snap(lookahead.kind)?;
                let comment = tok.content.to_string();
                self.ast.push(Node::from(LineComment(comment)));
                self.match_line_end()
            }
            TokenType::BlockComment => {
                let tok = self.snap(lookahead.kind)?;
                let comment = tok.content.to_string();
                self.ast.push(Node::from(BlockComment(comment)));
                self.match_line_end()
            }
            _ => {
	        // Reject
	        Err(Error::new(lookahead,ErrorCode::ExpectedLineEnd))
            }
        }
    }

    /// Check whether a specific token is next (ignoring gaps)
    fn lookahead(&mut self, kind : TokenType) -> bool {
        self.skip_gap();
        let lookahead = self.lexer.peek();
        return lookahead.kind == kind;
    }

    /// Match a given token type in the current stream, allowing for
    /// an optional gap beforehand.
    fn gap_snap(&mut self, kind : TokenType) -> Result<'a,Token<'a>> {
        self.skip_gap();
        self.snap(kind)
    }

    /// Match a given token type in the current stream without consuming it.
    fn matches(&mut self, kind : TokenType) -> Result<'a,Token<'a>> {
        // Peek at the next token
	let lookahead = self.lexer.peek();
	// Check it!
	if lookahead.kind == kind {
	    Ok(lookahead)
	} else {
	    // Reject
	    Err(Error::new(lookahead,ErrorCode::ExpectedToken(kind)))
	}
    }

    /// Match a given token type in the current stream.  If the kind
    /// matches, then the token stream advances.  Otherwise, it
    /// remains at the same position and an error is returned.
    fn snap(&mut self, kind : TokenType) -> Result<'a,Token<'a>> {
	// Peek at the next token
	let lookahead = self.lexer.peek();
	// Check it!
	if lookahead.kind == kind {
	    // Accept it
	    self.lexer.next();
	    //
	    Ok(lookahead)
	} else {
	    // Reject
	    Err(Error::new(lookahead,ErrorCode::ExpectedToken(kind)))
	}
    }
}
