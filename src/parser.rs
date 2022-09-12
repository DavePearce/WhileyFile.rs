use std::collections::HashMap;
use crate::{Error,ErrorCode,Result};
use crate::lexer::{Lexer,Region,Span,Token};
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

/// Define the "empty ident" which is the root of all indentation
/// within a file.
const EMPTY_INDENT : Span<Token> = Span{kind:Token::Gap, region: Region{start:0,end:0}};

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

    /// Parse all declarations
    pub fn parse(&mut self) -> Result< ()> {
        let mut decls = Vec::new();
        self.skip_whitespace()?;
        while !self.lexer.is_eof() {
            decls.push(self.parse_decl()?);
            self.skip_whitespace()?;
        }
        // Done
        Ok(())
    }

    // =========================================================================
    // Declarations
    // =========================================================================

    /// Parse an arbitrary declaration
    pub fn parse_decl(&mut self) -> Result<Decl> {
        // Parse any modifiers
        let modifiers = self.parse_decl_modifiers()?;
        // Now see what we have.
	let lookahead = self.lexer.peek();
	// Attempt to parse declaration
	match lookahead.kind {
	    Token::Type => {
	        self.parse_decl_type(modifiers)
	    }
            Token::Function => {
                self.parse_decl_function(modifiers)
            }
            Token::Method => {
                self.parse_decl_method(modifiers)
            }
            // Token::Property => {
            //     self.parse_decl_property(modifiers)
            // }
	    _ => {
                // Temporary (for now).
                Err(Error::new(lookahead,ErrorCode::UnexpectedEof))
	    }
	}
    }

    pub fn parse_decl_function(&mut self, modifiers: Vec<decl::Modifier>) -> Result<Decl> {
        // "function"
        self.lexer.snap(Token::Function)?;
        let (name,params,returns,clauses) = self.parse_signature()?;
        self.gap_snap(Token::Colon)?;
        self.match_line_end()?;
        let body = self.parse_stmt_block(EMPTY_INDENT)?;
        // Construct node
        let n = Node::from(decl::Function::new(modifiers,name,params,returns,clauses,body));
        // Done
        Ok(Decl::new(self.ast,n))
    }

    pub fn parse_decl_method(&mut self, modifiers: Vec<decl::Modifier>) -> Result<Decl> {
        // "function"
        self.lexer.snap(Token::Method)?;
        let (name,params,returns,clauses) = self.parse_signature()?;
        self.gap_snap(Token::Colon)?;
        self.match_line_end()?;
        let body = self.parse_stmt_block(EMPTY_INDENT)?;
        // Construct node
        let n = Node::from(decl::Method::new(modifiers,name,params,returns,clauses,body));
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
    pub fn parse_decl_property(&mut self, modifiers: Vec<decl::Modifier>) -> Result<Decl> {
        todo![];
    }

    /// Parse the signature of a function, method or property.  Since
    /// this is common to all three, we abstract it here.
    pub fn parse_signature(&mut self) ->
        Result<(Name,Vec<decl::Parameter>,Vec<decl::Parameter>,Vec<decl::Clause>)> {
            let name = self.parse_identifier()?;
            let params = self.parse_decl_parameters()?;
            let returns = if self.gap_snap(Token::MinusGreater).is_ok() {
                self.parse_decl_returns()?
            } else {
                vec![]
            };
            let clauses = self.parse_spec_clauses()?;
            // Done
            Ok((name,params,returns,clauses))
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
    pub fn parse_decl_type(&mut self,  modifiers: Vec<decl::Modifier>) -> Result<Decl> {
        // "type"
        let start = self.lexer.snap(Token::Type)?;
        let name = self.parse_identifier()?;
        // "is"
        self.gap_snap(Token::Is)?;
        let typ_e = self.parse_type()?;
        // Determine declaration end
        self.match_line_end()?;
        // Done
        Ok(Decl::new(self.ast,Node::from(decl::Type::new(modifiers,name,typ_e))))
    }

    /// Parse a list of parameter declarations
    pub fn parse_decl_parameters(&mut self) -> Result<Vec<decl::Parameter>> {
	let mut params : Vec<decl::Parameter> = vec![];
        // Skip any preceeding gap
        self.skip_gap();
	// "("
	self.lexer.snap(Token::LeftBrace)?;
	// Keep going until a right brace
	while self.lexer.snap(Token::RightBrace).is_err() {
	    // Check if first time or not
	    if !params.is_empty() {
		// Not first time, so match comma
		self.lexer.snap(Token::Comma)?;
	    }
	    // Type
	    let f_type = self.parse_type()?;
	    // Identifier
	    let f_name = self.parse_identifier()?;
	    //
	    params.push(decl::Parameter{declared:f_type,name:f_name});
	}
	// Done
	Ok(params)
    }

    /// Parse a list of return declarations.  These are essentially
    /// identical to parameters except, at the moment, they can be
    /// "anonymous".
    pub fn parse_decl_returns(&mut self) -> Result<Vec<decl::Parameter>> {
        // Skip any preceeding gap
        self.skip_gap();
        //
        if self.lexer.matches(Token::LeftBrace).is_ok() {
            self.parse_decl_parameters()
        } else {
	    let mut params : Vec<decl::Parameter> = vec![];
            // Type
	    let f_type = self.parse_type()?;
	    // Anonymous identifier
	    let f_name = Name::new(self.ast,"$".to_string());
	    //
	    params.push(decl::Parameter{declared:f_type,name:f_name});
            // Done
            Ok(params)
        }
    }

    /// Parse modifiers for a given declaration, such as `public`,
    /// `private` or `export`.
    pub fn parse_decl_modifiers(&mut self) -> Result<Vec<decl::Modifier>> {
        let mut mods = vec![];
        //
        loop {
            // Skip any preceeding gap.
            self.skip_gap();
	    let lookahead = self.lexer.peek();
	    // Attempt to parse declaration
	    match lookahead.kind {
                Token::Export => {
                    self.lexer.snap(Token::Export)?;
                    mods.push(decl::Modifier::Export);
                }
                Token::Final => {
                    self.lexer.snap(Token::Final)?;
                    mods.push(decl::Modifier::Final);
                }
                Token::Native => {
                    self.lexer.snap(Token::Native)?;
                    mods.push(decl::Modifier::Native);
                }
                Token::Private => {
                    self.lexer.snap(Token::Private)?;
                    mods.push(decl::Modifier::Private);
                }
                Token::Public => {
                    self.lexer.snap(Token::Public)?;
                    mods.push(decl::Modifier::Public);
                }
                _ => {
                    return Ok(mods);
                }
            }
        }
    }

    // =========================================================================
    // Specification clauses
    // =========================================================================

    /// Parse zero or more _specification clauses_, such as
    /// `requires`, `ensures`, `modifies`, etc.  There is no specific
    /// order in which these must be given (though there are norms).
    pub fn parse_spec_clauses(&mut self) -> Result<Vec<decl::Clause>> {
        let mut clauses = Vec::new();
        // Skip any preceeding gap
        self.skip_whitespace()?;
        // Keep going until we meet a colon
        while self.lexer.peek().kind != Token::Colon {
            clauses.push(self.parse_spec_clause()?);
            // Skip any preceeding gap
            self.skip_whitespace()?;
        }
        // Done
        Ok(clauses)
    }

    /// Parse a single specification clause (e.g. `requires x > 0`,
    /// etc), otherwise produce an error.
    pub fn parse_spec_clause(&mut self)  -> Result<decl::Clause> {
        // Decide what type of clause (if any) we have
	let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            Token::Requires => {
                self.lexer.snap(Token::Requires)?;
	        Ok(decl::Clause::Requires(self.parse_expr()?))
            }
            Token::Ensures => {
                self.lexer.snap(Token::Ensures)?;
	        Ok(decl::Clause::Ensures(self.parse_expr()?))
            }
            _ => {
                // Nothing else is the start of a valid statement.
                Err(Error::new(lookahead,ErrorCode::InvalidSpecClause))
            }
        }
    }

    // =========================================================================
    // Statements
    // =========================================================================

    /// Parse a block of one or more statements at a given indentation
    /// level.  All statements in the block must have a strictly
    /// greater indentation (i.e. must be the given indentation +
    /// more).  There must be at least one statement to form a block.
    pub fn parse_stmt_block(&mut self, indent : Span<Token>) -> Result<Stmt> {
    	let mut stmts : Vec<Stmt> = Vec::new();
        // Determine indentation level for this block
    	let nindent = self.determine_block_indent(indent)?;
    	// Parse remaining statements at same indent
        while self.chars_match(self.lexer.peek(),nindent) {
            // Attempt to parse statement
            match self.parse_stmt(nindent)? {
                Some(stmt) => stmts.push(stmt),
                None => {}
            }
        }
        // Done
        Ok(Stmt::new(self.ast,Node::from(stmt::Block(stmts))))
    }

    /// Determine (and check) the indentation level for a new block.
    /// As such, the original indentation must be a prefix of the new
    /// indentation.  Also, we have to manage comments in this
    /// process.
    pub fn determine_block_indent(&mut self, indent : Span<Token>) -> Result<Span<Token>> {
        let lookahead = self.lexer.peek();
        match lookahead.kind {
            Token::BlockComment|Token::LineComment => {
                // Ignore comments!
                self.match_line_end();
                self.determine_block_indent(indent)
            }
            Token::Gap => {
                let indent_chars = self.lexer.get(indent);
                let nindent = self.lexer.get(lookahead);
                // Sanity check indentation
                if !nindent.starts_with(indent_chars) || indent_chars.len() == nindent.len() {
    	            // Parent indent not a strict prefix of current indent.
                    return Err(Error::new(lookahead,ErrorCode::InvalidBlockIndent));
                }
                Ok(lookahead)
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
    pub fn parse_stmt(&mut self, indent : Span<Token>) -> Result<Option<Stmt>> {
        // Parse indentation
        let nindent = self.lexer.snap(Token::Gap)?;
        // Sanity check indentation
        if !self.chars_match(nindent,indent) {
    	    // Parent indent not a strict prefix of current indent.
            return Err(Error::new(nindent,ErrorCode::InvalidBlockIndent));
        }
        // Continue
    	let lookahead = self.lexer.peek();
    	//
    	match lookahead.kind {
            // Match whitespace
            Token::LineComment | Token::BlockComment | Token::NewLine => {
                self.match_line_end();
                Ok(None)
            }
            // Match everything else!
    	    _ => Ok(Some(self.parse_unit_stmt()?))
    	}
    }
    fn parse_unit_stmt(&mut self) -> Result<Stmt> {
        // Skip any leading whitespace
        self.skip_whitespace()?;
    	// Dispatch on lookahead
    	let stmt = match self.lexer.peek().kind {
    	    Token::Assert => self.parse_stmt_assert(),
    	    Token::Assume => self.parse_stmt_assume(),
    	    Token::Return => self.parse_stmt_return(),
    	    Token::Skip => self.parse_stmt_skip(),
            _ => self.parse_stmt_vardecl()
        };
        // Match line end
        self.match_line_end()?;
        // Done
        stmt
    }

    pub fn parse_stmt_assert(&mut self) -> Result<Stmt> {
	let tok = self.lexer.snap(Token::Assert)?;
	let expr = self.parse_expr()?;
	let stmt = Stmt::new(self.ast,Node::from(stmt::Assert(expr)));
        self.finalise(stmt,tok)
    }

    pub fn parse_stmt_assume(&mut self) -> Result<Stmt> {
	let tok = self.lexer.snap(Token::Assume)?;
	let expr = self.parse_expr()?;
	let stmt = Stmt::new(self.ast,Node::from(stmt::Assume(expr)));
        self.finalise(stmt,tok)
    }

    pub fn parse_stmt_return(&mut self) -> Result<Stmt> {
    	// "return"
    	let tok = self.lexer.snap(Token::Return)?;
    	//
        self.skip_linespace();
        // See whether an expression follows
        let stmt = match self.lexer.snap(Token::NewLine) {
            Ok(_) => Node::from(stmt::Return(Option::None)),
            Err(_) => {
                let expr = self.parse_expr()?;
                Node::from(stmt::Return(Option::Some(expr)))
            }
        };
        // Done
    	let stmt = Stmt::new(self.ast,stmt);
        self.finalise(stmt,tok)
    }

    pub fn parse_stmt_skip(&mut self) -> Result<Stmt> {
    	// "skip"
    	let tok = self.lexer.snap(Token::Skip)?;
    	// Done
    	let stmt = Stmt::new(self.ast,Node::from(stmt::Skip()));
        self.finalise(stmt,tok)
    }

    pub fn parse_stmt_vardecl(&mut self) -> Result<Stmt> {
    	// type
        let vtype = self.parse_type()?;
        // name
        let name = self.parse_identifier()?;
        // Skip over any linespace
        self.skip_linespace();
    	// Initialiser (Optional)
    	let expr = match self.lexer.snap(Token::Equals) {
            Ok(_) => {
                Option::Some(self.parse_expr()?)
            },
            Err(_) => Option::None
        };
    	// Done
    	Ok(Stmt::new(self.ast,Node::from(stmt::VarDecl(vtype,name,expr))))
    }

    // =========================================================================
    // Expressions
    // =========================================================================

    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_expr_binary(3)
    }

    /// Parse a binary expression at a given _level_.  Higher levels
    /// indicate expressions which bind _less tightly_.  Furthermore,
    /// level `0` corresponds simply to parsing a unary expression.
    pub fn parse_expr_binary(&mut self, level: usize) -> Result<Expr> {
        if level == 0 {
            self.parse_expr_term()
        } else {
            let tokens = BINARY_CONNECTIVES[level-1];
            // Parse level below
    	    let lhs = self.parse_expr_binary(level-1)?;
            // Skip remaining whitespace (on this line)
            self.skip_linespace();
	    // Check whether logical connective follows
    	    let lookahead = self.lexer.snap_any(tokens);
            //
            match lookahead {
                Ok(t) => {
                    // FIXME: turn this into a loop!
	            let rhs = self.parse_expr_binary(level-1)?;
                    // NOTE: following is safe because can only match
                    // tokens which will be accepted.
                    let bop = Self::binop_from_token(t.kind).unwrap();
	            let node = Node::from(expr::Binary(bop,lhs,rhs));
	            Ok(Expr::new(self.ast,node))
                }
                Err(_) => {
                    Ok(lhs)
                }
            }
        }
    }

    pub fn parse_expr_term(&mut self) -> Result<Expr> {
        // Skip whitespace
        self.skip_whitespace()?;
        //
	let lookahead = self.lexer.peek();
	//
        match lookahead.kind {
    	    // Token::Bar => self.parse_expr_arraylength()?,
    	    Token::Character => self.parse_literal_char(),
    	    Token::False => self.parse_literal_bool(),
	    Token::Identifier => self.parse_expr_varaccess(),
    	    Token::Integer => self.parse_literal_int(),
    	    Token::LeftBrace => self.parse_expr_bracketed(),
            // Token::LeftSquare => self.parse_expr_arrayinitialiser()?,
    	    Token::True => self.parse_literal_bool(),
            Token::String => self.parse_literal_string(),
	    _ => {
		return Err(Error::new(lookahead,ErrorCode::UnexpectedToken));
	    }
	}
    }

    pub fn parse_expr_varaccess(&mut self) -> Result<Expr> {
        let tok = self.lexer.peek();
	let n = self.parse_identifier();
	let expr = Expr::new(self.ast,Node::from(expr::VarAccess(n.unwrap())));
        self.finalise(expr,tok)
    }

    pub fn parse_expr_bracketed(&mut self) -> Result<Expr> {
	self.lexer.snap(Token::LeftBrace)?;
	let expr = self.parse_expr();
	self.lexer.snap(Token::RightBrace)?;
        expr
    }

    // =========================================================================
    // Literals
    // =========================================================================

    /// Parse a _boolean literal_ (i.e. `true` or `false`).
    pub fn parse_literal_bool(&mut self) -> Result<Expr> {
        let tok = self.lexer.snap_any(&[Token::True,Token::False])?;
        let expr = match tok.kind {
            Token::False => Expr::new(self.ast,Node::from(expr::BoolLiteral(false))),
            Token::True => Expr::new(self.ast,Node::from(expr::BoolLiteral(true))),
            _ => {
                unreachable!();
            }
        };
        // Done
        self.finalise(expr,tok)
    }

    /// Parse a _character literal_ (e.g. `'a'`, etc).
    pub fn parse_literal_char(&mut self) -> Result<Expr> {
        let tok = self.lexer.snap(Token::Character)?;
        // Sanity check this is a valid chacter.
        if tok.len() != 3 {
            // FIXME: handle escape sequences!
            Err(Error::new(tok,ErrorCode::InvalidCharacterLiteral))
        } else {
            let chars = self.lexer.get(tok);
    	    let expr = Expr::new(self.ast,Node::from(expr::CharLiteral(chars[1])));
            self.finalise(expr,tok)
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

    /// Parse a _string literal_ (e.g. `"hello"`, `"world\n"`), whilst
    /// parsing all valid escape sequences.
    pub fn parse_literal_string(&mut self) -> Result<Expr> {
        let tok = self.lexer.snap(Token::String)?;
        let contents = self.lexer.get_str(tok);
        let expr = Expr::new(self.ast,Node::from(expr::StringLiteral(contents)));
        self.finalise(expr,tok)
    }

    // =========================================================================
    // Types
    // =========================================================================

    pub fn parse_type(&mut self) -> Result<Type> {
        self.skip_gap();
	self.parse_type_compound()
    }

    pub fn parse_type_compound(&mut self) -> Result<Type> {
        let lookahead = self.lexer.peek();
        // Attemp to distinguish
        match lookahead.kind {
            Token::EOF => {
        	// Something went wrong
        	Err(Error::new(lookahead,ErrorCode::UnexpectedEof))
            }
            Token::Ampersand => self.parse_type_ref(),
            Token::LeftCurly => self.parse_type_record(),
            _ => self.parse_type_array()
        }
    }

    /// Parse a reference type, such as `&i32`, `&(i32[])`, `&&u16`,
    /// etc.
    pub fn parse_type_ref(&mut self) -> Result<Type> {
	let mut n = 1;
	// "&"
	self.lexer.snap(Token::Ampersand)?;
	// Check for nested references
	while self.lexer.snap(Token::Ampersand).is_ok() {
	    n = n + 1;
	}
	// Type
	let mut t = self.parse_type_bracketed()?;
	// Unwind references
	for _i in 0..n {
            t = Type::new(self.ast,Node::from(types::Reference(t)));
	}
	// Done
	Ok(t)
    }

    /// Parse a record type, such as `{ i32 f }`, `{ bool f, u64 f }`,
    /// `{ &bool f, u64[] f }`, etc.
    pub fn parse_type_record(&mut self) -> Result<Type> {
	let mut fields : Vec<(Type,Name)> = vec![];
	// "{"
	self.lexer.snap(Token::LeftCurly)?;
	// Keep going until a right brace
	while self.lexer.snap(Token::RightCurly).is_err() {
	    // Check if first time or not
	    if !fields.is_empty() {
		// Not first time, so match comma
		self.lexer.snap(Token::Comma)?;
	    }
	    // Type
	    let f_type = self.parse_type()?;
	    // Identifier
	    let f_name = self.parse_identifier()?;
	    //
	    fields.push((f_type,f_name));
	}
	// Done
	Ok(Type::new(self.ast,Node::from(types::Record(fields))))
    }

    /// Parse an array type, such as `i32[]`, `bool[][]`, etc.
    pub fn parse_type_array(&mut self) -> Result<Type> {
	// Type
	let mut t = self.parse_type_bracketed()?;
	// ([])*
	while self.lexer.snap(Token::LeftSquare).is_ok() {
	    self.lexer.snap(Token::RightSquare)?;
            t = Type::new(self.ast,Node::from(types::Array(t)));
	}
	//
	Ok(t)
    }

    /// Parse a type which may (or may not) be bracketed.  For
    /// example, in `(&int)[]` the type `&int` is bracketed.
    pub fn parse_type_bracketed(&mut self) -> Result<Type> {
	// Try and match bracket!
	if self.lexer.snap(Token::LeftBrace).is_ok() {
	    // Bingo!
	    let typ_e = self.parse_type()?;
	    // Must match closing brace
	    self.lexer.snap(Token::RightBrace)?;
	    // Done
	    Ok(typ_e)
	} else {
	    self.parse_type_base()
	}
    }

    pub fn parse_type_base(&mut self) -> Result<Type> {
        let lookahead = self.lexer.peek();
        // Look at what we've got!
        let node = match lookahead.kind {
            Token::Null => Node::from(types::Null()),
            Token::Bool => Node::from(types::Bool()),
            Token::Int(s) => Node::from(types::Int(true,s)),
            Token::Uint(s) => Node::from(types::Int(false,s)),
            Token::Void => Node::from(types::Void()),
            // Nominals
            Token::Identifier => {
                // TODO: manage qualified names here.
                // TODO: distinguish static / local names.
                let n = Name::new(self.ast,self.lexer.get_str(lookahead));
                Node::from(types::Nominal(n))
            }
            _ => {
		return Err(Error::new(lookahead, ErrorCode::UnexpectedToken));
            }
        };
        // Move over it
        self.lexer.snap(lookahead.kind)?;
        let typ_e = Type::new(self.ast,node);
        // Done
        self.finalise(typ_e,lookahead)
    }

    // =========================================================================
    // Misc
    // =========================================================================

    pub fn parse_identifier(&mut self) -> Result<Name> {
        self.skip_gap();
	let tok = self.lexer.snap(Token::Identifier)?;
        let name = self.lexer.get_str(tok);
	// FIXME: should employ cache!
	Ok(Name::new(self.ast,name))
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

    /// If the next token is a gap, just skip over it.
    fn skip_gap(&mut self) {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            Token::Gap => {
                self.lexer.snap(Token::Gap).unwrap();
            }
            _ => {
                // Do nothing
            }
        }
    }

    /// Match various forms of whitespace which can occur on a single
    /// line, including gaps and comments (but not newlines).
    fn skip_linespace(&mut self) -> Result<()> {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            Token::EOF => {
                Ok(())
            }
            Token::Gap => {
                self.lexer.snap(lookahead.kind)?;
                self.skip_linespace()
            }
            Token::LineComment => {
                let tok = self.lexer.snap(lookahead.kind)?;
                let comment = self.lexer.get_str(tok);
                self.ast.push(Node::from(comment::Line(comment)));
                self.skip_linespace()
            }
            Token::BlockComment => {
                let tok = self.lexer.snap(lookahead.kind)?;
                let comment = self.lexer.get_str(tok);
                self.ast.push(Node::from(comment::Block(comment)));
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
    fn skip_whitespace(&mut self) -> Result<()> {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            Token::EOF => {
                Ok(())
            }
            Token::Gap => {
                self.lexer.snap(lookahead.kind)?;
                self.skip_whitespace()
            }
            Token::NewLine => {
                self.lexer.snap(lookahead.kind)?;
                self.skip_whitespace()
            }
            Token::LineComment => {
                let tok = self.lexer.snap(lookahead.kind)?;
                let comment = self.lexer.get_str(tok);
                self.ast.push(Node::from(comment::Line(comment)));
                self.skip_whitespace()
            }
            Token::BlockComment => {
                let tok = self.lexer.snap(lookahead.kind)?;
                let comment = self.lexer.get_str(tok);
                self.ast.push(Node::from(comment::Block(comment)));
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
    fn match_line_end(&mut self) -> Result<()> {
        let lookahead = self.lexer.peek();
        //
        match lookahead.kind {
            Token::EOF => {
                Ok(())
            }
            Token::Gap => {
                self.lexer.snap(lookahead.kind)?;
                self.match_line_end()
            }
            Token::NewLine => {
                self.lexer.snap(lookahead.kind)?;
                Ok(())
            }
            Token::LineComment => {
                let tok = self.lexer.snap(lookahead.kind)?;
                let comment = self.lexer.get_str(tok);
                self.ast.push(Node::from(comment::Line(comment)));
                self.match_line_end()
            }
            Token::BlockComment => {
                let tok = self.lexer.snap(lookahead.kind)?;
                let comment = self.lexer.get_str(tok);
                self.ast.push(Node::from(comment::Block(comment)));
                self.match_line_end()
            }
            _ => {
	        // Reject
	        Err(Error::new(lookahead,ErrorCode::ExpectedLineEnd))
            }
        }
    }

    /// Match a given token type in the current stream, allowing for
    /// an optional gap beforehand.
    fn gap_snap(&mut self, kind : Token) -> Result<Span<Token>> {
        self.skip_gap();
        Ok(self.lexer.snap(kind)?)
    }

    /// Check whether the characters associated with two spans match (or not).
    fn chars_match(&self,lhs: Span<Token>, rhs: Span<Token>) -> bool {
        self.lexer.get(lhs) == self.lexer.get(rhs)
    }

    /// Construct a `BinOp` from a `Token`.
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
