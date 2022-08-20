pub mod ast;
pub mod lexer;
pub mod parser;
pub mod source_map;

use std::result;

use crate::ast::AbstractSyntaxTree;
use crate::lexer::{Token,TokenType};
use crate::parser::Parser;
use crate::source_map::SourceMap;

// =================================================================
// Error
// =================================================================

#[derive(Clone,Debug,PartialEq)]
pub enum ErrorCode {
    InvalidBlockIndent,
    InvalidSpecClause,
    UnexpectedToken,
    UnexpectedEof,
    ExpectedLineEnd,
    ExpectedToken(TokenType),
    ExpectedTokenIn(Vec<TokenType>)
}

/// Identifies possible errors stemming from the parser.
#[derive(Debug)]
pub struct Error<'a> {
    pub token: Token<'a>,
    pub code: ErrorCode
}

impl<'a> Error<'a> {
    pub fn new(token: Token<'a>, code: ErrorCode) -> Error<'a> {
	Error{token,code}
    }
}

pub type Result<'a,T> = result::Result<T, Error<'a>>;

// ===============================================================
// Whiley File
// ===============================================================

pub struct WhileyFile {
    pub ast : Box<AbstractSyntaxTree>,
    //pub source_map : HashMap<usize,Highlight>	
}

impl WhileyFile {
    pub fn from_str<'a>(input: &'a str) -> Result<'a,WhileyFile> {
        let mut ast = AbstractSyntaxTree::new();
	let mut mapper = SourceMap::new(input);
	let mut parser = Parser::new(input, &mut ast, |i,s| mapper.map(i,s));
        let r = parser.parse();
	// Parse entire file
	match r {
            Ok(()) => {
	        Ok(WhileyFile{ast:Box::new(ast)})
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}
