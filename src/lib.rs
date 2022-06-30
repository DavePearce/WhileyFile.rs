pub mod ast;
pub mod lexer;
pub mod parser;
// hidden
mod nodes;

use std::result;

use crate::ast::AbstractSyntaxTree;
use crate::lexer::{Token,TokenType};
use crate::parser::Parser;

// =================================================================
// Error
// =================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum ErrorCode {
    InvalidBlockIndent,
    UnexpectedToken,
    UnexpectedEof,
    ExpectedLineEnd,
    ExpectedToken(TokenType)
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

/// A dummy source mapper which does nothing (FOR NOW).
fn source_mapper<'a>(_: usize, _: &'a str) { }

pub struct WhileyFile {
    pub ast : Box<AbstractSyntaxTree>
}

impl WhileyFile {
    pub fn from_str<'a>(input: &'a str) -> Result<'a,WhileyFile> {
        let mut ast = AbstractSyntaxTree::new();
	let mut parser = Parser::new(input, &mut ast, source_mapper);
	// Parse entire file
	match parser.parse() {
            Ok(_) => {
	        Ok(WhileyFile{ast:Box::new(ast)})
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}
