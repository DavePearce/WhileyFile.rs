pub mod ast;
pub mod lexer;
pub mod parser;
// hidden
mod nodes;

use std::convert::From;
use std::result;

use crate::ast::AbstractSyntaxTree;
use crate::lexer::Token;
use crate::parser::Parser;

// =================================================================
// Error
// =================================================================

/// Identifies possible errors stemming from the parser.
#[derive(Debug)]
pub struct Error {
    pub start: usize,
    pub end: usize,
    pub message: String
}

impl Error {
    pub fn new<'a>(tok: Token<'a>, message: &str) -> Error {
	let start = tok.start;
	let end = tok.end();
	Error{start,end,message: message.to_string()}
    }
}

pub type Result<T> = result::Result<T, Error>;

// ===============================================================
// Whiley File
// ===============================================================

/// A dummy source mapper which does nothing (FOR NOW).
fn source_mapper<'a>(_: usize, _: &'a str) { }

pub struct WhileyFile {
    pub ast : Box<AbstractSyntaxTree>
}

impl WhileyFile {
    pub fn from_str(input: &str) -> Result<WhileyFile> {
	let mut ast = AbstractSyntaxTree::new();
	let mut parser = Parser::new(input,&mut ast, source_mapper);
	// Parse entire file
	let _ = parser.parse()?;
	// Done
	Ok(WhileyFile{ast:Box::new(ast)})
    }
}
