pub mod ast;
pub mod lexer;
pub mod parser;
pub mod syntactic_fn;
pub mod type_checker;
pub mod source_map;
pub mod util;

use std::result;
use std::fmt;

use crate::ast::AbstractSyntaxTree;
use crate::lexer::{Span,Token};
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
    ExpectedToken(Token),
    ExpectedTokenIn(Vec<Token>),
    InvalidCharacterLiteral,
    InvalidLVal,
    ExpectedType
}

/// Identifies possible errors stemming from the parser.
#[derive(Debug)]
pub struct Error {
    pub span: Span<Token>,
    pub code: ErrorCode
}

impl Error {
    pub fn new(span: Span<Token>, code: ErrorCode) -> Error {
	Error{span,code}
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // temporary for now.
        write!(f,"{:?}",self)
    }
}

impl std::error::Error for Error { }

impl From<lexer::Error<Token>> for Error {
    fn from(p:lexer::Error<Token>) -> Error {
        match p {
            lexer::Error::Expected(t,s) => {
                Error{span:s,code:ErrorCode::ExpectedToken(t)}
            }
            lexer::Error::ExpectedIn(ts,s) => {
                Error{span:s,code:ErrorCode::ExpectedTokenIn(ts)}
            }
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

// ===============================================================
// Whiley File
// ===============================================================

pub struct WhileyFile {
    pub ast : Box<AbstractSyntaxTree>,
    //pub source_map : HashMap<usize,Highlight>
}

impl WhileyFile {
    pub fn from_str<'a>(input: &'a str) -> Result<WhileyFile> {
        let mut ast = AbstractSyntaxTree::new();
	// let mut mapper = SourceMap::new(input);
	let mut parser = Parser::new(input, &mut ast);
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
//        todo!("fix me");
    }
}
