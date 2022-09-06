pub mod ast;
pub mod lexer;
pub mod parser;
pub mod source_map;

use std::result;
use std::fmt;

use crate::ast::AbstractSyntaxTree;
use crate::lexer::{SnapError,Span,Token};
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
    ExpectedTokenIn(Vec<Token>)
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

impl From<SnapError<Token>> for Error {
    fn from(p:SnapError<Token>) -> Error {
        match p {
            SnapError::Expected(t,s) => {
                Error{span:s,code:ErrorCode::ExpectedToken(t)}
            }
            SnapError::ExpectedIn(ts,s) => {
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
        // let mut ast = AbstractSyntaxTree::new();
	// let mut mapper = SourceMap::new(input);
	// let mut parser = Parser::new(input, &mut ast, |i,s| mapper.map(i,s));
        // let r = parser.parse();
	// // Parse entire file
	// match r {
        //     Ok(()) => {
	//         Ok(WhileyFile{ast:Box::new(ast)})
        //     }
        //     Err(e) => {
        //         Err(e)
        //     }
        // }
        todo!("fix me");
    }
}
