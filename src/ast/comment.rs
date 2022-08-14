use crate::ast::*;

#[derive(Clone,Debug,PartialEq)]
pub struct Line(pub String);

impl From<Line> for Node {
    fn from(s: Line) -> Self { Node::LineComment(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Block(pub String);

impl From<Block> for Node {
    fn from(s: Block) -> Self { Node::BlockComment(s) }
}
