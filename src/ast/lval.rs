use crate::ast::*;

// =============================================================================
// LVals
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct LVal(pub usize);

impl LVal {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(LVal::is(ast, &t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        LVal(index)
    }

    /// Determine whether a given node represents an LVal, or not.
    pub fn is(ast: &AbstractSyntaxTree, t: &Node) -> bool {
        match t {
	    Node::ArrayAccessExpr(e) => Self::is(ast, ast.get(e.0.into())),
	    Node::VarAccessExpr(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for LVal {
    fn into(self) -> usize { self.0 }
}
