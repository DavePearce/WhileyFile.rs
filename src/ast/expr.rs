use crate::ast::*;

// =============================================================================
// Expressions
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Expr(pub usize);

impl Expr {
    pub fn new(ast: &mut AbstractSyntaxTree, t : Node) -> Self {
        // Sanity check is declaration
        assert!(Expr::is(&t));
        // Create new node
        let index = ast.push(t).raw_index();
        // Done
        Expr(index)
    }

    /// Determine whether a given term is a declaration or not.
    pub fn is(t: &Node) -> bool {
        match t {
	    Node::ArrayAccessExpr(_) => true,
	    Node::ArrayInitialiserExpr(_) => true,
            Node::ArrayGeneratorExpr(_) => true,
	    Node::ArrayLengthExpr(_) => true,
	    Node::BoolLiteral(_) => true,
	    Node::CharLiteral(_) => true,
	    Node::BinaryExpr(_) => true,
	    Node::UnaryExpr(_) => true,
	    Node::IntLiteral(_) => true,
	    Node::LambdaLiteral(_) => true,
	    Node::NullLiteral(_) => true,
	    Node::InvokeExpr(_) => true,
	    Node::IsTypeExpr(_) => true,
	    Node::RangeExpr(_) => true,
	    Node::QuantifierExpr(_) => true,
	    Node::VarAccessExpr(_) => true,
	    Node::StringLiteral(_) => true,
            _ => false
        }
    }
}

impl Into<usize> for Expr {
    fn into(self) -> usize { self.0 }
}

impl Into<LVal> for Expr {
    fn into(self) -> LVal {
        // FIXME: a check here would be nice.
        LVal(self.0)
    }
}

// =============================================================================
// Literals
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct BoolLiteral(pub bool);

impl From<BoolLiteral> for Node {
    fn from(s: BoolLiteral) -> Self { Node::BoolLiteral(s) }
}

impl TryFromRef<Node> for BoolLiteral {
    fn try_from_ref(r:&Node) -> Option<&Self> {
        match r {
            Node::BoolLiteral(s) => Some(&s),
            _ => None
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct CharLiteral(pub char);

impl From<CharLiteral> for Node {
    fn from(s: CharLiteral) -> Self { Node::CharLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct IntLiteral(pub i32);

impl From<IntLiteral> for Node {
    fn from(s: IntLiteral) -> Self { Node::IntLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct LambdaLiteral(pub Name, pub Option<Vec<Type>>);

impl From<LambdaLiteral> for Node {
    fn from(s: LambdaLiteral) -> Self { Node::LambdaLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct NullLiteral();

impl From<NullLiteral> for Node {
    fn from(s: NullLiteral) -> Self { Node::NullLiteral(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct StringLiteral(pub String);

impl From<StringLiteral> for Node {
    fn from(s: StringLiteral) -> Self { Node::StringLiteral(s) }
}

// =============================================================================
// Variables
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct VarAccess(pub Name);

impl From<VarAccess> for Node {
    fn from(s: VarAccess) -> Self { Node::VarAccessExpr(s) }
}

// =============================================================================
// Invoke
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Invoke(pub Expr, pub Vec<Expr>);

impl From<Invoke> for Node {
    fn from(s: Invoke) -> Self { Node::InvokeExpr(s) }
}

// =============================================================================
// Binary Expressions
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Binary(pub BinOp, pub Expr, pub Expr);

impl From<Binary> for Node {
    fn from(s: Binary) -> Self { Node::BinaryExpr(s) }
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Subtract,
    Divide,
    Multiply,
    Remainder,
    // Comparators
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
    // Logical
    LogicalAnd,
    LogicalOr,
    LogicalXor
}

// =============================================================================
// Unary Expressions
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Unary(pub UnOp, pub Expr);

impl From<Unary> for Node {
    fn from(s: Unary) -> Self { Node::UnaryExpr(s) }
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum UnOp {
    LogicalNot,
    Dereference
}

// =============================================================================
// Arrays
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayAccess(pub Expr, pub Expr);

impl From<ArrayAccess> for Node {
    fn from(s: ArrayAccess) -> Self { Node::ArrayAccessExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayGenerator(pub Expr, pub Expr);

impl From<ArrayGenerator> for Node {
    fn from(s: ArrayGenerator) -> Self { Node::ArrayGeneratorExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayInitialiser(pub Vec<Expr>);

impl From<ArrayInitialiser> for Node {
    fn from(s: ArrayInitialiser) -> Self { Node::ArrayInitialiserExpr(s) }
}

#[derive(Clone,Debug,PartialEq)]
pub struct ArrayLength(pub Expr);

impl From<ArrayLength> for Node {
    fn from(s: ArrayLength) -> Self { Node::ArrayLengthExpr(s) }
}

// =============================================================================
// IsType
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct IsType(pub Expr, pub Type);

impl From<IsType> for Node {
    fn from(s: IsType) -> Self { Node::IsTypeExpr(s) }
}

// =============================================================================
// Range
// =============================================================================

#[derive(Clone,Debug,PartialEq)]
pub struct Range(pub Expr, pub Expr);

impl From<Range> for Node {
    fn from(s: Range) -> Self { Node::RangeExpr(s) }
}

// =============================================================================
// Quantifier
// =============================================================================

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum QuantOp {
    // Universal quantifier
    All,
    // Existential quantifier
    Exists
}

#[derive(Clone,Debug,PartialEq)]
pub struct Quantifier(pub QuantOp, pub Vec<(Name,Expr)>, pub Expr);

impl From<Quantifier> for Node {
    fn from(s: Quantifier) -> Self { Node::QuantifierExpr(s) }
}
