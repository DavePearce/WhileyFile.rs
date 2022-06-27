use whiley_file::ast::*;
use whiley_file::parser::Parser;

// ======================================================
// Tests (Type Declarations)
// ======================================================

#[test]
fn test_type_01() {
    check_parse_error("type nat is i32 x");
}

#[test]
fn test_type_02() {
    check_parse_error("type nat is i8;");
}

#[test]
fn test_type_03() {
    let ast = check_parse("type t is bool");
    check_name(ast.get(0),"t");
    assert_eq!(ast.get(1),&Node::from(BoolType{}));
}

#[test]
fn test_type_04() {
    let ast = check_parse("type nat is i8");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,8)));
}

#[test]
fn test_type_05() {
    let ast = check_parse("type nat is i16");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,16)));
}

#[test]
fn test_type_06() {
    let ast = check_parse("type nat is i32");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,32)));
}

#[test]
fn test_type_07() {
    let ast = check_parse("type nat is i64");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,64)));
}

#[test]
fn test_type_08() {
    let ast = check_parse("type nat is u8");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(false,8)));
}

#[test]
fn test_type_09() {
    let ast = check_parse("type nat is u16");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(false,16)));
}

#[test]
fn test_type_10() {
    let ast = check_parse("type nat is u32");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(false,32)));
}

#[test]
fn test_type_11() {
    let ast = check_parse("type nat is u64");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(false,64)));
}

#[test]
fn test_type_12() {
    let ast = check_parse("type nat is i32[]");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,32)));
    assert_eq!(ast.get(2),&Node::from(ArrayType::new(Type(1))));
}

#[test]
fn test_type_13() {
    let ast = check_parse("type nat is i32[][]");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,32)));
    assert_eq!(ast.get(2),&Node::from(ArrayType::new(Type(1))));
    assert_eq!(ast.get(3),&Node::from(ArrayType::new(Type(2))));
}

#[test]
fn test_type_14() {
    let ast = check_parse("type ref is &i16");
    check_name(ast.get(0),"ref");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,16)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType::new(Type(1))));
}

#[test]
fn test_type_15() {
    let ast = check_parse("type ref is &&i16");
    check_name(ast.get(0),"ref");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,16)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType::new(Type(1))));
    assert_eq!(ast.get(3),&Node::from(ReferenceType::new(Type(2))));
}

#[test]
fn test_type_16() {
    let ast = check_parse("type rec is {i64 f}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,64)));
    check_name(ast.get(2),"f");
    assert_eq!(ast.get(3),&Node::from(RecordType::new(vec![(Type(1),Name(2))])));
}

#[test]
fn test_type_17() {
    let ast = check_parse("type rec is {i32 f, u16 g}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,32)));
    check_name(ast.get(2),"f");
    assert_eq!(ast.get(3),&Node::from(IntType::new(false,16)));
    check_name(ast.get(4),"g");
    assert_eq!(ast.get(5),&Node::from(RecordType::new(vec![(Type(1),Name(2)),(Type(3),Name(4))])));
}

#[test]
fn test_type_18() {
    let ast = check_parse("type rar is (&u32)[]");
    check_name(ast.get(0),"rar");
    assert_eq!(ast.get(1),&Node::from(IntType::new(false,32)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType::new(Type(1))));
    assert_eq!(ast.get(3),&Node::from(ArrayType::new(Type(2))));
}

#[test]
fn test_type_19() {
    let ast = check_parse("type rec is {&i8 f, u16[] g}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(IntType::new(true,8)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType::new(Type(1))));
    check_name(ast.get(3),"f");
    assert_eq!(ast.get(4),&Node::from(IntType::new(false,16)));
    assert_eq!(ast.get(5),&Node::from(ArrayType::new(Type(4))));
    check_name(ast.get(6),"g");
    assert_eq!(ast.get(7),&Node::from(RecordType::new(vec![(Type(2),Name(3)),(Type(5),Name(6))])));
}

// ======================================================
// Tests (Function Declarations)
// ======================================================

#[test]
fn test_function_01() {
    check_parse_error("func");
}

#[test]
fn test_function_02() {
    check_parse_error("function");
}

#[test]
fn test_function_03() {
    check_parse_error("function f");
}

#[test]
fn test_function_04() {
    check_parse_error("function f(");
}

#[test]
fn test_function_05() {
    check_parse_error("function f():");
}

#[test]
fn test_function_06a() {
    check_parse_error("function f()->():");
}

#[test]
fn test_function_06b() {
    check_parse_error("function f()->():\n");
}

#[test]
fn test_function_06c() {
    check_parse_error("function f()->():\n ");
}

#[test]
fn test_function_06d() {
    let ast = check_parse("function f()->() :\n skip");
}

#[test]
fn test_function_06e() {
    let ast = check_parse("function f()->():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt::new()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt::new(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_06f() {
    let ast = check_parse("function f() ->():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt::new()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt::new(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_06g() {
    let ast = check_parse("function f()-> ():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt::new()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt::new(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}


#[test]
fn test_function_06h() {
    let ast = check_parse("function f() -> ():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt::new()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt::new(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_07() {
    let ast = check_parse("function f(i32 x):\n skip");
    println!("{:?}",ast);
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt::new()));
    assert_eq!(ast.get(2),&Node::from(IntType::new(true,32)));
    check_name(ast.get(3),"x");
    assert_eq!(ast.get(4),&Node::from(BlockStmt::new(vec![Stmt(1)])));
    let params = vec![Parameter{declared:Type(2),name:Name(3)}];
    assert_eq!(ast.get(5),&Node::from(MethodDecl::new(Name(0),vec![],params,Stmt(4))));
}

#[test]
fn test_function_08() {
    let ast = check_parse("bool f(i32 i, bool b) {}");
    assert_eq!(ast.get(0),&Node::from(BoolType::new()));
    check_name(ast.get(1),"f");
    assert_eq!(ast.get(2),&Node::from(IntType::new(true,32)));
    check_name(ast.get(3),"i");
    assert_eq!(ast.get(4),&Node::from(BoolType::new()));
    check_name(ast.get(5),"b");
    assert_eq!(ast.get(6),&Node::from(BlockStmt::new(vec![])));
    let params = vec![Parameter{declared:Type(2),name:Name(3)},Parameter{declared:Type(4),name:Name(5)}];
    assert_eq!(ast.get(7),&Node::from(MethodDecl::new(Name(1),vec![],params,Stmt(6))));
}

// ======================================================
// Tests (Method Declarations)
// ======================================================


// ======================================================
// Tests (Skip)
// ======================================================

#[test]
fn test_skip_01() {
    check_parse_error("void f() { ski }");
}

#[test]
fn test_skip_02() {
    check_parse_error("void f() { skip }");
}

#[test]
fn test_skip_03() {
    let ast = check_parse("void f() { skip; }");
    assert_eq!(ast.get(2),&Node::from(SkipStmt::new()));
}

// ======================================================
// Tests (Assert)
// ======================================================

#[test]
fn test_assert_04() {
    check_parse_error("void f() { asse");
}

#[test]
fn test_assert_05() {
    check_parse_error("void f() { assert");
}

#[test]
fn test_assert_06() {
    check_parse_error("void f() { assert true }");
}

#[test]
fn test_assert_07() {
    let ast = check_parse("void f() { assert true; }");
    assert_eq!(ast.get(2),&Node::from(BoolExpr::new(true)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt::new(Expr(2))));
}

#[test]
fn test_assert_08() {
    let ast = check_parse("void f() { assert false; }");
    assert_eq!(ast.get(2),&Node::from(BoolExpr::new(false)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt::new(Expr(2))));
}

#[test]
fn test_assert_09() {
    let ast = check_parse("void f() { assert (false); }");
    assert_eq!(ast.get(2),&Node::from(BoolExpr::new(false)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt::new(Expr(2))));
}

#[test]
fn test_assert_10() {
    check_type_error("void f() { assert b; }");
}

#[test]
fn test_assert_11() {
    let ast = check_parse("void f(bool b) { assert b; }");
    check_name(ast.get(4),"b");
    assert_eq!(ast.get(5),&Node::from(VarExpr::new(Name(4))));
    assert_eq!(ast.get(6),&Node::from(AssertStmt::new(Expr(5))));
}

#[test]
fn test_assert_12() {
    let ast = check_parse("void f(i32 i) { assert i < 0; }");
    assert_eq!(ast.get(4),&Node::from(VarExpr::new(Name(3))));
    assert_eq!(ast.get(5),&Node::from(IntExpr::new(0)));
    assert_eq!(ast.get(6),&Node::from(LessThanExpr::new(Expr(4),Expr(5))));
    assert_eq!(ast.get(7),&Node::from(AssertStmt::new(Expr(6))));
}

// ======================================================
// Helpers
// ======================================================

/// A dummy source mapper which does nothing.
fn source_mapper<'a>(_: usize, _: &'a str) { }

/// A dummy type mapper which does nothing.
fn type_mapper<'a>(_: usize, _: Type) { }

#[cfg(test)]
fn check_parse(input: &str) -> Box<AbstractSyntaxTree> {
    let mut ast = AbstractSyntaxTree::new();
    let mut parser = Parser::new(input,&mut ast, source_mapper);
    // Parse input
    let d = parser.parse_decl();
    println!("PARSED {:?}",d);
    assert!(!d.is_err());
    // Type input
    // let mut typer = TypeChecker::new(&mut ast, type_mapper);
    // let r = typer.check(d.unwrap());
    // assert!(!r.is_err());
    // Done
    Box::new(ast)
}

#[cfg(test)]
fn check_parse_error(input: &str) {
    let mut ast = AbstractSyntaxTree::new();
    let mut p = Parser::new(input,&mut ast, source_mapper);
    let d = p.parse_decl();
    assert!(d.is_err());
}

#[cfg(test)]
fn check_type_error(input: &str) {
    let mut ast = AbstractSyntaxTree::new();
    let mut parser = Parser::new(input,&mut ast, source_mapper);
    // Parse input
    let d = parser.parse_decl();
    println!("PARSED {:?}",d);
    assert!(!d.is_err());
    // Type input
    // let mut typer = TypeChecker::new(&mut ast, type_mapper);
    // let r = typer.check(d.unwrap());
    // assert!(r.is_err());
}

/// Check that a given node is an instance of Node::Utf8 and matches
/// the corresponding string.
#[cfg(test)]
fn check_name(n: &Node, s: &str) {
    let r = match n {
	Node::Utf8(m) => Some(m),
	_ => None
    };
    assert_eq!(r.unwrap(),s);
}
