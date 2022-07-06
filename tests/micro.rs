use whiley_file::WhileyFile;
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
    assert_eq!(ast.get(1),&Node::from(IntType(true,8)));
}

#[test]
fn test_type_05() {
    let ast = check_parse("type nat is i16");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,16)));
}

#[test]
fn test_type_06() {
    let ast = check_parse("type nat is i32");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,32)));
}

#[test]
fn test_type_07() {
    let ast = check_parse("type nat is i64");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,64)));
}

#[test]
fn test_type_08() {
    let ast = check_parse("type nat is u8");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(false,8)));
}

#[test]
fn test_type_09() {
    let ast = check_parse("type nat is u16");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(false,16)));
}

#[test]
fn test_type_10() {
    let ast = check_parse("type nat is u32");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(false,32)));
}

#[test]
fn test_type_11() {
    let ast = check_parse("type nat is u64");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(false,64)));
}

#[test]
fn test_type_12() {
    let ast = check_parse("type nat is i32[]");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,32)));
    assert_eq!(ast.get(2),&Node::from(ArrayType(Type(1))));
}

#[test]
fn test_type_13() {
    let ast = check_parse("type nat is i32[][]");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,32)));
    assert_eq!(ast.get(2),&Node::from(ArrayType(Type(1))));
    assert_eq!(ast.get(3),&Node::from(ArrayType(Type(2))));
}

#[test]
fn test_type_14() {
    let ast = check_parse("type ref is &i16");
    check_name(ast.get(0),"ref");
    assert_eq!(ast.get(1),&Node::from(IntType(true,16)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType(Type(1))));
}

#[test]
fn test_type_15() {
    let ast = check_parse("type ref is &(&i16)");
    check_name(ast.get(0),"ref");
    assert_eq!(ast.get(1),&Node::from(IntType(true,16)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType(Type(1))));
    assert_eq!(ast.get(3),&Node::from(ReferenceType(Type(2))));
}

#[test]
fn test_type_16() {
    let ast = check_parse("type rec is {i64 f}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(IntType(true,64)));
    check_name(ast.get(2),"f");
    assert_eq!(ast.get(3),&Node::from(RecordType(vec![(Type(1),Name(2))])));
}

#[test]
fn test_type_17() {
    let ast = check_parse("type rec is {i32 f, u16 g}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(IntType(true,32)));
    check_name(ast.get(2),"f");
    assert_eq!(ast.get(3),&Node::from(IntType(false,16)));
    check_name(ast.get(4),"g");
    assert_eq!(ast.get(5),&Node::from(RecordType(vec![(Type(1),Name(2)),(Type(3),Name(4))])));
}

#[test]
fn test_type_18() {
    let ast = check_parse("type rar is (&u32)[]");
    check_name(ast.get(0),"rar");
    assert_eq!(ast.get(1),&Node::from(IntType(false,32)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType(Type(1))));
    assert_eq!(ast.get(3),&Node::from(ArrayType(Type(2))));
}

#[test]
fn test_type_19() {
    let ast = check_parse("type rec is {&i8 f, u16[] g}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(IntType(true,8)));
    assert_eq!(ast.get(2),&Node::from(ReferenceType(Type(1))));
    check_name(ast.get(3),"f");
    assert_eq!(ast.get(4),&Node::from(IntType(false,16)));
    assert_eq!(ast.get(5),&Node::from(ArrayType(Type(4))));
    check_name(ast.get(6),"g");
    assert_eq!(ast.get(7),&Node::from(RecordType(vec![(Type(2),Name(3)),(Type(5),Name(6))])));
}

#[test]
fn test_type_20() {
    let ast = check_parse("type nat is i64 // line comment");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,64)));
    assert_eq!(ast.get(2),&Node::from(LineComment("// line comment".to_string())));
}

#[test]
fn test_type_21() {
    let ast = check_parse("// line comment\ntype nat is i64");
    assert_eq!(ast.get(0),&Node::from(LineComment("// line comment".to_string())));
    check_name(ast.get(1),"nat");
    assert_eq!(ast.get(2),&Node::from(IntType(true,64)));
}

#[test]
fn test_type_22() {
    let ast = check_parse("type nat is i64 /* block comment */");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(IntType(true,64)));
    assert_eq!(ast.get(2),&Node::from(BlockComment("/* block comment */".to_string())));
}

#[test]
fn test_type_23() {
    let ast = check_parse("/* block comment */\ntype nat is i64");
    assert_eq!(ast.get(0),&Node::from(BlockComment("/* block comment */".to_string())));
    check_name(ast.get(1),"nat");
    assert_eq!(ast.get(2),&Node::from(IntType(true,64)));
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
fn test_function_06() {
    check_parse_error("function f()->():");
}

#[test]
fn test_function_07() {
    check_parse_error("function f()->():\n");
}

#[test]
fn test_function_08() {
    check_parse_error("function f()->():\n ");
}

#[test]
fn test_function_09() {
    let ast = check_parse("function f()->() :\n skip");
}

#[test]
fn test_function_10() {
    let ast = check_parse("function f()->():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_11() {
    let ast = check_parse("function f() ->():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_12() {
    let ast = check_parse("function f()-> ():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}


#[test]
fn test_function_13() {
    let ast = check_parse("function f() -> ():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_14() {
    let ast = check_parse("function f(i32 x):\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(IntType(true,32)));
    check_name(ast.get(2),"x");
    assert_eq!(ast.get(3),&Node::from(SkipStmt()));
    assert_eq!(ast.get(4),&Node::from(BlockStmt(vec![Stmt(3)])));
    let params = vec![Parameter{declared:Type(1),name:Name(2)}];
    assert_eq!(ast.get(5),&Node::from(FunctionDecl::new(Name(0),params,vec![],Stmt(4))));
}

#[test]
fn test_function_15() {
    let ast = check_parse("function f(i32 i, bool b) -> (bool r):\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(IntType(true,32)));
    check_name(ast.get(2),"i");
    assert_eq!(ast.get(3),&Node::from(BoolType()));
    check_name(ast.get(4),"b");
    assert_eq!(ast.get(5),&Node::from(BoolType()));
    check_name(ast.get(6),"r");
    assert_eq!(ast.get(7),&Node::from(SkipStmt()));
    assert_eq!(ast.get(8),&Node::from(BlockStmt(vec![Stmt(7)])));
    let params = vec![Parameter{declared:Type(1),name:Name(2)},Parameter{declared:Type(3),name:Name(4)}];
    let returns = vec![Parameter{declared:Type(5),name:Name(6)}];
    assert_eq!(ast.get(9),&Node::from(FunctionDecl::new(Name(0),params,returns,Stmt(8))));
}

#[test]
fn test_function_16() {
    let ast = check_parse("function f()->(): // line comment\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(LineComment("// line comment".to_string())));
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_17() {
    let ast = check_parse("// line comment\nfunction f()->():\n skip");
    assert_eq!(ast.get(0),&Node::from(LineComment("// line comment".to_string())));
    check_name(ast.get(1),"f");
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(1),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_18() {
    let ast = check_parse("function f()->(): /* block comment */\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/* block comment */".to_string())));
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_19() {
    let ast = check_parse("/* block comment */\nfunction f()->():\n skip");
    assert_eq!(ast.get(0),&Node::from(BlockComment("/* block comment */".to_string())));
    check_name(ast.get(1),"f");
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(1),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_20() {
    let ast = check_parse("function f()->():\n // line comment\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(LineComment("// line comment".to_string())));
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_21() {
    let ast = check_parse("function f()->():\n skip // line comment");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(LineComment("// line comment".to_string())));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_22() {
    let ast = check_parse("function f()->():\n /** line comment */\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/** line comment */".to_string())));
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_23() {
    let ast = check_parse("function f()->():\n skip /* line comment */");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockComment("/* line comment */".to_string())));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_24() {
    let ast = check_parse("function f()->():\n skip /* line\n comment */");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockComment("/* line\n comment */".to_string())));
    assert_eq!(ast.get(3),&Node::from(BlockStmt(vec![Stmt(1)])));
    assert_eq!(ast.get(4),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_25() {
    let ast = check_parse("function f()->():\n skip /* line\n comment */\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(BlockComment("/* line\n comment */".to_string())));
    assert_eq!(ast.get(3),&Node::from(SkipStmt()));
    assert_eq!(ast.get(4),&Node::from(BlockStmt(vec![Stmt(1),Stmt(3)])));
    assert_eq!(ast.get(5),&Node::from(FunctionDecl::new(Name(0),vec![],vec![],Stmt(4))));
}

// ======================================================
// Tests (Method Declarations)
// ======================================================


// ======================================================
// Tests (Skip)
// ======================================================

#[test]
fn test_skip_01() {
    check_parse_error("function f() -> ():\n ski");
}

#[test]
fn test_skip_02() {
    let ast = check_parse("function f() -> ():\n skip");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
}

#[test]
fn test_skip_03() {
    let ast = check_parse("function f() -> ():\n skip\n skip");
    assert_eq!(ast.get(1),&Node::from(SkipStmt()));
    assert_eq!(ast.get(2),&Node::from(SkipStmt()));
}

// ======================================================
// Tests (Assert)
// ======================================================

#[test]
fn test_assert_04() {
    check_parse_error("function f() -> ():\n asse");
}

#[test]
fn test_assert_05() {
    check_parse_error("function f() -> ():\n assert");
}

#[test]
fn test_assert_06() {
    let ast = check_parse("function f() -> ():\n assert true");
    assert_eq!(ast.get(1),&Node::from(BoolExpr(true)));
    assert_eq!(ast.get(2),&Node::from(AssertStmt(Expr(1))));
}

#[test]
fn test_assert_06b() {
    let ast = check_parse("function f() -> ():\n assert /* nothing */ true");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(BoolExpr(true)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt(Expr(2))));
}

#[test]
fn test_assert_06c() {
    let ast = check_parse("function f() -> ():\n assert // nothing\n    true");
    assert_eq!(ast.get(1),&Node::from(LineComment("// nothing".to_string())));
    assert_eq!(ast.get(2),&Node::from(BoolExpr(true)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt(Expr(2))));
}

#[test]
fn test_assert_07() {
    let ast = check_parse("function f() -> ():\n assert false");
    assert_eq!(ast.get(1),&Node::from(BoolExpr(false)));
    assert_eq!(ast.get(2),&Node::from(AssertStmt(Expr(1))));
}

#[test]
fn test_assert_08() {
    let ast = check_parse("function f() -> ():\n assert (false)");
    assert_eq!(ast.get(1),&Node::from(BoolExpr(false)));
    assert_eq!(ast.get(2),&Node::from(AssertStmt(Expr(1))));
}

#[test]
fn test_assert_08b() {
    let ast = check_parse("function f() -> ():\n assert ( /* nothing */ false)");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(BoolExpr(false)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt(Expr(2))));
}

#[test]
fn test_assert_08c() {
    let ast = check_parse("function f() -> ():\n assert (/* nothing */ false)");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(BoolExpr(false)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt(Expr(2))));
}

#[test]
fn test_assert_08d() {
    let ast = check_parse("function f() -> ():\n assert ( /* nothing */false)");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(BoolExpr(false)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt(Expr(2))));
}

#[test]
fn test_assert_08e() {
    let ast = check_parse("function f() -> ():\n assert (/* nothing */false)");
    assert_eq!(ast.get(1),&Node::from(BlockComment("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(BoolExpr(false)));
    assert_eq!(ast.get(3),&Node::from(AssertStmt(Expr(2))));
}

#[test]
fn test_assert_10() {
    let ast = check_type_error("function f() -> ():\n assert b");
}

#[test]
fn test_assert_11() {
    let ast = check_parse("function f(bool b) -> ():\n assert b");
    check_name(ast.get(2),"b");
    assert_eq!(ast.get(4),&Node::from(VarExpr(Name(3))));
    assert_eq!(ast.get(5),&Node::from(AssertStmt(Expr(4))));
}

#[test]
fn test_assert_12() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i < 0");
    assert_eq!(ast.get(4),&Node::from(VarExpr(Name(3))));
    assert_eq!(ast.get(5),&Node::from(IntExpr(0)));
    assert_eq!(ast.get(6),&Node::from(LessThanExpr(Expr(4),Expr(5))));
    assert_eq!(ast.get(7),&Node::from(AssertStmt(Expr(6))));
}

#[test]
fn test_assert_12b() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i /*nout*/ < 0");
    assert_eq!(ast.get(4),&Node::from(VarExpr(Name(3))));
    assert_eq!(ast.get(5),&Node::from(BlockComment("/*nout*/".to_string())));
    assert_eq!(ast.get(6),&Node::from(IntExpr(0)));
    assert_eq!(ast.get(7),&Node::from(LessThanExpr(Expr(4),Expr(6))));
    assert_eq!(ast.get(8),&Node::from(AssertStmt(Expr(7))));
}

#[test]
fn test_assert_12c() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i < /*nout*/ 0");
    assert_eq!(ast.get(4),&Node::from(VarExpr(Name(3))));
    assert_eq!(ast.get(5),&Node::from(BlockComment("/*nout*/".to_string())));
    assert_eq!(ast.get(6),&Node::from(IntExpr(0)));
    assert_eq!(ast.get(7),&Node::from(LessThanExpr(Expr(4),Expr(6))));
    assert_eq!(ast.get(8),&Node::from(AssertStmt(Expr(7))));
}

#[test]
fn test_assert_12d() {
    let ast = check_parse_error("function f(i32 i) -> ():\n assert i // nout\n    < 0");
}

#[test]
fn test_assert_12e() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i < //nout\n    0");
    assert_eq!(ast.get(4),&Node::from(VarExpr(Name(3))));
    assert_eq!(ast.get(5),&Node::from(LineComment("//nout".to_string())));
    assert_eq!(ast.get(6),&Node::from(IntExpr(0)));
    assert_eq!(ast.get(7),&Node::from(LessThanExpr(Expr(4),Expr(6))));
    assert_eq!(ast.get(8),&Node::from(AssertStmt(Expr(7))));
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
    let mut wf = WhileyFile::from_str(input);
    assert!(!wf.is_err());
    // Type input
    // let mut typer = TypeChecker::new(&mut ast, type_mapper);
    // let r = typer.check(d.unwrap());
    // assert!(!r.is_err());
    // Done
    wf.unwrap().ast
}

#[cfg(test)]
fn check_parse_error(input: &str) {
    let mut wf = WhileyFile::from_str(input);
    assert!(wf.is_err());
}

#[cfg(test)]
fn check_type_error(input: &str) {
    let mut wf = WhileyFile::from_str(input);
    assert!(!wf.is_err());
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
