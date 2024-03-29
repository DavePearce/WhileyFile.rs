use whiley_file::WhileyFile;
use whiley_file::ast::*;
use whiley_file::ast::decl::{Clause,Parameter};
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
    assert_eq!(ast.get(1),&Node::from(types::Bool{}));
}

#[test]
fn test_type_04() {
    let ast = check_parse("type nat is i8");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,8)));
}

#[test]
fn test_type_05() {
    let ast = check_parse("type nat is i16");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,16)));
}

#[test]
fn test_type_06() {
    let ast = check_parse("type nat is i32");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,32)));
}

#[test]
fn test_type_07() {
    let ast = check_parse("type nat is i64");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,64)));
}

#[test]
fn test_type_08() {
    let ast = check_parse("type nat is u8");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(false,8)));
}

#[test]
fn test_type_09() {
    let ast = check_parse("type nat is u16");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(false,16)));
}

#[test]
fn test_type_10() {
    let ast = check_parse("type nat is u32");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(false,32)));
}

#[test]
fn test_type_11() {
    let ast = check_parse("type nat is u64");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(false,64)));
}

#[test]
fn test_type_12() {
    let ast = check_parse("type nat is i32[]");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,32)));
    assert_eq!(ast.get(2),&Node::from(types::Array(Type(1))));
}

#[test]
fn test_type_13() {
    let ast = check_parse("type nat is i32[][]");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,32)));
    assert_eq!(ast.get(2),&Node::from(types::Array(Type(1))));
    assert_eq!(ast.get(3),&Node::from(types::Array(Type(2))));
}

#[test]
fn test_type_14() {
    let ast = check_parse("type ref is &int");
    check_name(ast.get(0),"ref");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,0)));
    assert_eq!(ast.get(2),&Node::from(types::Reference(Type(1))));
}

#[test]
fn test_type_15() {
    let ast = check_parse("type ref is &(&uint)");
    check_name(ast.get(0),"ref");
    assert_eq!(ast.get(1),&Node::from(types::Int(false,0)));
    assert_eq!(ast.get(2),&Node::from(types::Reference(Type(1))));
    assert_eq!(ast.get(3),&Node::from(types::Reference(Type(2))));
}

#[test]
fn test_type_16() {
    let ast = check_parse("type rec is {i64 f}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,64)));
    check_name(ast.get(2),"f");
    assert_eq!(ast.get(3),&Node::from(types::Record(vec![(Type(1),Name(2))])));
}

#[test]
fn test_type_17() {
    let ast = check_parse("type rec is {i32 f, u16 g}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,32)));
    check_name(ast.get(2),"f");
    assert_eq!(ast.get(3),&Node::from(types::Int(false,16)));
    check_name(ast.get(4),"g");
    assert_eq!(ast.get(5),&Node::from(types::Record(vec![(Type(1),Name(2)),(Type(3),Name(4))])));
}

#[test]
fn test_type_18() {
    let ast = check_parse("type rar is ((&u32)[] r)");
    check_name(ast.get(0),"rar");
    assert_eq!(ast.get(1),&Node::from(types::Int(false,32)));
    assert_eq!(ast.get(2),&Node::from(types::Reference(Type(1))));
    assert_eq!(ast.get(3),&Node::from(types::Array(Type(2))));
}

#[test]
fn test_type_19() {
    let ast = check_parse("type rec is {&i8 f, u16[] g}");
    check_name(ast.get(0),"rec");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,8)));
    assert_eq!(ast.get(2),&Node::from(types::Reference(Type(1))));
    check_name(ast.get(3),"f");
    assert_eq!(ast.get(4),&Node::from(types::Int(false,16)));
    assert_eq!(ast.get(5),&Node::from(types::Array(Type(4))));
    check_name(ast.get(6),"g");
    assert_eq!(ast.get(7),&Node::from(types::Record(vec![(Type(2),Name(3)),(Type(5),Name(6))])));
}

#[test]
fn test_type_20() {
    let ast = check_parse("type nat is i64 // line comment");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,64)));
    assert_eq!(ast.get(2),&Node::from(comment::Line("// line comment".to_string())));
}

#[test]
fn test_type_21() {
    let ast = check_parse("// line comment\ntype nat is i64");
    assert_eq!(ast.get(0),&Node::from(comment::Line("// line comment".to_string())));
    check_name(ast.get(1),"nat");
    assert_eq!(ast.get(2),&Node::from(types::Int(true,64)));
}

#[test]
fn test_type_22() {
    let ast = check_parse("type nat is i64 /* block comment */");
    check_name(ast.get(0),"nat");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,64)));
    assert_eq!(ast.get(2),&Node::from(comment::Block("/* block comment */".to_string())));
}

#[test]
fn test_type_23() {
    let ast = check_parse("/* block comment */\ntype nat is i64");
    assert_eq!(ast.get(0),&Node::from(comment::Block("/* block comment */".to_string())));
    check_name(ast.get(1),"nat");
    assert_eq!(ast.get(2),&Node::from(types::Int(true,64)));
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
fn test_function_0A() {
    let ast = check_parse("public function f()->() :\n skip");
}

#[test]
fn test_function_0B() {
    let ast = check_parse("private function f()->() :\n skip");
}

#[test]
fn test_function_0C() {
    let ast = check_parse("export function f()->() :\n skip");
}

#[test]
fn test_function_0D() {
    let ast = check_parse("export function f()->(int x):\n skip");
}

#[test]
fn test_function_0E() {
    let ast = check_parse("export function f()->int:\n skip");
}

#[test]
fn test_function_10() {
    let ast = check_parse("function f()->():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_11() {
    let ast = check_parse("function f() ->():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_12() {
    let ast = check_parse("function f()-> ():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(2))));
}


#[test]
fn test_function_13() {
    let ast = check_parse("function f() -> ():\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(3),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(2))));
}

#[test]
fn test_function_14() {
    let ast = check_parse("function f(i32 x):\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,32)));
    check_name(ast.get(2),"x");
    assert_eq!(ast.get(3),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(4),&Node::from(stmt::Block(vec![Stmt(3)])));
    let params = vec![Parameter{declared:Type(1),name:Name(2)}];
    assert_eq!(ast.get(5),&Node::from(decl::Function::new(vec![],Name(0),params,vec![],vec![],Stmt(4))));
}

#[test]
fn test_function_15() {
    let ast = check_parse("function f(i32 i, bool b) -> (bool r):\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(types::Int(true,32)));
    check_name(ast.get(2),"i");
    assert_eq!(ast.get(3),&Node::from(types::Bool()));
    check_name(ast.get(4),"b");
    assert_eq!(ast.get(5),&Node::from(types::Bool()));
    check_name(ast.get(6),"r");
    assert_eq!(ast.get(7),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(8),&Node::from(stmt::Block(vec![Stmt(7)])));
    let params = vec![Parameter{declared:Type(1),name:Name(2)},Parameter{declared:Type(3),name:Name(4)}];
    let returns = vec![Parameter{declared:Type(5),name:Name(6)}];
    assert_eq!(ast.get(9),&Node::from(decl::Function::new(vec![],Name(0),params,returns,vec![],Stmt(8))));
}

#[test]
fn test_function_16() {
    let ast = check_parse("function f()->(): // line comment\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(comment::Line("// line comment".to_string())));
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_17() {
    let ast = check_parse("// line comment\nfunction f()->():\n skip");
    assert_eq!(ast.get(0),&Node::from(comment::Line("// line comment".to_string())));
    check_name(ast.get(1),"f");
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(1),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_18() {
    let ast = check_parse("function f()->(): /* block comment */\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/* block comment */".to_string())));
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_19() {
    let ast = check_parse("/* block comment */\nfunction f()->():\n skip");
    assert_eq!(ast.get(0),&Node::from(comment::Block("/* block comment */".to_string())));
    check_name(ast.get(1),"f");
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(1),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_20() {
    let ast = check_parse("function f()->():\n // line comment\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(comment::Line("// line comment".to_string())));
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_21() {
    let ast = check_parse("function f()->():\n skip // line comment");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(comment::Line("// line comment".to_string())));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_22() {
    let ast = check_parse("function f()->():\n /** line comment */\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/** line comment */".to_string())));
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(2)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_23() {
    let ast = check_parse("function f()->():\n skip /* line comment */");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(comment::Block("/* line comment */".to_string())));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_24() {
    let ast = check_parse("function f()->():\n skip /* line\n comment */");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(comment::Block("/* line\n comment */".to_string())));
    assert_eq!(ast.get(3),&Node::from(stmt::Block(vec![Stmt(1)])));
    assert_eq!(ast.get(4),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(3))));
}

#[test]
fn test_function_25() {
    let ast = check_parse("function f()->():\n skip /* line\n comment */\n skip");
    check_name(ast.get(0),"f");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(comment::Block("/* line\n comment */".to_string())));
    assert_eq!(ast.get(3),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(4),&Node::from(stmt::Block(vec![Stmt(1),Stmt(3)])));
    assert_eq!(ast.get(5),&Node::from(decl::Function::new(vec![],Name(0),vec![],vec![],vec![],Stmt(4))));
}

#[test]
fn test_function_26() {
    let ast = check_parse("function f(i32 n)->() requires n < 0:\n skip");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(6),&Node::from(expr::Binary(BinOp::LessThan,Expr(4),Expr(5))));
    let params = vec![Parameter{declared:Type(1),name:Name(2)}];
    let clauses = vec![Clause::Requires(Expr(6))];
    assert_eq!(ast.get(9),&Node::from(decl::Function::new(vec![],Name(0),params,vec![],clauses,Stmt(8))));
}

#[test]
fn test_function_27() {
    let ast = check_parse("function f(int n)->() ensures n > 0:\n skip");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(6),&Node::from(expr::Binary(BinOp::GreaterThan,Expr(4),Expr(5))));
    let params = vec![Parameter{declared:Type(1),name:Name(2)}];
    let clauses = vec![Clause::Ensures(Expr(6))];
    assert_eq!(ast.get(9),&Node::from(decl::Function::new(vec![],Name(0),params,vec![],clauses,Stmt(8))));
}

#[test]
fn test_function_27b() {
    // Check string literals.
    let ast = check_parse("function f()->():\n int[] s = \"hello\"\n skip");
    assert_eq!(ast.get(4),&Node::from(expr::StringLiteral("\"hello\"".to_string())));
    assert_eq!(ast.get(6),&Node::from(stmt::Skip()));
}


#[test]
fn test_function_28() {
    check_parse("function f(i32 n)->()\n requires n < 0:\n skip");
}

#[test]
fn test_function_29() {
    check_parse_error("function f(i32 n)->()\n where n < 0:\n skip");
}

// ======================================================
// Tests (Method Declarations)
// ======================================================

#[test]
fn test_method_01() {
    check_parse("method f()->() :\n skip");
}

#[test]
fn test_method_02() {
    check_parse("public method f()->() :\n skip");
}

#[test]
fn test_method_03() {
    check_parse("export method f()->() :\n skip");
}

#[test]
fn test_method_04() {
    let ast = check_parse("method f(i32 n)->()\n requires n <= 0:\n skip");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(6),&Node::from(expr::Binary(BinOp::LessThanOrEquals,Expr(4),Expr(5))));
    let params = vec![Parameter{declared:Type(1),name:Name(2)}];
    let clauses = vec![Clause::Requires(Expr(6))];
    assert_eq!(ast.get(9),&Node::from(decl::Method::new(vec![],Name(0),params,vec![],clauses,Stmt(8))));
}

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
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
}

#[test]
fn test_skip_03() {
    let ast = check_parse("function f() -> ():\n skip\n skip");
    assert_eq!(ast.get(1),&Node::from(stmt::Skip()));
    assert_eq!(ast.get(2),&Node::from(stmt::Skip()));
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
fn test_assert_06a() {
    let ast = check_parse("function f() -> ():\n assert true");
    assert_eq!(ast.get(1),&Node::from(expr::BoolLiteral(true)));
    assert_eq!(ast.get(2),&Node::from(stmt::Assert(Expr(1))));
}

#[test]
fn test_assert_06b() {
    let ast = check_parse("function f() -> ():\n assert /* nothing */ true");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(expr::BoolLiteral(true)));
    assert_eq!(ast.get(3),&Node::from(stmt::Assert(Expr(2))));
}

#[test]
fn test_assert_06c() {
    let ast = check_parse("function f() -> ():\n assert // nothing\n    true");
    assert_eq!(ast.get(1),&Node::from(comment::Line("// nothing".to_string())));
    assert_eq!(ast.get(2),&Node::from(expr::BoolLiteral(true)));
    assert_eq!(ast.get(3),&Node::from(stmt::Assert(Expr(2))));
}

#[test]
fn test_assert_07() {
    let ast = check_parse("function f() -> ():\n assert false");
    assert_eq!(ast.get(1),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(2),&Node::from(stmt::Assert(Expr(1))));
}

#[test]
fn test_assert_07b() {
    let ast = check_parse("function f() -> ():\n assume false");
    assert_eq!(ast.get(1),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(2),&Node::from(stmt::Assume(Expr(1))));
}

#[test]
fn test_assert_08() {
    let ast = check_parse("function f() -> ():\n assert (false)");
    assert_eq!(ast.get(1),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(2),&Node::from(stmt::Assert(Expr(1))));
}

#[test]
fn test_assert_08b() {
    let ast = check_parse("function f() -> ():\n assert ( /* nothing */ false)");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(3),&Node::from(stmt::Assert(Expr(2))));
}

#[test]
fn test_assert_08c() {
    let ast = check_parse("function f() -> ():\n assert (/* nothing */ false)");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(3),&Node::from(stmt::Assert(Expr(2))));
}

#[test]
fn test_assert_08d() {
    let ast = check_parse("function f() -> ():\n assert ( /* nothing */false)");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(3),&Node::from(stmt::Assert(Expr(2))));
}

#[test]
fn test_assert_08e() {
    let ast = check_parse("function f() -> ():\n assert (/* nothing */false)");
    assert_eq!(ast.get(1),&Node::from(comment::Block("/* nothing */".to_string())));
    assert_eq!(ast.get(2),&Node::from(expr::BoolLiteral(false)));
    assert_eq!(ast.get(3),&Node::from(stmt::Assert(Expr(2))));
}

#[test]
fn test_assert_10() {
    let ast = check_type_error("function f() -> ():\n assert b");
}

#[test]
fn test_assert_11() {
    let ast = check_parse("function f(bool b) -> ():\n assert b");
    check_name(ast.get(2),"b");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(stmt::Assert(Expr(4))));
}

#[test]
fn test_assert_11b() {
    let ast = check_parse("function f(bool b) -> ():\n assume b");
    check_name(ast.get(2),"b");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(stmt::Assume(Expr(4))));
}

#[test]
fn test_assert_12() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i < 0");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(6),&Node::from(expr::Binary(BinOp::LessThan,Expr(4),Expr(5))));
    assert_eq!(ast.get(7),&Node::from(stmt::Assert(Expr(6))));
}

#[test]
fn test_assert_12b() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i /*nout*/ < 0");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(comment::Block("/*nout*/".to_string())));
    assert_eq!(ast.get(6),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(7),&Node::from(expr::Binary(BinOp::LessThan,Expr(4),Expr(6))));
    assert_eq!(ast.get(8),&Node::from(stmt::Assert(Expr(7))));
}

#[test]
fn test_assert_12c() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i < /*nout*/ 0");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(comment::Block("/*nout*/".to_string())));
    assert_eq!(ast.get(6),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(7),&Node::from(expr::Binary(BinOp::LessThan,Expr(4),Expr(6))));
    assert_eq!(ast.get(8),&Node::from(stmt::Assert(Expr(7))));
}

#[test]
fn test_assert_12d() {
    let ast = check_parse_error("function f(i32 i) -> ():\n assert i // nout\n    < 0");
}

#[test]
fn test_assert_12e() {
    let ast = check_parse("function f(i32 i) -> ():\n assert i < //nout\n    0");
    assert_eq!(ast.get(4),&Node::from(expr::VarAccess(Name(3))));
    assert_eq!(ast.get(5),&Node::from(comment::Line("//nout".to_string())));
    assert_eq!(ast.get(6),&Node::from(expr::IntLiteral(0)));
    assert_eq!(ast.get(7),&Node::from(expr::Binary(BinOp::LessThan,Expr(4),Expr(6))));
    assert_eq!(ast.get(8),&Node::from(stmt::Assert(Expr(7))));
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
