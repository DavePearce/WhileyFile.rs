pub use delta_inc::lex::{SnapError,Span};
use delta_inc::lex::{SnapResult,Scanner,TableTokenizer};
use delta_inc::lex;

// =================================================================
// Token
// =================================================================
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum Token {
    Ampersand,
    AmpersandAmpersand,
    Assert,
    Assume,
    Bar,
    BarBar,
    BlockComment,
    Bool,
    Break,
    Case,
    Character,
    Colon,
    Comma,
    Continue,
    Default,
    Do,
    Dot,
    Delete,
    Else,
    Ensures,
    EOF,
    Equals,
    EqualsEquals,
    Export,
    Fail,
    False,
    For,
    Final,
    Function,
    Gap,
    Identifier,
    Is,
    If,
    Int(u8),
    Integer,
    LeftAngle,
    LeftAngleEquals,
    LeftBrace,
    LeftCurly,
    LeftSquare,
    LineComment,
    Method,
    Minus,
    MinusGreater,
    Native,
    New,
    NewLine,
    Null,
    Percent,
    Plus,
    Private,
    Public,
    Return,
    Requires,
    RightAngle,
    RightAngleEquals,
    RightBrace,
    RightCurly,
    RightSlash,
    RightSlashSlash,
    RightSquare,
    Shreak,
    ShreakEquals,
    SemiColon,
    Skip,
    String,
    Switch,
    Star,
    True,
    Type,
    While,
    Uint(u8),
    Void,
    Where
}

// ======================================================
// Rules
// ======================================================

const ASSERT : &'static [char] = &['a','s','s','e','r','t'];
const ASSUME : &'static [char] = &['a','s','s','u','m','e'];
const BOOL : &'static [char] = &['b','o','o','l'];
const BREAK : &'static [char] = &['b','r','e','a','k'];
const CASE : &'static [char] = &['c','a','s','e'];
const CONTINUE : &'static [char] = &['c','o','n','t','i','n','u','e'];
const DEFAULT : &'static [char] = &['d','e','f','a','u','l','t'];
const DELETE : &'static [char] = &['d','e','l','e','t','e'];
const DO : &'static [char] = &['d','o'];
const ELSE : &'static [char] = &['e','l','s','e'];
const ENSURES : &'static [char] = &['e','n','s','u','r','e','s'];
const EXPORT : &'static [char] = &['e','x','p','o','r','t'];
const FAIL : &'static [char] = &['f','a','i','l'];
const FALSE : &'static [char] = &['f','a','l','s','e'];
const FOR : &'static [char] = &['f','o','r'];
const FINAL : &'static [char] = &['f','i','n','a','l'];
const FUNCTION : &'static [char] = &['f','u','n','c','t','i','o','n'];
const IF : &'static [char] = &['i','f'];
const IS : &'static [char] = &['i','s'];
const INT : &'static [char] = &['i','n','t'];
const I8 : &'static [char] = &['i','8'];
const I16 : &'static [char] = &['i','1','6'];
const I32 : &'static [char] = &['i','3','2'];
const I64 : &'static [char] = &['i','6','4'];
const METHOD : &'static [char] = &['m','e','t','h','o','d'];
const NEW : &'static [char] = &['n','e','w'];
const NULL : &'static [char] = &['n','u','l','l'];
const NATIVE : &'static [char] = &['n','a','t','i','v','e'];
const PRIVATE : &'static [char] = &['p','r','i','v','a','t','e'];
const PUBLIC : &'static [char] = &['p','u','b','l','i','c'];
const RETURN : &'static [char] = &['r','e','t','u','r','n'];
const REQUIRES : &'static [char] = &['r','e','q','u','i','r','e','s'];
const SKIP : &'static [char] = &['s','k','i','p'];
const SWITCH : &'static [char] = &['s','w','i','t','c','h'];
const TRUE : &'static [char] = &['t','r','u','e'];
const TYPE : &'static [char] = &['t','y','p','e'];
const WHILE : &'static [char] = &['w','h','i','l','e'];
const UINT : &'static [char] = &['u','i','n','t'];
const U8 : &'static [char] = &['u','8'];
const U16 : &'static [char] = &['u','1','6'];
const U32 : &'static [char] = &['u','3','2'];
const U64 : &'static [char] = &['u','6','4'];
const VOID : &'static [char] = &['v','o','i','d'];
const WHERE : &'static [char] = &['w','h','e','r','e'];

/// Handy type alias for the result type used for all of the lexical
/// rules.
type Result = std::result::Result<Span<Token>,()>;

/// Scan an (unsigned) integer literal.
fn scan_int_literal(input: &[char]) -> Result {
    scan_whilst(input, Token::Integer, |c| c.is_digit(10))
}

/// Scan a keyword, which is simple identifier matching a predefined
/// pattern.
fn scan_keyword(input: &[char]) -> Result {
    // Extract keyword identifier (if applicable)
    let r = scan_whilst(input, Token::Gap, |c| c.is_ascii_alphabetic())?;
    // Attempt to match it
    let t = match &input[r.range()] {
	ASSERT => Token::Assert,
	ASSUME => Token::Assume,
	BOOL => Token::Bool,
	BREAK => Token::Break,
	CASE => Token::Case,
	CONTINUE => Token::Continue,
	DEFAULT => Token::Default,
	DO => Token::Do,
	DELETE => Token::Delete,
	ELSE => Token::Else,
        ENSURES => Token::Ensures,
        EXPORT => Token::Export,
	FAIL => Token::Fail,
	FALSE => Token::False,
	FOR => Token::For,
        FINAL => Token::Final,
        FUNCTION => Token::Function,
        IF => Token::If,
        IS => Token::Is,
        INT => Token::Int(0),
	I8 => Token::Int(8),
	I16 => Token::Int(16),
	I32 => Token::Int(32),
	I64 => Token::Int(64),
        METHOD => Token::Method,
	NEW => Token::New,
	NULL => Token::Null,
	NATIVE => Token::Native,
	PRIVATE => Token::Private,
        PUBLIC => Token::Public,
	RETURN => Token::Return,
        REQUIRES => Token::Requires,
	SKIP => Token::Skip,
	SWITCH => Token::Switch,
	TRUE => Token::True,
	TYPE => Token::Type,
        WHILE => Token::While,
        UINT => Token::Uint(0),
	U8 => Token::Uint(8),
	U16 => Token::Uint(16),
	U32 => Token::Uint(32),
	U64 => Token::Uint(64),
	VOID => Token::Void,
	WHERE => Token::Where,
        _ => { return Err(()); }
    };
    // Success!
    Ok(Span::new(t,r.region.into()))
}

/// Scan an identifier which starts with an alpabetic character, or an
/// underscore and subsequently contains zero or more alpha-number
/// characters or underscores.
fn scan_identifier(input: &[char]) -> Result {
    if input.len() > 0 && is_identifier_start(input[0]) {
        scan_whilst(input, Token::Identifier, is_identifier_middle)
    } else {
        Err(())
    }
}

/// Scan all single-character operators.
fn scan_single_operators(input: &[char]) -> Result {
    if input.len() == 0 {
        Err(())
    } else {
        let t = match input[0] {
            '&' => Token::Ampersand,
            '|' => Token::Bar,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '.' => Token::Dot,
            '=' => Token::Equals,
            '<' => Token::LeftAngle,
            '(' => Token::LeftBrace,
            '{' => Token::LeftCurly,
            '[' => Token::LeftSquare,
            '-' => Token::Minus,
            '%' => Token::Percent,
            '+' => Token::Plus,
            '>' => Token::RightAngle,
            ')' => Token::RightBrace,
            '}' => Token::RightCurly,
            '/' => Token::RightSlash,
            ']' => Token::RightSquare,
            ';' => Token::SemiColon,
            '!' => Token::Shreak,
            '*' => Token::Star,
            _ => { return Err(()); }
        };
        //
        Ok(Span::new(t,0..1))
    }
}

/// Scan all double-character operators.
fn scan_double_operators(input: &[char]) -> Result {
    if input.len() <= 1 {
        Err(())
    } else {
        let t = match (input[0], input[1]) {
            ('&','&') => Token::AmpersandAmpersand,
            ('|','|') => Token::BarBar,
            ('=','=') => Token::EqualsEquals,
            ('<','=') => Token::LeftAngleEquals,
            ('-','>') => Token::MinusGreater,
            ('>','=') => Token::RightAngleEquals,
            ('!','=') => Token::ShreakEquals,
            _ => { return Err(()); }
        };
        //
        Ok(Span::new(t,0..2))
    }
}

// /// Scan contents of a character literal, whilst decoding any escaped characters.
// fn scan_character(&mut self, start: usize) -> Token<'a> {
//     let kind = TokenType::Character;
//     // Skip initial quote
//     self.chars.next();
//     let end = self.scan_whilst(|c| c != '\'');
//     // Skip final quote
//     self.chars.next();
//     let content = &self.input[start..end+1];
//     Token{kind,start,content}
// }

// /// Scan contents of a string, whilst decoding any escaped characters.
// fn scan_string(&mut self, start: usize) -> Token<'a> {
//     let kind = TokenType::String;
//     // Skip initial quote
//     self.chars.next();
//     let end = self.scan_whilst(|c| c != '\"');
//     // Skip final quote
//     self.chars.next();
//     let content = &self.input[start..end+1];
//     Token{kind,start,content}
// }

/// Scan a line comment which runs all the way until the end of the
/// line.  We can assume the comment start has already been matched.
fn scan_line_comment(input: &[char]) -> Result {
    if input.len() < 2 || input[0] != '/' || input[1] != '/' {
        Err(())
    } else {
        scan_whilst(input, Token::LineComment, |c| c != '\n')
    }
}

/// Scan a block comment which runs all the way until the comment
/// terminator is reached.  We can assume the comment start has
/// already been matched.
fn scan_block_comment(input: &[char]) -> Result {
    if input.len() < 2 || input[0] != '/' || input[1] != '*' {
        Err(())
    } else {
        let mut i = 3;
        while i < input.len() && !(input[i-1] == '*' && input[i] == '/') { i = i + 1; }
        if i == 3 {
            Err(())
        } else if i == input.len() {
            Ok(Span::new(Token::BlockComment, 0..i))
        } else {
            Ok(Span::new(Token::BlockComment, 0..i+1))
        }
    }
}

/// Determine whether a given character is the start of an identifier.
fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

/// Determine whether a given character can occur in the middle of an
/// identifier
fn is_identifier_middle(c: char) -> bool {
    c.is_digit(10) || is_identifier_start(c)
}

/// Scan a "gap" which is a sequence of zero or more tabs and spaces.
fn scan_gap(input: &[char]) -> Result {
    scan_whilst(input, Token::Gap, |c| c == ' ' || c == '\t')
}

fn scan_newline(input: &[char]) -> Result {
    scan_one(input,Token::NewLine,'\n')
}

/// If there is nothing left to scan, then we've reached the
/// End-Of-File.
fn scan_eof(input: &[char]) -> Result {
    if input.len() == 0 {
        Ok(Span::new(Token::EOF,0..0))
    } else {
        Err(())
    }
}

/// Helper which scans an item matching a given predicate.  If no
/// characters match, then it fails.
fn scan_whilst<P>(input: &[char], t: Token, pred: P) -> Result
where P: Fn(char) -> bool {
    let mut i = 0;
    // Continue whilst predicate matches
    while i < input.len() && pred(input[i]) { i = i + 1; }
    // Check what happened
    if i == 0 {
        // Nothing matched
        Err(())
    } else {
        // Something matched
        Ok(Span::new(t, 0..i))
    }
}

fn scan_one(input: &[char], t: Token, c: char) -> Result {
    if input.len() > 0 && input[0] == c {
        Ok(Span::new(t, 0..1))
    } else {
        Err(())
    }
}

/// The set of rules used for lexing.
static RULES : &'static [Scanner<char,Token>] = &[
    scan_line_comment,
    scan_block_comment,
    scan_double_operators,
    scan_single_operators,
    // scan_character,
    // scan_string
    scan_keyword,
    scan_identifier,
    scan_int_literal,
    scan_gap,
    scan_newline,
    scan_eof
];

// ======================================================
// Lexer
// ======================================================

pub struct Lexer {
    /// Internal lexer used for the heavy lifting.
    lexer: lex::Lexer<TableTokenizer<char,Token>>
}

impl Lexer {
    /// Construct a `Lexer` from a given string slice.
    pub fn new(input: &str) -> Lexer {
        let tokenizer = TableTokenizer::new(RULES.to_vec());
        let chars = input.chars().collect();
        Lexer{lexer:lex::Lexer::new(chars, tokenizer)}
    }

    /// Turn an integer token into a `i32`.  Observe that this will
    /// panic if the underlying characters of the token don't parse.
    pub fn get_int(&self, t: Span<Token>) -> u32 {
        // Sanity check this makes sense.
        assert!(t.kind == Token::Integer);
        // Extract characters from token.
        let chars = self.lexer.get(t);
        // Convert to string
        let s: String = chars.into_iter().collect();
        // Parse to i32
        s.parse().unwrap()
    }

    pub fn get_str(&self, t: Span<Token>) -> String {
        // Extract characters from token.
        let chars = self.lexer.get(t);
        // Convert to string
        chars.into_iter().collect()
    }

    pub fn get(&self, t: Span<Token>) -> &[char] {
        self.lexer.get(t)
    }

    /// Pass through request to underlying lexer
    pub fn is_eof(&self) -> bool { self.lexer.is_eof() }
    /// Pass through request to underlying lexer
    pub fn peek(&self) -> Span<Token> { self.lexer.peek() }
    /// Pass through request to underlying lexer
    pub fn snap(&mut self, kind : Token) -> SnapResult<Token> {
        self.lexer.snap(kind)
    }
    /// Pass through request to underlying lexer
    pub fn snap_any(&mut self, kinds : &[Token]) -> SnapResult<Token> {
        self.lexer.snap_any(kinds)
    }
}

// ======================================================
// Tests
// ======================================================


#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer,Token};

    /// Handy definition
    macro_rules! assert_ok {
        ($result:expr) => { assert!($result.is_ok()); };
    }

    #[test]
    fn test_01() {
        let mut l = Lexer::new("");
        assert!(l.peek().kind == Token::EOF);
        assert_ok!(l.snap(Token::EOF));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_02() {
        let mut l = Lexer::new(" ");
        assert!(l.peek().kind == Token::Gap);
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_03() {
        let mut l = Lexer::new("  ");
        assert!(l.peek().kind == Token::Gap);
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_04() {
        let mut l = Lexer::new("\n");
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_05() {
        let mut l = Lexer::new(" \n");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_06() {
        let mut l = Lexer::new("\n ");
        assert!(l.peek().kind == Token::NewLine);
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_07() {
        let mut l = Lexer::new("\t");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_08() {
        let mut l = Lexer::new("\t ");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_09() {
        let mut l = Lexer::new(" \t");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    // Literals

    #[test]
    fn test_10() {
        let mut l = Lexer::new("1");
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_11() {
        let mut l = Lexer::new("  1");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_12() {
        let mut l = Lexer::new("1234");
        assert!(l.get_int(l.peek()) == 1234);
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_13() {
        let mut l = Lexer::new("1234 ");
        assert!(l.get_int(l.peek()) == 1234);
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_14() {
        let mut l = Lexer::new("1234_");
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_15() {
        let mut l = Lexer::new("1234X");
        assert!(l.get_int(l.peek()) == 1234);
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_16() {
        let mut l = Lexer::new("1234 12");
        assert!(l.get_int(l.peek()) == 1234);
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::EOF));
    }

    // Identifiers

    #[test]
    fn test_20() {
        let mut l = Lexer::new("abc");
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_21() {
        let mut l = Lexer::new("  abc");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_22() {
        let mut l = Lexer::new("_abc");
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_23() {
        let mut l = Lexer::new("a_bD12233_");
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_24() {
        let mut l = Lexer::new("_abc cd");
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::Identifier));
        assert_ok!(l.snap(Token::EOF));
    }

    // Keywords

    #[test]
    fn test_30() {
        let mut l = Lexer::new("if");
        assert_ok!(l.snap(Token::If));
        assert_ok!(l.snap(Token::EOF));
    }


    #[test]
    fn test_31() {
        let mut l = Lexer::new("while");
        assert_ok!(l.snap(Token::While));
        assert_ok!(l.snap(Token::EOF));
    }

    // Operators

    #[test]
    fn test_40() {
        let mut l = Lexer::new("(");
        assert_ok!(l.snap(Token::LeftBrace));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_41() {
        let mut l = Lexer::new("((");
        assert_ok!(l.snap(Token::LeftBrace));
        assert_ok!(l.snap(Token::LeftBrace));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_42() {
        let mut l = Lexer::new(")");
        assert_ok!(l.snap(Token::RightBrace));
    }

    #[test]
    fn test_43() {
        let mut l = Lexer::new("))");
        assert_ok!(l.snap(Token::RightBrace));
        assert_ok!(l.snap(Token::RightBrace));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_44() {
        let mut l = Lexer::new("()");
        assert_ok!(l.snap(Token::LeftBrace));
        assert_ok!(l.snap(Token::RightBrace));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_45() {
        let mut l = Lexer::new("<=");
        assert_ok!(l.snap(Token::LeftAngleEquals));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_46() {
        let mut l = Lexer::new(">=");
        assert_ok!(l.snap(Token::RightAngleEquals));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_47() {
        let mut l = Lexer::new("==");
        assert_ok!(l.snap(Token::EqualsEquals));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_48() {
        let mut l = Lexer::new("!=");
        assert_ok!(l.snap(Token::ShreakEquals));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_49() {
        let mut l = Lexer::new("&&");
        assert_ok!(l.snap(Token::AmpersandAmpersand));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_4a() {
        let mut l = Lexer::new("||");
        assert_ok!(l.snap(Token::BarBar));
        assert_ok!(l.snap(Token::EOF));
    }

    // Comments

    #[test]
    fn test_50() {
        let mut l = Lexer::new("// hello world");
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_51() {
        let mut l = Lexer::new("// hello world\n");
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_52() {
        let mut l = Lexer::new("  // hello world\n");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_53() {
        let mut l = Lexer::new("  // hello world\n// another comment");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_54() {
        let mut l = Lexer::new("  /// hello world");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_55() {
        let mut l = Lexer::new("/*hello world*/");
        assert_ok!(l.snap(Token::BlockComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_56() {
        let mut l = Lexer::new("/*/\n");
        assert_ok!(l.snap(Token::BlockComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_57() {
        let mut l = Lexer::new("/*hello\n world*/");
        assert_ok!(l.snap(Token::BlockComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_58() {
        let mut l = Lexer::new(" /*hello world*/");
        assert_ok!(l.snap(Token::Gap));
        assert_ok!(l.snap(Token::BlockComment));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_59() {
        let mut l = Lexer::new("/*hello world*/\n// Hello");
        assert_ok!(l.snap(Token::BlockComment));
        assert_ok!(l.snap(Token::NewLine));
        assert_ok!(l.snap(Token::LineComment));
        assert_ok!(l.snap(Token::EOF));
    }

    // Combinations

    #[test]
    fn test_60() {
        let mut l = Lexer::new("while(");
        assert_ok!(l.snap(Token::While));
        assert_ok!(l.snap(Token::LeftBrace));
        assert_ok!(l.snap(Token::EOF));
    }

    #[test]
    fn test_61() {
        let mut l = Lexer::new("12345(");
        assert_ok!(l.snap(Token::Integer));
        assert_ok!(l.snap(Token::LeftBrace));
        assert_ok!(l.snap(Token::EOF));
    }
}
