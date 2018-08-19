#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(String),
    Ident(String),
    Int(String),
    Assign,    // =
    Plus,      // +
    Minus,     // -
    Bang,      // !
    Asterisk,  // *
    Slash,     // /
    Lt,        // <
    Gt,        // >
    Eq,        // ==
    NotEq,     // !=
    Comma,     // ,
    SemiColon, // ;
    Colon,     // :
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Str(String),
    Macro,      // macro
    EOF,
}