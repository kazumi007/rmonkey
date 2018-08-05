use token::Token;

pub struct Lexer {
    buf: Vec<char>,
    pos: usize,
    read_pos: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn with_string(str: &str) -> Lexer {
        let mut l = Lexer {
            buf: str.chars().collect(),
            pos: 0,
            read_pos: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.buf.len() {
            self.ch = None
        } else {
            self.ch = Some(self.buf[self.read_pos]);
            self.pos = self.read_pos;
            self.read_pos += 1
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some(ch) => match ch {
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::Eq
                    }
                    _ => Token::Assign,
                },
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '[' => Token::LBracket,
                ']' => Token::RBracket,
                ';' => Token::SemiColon,
                ':' => Token::Colon,
                ',' => Token::Comma,
                '!' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::NotEq
                    }
                    _ => Token::Bang,
                },
                '<' => Token::Lt,
                '>' => Token::Gt,
                '"' => {
                    let str1 = self.read_str();
                    Token::Str(str1)
                }
                ch if Lexer::is_letter(ch) => {
                    let identifier = self.read_identifier();
                    return self.lookup_ident(identifier);
                }
                ch if Lexer::is_digit(ch) => {
                    let number = self.read_number();
                    return Token::Int(number);
                }
                _ => Token::Illegal(ch.to_string()),
            },
            None => Token::EOF,
        };
        self.read_char();
        token
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            match ch {
                ch if ch.is_ascii_whitespace() => self.read_char(),
                _ => break,
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;
        let mut len = 0;
        while let Some(ch) = self.ch {
            if Lexer::is_letter(ch) || Lexer::is_digit(ch) {
                len += 1;
                self.read_char()
            } else {
                break;
            }
        }
        self.buf[pos..(pos+len)].into_iter().collect()
    }

    fn read_number(&mut self) -> String {
        let pos = self.pos;
        let mut len = 0;
        while let Some(ch) = self.ch {
            if Lexer::is_digit(ch) {
                len += 1;
                self.read_char()
            } else {
                break;
            }
        }
        self.buf[pos..(pos+len)].into_iter().collect()
    }

    fn read_str(&mut self) -> String {
        let pos = self.pos + 1;
        self.read_char();
        let mut len = 0;
        loop {
            match self.ch {
                Some('"') | None => break,
                _ => {
                    self.read_char();
                    len += 1;
                }
            }
        }
        self.buf[pos..(pos + len)].into_iter().collect()
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_pos >= self.buf.len() {
            None
        } else {
            Some(self.buf[self.read_pos])
        }
    }

    fn lookup_ident(&self, identifier: String) -> Token {
        match identifier.as_str() {
            "let" => Token::Let,
            "fn" => Token::Function,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(identifier),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){};";
        let expected = [
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::SemiColon,
            Token::EOF,
        ];

        let mut l = Lexer::with_string(input);
        for t in expected.iter() {
            let token = l.next_token();
            assert_eq!(t, &token);
        }
    }

    #[test]
    fn test_next_token2() {
        let input = "
!10
let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-*/5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
";
        let expected = [
            Token::Bang,
            Token::Int("10".to_string()),
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::SemiColon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::SemiColon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::SemiColon,
            Token::Bang,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Int("5".to_string()),
            Token::SemiColon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::SemiColon,
            Token::If,
            Token::LParen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::SemiColon,
            Token::RBrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::SemiColon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::SemiColon,
            Token::Str("foobar".to_string()),
            Token::Str("foo bar".to_string()),
            Token::LBracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::RBracket,
            Token::SemiColon,
            Token::LBrace,
            Token::Str("foo".to_string()),
            Token::Colon,
            Token::Str("bar".to_string()),
            Token::RBrace,
            Token::EOF,
        ];

        let mut l = Lexer::with_string(input);
        for t in expected.iter() {
            let token = l.next_token();
            assert_eq!(t, &token);
        }
    }
}
