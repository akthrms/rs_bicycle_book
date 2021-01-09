use std::cmp::{max, min};
use std::io::{self, BufRead, BufReader, Write};
use std::str::from_utf8;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Location(usize, usize);

impl Location {
    fn merge(&self, other: &Location) -> Self {
        Self(min(self.0, other.0), max(self.1, other.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Annotation<T> {
    value: T,
    location: Location,
}

impl<T> Annotation<T> {
    fn new(value: T, location: Location) -> Self {
        Self { value, location }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TokenKind {
    Number(u64),
    Plus,
    Minus,
    Asterisk,
    Slash,
    LParen,
    RParen,
}

type Token = Annotation<TokenKind>;

impl Token {
    fn number(n: u64, location: Location) -> Self {
        Self::new(TokenKind::Number(n), location)
    }

    fn plus(location: Location) -> Self {
        Self::new(TokenKind::Plus, location)
    }

    fn minus(location: Location) -> Self {
        Self::new(TokenKind::Minus, location)
    }

    fn asterisk(location: Location) -> Self {
        Self::new(TokenKind::Asterisk, location)
    }

    fn slash(location: Location) -> Self {
        Self::new(TokenKind::Slash, location)
    }

    fn l_paren(location: Location) -> Self {
        Self::new(TokenKind::LParen, location)
    }

    fn r_paren(location: Location) -> Self {
        Self::new(TokenKind::RParen, location)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LexErrorKind {
    InvalidChar(char),
    Eof,
}

type LexError = Annotation<LexErrorKind>;

impl LexError {
    fn invalid_char(c: char, location: Location) -> Self {
        LexError::new(LexErrorKind::InvalidChar(c), location)
    }

    fn eof(location: Location) -> Self {
        LexError::new(LexErrorKind::Eof, location)
    }
}

fn consume_byte(input: &[u8], position: usize, b: u8) -> Result<(u8, usize), LexError> {
    if input.len() <= position {
        return Err(LexError::eof(Location(position, position)));
    }
    if input[position] != b {
        return Err(LexError::invalid_char(
            input[position] as char,
            Location(position, position + 1),
        ));
    }
    Ok((b, position + 1))
}

fn lex_plus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'+').map(|(_, end)| (Token::plus(Location(start, end)), end))
}

fn lex_minus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'-').map(|(_, end)| (Token::minus(Location(start, end)), end))
}

fn lex_asterisk(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'*').map(|(_, end)| (Token::asterisk(Location(start, end)), end))
}

fn lex_slash(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'/').map(|(_, end)| (Token::slash(Location(start, end)), end))
}

fn lex_l_paren(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'(').map(|(_, end)| (Token::l_paren(Location(start, end)), end))
}

fn lex_r_paren(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b')').map(|(_, end)| (Token::r_paren(Location(start, end)), end))
}

fn recognize_many(input: &[u8], mut position: usize, mut f: impl FnMut(u8) -> bool) -> usize {
    while position < input.len() && f(input[position]) {
        position += 1;
    }
    position
}

fn lex_number(input: &[u8], position: usize) -> Result<(Token, usize), LexError> {
    let start = position;
    let end = recognize_many(input, start, |b| b"1234567890".contains(&b));
    let n = from_utf8(&input[start..end]).unwrap().parse().unwrap();
    Ok((Token::number(n, Location(start, end)), end))
}

fn skip_spaces(input: &[u8], position: usize) -> Result<((), usize), LexError> {
    let position = recognize_many(input, position, |b| b" \n\t".contains(&b));
    Ok(((), position))
}

fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let input = input.as_bytes();
    let mut position = 0;

    macro_rules! lex_a_token {
        ($lexer:expr) => {{
            let (t, p) = $lexer?;
            tokens.push(t);
            position = p;
        }};
    }

    while position < input.len() {
        match input[position] {
            b'0'..=b'9' => lex_a_token!(lex_number(input, position)),
            b'+' => lex_a_token!(lex_plus(input, position)),
            b'-' => lex_a_token!(lex_minus(input, position)),
            b'*' => lex_a_token!(lex_asterisk(input, position)),
            b'/' => lex_a_token!(lex_slash(input, position)),
            b'(' => lex_a_token!(lex_l_paren(input, position)),
            b')' => lex_a_token!(lex_r_paren(input, position)),
            b' ' | b'\n' | b'\t' => {
                let ((), p) = skip_spaces(input, position)?;
                position = p;
            }
            b => {
                return Err(LexError::invalid_char(
                    b as char,
                    Location(position, position + 1),
                ))
            }
        }
    }

    Ok(tokens)
}

#[test]
fn test_lexer() {
    assert_eq!(
        lex("1 + 2 * 3 - -10"),
        Ok(vec![
            Token::number(1, Location(0, 1)),
            Token::plus(Location(2, 3)),
            Token::number(2, Location(4, 5)),
            Token::asterisk(Location(6, 7)),
            Token::number(3, Location(8, 9)),
            Token::minus(Location(10, 11)),
            Token::minus(Location(12, 13)),
            Token::number(10, Location(13, 15)),
        ])
    )
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AstKind {
    Num(u64),
    UniOp {
        uni_op: UniOp,
        element: Box<Ast>,
    },
    BinOp {
        bin_op: BinOp,
        left: Box<Ast>,
        right: Box<Ast>,
    },
}

type Ast = Annotation<AstKind>;

impl Ast {
    fn num(n: u64, location: Location) -> Self {
        Self::new(AstKind::Num(n), location)
    }

    fn uni_op(uni_op: UniOp, element: Ast, location: Location) -> Self {
        Self::new(
            AstKind::UniOp {
                uni_op,
                element: Box::new(element),
            },
            location,
        )
    }

    fn bin_op(bin_op: BinOp, left: Ast, right: Ast, location: Location) -> Self {
        Self::new(
            AstKind::BinOp {
                bin_op,
                left: Box::new(left),
                right: Box::new(right),
            },
            location,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UniOpKind {
    Plus,
    Minus,
}

type UniOp = Annotation<UniOpKind>;

impl UniOp {
    fn plus(location: Location) -> Self {
        Self::new(UniOpKind::Plus, location)
    }

    fn minus(location: Location) -> Self {
        Self::new(UniOpKind::Minus, location)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

type BinOp = Annotation<BinOpKind>;

impl BinOp {
    fn add(location: Location) -> Self {
        Self::new(BinOpKind::Add, location)
    }

    fn sub(location: Location) -> Self {
        Self::new(BinOpKind::Sub, location)
    }

    fn mul(location: Location) -> Self {
        Self::new(BinOpKind::Mul, location)
    }

    fn div(location: Location) -> Self {
        Self::new(BinOpKind::Div, location)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ParseError {
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}

fn prompt(s: &str) -> io::Result<()> {
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    stdout.write(s.as_bytes())?;
    stdout.flush()
}

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdin = BufReader::new(stdin);
    let mut lines = stdin.lines();

    loop {
        prompt("> ").unwrap();
        if let Some(Ok(line)) = lines.next() {
            let token = lex(&line);
            println!("{:?}", token);
        } else {
            break;
        }
    }
}
