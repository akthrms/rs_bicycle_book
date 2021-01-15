use std::cmp::{max, min};
use std::error::Error as StdError;
use std::fmt;
use std::io::{self, BufRead, BufReader, Write};
use std::iter::Peekable;
use std::str::from_utf8;
use std::str::FromStr;

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
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}

fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    parse_expr3(tokens)
}

fn parse_left_bin_op<Tokens>(
    tokens: &mut Peekable<Tokens>,
    sub_expr_parser: fn(&mut Peekable<Tokens>) -> Result<Ast, ParseError>,
    bin_op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp, ParseError>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut expr = sub_expr_parser(tokens)?;

    loop {
        match tokens.peek() {
            Some(_) => {
                let bin_op = match bin_op_parser(tokens) {
                    Ok(bin_op) => bin_op,
                    Err(_) => break,
                };
                let right = sub_expr_parser(tokens)?;
                let location = expr.location.merge(&right.location);
                expr = Ast::bin_op(bin_op, expr, right, location);
            }
            _ => break,
        }
    }

    Ok(expr)
}

fn parse_expr3<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr3_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let bin_op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Plus => Ok(BinOp::add(token.location.clone())),
                TokenKind::Minus => Ok(BinOp::sub(token.location.clone())),
                _ => Err(ParseError::NotOperator(token.clone())),
            })?;
        tokens.next();
        Ok(bin_op)
    }

    parse_left_bin_op(tokens, parse_expr2, parse_expr3_op)
}

fn parse_expr2<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr2_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let bin_op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Asterisk => Ok(BinOp::mul(token.location.clone())),
                TokenKind::Slash => Ok(BinOp::div(token.location.clone())),
                _ => Err(ParseError::NotOperator(token.clone())),
            })?;
        tokens.next();
        Ok(bin_op)
    }
    parse_left_bin_op(tokens, parse_expr1, parse_expr2_op)
}

fn parse_expr1<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|token| token.value) {
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
            let uni_op = match tokens.next() {
                Some(Token {
                    value: TokenKind::Plus,
                    location,
                }) => UniOp::plus(location),
                Some(Token {
                    value: TokenKind::Minus,
                    location,
                }) => UniOp::minus(location),
                _ => unreachable!(),
            };
            let expr = parse_atom(tokens)?;
            let location = uni_op.location.merge(&expr.location);
            Ok(Ast::uni_op(uni_op, expr, location))
        }
        _ => parse_atom(tokens),
    }
}

fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|token| match token.value {
            TokenKind::Number(n) => Ok(Ast::num(n, token.location)),
            TokenKind::LParen => {
                let expr = parse_expr(tokens)?;
                match tokens.next() {
                    Some(Token {
                        value: TokenKind::RParen,
                        ..
                    }) => Ok(expr),
                    Some(t) => Err(ParseError::RedundantExpression(t)),
                    _ => Err(ParseError::UnclosedOpenParen(token)),
                }
            }
            _ => Err(ParseError::NotExpression(token)),
        })
}

fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let ast = parse_expr(&mut tokens)?;
    match tokens.next() {
        Some(token) => Err(ParseError::RedundantExpression(token)),
        None => Ok(ast),
    }
}

#[test]
fn test_parser() {
    let ast = parse(vec![
        Token::number(1, Location(0, 1)),
        Token::plus(Location(2, 3)),
        Token::number(2, Location(4, 5)),
        Token::asterisk(Location(6, 7)),
        Token::number(3, Location(8, 9)),
        Token::minus(Location(10, 11)),
        Token::minus(Location(12, 13)),
        Token::number(10, Location(13, 15)),
    ]);
    assert_eq!(
        ast,
        Ok(Ast::bin_op(
            BinOp::sub(Location(10, 11)),
            Ast::bin_op(
                BinOp::add(Location(2, 3)),
                Ast::num(1, Location(0, 1)),
                Ast::bin_op(
                    BinOp::mul(Location(6, 7)),
                    Ast::num(2, Location(4, 5)),
                    Ast::num(3, Location(8, 9)),
                    Location(4, 9)
                ),
                Location(0, 9)
            ),
            Ast::uni_op(
                UniOp::minus(Location(12, 13)),
                Ast::num(10, Location(13, 15)),
                Location(12, 15)
            ),
            Location(0, 15)
        ))
    )
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Error {
    Lexer(LexError),
    Parser(ParseError),
}

impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error::Lexer(e)
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parser(e)
    }
}

impl FromStr for Ast {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = lex(s)?;
        let ast = parse(tokens)?;
        Ok(ast)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Number(n) => n.fmt(f),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let location = &self.location;
        match &self.value {
            LexErrorKind::InvalidChar(c) => write!(f, "{}: invalid char '{}'", location, c),
            LexErrorKind::Eof => write!(f, "end of file"),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::NotExpression(token) => write!(
                f,
                "{}: {} is not a start of expression",
                token.location, token.value
            ),
            ParseError::NotOperator(token) => {
                write!(f, "{}: {} is not an operator", token.location, token.value)
            }
            ParseError::UnclosedOpenParen(token) => {
                write!(f, "{}: {} is not closed", token.location, token.value)
            }
            ParseError::RedundantExpression(token) => write!(
                f,
                "{}: expression after {} is redundant",
                token.location, token.value
            ),
            ParseError::Eof => write!(f, "End of file"),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parser error")
    }
}

impl StdError for LexError {}

impl StdError for ParseError {}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::Lexer(e) => Some(e),
            Error::Parser(e) => Some(e),
        }
    }
}

fn print_annotation(input: &str, location: Location) {
    eprintln!("{}", input);
    eprintln!(
        "{}{}",
        " ".repeat(location.0),
        "^".repeat(location.1 - location.0)
    );
}

impl Error {
    fn show_diagnostic(&self, input: &str) {
        let (e, location): (&dyn StdError, Location) = match self {
            Error::Lexer(e) => (e, e.location.clone()),
            Error::Parser(e) => {
                let location = match e {
                    ParseError::NotExpression(Token { location, .. })
                    | ParseError::NotOperator(Token { location, .. })
                    | ParseError::UnclosedOpenParen(Token { location, .. }) => location.clone(),
                    ParseError::RedundantExpression(Token { location, .. }) => {
                        Location(location.0, input.len())
                    }
                    ParseError::Eof => Location(input.len(), input.len() + 1),
                };
                (e, location)
            }
        };
        eprintln!("{}", e);
        print_annotation(input, location);
    }
}

fn show_trace<E: StdError>(e: E) {
    eprintln!("{}", e);
    let mut source = e.source();
    while let Some(e) = source {
        eprintln!("caused by {}", e);
        source = e.source();
    }
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
            let ast = match line.parse::<Ast>() {
                Ok(ast) => ast,
                Err(e) => {
                    e.show_diagnostic(&line);
                    show_trace(e);
                    continue;
                }
            };
            println!("{:?}", ast);
        } else {
            break;
        }
    }
}
