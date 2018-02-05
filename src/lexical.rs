use std::char;

/// State machine which converts source file text into tokens.
///
/// Implements translation phases 1-3.
pub struct Lex<'source> {
    current: Option<u8>,
    next: &'source [u8],
}

pub type Symbol = Vec<u8>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum PreprocessingToken {
    Identifier(Symbol),
    Number(Symbol),
    Character(CharEncoding, Symbol),
    String(StringEncoding, Symbol),
    Punctuator(Punctuator),
    Byte(u8),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Include {
    Bracket,
    Quote,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum CharEncoding {
    Char,
    Wide,
    Char16,
    Char32,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum StringEncoding {
    Char,
    Utf8,
    Wide,
    Char16,
    Char32,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Punctuator {
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Dot,
    Arrow,
    Comma,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusPlus,
    MinusMinus,

    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    ExclaimEqual,

    Tilde,
    Exclaim,
    And,
    Pipe,
    Caret,
    LessLess,
    GreaterGreater,
    AndAnd,
    PipePipe,

    Equal,
    StarEqual,
    SlashEqual,
    PercentEqual,
    PlusEqual,
    MinusEqual,
    LessLessEqual,
    GreaterGreaterEqual,
    AndEqual,
    CaretEqual,
    PipeEqual,

    Question,
    Colon,
    Semi,
    Elipsis,

    Hash,
    HashHash,
}

impl<'source> Lex<'source> {
    pub fn new(source: &'source [u8]) -> Self {
        let mut lex = Lex { current: None, next: source };
        lex.read_byte();
        lex
    }

    /// Read whitespace and comments from translation phase 3.
    ///
    /// Returns whether any newlines were encountered.
    pub fn whitespace_and_comments(&mut self) -> bool {
        let mut newlines = false;
        loop {
            let (byte, rest) = decode_byte(self.next);
            match self.current {
                Some(b) if is_whitespace(b) => {
                    if b == b'\n' {
                        newlines = true;
                    }
                    self.read_byte();
                }

                Some(b'/') if byte == Some(b'*') => {
                    self.skip_to(rest);
                    loop {
                        let (byte, rest) = decode_byte(self.next);
                        match (self.current, byte) {
                            (Some(b'*'), Some(b'/')) | (_, None) => {
                                self.skip_to(rest);
                                break;
                            }

                            _ => {
                                self.current = byte;
                                self.next = rest;
                            }
                        }
                    }
                }

                Some(b'/') if byte == Some(b'/') => {
                    self.skip_to(rest);
                    while ![Some(b'\n'), None].contains(&self.current) {
                        self.read_byte();
                    }
                }

                _ => break,
            }
        }

        newlines
    }

    /// Read a preprocessing token from translation phase 3.
    ///
    /// Does not include header names; does not skip whitespace or comments.
    pub fn preprocessing_token(&mut self) -> Option<PreprocessingToken> {
        use self::PreprocessingToken::*;

        let token = None
            .or_else(|| self.character_constant().map(|(e, s)| Character(e, s)))
            .or_else(|| self.string_literal().map(|(e, s)| String(e, s)))
            .or_else(|| self.identifier().map(Identifier))
            .or_else(|| self.pp_number().map(Number))
            .or_else(|| self.punctuator().map(Punctuator))
            .or_else(|| {
                let byte = self.current?;
                self.read_byte();
                Some(Byte(byte))
            })?;

        Some(token)
    }

    /// Read a header name preprocessing token from translation phase 3.
    pub fn header_name(&mut self) -> Option<(Include, Symbol)> {
        let (include, delimiter) = match self.current? {
            b'<' => (Include::Bracket, b'>'),
            b'"' => (Include::Quote, b'"'),
            _ => return None,
        };
        self.read_byte();

        let mut path = Symbol::new();
        while let Some(byte) = self.current {
            if [delimiter, b'\n'].contains(&byte) {
                break;
            }

            path.push(byte);

            self.read_byte();
        }

        match self.current {
            Some(b) if b == delimiter => self.read_byte(),
            _ => (),
        }

        Some((include, path))
    }

    /// Read a preprocessing identifer.
    ///
    /// This includes keywords; they are recognized later.
    fn identifier(&mut self) -> Option<Symbol> {
        let mut name = String::new();

        let start = self.identifier_nondigit()?;
        name.push(start);

        while let Some(character) = None
            .or_else(|| self.identifier_nondigit())
            .or_else(|| self.digit())
        {
            name.push(character);
        }

        Some(name.into_bytes())
    }

    fn identifier_nondigit(&mut self) -> Option<char> {
        let character = None
            .or_else(|| self.nondigit())
            .or_else(|| self.universal_character_name())?;

        Some(character)
    }

    fn nondigit(&mut self) -> Option<char> {
        let byte = self.current?;
        let nondigit = match byte {
            b'_' | b'a'..=b'z' | b'A'..=b'Z' => byte,
            _ => return None,
        };
        self.read_byte();

        Some(nondigit as char)
    }

    fn universal_character_name(&mut self) -> Option<char> {
        let (byte, mut next) = decode_byte(self.next);
        let len = match &[self.current?, byte?] {
            br"\u" => 4,
            br"\U" => 8,
            _ => return None,
        };

        let mut value = 0;
        for _ in 0..len {
            let (byte, rest) = decode_byte(next);
            let digit = byte.and_then(to_hexadecimal_digit)? as u32;
            next = rest;

            value <<= 4;
            value |= digit;
        }

        self.skip_to(next);
        // TODO: issue a diagnostic on values from the basic character set
        char::from_u32(value)
    }

    /// Read a preprocessing number.
    ///
    /// This does not recognize a lot of forms that are invalid later.
    fn pp_number(&mut self) -> Option<Symbol> {
        let mut name = String::new();

        let (byte, rest) = decode_byte(self.next);
        match (self.current?, byte) {
            (digit @ b'0'..=b'9', _) => {
                name.push(digit as char);

                self.current = byte;
                self.next = rest;
            }

            (b'.', Some(digit @ b'0'..=b'9')) => {
                name.push('.');
                name.push(digit as char);

                self.skip_to(rest)
            }

            _ => return None,
        }

        while let Some(character) = None
            .or_else(|| self.digit())
            .or_else(|| self.identifier_nondigit())
            .or_else(|| self.byte(b'.'))
        {
            name.push(character);

            if ['e', 'E', 'p', 'P'].contains(&character) {
                if let Some(sign) = None
                    .or_else(|| self.byte(b'+'))
                    .or_else(|| self.byte(b'-'))
                {
                    name.push(sign);
                }
            }
        }

        Some(name.into_bytes())
    }

    fn digit(&mut self) -> Option<char> {
        let byte = self.current?;
        let digit = match byte {
            b'0'..=b'9' => byte,
            _ => return None,
        };
        self.read_byte();

        Some(digit as char)
    }

    /// Read a character constant.
    ///
    /// This does not encode the character's execution value.
    fn character_constant(&mut self) -> Option<(CharEncoding, Symbol)> {
        let mut current = self.current;
        let mut next = self.next;

        let (byte, rest) = decode_byte(next);
        use self::CharEncoding::*;
        let encoding = match current? {
            b'L' => { current = byte; next = rest; Wide },
            b'u' => { current = byte; next = rest; Char16 },
            b'U' => { current = byte; next = rest; Char32 },
            _ => Char,
        };

        match current? {
            b'\'' => (),
            _ => return None,
        }

        self.skip_to(next);
        let symbol = self.char_sequence(b'\'');
        // TODO: issue a diagnostic on empty char literals

        match self.current {
            Some(b'\'') => self.read_byte(),
            _ => (),
        }

        Some((encoding, symbol))
    }

    /// Read a string literal.
    ///
    /// This does not encode the string in the execution character set.
    fn string_literal(&mut self) -> Option<(StringEncoding, Symbol)> {
        let mut current = self.current;
        let mut next = self.next;

        let (byte, rest) = decode_byte(next);
        use self::StringEncoding::*;
        let encoding = match current? {
            b'u' => {
                current = byte;
                next = rest;
                let (byte, rest) = decode_byte(next);
                if current? == b'8' {
                    current = byte;
                    next = rest;
                    Utf8
                } else {
                    Char16
                }
            }
            b'U' => { current = byte; next = rest; Char32 }
            b'L' => { current = byte; next = rest; Wide }
            _ => Char,
        };

        match current? {
            b'"' => (),
            _ => return None,
        }

        self.skip_to(next);
        let symbol = self.char_sequence(b'"');

        match self.current {
            Some(b'"') => self.read_byte(),
            _ => (),
        }

        Some((encoding, symbol))
    }

    fn char_sequence(&mut self, delimiter: u8) -> Symbol {
        let mut name = Symbol::new();

        while let Some(character) = self.current {
            if [delimiter, b'\n'].contains(&character) {
                break;
            }

            if let Some(character) = self.escape_sequence() {
                let start = name.len();
                let end = start + character.len_utf8();
                name.resize(end, 0);
                character.encode_utf8(&mut name[start..end]);
            } else {
                self.read_byte();

                name.push(character);
            }
        }

        name
    }

    fn escape_sequence(&mut self) -> Option<char> {
        if let Some(character) = self.universal_character_name() {
            return Some(character);
        }

        let (current, mut next) = match self.current? {
            b'\\' => decode_byte(self.next),
            _ => return None,
        };

        let character = match current? {
            b'\'' => { self.skip_to(next); '\'' }
            b'"' => { self.skip_to(next); '"' }
            b'?' => { self.skip_to(next); '?' }
            b'\\' => { self.skip_to(next); '\\' }
            b'a' => { self.skip_to(next); '\x06' }
            b'b' => { self.skip_to(next); '\x08' }
            b'f' => { self.skip_to(next); '\x0c' }
            b'n' => { self.skip_to(next); '\n' }
            b'r' => { self.skip_to(next); '\r' }
            b't' => { self.skip_to(next); '\t' }
            b'v' => { self.skip_to(next); '\x0b' }

            digit @ b'0'..=b'7' => {
                let mut value = digit - b'0';
                for _ in 1..3 {
                    let (byte, rest) = decode_byte(next);
                    let digit = match byte.and_then(to_octal_digit) {
                        Some(digit) => digit,
                        None => break,
                    };
                    next = rest;

                    value <<= 3;
                    value |= digit;
                }
                self.skip_to(next);

                value as char
            }

            b'x' => {
                let mut value = 0;
                loop {
                    let (byte, rest) = decode_byte(next);
                    let digit = match byte.and_then(to_hexadecimal_digit) {
                        Some(digit) => digit,
                        None => break,
                    };
                    next = rest;

                    value <<= 4;
                    value |= digit;
                }
                self.skip_to(next);

                // TODO: issue a diagnostic and skip incomplete hex escapes
                value as char
            }

            // TODO: issue a diagnostic on invalid escape sequences
            _ => return None,
        };

        Some(character)
    }

    /// Read a punctuator.
    ///
    /// TODO: preserve digraph spelling for the preprocessor.
    fn punctuator(&mut self) -> Option<Punctuator> {
        use self::Punctuator::*;

        let punctuator = match self.current {
            Some(b'[') => { self.read_byte(); LeftBracket }
            Some(b']') => { self.read_byte(); RightBracket }
            Some(b'(') => { self.read_byte(); LeftParen }
            Some(b')') => { self.read_byte(); RightParen }
            Some(b'{') => { self.read_byte(); LeftBrace }
            Some(b'}') => { self.read_byte(); RightBrace }

            Some(b'.') => {
                self.read_byte();
                let (byte, rest) = decode_byte(self.next);
                match (self.current, byte) {
                    (Some(b'.'), Some(b'.')) => { self.skip_to(rest); Elipsis }
                    _ => Dot,
                }
            },

            Some(b'+') => {
                self.read_byte();
                match self.current {
                    Some(b'+') => { self.read_byte(); PlusPlus }
                    Some(b'=') => { self.read_byte(); PlusEqual }
                    _ => Plus,
                }
            }
            Some(b'-') => {
                self.read_byte();
                match self.current {
                    Some(b'>') => { self.read_byte(); Arrow }
                    Some(b'-') => { self.read_byte(); MinusMinus }
                    Some(b'=') => { self.read_byte(); MinusEqual }
                    _ => Minus,
                }
            }
            Some(b'*') => {
                self.read_byte();
                match self.current {
                    Some(b'=') => { self.read_byte(); StarEqual }
                    _ => Star,
                }
            }
            Some(b'/') => {
                self.read_byte();
                match self.current {
                    Some(b'=') => { self.read_byte(); SlashEqual }
                    _ => Slash,
                }
            }
            Some(b'%') => {
                self.read_byte();
                match self.current {
                    Some(b'=') => { self.read_byte(); PercentEqual }
                    Some(b'>') => { self.read_byte(); RightBrace }
                    Some(b':') => {
                        self.read_byte();
                        let (byte, rest) = decode_byte(self.next);
                        match (self.current, byte) {
                            (Some(b'%'), Some(b':')) => { self.skip_to(rest); HashHash }
                            _ => { self.read_byte(); Hash }
                        }
                    }
                    _ => Percent,
                }
            }

            Some(b'<') => {
                self.read_byte();
                match self.current {
                    Some(b'<') => {
                        self.read_byte();
                        match self.current {
                            Some(b'=') => { self.read_byte(); LessLessEqual }
                            _ => LessLess,
                        }
                    }
                    Some(b'=') => { self.read_byte(); LessEqual }
                    Some(b':') => { self.read_byte(); LeftBracket }
                    Some(b'%') => { self.read_byte(); LeftBrace }
                    _ => Less,
                }
            }
            Some(b'>') => {
                self.read_byte();
                match self.current {
                    Some(b'>') => {
                        self.read_byte();
                        match self.current {
                            Some(b'=') => { self.read_byte(); GreaterGreaterEqual }
                            _ => GreaterGreater,
                        }
                    }
                    Some(b'=') => { self.read_byte(); GreaterEqual }
                    _ => Greater,
                }
            }
            Some(b'=') => {
                self.read_byte();
                match self.current {
                    Some(b'=') => { self.read_byte(); EqualEqual }
                    _ => Equal,
                }
            }

            Some(b'~') => { self.read_byte(); Tilde },
            Some(b'!') => {
                self.read_byte();
                match self.current {
                    Some(b'=') => { self.read_byte(); ExclaimEqual }
                    _ => Exclaim
                }
            },
            Some(b'&') => {
                self.read_byte();
                match self.current {
                    Some(b'&') => { self.read_byte(); AndAnd }
                    Some(b'=') => { self.read_byte(); AndEqual }
                    _ => And,
                }
            }
            Some(b'|') => {
                self.read_byte();
                match self.current {
                    Some(b'|') => { self.read_byte(); PipePipe }
                    Some(b'=') => { self.read_byte(); PipeEqual }
                    _ => Pipe,
                }
            }
            Some(b'^') => {
                self.read_byte();
                match self.current {
                    Some(b'=') => { self.read_byte(); CaretEqual }
                    _ => Caret,
                }
            }

            Some(b'?') => { self.read_byte(); Question }
            Some(b':') => {
                self.read_byte();
                match self.current {
                    Some(b'>') => { self.read_byte(); RightBracket }
                    _ => Colon,
                }
            }
            Some(b';') => { self.read_byte(); Semi }
            Some(b',') => { self.read_byte(); Comma }

            Some(b'#') => {
                self.read_byte();
                match self.current {
                    Some(b'#') => { self.read_byte(); HashHash }
                    _ => Hash,
                }
            }

            _ => return None,
        };

        Some(punctuator)
    }

    fn byte(&mut self, byte: u8) -> Option<char> {
        if self.current? != byte {
            return None;
        }

        self.read_byte();
        Some(byte as char)
    }

    fn skip_to(&mut self, rest: &'source [u8]) {
        self.next = rest;
        self.read_byte();
    }

    fn read_byte(&mut self) {
        let (current, rest) = decode_byte(self.next);
        self.current = current;
        self.next = rest;
    }
}

fn is_whitespace(b: u8) -> bool {
    [b' ', b'\t', b'\n', b'\x0b', b'\x0c'].contains(&b)
}

fn to_octal_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'7' => Some(b - b'0'),
        _ => None,
    }
}

fn to_hexadecimal_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(10 + b - b'a'),
        b'A'..=b'F' => Some(10 + b - b'A'),
        _ => None,
    }
}

/// Read a byte from translation phase 2.
///
/// This removes escaped newlines.
fn decode_byte<'source>(mut source: &'source [u8]) -> (Option<u8>, &'source [u8]) {
    loop {
        let (byte, next) = decode_multibyte(source);
        if let (Some(b'\\'), (Some(b'\n'), next)) = (byte, decode_multibyte(next)) {
            source = next;
            continue;
        }

        break (byte, next);
    }
}

/// Read a byte from translation phase 1.
///
/// This maps source file multibyte characters to the source character set and
/// replaces trigraphs with their single-character representations.
fn decode_multibyte<'source>(source: &'source [u8]) -> (Option<u8>, &'source [u8]) {
    match *source {
        [b'?', b'?', b'=', ref rest..] => (Some(b'#'), rest),
        [b'?', b'?', b')', ref rest..] => (Some(b']'), rest),
        [b'?', b'?', b'!', ref rest..] => (Some(b'|'), rest),
        [b'?', b'?', b'(', ref rest..] => (Some(b'['), rest),
        [b'?', b'?', b'\'', ref rest..] => (Some(b'^'), rest),
        [b'?', b'?', b'>', ref rest..] => (Some(b'}'), rest),
        [b'?', b'?', b'/', ref rest..] => (Some(b'\\'), rest),
        [b'?', b'?', b'<', ref rest..] => (Some(b'{'), rest),
        [b'?', b'?', b'-', ref rest..] => (Some(b'~'), rest),
        [b'\r', b'\n', ref rest..] => (Some(b'\n'), rest),
        [byte, ref rest..] => (Some(byte), rest),
        [ref rest..] => (None, rest),
    }
}

#[cfg(test)]
#[path = "lexical-test.rs"]
mod tests;
