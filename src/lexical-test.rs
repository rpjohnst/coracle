use Lex;
use lexical::{self, Symbol, Include, CharEncoding, StringEncoding};

#[test]
fn preprocessing_token() {
    use lexical::PreprocessingToken::*;

    let mut lex = Lex::new(b"u8");
    assert_eq!(lex.preprocessing_token(), Some(Identifier(s("u8"))));

    let mut lex = Lex::new(b"123");
    assert_eq!(lex.preprocessing_token(), Some(Number(s("123"))));

    let mut lex = Lex::new(b"L'x'");
    assert_eq!(lex.preprocessing_token(), Some(Character(CharEncoding::Wide, s("x"))));

    let mut lex = Lex::new(br#"L"abc""#);
    assert_eq!(lex.preprocessing_token(), Some(String(StringEncoding::Wide, s("abc"))));

    let mut lex = Lex::new(b"+");
    assert_eq!(lex.preprocessing_token(), Some(Punctuator(lexical::Punctuator::Plus)));

    let mut lex = Lex::new(b"@");
    assert_eq!(lex.preprocessing_token(), Some(Byte(b'@')));

    let mut lex = Lex::new(b"");
    assert_eq!(lex.preprocessing_token(), None);

    fn s(name: &str) -> Symbol {
        Vec::from(name)
    }
}

#[test]
fn header_name() {
    let mut lex = Lex::new(b"");
    assert_eq!(lex.header_name(), None);

    // TODO: error on unterminated header names
    let mut lex = Lex::new(b"<x");
    assert_eq!(lex.header_name(), h(b"x"));
    assert_eq!(lex.current, None);

    // TODO: error on newline-terminated header names
    let mut lex = Lex::new(b"<y\n");
    assert_eq!(lex.header_name(), h(b"y"));
    assert_eq!(lex.current, Some(b'\n'));

    // TODO: error on empty header names
    let mut lex = Lex::new(b"<>");
    assert_eq!(lex.header_name(), h(b""));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""abc/def""#);
    assert_eq!(lex.header_name(), q(b"abc/def"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"<abc/def>");
    assert_eq!(lex.header_name(), h(b"abc/def"));
    assert_eq!(lex.current, None);

    fn h(name: &[u8]) -> Option<(Include, Symbol)> {
        Some((Include::Bracket, Vec::from(name)))
    }

    fn q(name: &[u8]) -> Option<(Include, Symbol)> {
        Some((Include::Quote, Vec::from(name)))
    }
}

#[test]
fn identifier() {
    let mut lex = Lex::new(b"");
    assert_eq!(lex.identifier(), None);

    let mut lex = Lex::new(b"123abc");
    assert_eq!(lex.identifier(), None);
    assert_eq!(lex.current, Some(b'1'));

    let mut lex = Lex::new(b"abc_123");
    assert_eq!(lex.identifier(), i("abc_123"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"abc!");
    assert_eq!(lex.identifier(), i("abc"));
    assert_eq!(lex.current, Some(b'!'));

    let mut lex = Lex::new(br"pok\u00e9mon");
    assert_eq!(lex.identifier(), i("pokémon"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"\U0001f642!");
    assert_eq!(lex.identifier(), i("🙂"));
    assert_eq!(lex.current, Some(b'!'));

    fn i(name: &str) -> Option<Symbol> {
        Some(Vec::from(name))
    }
}

#[test]
fn pp_number() {
    let mut lex = Lex::new(b"");
    assert_eq!(lex.pp_number(), None);

    let mut lex = Lex::new(b"abc");
    assert_eq!(lex.pp_number(), None);
    assert_eq!(lex.current, Some(b'a'));

    let mut lex = Lex::new(b".a");
    assert_eq!(lex.pp_number(), None);
    assert_eq!(lex.current, Some(b'.'));

    let mut lex = Lex::new(b"123");
    assert_eq!(lex.pp_number(), n(b"123"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b".3");
    assert_eq!(lex.pp_number(), n(b".3"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"5.");
    assert_eq!(lex.pp_number(), n(b"5."));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"8.13");
    assert_eq!(lex.pp_number(), n(b"8.13"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"6.2e-10");
    assert_eq!(lex.pp_number(), n(b"6.2e-10"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"3.e+");
    assert_eq!(lex.pp_number(), n(b"3.e+"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"5e");
    assert_eq!(lex.pp_number(), n(b"5e"));
    assert_eq!(lex.current, None);

    fn n(name: &[u8]) -> Option<Symbol> {
        Some(Vec::from(name))
    }
}

#[test]
fn character_constant() {
    let mut lex = Lex::new(b"");
    assert_eq!(lex.character_constant(), None);

    let mut lex = Lex::new(b"L");
    assert_eq!(lex.character_constant(), None);
    assert_eq!(lex.current, Some(b'L'));

    let mut lex = Lex::new(b"u");
    assert_eq!(lex.character_constant(), None);
    assert_eq!(lex.current, Some(b'u'));

    let mut lex = Lex::new(b"U");
    assert_eq!(lex.character_constant(), None);
    assert_eq!(lex.current, Some(b'U'));

    // TODO: error on empty character constants
    let mut lex = Lex::new(b"''");
    assert_eq!(lex.character_constant(), c(""));
    assert_eq!(lex.current, None);

    // TODO: error on unterminated character constants
    let mut lex = Lex::new(b"'x");
    assert_eq!(lex.character_constant(), c("x"));
    assert_eq!(lex.current, None);

    // TODO: error on newline-terminated character constants
    let mut lex = Lex::new(b"'y\n");
    assert_eq!(lex.character_constant(), c("y"));
    assert_eq!(lex.current, Some(b'\n'));

    // TODO: error on incomplete hex escapes
    let mut lex = Lex::new(br"'\x'");
    assert_eq!(lex.character_constant(), c("\0"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"'a'");
    assert_eq!(lex.character_constant(), c("a"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"L'a'");
    assert_eq!(lex.character_constant(), l("a"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"u'a'");
    assert_eq!(lex.character_constant(), c16("a"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(b"U'a'");
    assert_eq!(lex.character_constant(), c32("a"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"'\''");
    assert_eq!(lex.character_constant(), c("'"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"'\\'");
    assert_eq!(lex.character_constant(), c("\\"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"'\0'");
    assert_eq!(lex.character_constant(), c("\0"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"'\xff'");
    assert_eq!(lex.character_constant(), c("ÿ"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"U'\u00e9'");
    assert_eq!(lex.character_constant(), c32("é"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br"U'\U0001f642'");
    assert_eq!(lex.character_constant(), c32("🙂"));
    assert_eq!(lex.current, None);

    fn c(name: &str) -> Option<(CharEncoding, Symbol)> {
        Some((CharEncoding::Char, Vec::from(name)))
    }

    fn l(name: &str) -> Option<(CharEncoding, Symbol)> {
        Some((CharEncoding::Wide, Vec::from(name)))
    }

    fn c16(name: &str) -> Option<(CharEncoding, Symbol)> {
        Some((CharEncoding::Char16, Vec::from(name)))
    }

    fn c32(name: &str) -> Option<(CharEncoding, Symbol)> {
        Some((CharEncoding::Char32, Vec::from(name)))
    }
}

#[test]
fn string_literal() {
    let mut lex = Lex::new(b"");
    assert_eq!(lex.string_literal(), None);

    let mut lex = Lex::new(b"u8");
    assert_eq!(lex.string_literal(), None);
    assert_eq!(lex.current, Some(b'u'));

    let mut lex = Lex::new(b"u");
    assert_eq!(lex.string_literal(), None);
    assert_eq!(lex.current, Some(b'u'));

    let mut lex = Lex::new(b"U");
    assert_eq!(lex.string_literal(), None);
    assert_eq!(lex.current, Some(b'U'));

    let mut lex = Lex::new(b"L");
    assert_eq!(lex.string_literal(), None);
    assert_eq!(lex.current, Some(b'L'));

    // TODO: error on unterminated string literals
    let mut lex = Lex::new(br#""x"#);
    assert_eq!(lex.string_literal(), c("x"));
    assert_eq!(lex.current, None);

    // TODO: error on newline-terminated string literals
    let mut lex = Lex::new(b"\"y\n");
    assert_eq!(lex.string_literal(), c("y"));
    assert_eq!(lex.current, Some(b'\n'));

    // TODO: error on incomplete hex escapes
    let mut lex = Lex::new(br#""\x""#);
    assert_eq!(lex.string_literal(), c("\0"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""""#);
    assert_eq!(lex.string_literal(), c(""));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""abc def""#);
    assert_eq!(lex.string_literal(), c("abc def"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#"u8"123""#);
    assert_eq!(lex.string_literal(), u("123"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#"u"abc""#);
    assert_eq!(lex.string_literal(), c16("abc"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#"U"abc""#);
    assert_eq!(lex.string_literal(), c32("abc"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#"L"abc""#);
    assert_eq!(lex.string_literal(), l("abc"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""\"""#);
    assert_eq!(lex.string_literal(), c("\""));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""\\""#);
    assert_eq!(lex.string_literal(), c("\\"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""\0""#);
    assert_eq!(lex.string_literal(), c("\0"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#""\xff""#);
    assert_eq!(lex.string_literal(), c("ÿ"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#"u8"pok\u00e9mon""#);
    assert_eq!(lex.string_literal(), u("pokémon"));
    assert_eq!(lex.current, None);

    let mut lex = Lex::new(br#"U"\U0001f642""#);
    assert_eq!(lex.string_literal(), c32("🙂"));
    assert_eq!(lex.current, None);

    fn c(name: &str) -> Option<(StringEncoding, Symbol)> {
        Some((StringEncoding::Char, Vec::from(name)))
    }

    fn u(name: &str) -> Option<(StringEncoding, Symbol)> {
        Some((StringEncoding::Utf8, Vec::from(name)))
    }

    fn c16(name: &str) -> Option<(StringEncoding, Symbol)> {
        Some((StringEncoding::Char16, Vec::from(name)))
    }

    fn c32(name: &str) -> Option<(StringEncoding, Symbol)> {
        Some((StringEncoding::Char32, Vec::from(name)))
    }

    fn l(name: &str) -> Option<(StringEncoding, Symbol)> {
        Some((StringEncoding::Wide, Vec::from(name)))
    }
}
