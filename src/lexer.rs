use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, PartialEq, Logos, Hash, Eq, PartialOrd, Ord, FromPrimitive, ToPrimitive)]
pub(crate) enum SyntaxKind {
	#[regex(" +")]
	Whitespace,

	#[token("fun")]
	FunKw,

	#[token("var")]
	VarKw,

	#[token("const")]
	ConstKw,

	#[regex("[A-Za-z][A-Za-z0-9]*")]
	Ident,

	#[regex("[0-9]+")]
	Number,

	#[token("+")]
	Plus,

	#[token("-")]
	Minus,
	
	#[token("*")]
	Asterisk,
	
	#[token("/")]
	Slash,

	#[token("=")]
	Equals,

	#[token("(")]
	LParen,

	#[token(")")]
	RParen,

	#[token("{")]
	LBrace,

	#[token("}")]
	RBrace,

	#[error]
	Error,

	Root,
	BinaryExpr,
	PrefixExpr,
}

pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
}

impl<'a> Lexer<'a> {
	pub(crate) fn new(input: &'a str) -> Self {
		Self {
			inner: SyntaxKind::lexer(input)
		}
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = (SyntaxKind, &'a str);

	fn next(&mut self) -> Option<Self::Item> {
		let kind = self.inner.next()?;
		let text = self.inner.slice();

		Some((kind, text))
	}
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: SyntaxKind) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.next(), Some((kind, input)));
    }

    #[test]
    fn lex_spaces() {
        check("   ", SyntaxKind::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check("fun", SyntaxKind::FunKw);
    }

	#[test]
    fn lex_left_parenthesis() {
        check("(", SyntaxKind::LParen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check(")", SyntaxKind::RParen);
    }

    #[test]
    fn lex_var_keyword() {
        check("var", SyntaxKind::VarKw);
    }

	#[test]
	fn lex_const_keyword() {
        check("const", SyntaxKind::ConstKw);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check("abcd", SyntaxKind::Ident);
    }

	#[test]
	fn lex_alphabetic_single_identifier() {
        check("a", SyntaxKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check("ab123cde456", SyntaxKind::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check("ABCdef", SyntaxKind::Ident);
    }

    #[test]
    fn lex_number() {
        check("123456", SyntaxKind::Number);
    }

    #[test]
    fn lex_plus() {
        check("+", SyntaxKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", SyntaxKind::Minus);
    }

    #[test]
    fn lex_star() {
        check("*", SyntaxKind::Asterisk);
    }

    #[test]
    fn lex_slash() {
        check("/", SyntaxKind::Slash);
    }

    #[test]
    fn lex_equals() {
        check("=", SyntaxKind::Equals);
    }

    #[test]
    fn lex_left_brace() {
        check("{", SyntaxKind::LBrace);
    }

    #[test]
    fn lex_right_brace() {
        check("}", SyntaxKind::RBrace);
    }
}