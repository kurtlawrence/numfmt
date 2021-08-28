use super::*;
use Token::*;

pub fn parse_formatter(s: &str) -> std::result::Result<Formatter, ParseError> {
    // follows grammar prefix[[.#|~#][%|s|b|n][/<char>]]suffix
    // has 3 iteration phases
    // 1. Prefix
    // 2. Number
    // 3. Suffix
    if s.is_empty() {
        return Ok(Formatter::default());
    }

    let tokens = {
        let mut tokens: Vec<(Token, Option<Token>)> =
            s.chars().map(Token::recognise).map(|t| (t, None)).collect();
        for i in 1..tokens.len() {
            tokens[i - 1].1 = Some(tokens[i].0);
        }
        tokens
    };

    let mut iter = tokens.into_iter();

    // Prefix Phase
    let prefix = {
        let mut prefix = String::new();
        while let Some((token, next)) = iter.next() {
            match token {
                OpenBracket if next == Some(OpenBracket) => {
                    prefix.push('['); // part of prefix
                    iter.next(); // skip next
                }
                OpenBracket => break, // start num phase
                CloseBracket if next == Some(CloseBracket) => {
                    prefix.push(']'); // part of prefix
                    iter.next(); // skip next
                }
                CloseBracket => return Err(ParseError::OutOfPlace(']')),
                tt => prefix.push(tt.as_char()),
            }
        }
        prefix
    };

    // Number Phase
    let mut scaler = None;
    let mut no_precision = false;
    let mut use_dec = false;
    let mut prec = None;
    let mut digits = false;
    let mut sep = Some(',');

    while let Some((token, next)) = iter.next() {
        let next_is_digit = next.map(|t| matches!(t, Digit(_))).unwrap_or_default();

        match token {
            Digit(d) if digits => {
                prec = if let Some(p) = prec {
                    if p >= 26 || (p == 25 && d >= 6) {
                        return Err(ParseError::PrecisionTooLarge);
                    } else {
                        Some(p * 10 + d)
                    }
                } else {
                    Some(d)
                };
                digits = next_is_digit;
            }
            Period if next == Some(Char('*')) => {
                no_precision = true;
                iter.next();
            }
            Tilde | Period if !next_is_digit => {
                return Err(ParseError::Unexp(
                    "digit",
                    next.map(|t| t.as_char()).unwrap_or(' '),
                ))
            }
            Tilde | Period if no_precision => return Err(ParseError::DupPrec(no_precision, 0)),
            Tilde | Period if prec.is_some() => {
                return Err(ParseError::DupPrec(no_precision, prec.unwrap()))
            }
            Period => {
                digits = true;
                use_dec = true;
            }
            Tilde => digits = true,
            // Single Char Matches
            Char('%') if scaler.is_none() => scaler = Some(Scaler::Percent),
            Char('s') if scaler.is_none() => scaler = Some(Scaler::Short),
            Char('m') if scaler.is_none() => scaler = Some(Scaler::Metric),
            Char('b') if scaler.is_none() => scaler = Some(Scaler::Binary),
            Char('n') if scaler.is_none() => scaler = Some(Scaler::No),
            Char('%') | Char('s') | Char('b') | Char('n') if scaler.is_some() => {
                return Err(ParseError::DupScaler(token.as_char()))
            }
            Slash if next == Some(CloseBracket) => sep = None,
            Slash => {
                sep = next.map(|t| t.as_char());
                iter.next();
            }
            CloseBracket => break,
            tt => return Err(ParseError::Unexp("., ~, %, s, m, b, n, /, ]", tt.as_char())),
        }
    }

    // Suffix Phase
    let mut suffix: String = iter
        .filter_map(|x| match x {
            (OpenBracket, Some(OpenBracket)) => None,
            (CloseBracket, Some(CloseBracket)) => None,
            (tt, _) => Some(tt.as_char()),
        })
        .collect();

    // Create Formatter
    let mut f = match scaler {
        Some(Scaler::Percent) => {
            suffix.insert(0, '%');
            Formatter::percentage()
        }
        Some(Scaler::Short) => Formatter::default().scales(Scales::short()),
        Some(Scaler::Metric) => Formatter::default().scales(Scales::metric()),
        Some(Scaler::Binary) => Formatter::default().scales(Scales::binary()),
        Some(Scaler::No) => Formatter::default().scales(Scales::none()),
        None => Formatter::default(),
    };

    f = f.separator(sep)?.prefix(&prefix)?.suffix(&suffix)?;

    if no_precision {
        f = f.precision(Unspecified);
    } else if let Some(prec) = prec {
        f = f.precision(if use_dec {
            Decimals(prec)
        } else {
            Significance(prec)
        });
    }

    Ok(f)
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Token {
    OpenBracket,
    CloseBracket,
    Period,
    Tilde,
    Slash,
    Digit(u8),
    Char(char),
}

impl Token {
    fn recognise(ch: char) -> Self {
        match ch {
            '[' => OpenBracket,
            ']' => CloseBracket,
            '.' => Period,
            '~' => Tilde,
            '/' => Slash,
            x if x.is_ascii_digit() => Digit(x.to_digit(10).unwrap() as u8),
            x => Char(x),
        }
    }

    fn as_char(&self) -> char {
        match self {
            OpenBracket => '[',
            CloseBracket => ']',
            Period => '.',
            Tilde => '~',
            Slash => '/',
            Digit(d) => (d + 48).into(), // checked in tests
            Char(ch) => *ch,
        }
    }
}

enum Scaler {
    Percent,
    Short,
    Metric,
    Binary,
    No,
}

/// Errors that occur when parsing a formatting string.
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub enum ParseError {
    FormatterError(Error),
    OutOfPlace(char),
    Unexp(&'static str, char),
    PrecisionTooLarge,
    DupScaler(char),
    DupPrec(bool, u8),
}

impl From<Error> for ParseError {
    fn from(err: Error) -> Self {
        ParseError::FormatterError(err)
    }
}

impl error::Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::FormatterError(e) => write!(f, "formatter error: {}", e),
            ParseError::OutOfPlace(ch) => write!(
                f,
                "the character `{}` was found when parser was in prefix state",
                ch
            ),
            ParseError::Unexp(exp, found) => write!(
                f,
                "unexpected character. expected a {} but found '{}'",
                exp, found
            ),
            ParseError::PrecisionTooLarge => write!(f, "precision is larger than 255"),
            ParseError::DupScaler(ch) => write!(
                f,
                "a scaler has already been set and '{}' can not override",
                ch
            ),
            ParseError::DupPrec(no, n) => {
                if *no {
                    write!(f, "unspecified precision has already been set")
                } else {
                    write!(f, "precision of {} has already been set", n)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing_empty_num_formatter() {
        let p = parse_formatter;
        let r = p("");
        assert_eq!(r, Ok(Formatter::default()));

        let r = p("a prefix");
        assert_eq!(r, Ok(Formatter::default().prefix("a prefix").unwrap()));

        let r = p("[]a suffix");
        assert_eq!(r, Ok(Formatter::default().suffix("a suffix").unwrap()));

        let r = p("a prefix[]a suffix");
        assert_eq!(
            r,
            Ok(Formatter::default()
                .prefix("a prefix")
                .unwrap()
                .suffix("a suffix")
                .unwrap())
        );

        let r = p("a[[p]]refix[]a[[s]]uffix");
        assert_eq!(
            r,
            Ok(Formatter::default()
                .prefix("a[p]refix")
                .unwrap()
                .suffix("a[s]uffix")
                .unwrap())
        );
    }

    #[test]
    fn parsing_percentage() {
        let p = parse_formatter;
        let r = p("[%]");
        assert_eq!(r, Ok(Formatter::percentage().separator(',').unwrap()));

        let r = p("prefix [%] suffix");
        assert_eq!(
            r,
            Ok(Formatter::percentage()
                .separator(',')
                .unwrap()
                .prefix("prefix ")
                .unwrap()
                .suffix("% suffix")
                .unwrap())
        );
    }

    #[test]
    fn parsing_precision() {
        let p = parse_formatter;
        let r = p("[.*]");
        assert_eq!(r, Ok(Formatter::default().precision(Unspecified)));

        let r = p("[.0]");
        assert_eq!(r, Ok(Formatter::default().precision(Decimals(0))));
        let r = p("[.5]");
        assert_eq!(r, Ok(Formatter::default().precision(Decimals(5))));
        let r = p("[.15]");
        assert_eq!(r, Ok(Formatter::default().precision(Decimals(15))));

        let r = p("[~0]");
        assert_eq!(r, Ok(Formatter::default().precision(Significance(0))));
        let r = p("[~5]");
        assert_eq!(r, Ok(Formatter::default().precision(Significance(5))));
        let r = p("[~15]");
        assert_eq!(r, Ok(Formatter::default().precision(Significance(15))));
    }

    #[test]
    fn parsing_test_digit_token_as_char() {
        let t = |x| Token::Digit(x).as_char();
        assert_eq!(t(0), '0');
        assert_eq!(t(1), '1');
        assert_eq!(t(2), '2');
        assert_eq!(t(3), '3');
        assert_eq!(t(4), '4');
        assert_eq!(t(5), '5');
        assert_eq!(t(6), '6');
        assert_eq!(t(7), '7');
        assert_eq!(t(8), '8');
        assert_eq!(t(9), '9');
    }

    #[test]
    fn parsing_separator() {
        let p = parse_formatter;
        let r = p("[/]");
        assert_eq!(r, Ok(Formatter::default().separator(None).unwrap()));

        let r = p("[//]");
        assert_eq!(r, Ok(Formatter::default().separator('/').unwrap()));

        let r = p("[/%]");
        assert_eq!(r, Ok(Formatter::default().separator('%').unwrap()));

        let r = p("[/.]");
        assert_eq!(r, Ok(Formatter::default().separator('.').unwrap()));
    }

    #[test]
    fn scalers() {
        let p = parse_formatter;
        let r = p("[s]");
        assert_eq!(r, Ok(Formatter::default()));

        let r = p("[m]");
        assert_eq!(r, Ok(Formatter::default().scales(Scales::metric())));

        let r = p("[b]");
        assert_eq!(r, Ok(Formatter::default().scales(Scales::binary())));

        let r = p("[n]");
        assert_eq!(r, Ok(Formatter::default().scales(Scales::none())));
    }

    #[test]
    fn error_testing() {
        let p = parse_formatter;
        let r = p("prefix]");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "the character `]` was found when parser was in prefix state"
        );

        let r = p("pre[fix]");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "unexpected character. expected a ., ~, %, s, m, b, n, /, ] but found 'f'"
        );

        let r = p("[.3~1]");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "precision of 3 has already been set"
        );

        let r = p("[.*~1]");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "unspecified precision has already been set"
        );

        let r = p("prefix[./.]suffix");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "unexpected character. expected a digit but found '/'"
        );

        let r = p("prefix[.3/.%n]suffix");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "a scaler has already been set and 'n' can not override"
        );

        let r = p("pre~fix[.256]suffix");
        assert!(r.is_err());
        assert_eq!(&r.unwrap_err().to_string(), "precision is larger than 255");

        let r = p("pre~fix[~300]suffix");
        assert!(r.is_err());
        assert_eq!(&r.unwrap_err().to_string(), "precision is larger than 255");

        let r = p("prefix that is long[~3]suffix");
        assert!(r.is_err());
        assert_eq!(
            &r.unwrap_err().to_string(),
            "formatter error: Invalid prefix `prefix that is long`. Prefix is longer than the supported 12 bytes"
        );
    }
}
