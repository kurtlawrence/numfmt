//! Fast and friendly number formatting.
//!
//! Provides a [`Formatter`] to format decimal numbers with various methods. Formatting is
//! performance focused, it is generally faster than `std` with more features. There is also a
//! [string parser](#parsing) which can use a string to define a [`Formatter`] following a specific
//! grammar.
//!
//! # Procedure
//! Formatting is done through the [`Formatter::fmt`] which follows the procedure:
//! 1. Scale the number with the defined [`Scales`],
//! 2. Check if _scaled number_ is above or below the [scientific notation
//!    cutoffs](#scientific-notation),
//! 3. Add defined thousands separator,
//! 4. Stop at defined [`Precision`],
//! 5. Applies valid prefix, suffix, and unit decorations.
//!
//! # Usage
//! ## Default use
//! [`Default::default`] provides a general use default formatter with the following properties:
//! - [`Scales::short`] scaling,
//! - `,` thousands separator,
//! - 3 decimal places
//!
//! ```rust
//! # use numfmt::*;
//! let mut f = Formatter::default();
//! assert_eq!(f.fmt(0.0), "0");
//! assert_eq!(f.fmt(12345.6789), "12.345 K");
//! assert_eq!(f.fmt(0.00012345), "1.234e-4");
//! assert_eq!(f.fmt(123456e22), "1,234.559 Y");
//! ```
//!
//! ## Custom use
//! The [`Formatter`] has many different options to customise how the number should be formatted.
//! The example below shows how a currency format would be developed:
//! ```rust
//! # use numfmt::*;
//! let mut f = Formatter::new() // start with blank representation
//!     .separator(',').unwrap()
//!     .prefix("AU$").unwrap()
//!     .precision(Precision::Decimals(2));
//!
//! assert_eq!(f.fmt(0.52), "AU$0.52");
//! assert_eq!(f.fmt(1234.567), "AU$1,234.56");
//! assert_eq!(f.fmt(12345678900.0), "AU$12,345,678,900.0");
//! ```
//!
//! # Scientific Notation
//! Scientific notation kicks in when the scaled number is greater than 12 integer digits
//! (123,456,789,000) or less than 3 leading zeros (0.0001234). The number _always_ has a leading
//! integer digit and has a default of **7 significant figures**.
//!
//! # Precision
//! Precision, either with number of decimals or significant figures can be specified with
//! [`Precision`].
//! ```rust
//! # use numfmt::*;
//! let mut f = Formatter::new();
//! assert_eq!(f.fmt(1234.56789), "1234.56789");
//!
//! f = f.precision(Precision::Decimals(2));
//! assert_eq!(f.fmt(1234.56789), "1234.56");
//!
//! f = f.precision(Precision::Significance(5));
//! assert_eq!(f.fmt(1234.56789), "1234.5");
//! ```
//!
//! # Performance
//! Formatting is generally faster than `std`'s `f64::to_string` implementation. When constructing
//! a [`Formatter`] there is an allocation for the buffer, and an allocation for any scales.
//! Reusing a [`Formatter`] is recommended to avoid unnecessary allocations. The `cached` row shows
//! the better performance reusing a formatter.
//!
//! | Time (ns)        | 0.0 | 0.1234 | 2.718281828459045 | 1.797693148623157e307 |
//! | ---------------- | --- | ------ | ----------------- | --------------------- |
//! | numfmt - default | 35  | 115    | 153               | 195                   |
//! | numfmt - cached  | 2   | 75     | 89                | 126                   |
//! | std              | 35  | 96     | 105               | 214                   |
//!
//! # Example - File size formatter
//! Using a combination of a scale, suffix, and precision, a file size printer can be constructed:
//! ```rust
//! # use numfmt::*;
//! let mut f = Formatter::new()
//!                 .scales(Scales::binary())
//!                 .precision(Precision::Significance(3))
//!                 .suffix("B").unwrap();
//!
//! assert_eq!(f.fmt(123_f64), "123 B");
//! assert_eq!(f.fmt(1234_f64), "1.20 kiB");
//! assert_eq!(f.fmt(1_048_576_f64), "1.0 MiB");
//! assert_eq!(f.fmt(123456789876543_f64), "112 TiB");
//! ```
//!
//! # Parsing
//! A grammar is defined that can parse into a [`Formatter`]. This string representation can be
//! used as a user input for formatting numbers. The grammar is defined by a _prefix_, the number
//! format enclosed in brackets, and then the _suffix_.
//! ```text
//! prefix[[.#|,#|~#|.*|,*][%|s|b|n][/<char>]]suffix
//! ^----^ ^--------------^^-------^^-------^ ^----^
//! prefix precision scale    separator suffix
//! ```
//! > Each component is optional, including the number format. All formats are applied to the
//! _default_ [`Formatter`] so an empty format results in the default _formatter_.
//!
//! ## Prefix and Suffix
//! The prefix and suffix are bound to the supported lengths, and can have any character in them.
//! To use `[]` characters, a double bracket must be used.
//!
//! ### Example
//! ```rust
//! # use numfmt::*;
//! let mut f: Formatter;
//! f = "".parse().unwrap();
//! assert_eq!(f.fmt(1.234), "1.234");
//!
//! f = "prefix ".parse().unwrap();
//! assert_eq!(f.fmt(1.234), "prefix 1.234");
//!
//! f = "[] suffix".parse().unwrap();
//! assert_eq!(f.fmt(1.234), "1.234 suffix");
//!
//! f = "[[prefix [] suffix]]".parse().unwrap();
//! assert_eq!(f.fmt(1.234), "[prefix 1.234 suffix]");
//! ```
//!
//! ## Precision
//! Precision is defined using a `.`/`,` for decimals, or a `~` for significant figures, followed by
//! a number. A maximum of 255 is supported. There is a special case: `.*`/`,*` which removes any
//! default precision and uses [`Precision::Unspecified`].
//! Note that usage of `,` signals to use periods as the separator and comma as the
//! decimal marker. To use a comma with signficant figures, use a period separator.
//!
//! ### Example
//! ```rust
//! # use numfmt::*;
//! let mut f: Formatter;
//! f = "[.2]".parse().unwrap(); // use two decimal places
//! assert_eq!(f.fmt(1.2345), "1.23");
//!
//! f = "[,2]".parse().unwrap(); // use two decimal places with comma
//! assert_eq!(f.fmt(1.2345), "1,23");
//!
//! f = "[.0]".parse().unwrap(); // use zero decimal places
//! assert_eq!(f.fmt(10.234), "10");
//!
//! f = "[.*]".parse().unwrap(); // arbitrary precision
//! assert_eq!(f.fmt(1.234), "1.234");
//! assert_eq!(f.fmt(12.2), "12.2");
//!
//! f = "[,*]".parse().unwrap(); // arbitrary precision with comma
//! assert_eq!(f.fmt(1.234), "1,234");
//!
//! f = "[~3]".parse().unwrap(); // 3 significant figures
//! assert_eq!(f.fmt(1.234), "1.23");
//! assert_eq!(f.fmt(10.234), "10.2");

//! f = "[~3/.]".parse().unwrap(); // 3 significant figures with comma
//! assert_eq!(f.fmt(1.234), "1,23");
//! ```
//!
//! ## Scale
//! Scale uses a character to denote what scaling should be used. By default the SI scaling is
//! used. The following characters are supported:
//! - `s` for SI scaling ([`Scales::short`]),
//! - `%` for percentage scaling ([`Formatter::percentage`]),
//! - `m` for metric scaling ([`Scales::metric`]),
//! - `b` for binary scaling ([`Scales::binary`]),
//! - `n` for no scaling ([`Scales::none`])
//!
//! ### Example
//! ```rust
//! # use numfmt::*;
//! let mut f: Formatter;
//! f = "".parse().unwrap(); // default si scaling used
//! assert_eq!(f.fmt(12345.0), "12.345 K");
//!
//! f = "[n]".parse().unwrap(); // turn off scaling
//! assert_eq!(f.fmt(12345.0), "12,345.0");
//!
//! f = "[%.2]".parse().unwrap(); // format as percentages with 2 decimal places
//! assert_eq!(f.fmt(0.234), "23.40%");
//!
//! f = "[b]".parse().unwrap(); // use a binary scaler
//! assert_eq!(f.fmt(3.14 * 1024.0 * 1024.0), "3.14 Mi");
//! ```
//!
//! ## Separator
//! A separator character can be specified by using a forward slash `/` followed by a character.
//! The parser uses the _next character_, unless that character is `]` in which case the
//! separator is set to `None`. The default separator is a comma.
//! If a period separator `.` is specified, we take this as a signal to use a comma `,` as
//! the decimal signifier.
//!
//! ### Example
//! ```rust
//! # use numfmt::*;
//! let mut f: Formatter;
//! f = "[n]".parse().unwrap(); // turn off scaling to see separator
//! assert_eq!(f.fmt(12345.0), "12,345.0");
//!
//! f = "[n/]".parse().unwrap(); // use no separator
//! assert_eq!(f.fmt(12345.0), "12345.0");
//!
//! f = "[n/_]".parse().unwrap(); // use a underscroll
//! assert_eq!(f.fmt(12345.0), "12_345.0");
//!
//! f = "[n/ ]".parse().unwrap(); // use a space
//! assert_eq!(f.fmt(12345.0), "12 345.0");
//!
//! f = "[n/.]".parse().unwrap(); // use period and commas
//! assert_eq!(f.fmt(12345.0), "12.345,0");
//! ```
//!
//! ## Composing formats
//! There have been examples of composing formats already. The `prefix[num]suffix` order must be
//! adhered to, but the ordering within the number format is arbitrary. It is recommended to keep it
//! consistent with _precision, scaling, separator_ as this assists with readability and lowers the
//! risk of malformed formats (which will error on the parsing phase).
//!
//! ### Various composed examples
//! ```rust
//! # use numfmt::*;
//! let mut f: Formatter;
//!
//! // Percentages to two decimal places
//! f = "[.2%]".parse().unwrap();
//! assert_eq!(f.fmt(0.012345), "1.23%");
//!
//! // Currency to zero decimal places
//! // notice the `n` for no scaling
//! f = "$[.0n] USD".parse().unwrap();
//! assert_eq!(f.fmt(123_456_789.12345), "$123,456,789 USD");
//!
//! // Formatting file sizes
//! f = "[~3b]B".parse().unwrap();
//! assert_eq!(f.fmt(123_456_789.0), "117 MiB");
//!
//! // Units to 1 decimal place
//! f = "[.1n] m/s".parse().unwrap();
//! assert_eq!(f.fmt(12345.68), "12,345.6 m/s");
//!
//! // Using custom separator and period for decimals
//! f = "[,1n/_]".parse().unwrap();
//! assert_eq!(f.fmt(12345.68), "12_345,6");
//! ```
#![warn(missing_docs)]
use std::{cmp::*, error, fmt, hash::*};
use Precision::*;

mod numeric;
mod parse;
#[cfg(test)]
mod tests;

pub use numeric::Numeric;
pub use parse::ParseError;

/// Result type for [`Formatter`] methods.
pub type Result = std::result::Result<Formatter, Error>;

const SN_BIG_CUTOFF: f64 = 1_000_000_000_000f64;
const SN_SML_CUTOFF: f64 = 0.001;
const SN_PREC: Precision = Significance(7);
const PREFIX_LIM: usize = 12;
const UNITS_LIM: usize = 12;
const SUFFIX_LIM: usize = 12;
const FLOATBUF_LEN: usize = 22;
const BUF_LEN: usize = PREFIX_LIM + FLOATBUF_LEN + 3 + UNITS_LIM + SUFFIX_LIM;

// ########### FORMATTER #################################################################
/// The number formatter configurations. See the [module documentation for use][link].
///
/// [`Formatter`] has a `FromStr` implementation that can parse a string into a formatter using a
/// specific grammar. Please [consult the parsing section in the module
/// documentation](./index.html#parsing).
///
/// [link]: crate
#[derive(Debug, Clone)]
pub struct Formatter {
    /// The formatter uses a buffer to avoid allocating when constructing the formatted string.
    /// The formatting algorithm assumes the buffer size is large enough to accommodate writes into
    /// it, care must be taken when altering what gets written to buffer. Ensure buffer is of
    /// adequate size.
    ///
    /// The buffer is sized for:
    /// - 12 bytes: prefix
    /// - 22 bytes: float repr <https://github.com/dtolnay/dtoa/issues/22>
    /// - 3  bytes: 3x thou separator
    /// - 12 bytes: units
    /// - 12 bytes: suffix
    strbuf: Vec<u8>,
    /// Optional thousands separator character (restricted to a single byte)
    thou_sep: Option<u8>,
    /// comma separation
    comma: bool,
    /// If prefixed with something, this is the start of the _number_ portion.
    start: usize,
    /// Precision limits to formatting.
    precision: Precision,
    /// The auto scales.
    scales: Scales,
    /// Optional suffix.
    suffix: [u8; SUFFIX_LIM],
    suffix_len: usize,
    /// Direct conversion.
    convert: fn(f64) -> f64,
}

impl Formatter {
    /// Construct a new formatter.
    ///
    /// No scaling is set, so this is only does a single allocation for the buffer.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::new();
    /// assert_eq!(f.fmt(12345.6789), "12345.6789");
    /// ```
    pub fn new() -> Self {
        Self {
            strbuf: vec![0; BUF_LEN],
            thou_sep: None,
            start: 0,
            precision: Precision::Unspecified,
            scales: Scales::none(),
            suffix: [0; SUFFIX_LIM],
            suffix_len: 0,
            convert: |x| x,
            comma: false,
        }
    }

    /// Create a formatter that formats numbers as a currency.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::currency("$").unwrap();
    /// assert_eq!(f.fmt(12345.6789), "$12,345.67");
    /// assert_eq!(f.fmt(1234_f64), "$1,234.0");
    /// ```
    pub fn currency(prefix: &str) -> Result {
        Self::new()
            .separator(',')
            .unwrap()
            .precision(Decimals(2))
            .prefix(prefix)
    }

    /// Create a formatter that formats numbers as a percentage.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::percentage();
    /// assert_eq!(f.fmt(0.678912), "67.8912%");
    /// assert_eq!(f.fmt(1.23), "123.0%");
    /// assert_eq!(f.fmt(1.2), "120.0%");
    ///
    /// f = f.precision(Precision::Decimals(2));
    /// assert_eq!(f.fmt(0.01234), "1.23%");
    /// ```
    pub fn percentage() -> Self {
        Self::new().convert(|x| x * 100.0).suffix("%").unwrap()
    }

    /// Set the value converter.
    ///
    /// Use a converter to transform the input number into another number. This is done before all
    /// steps and the number follows the same procedure as normal. A good example of a use of a
    /// converter is to make a percentage number by _always_ multiplying by 100.
    pub fn convert(mut self, f: fn(f64) -> f64) -> Self {
        self.convert = f;
        self
    }

    /// Set the precision.
    pub fn precision(mut self, precision: Precision) -> Self {
        self.precision = precision;
        self
    }

    /// Set the scaling.
    pub fn scales(mut self, scales: Scales) -> Self {
        self.scales = scales;
        self
    }

    /// Set the scaling via [`Scales::new`].
    pub fn build_scales(mut self, base: u16, units: Vec<&'static str>) -> Result {
        let scales = Scales::new(base, units)?;
        self.scales = scales;
        Ok(self)
    }

    /// Set the thousands separator.
    ///
    /// If separator is not a single byte, an error is returned.
    /// If the separator is a period `.`, this signals to use a comma for the decimal marker.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::new().separator(',').unwrap(); // use a comma
    /// assert_eq!(f.fmt(12345.67), "12,345.67");
    ///
    /// f = f.separator(' ').unwrap(); // use a space
    /// assert_eq!(f.fmt(12345.67), "12 345.67");
    ///
    /// f = f.separator(None).unwrap(); // no separator
    /// assert_eq!(f.fmt(12345.67), "12345.67");
    ///
    /// f = f.separator('.').unwrap(); // use a period separator and comma for decimal
    /// assert_eq!(f.fmt(12345.67), "12.345,67");
    /// ```
    pub fn separator<S: Into<Option<char>>>(mut self, sep: S) -> Result {
        if let Some(sep) = sep.into() {
            if sep.len_utf8() != 1 {
                Err(Error::InvalidSeparator(sep))
            } else {
                if sep == '.' {
                    self.comma = true;
                }
                let mut buf = [0];
                sep.encode_utf8(&mut buf);
                self.thou_sep = Some(buf[0]);
                Ok(self)
            }
        } else {
            self.thou_sep = None;
            Ok(self)
        }
    }

    /// Set the comma option.
    ///
    /// If set to true it will use a comma instead of a period.
    /// If a comma is the separator, a period will be used instead.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::new();
    /// assert_eq!(f.fmt(12345.67), "12345.67");
    /// f = f.comma(true);
    /// assert_eq!(f.fmt(12345.67), "12345,67");
    ///
    /// f = f.separator('.').unwrap();
    /// assert_eq!(f.fmt(12345.67), "12.345,67");
    /// ```
    pub fn comma(mut self, comma: bool) -> Self {
        self.comma = comma;
        if comma && self.thou_sep == Some(b',') {
            self.thou_sep = Some(b'.');
        }
        self
    }

    /// Sets the prefix.
    ///
    /// If the prefix is longer than the supported length, an error is returned.
    pub fn prefix(mut self, prefix: &str) -> Result {
        if prefix.len() > PREFIX_LIM {
            Err(Error::InvalidPrefix(prefix.to_string()))
        } else {
            let n = prefix.len();
            self.strbuf[..n].copy_from_slice(prefix.as_bytes());
            self.start = n;
            Ok(self)
        }
    }

    /// Set the suffix.
    ///
    /// If the suffix is longer than the supported length, an error is returned.
    pub fn suffix(mut self, suffix: &str) -> Result {
        if suffix.len() > SUFFIX_LIM {
            Err(Error::InvalidSuffix(suffix.to_string()))
        } else {
            let n = suffix.len();
            self.suffix[..n].copy_from_slice(suffix.as_bytes());
            self.suffix_len = n;
            Ok(self)
        }
    }

    /// Format the number!
    #[deprecated = "consider using Formatter::fmt2 instead"]
    pub fn fmt(&mut self, num: f64) -> &str {
        self.fmt2(num)
    }

    /// Format any number implementing [`Numeric`].
    pub fn fmt2<N: Numeric>(&mut self, num: N) -> &str {
        let mut buf = std::mem::take(&mut self.strbuf);
        let bytes = self.fmt_into_buf(&mut buf, num);
        self.strbuf = buf;
        std::str::from_utf8(&self.strbuf[..bytes]).expect("will be valid string")
    }

    /// Format any number implementing [`Numeric`], appending to the supplied `buf`.
    ///
    /// This is functionally the same as [`Self::fmt2`], however it does not use the backing
    /// buffer, instead extending the supplied string.
    /// Useful when the receiver is shared.
    pub fn fmt_into<N: Numeric>(&self, buf: &mut String, num: N) {
        let start = buf.len();

        // pad string buffer to write into
        buf.extend(std::iter::repeat_n('\0', self.strbuf.len()));

        // SAFETY: Only UTF-8 characters will be written.
        let bytes = unsafe { buf.as_bytes_mut() };

        // write the prefix into the buffer
        bytes[start..start + self.start].copy_from_slice(&self.strbuf[..self.start]);

        let written = self.fmt_into_buf(&mut bytes[start..], num);
        let end = start + written;

        buf.truncate(end); // drop the unused padding

        debug_assert!(std::str::from_utf8(buf.as_bytes()).is_ok());
    }

    /// Format any number implementing [`Numeric`], returning an owned [`String`].
    ///
    /// This is functionally the same as [`Self::fmt2`], however it does not use the backing
    /// buffer, instead allocating a new string to write into.
    /// Useful when the receiver is shared.
    pub fn fmt_string<N: Numeric>(&self, num: N) -> String {
        let mut buf = String::new();
        self.fmt_into(&mut buf, num);
        buf
    }

    /// Format the number into `strbuf`. Returns the number of bytes written.
    fn fmt_into_buf<N: Numeric>(&self, strbuf: &mut [u8], num: N) -> usize {
        debug_assert_eq!(
            strbuf.len(),
            BUF_LEN,
            "the buffer is expected to be {BUF_LEN} wide"
        );

        if num.is_nan() {
            strbuf[..3].copy_from_slice(b"NaN");
            3
        } else if num.is_infinite() && num.is_negative() {
            strbuf[..4].copy_from_slice(b"-\xE2\x88\x9E"); // -∞
            4
        } else if num.is_infinite() {
            strbuf[..3].copy_from_slice(b"\xE2\x88\x9E");
            3
        } else if num.is_zero() {
            strbuf[..1].copy_from_slice(b"0");
            1
        } else {
            let num = (self.convert)(num.to_f64());

            // scale num to supplied scales
            let (scaled, unit) = self.scales.scale(num);

            // check if the scaled version hits sn cutoffs
            // use original number if it does
            let abs = scaled.abs();
            // This adjusts the sn cutoff if decimals is low
            let sn_sml_cutoff = match self.precision {
                Decimals(d) | Significance(d) if d <= 3 => 10f64.powi(d as i32).recip(),
                _ => SN_SML_CUTOFF,
            };
            if abs >= SN_BIG_CUTOFF || abs < sn_sml_cutoff {
                // fmt with scientific notation
                let (num, exponent) = reduce_to_sn(num);
                let precision = match self.precision {
                    Unspecified => SN_PREC,
                    x => x,
                };
                let cursor = self.start + self.write_num(strbuf, num, precision);
                strbuf[cursor] = b'e'; // exponent
                let cursor = 1 + cursor;
                let written = {
                    let mut buf = itoa::Buffer::new();
                    let s = buf.format(exponent);
                    let end = cursor + s.len();
                    strbuf[cursor..end].copy_from_slice(s.as_bytes());
                    s.len()
                };
                let cursor = cursor + written;
                self.apply_suffix(strbuf, cursor)
            } else {
                // write out the scaled number
                let mut cursor = self.start + self.write_num(strbuf, scaled, self.precision);
                if !unit.is_empty() {
                    let s = cursor;
                    cursor += unit.len();
                    strbuf[s..cursor].copy_from_slice(unit.as_bytes());
                }
                self.apply_suffix(strbuf, cursor)
            }
        }
    }

    /// Writes `num` into the string buffer with the specified `precision`.
    /// Returns the number of bytes written.
    /// Injects the thousands separator into the integer portion if it exists.
    fn write_num(&self, strbuf: &mut [u8], num: f64, precision: Precision) -> usize {
        let mut tmp = dtoa::Buffer::new();
        let s = tmp.format(num);
        let tmp = s.as_bytes();
        let n = tmp.len();
        let mut digits = 0;
        let mut written = 0;
        let mut in_frac = false;
        let mut thou = 2 - (num.abs().log10().trunc() as u8) % 3;
        let mut idx = self.start;

        for i in 0..n {
            let byte = tmp[i]; // obtain byte
            strbuf[idx] = byte; // write byte
            idx += 1;
            written += 1; // increment counter

            if byte.is_ascii_digit() {
                digits += 1;
                thou += 1;
            }

            // look ahead otherwise it would include the decimal always even for 0 precision
            if i + 1 < n && tmp[i + 1] == b'.' {
                in_frac = true;
                if let Decimals(_) = precision {
                    digits = 0
                }
            } else if in_frac && byte == b'.' && self.comma {
                strbuf[idx - 1] = b',';
            } else if !in_frac && thou == 3 {
                if let Some(sep) = self.thou_sep {
                    thou = 0;
                    strbuf[idx] = sep;
                    idx += 1;
                    written += 1;
                }
            }

            match precision {
                Significance(d) | Decimals(d) if in_frac => {
                    if digits >= d {
                        break;
                    }
                }
                _ => (),
            }
        }

        written
    }

    fn apply_suffix(&self, strbuf: &mut [u8], mut pos: usize) -> usize {
        if !self.suffix.is_empty() {
            let s = pos;
            pos = s + self.suffix_len;
            strbuf[s..pos].copy_from_slice(&self.suffix[..self.suffix_len]);
        }
        pos
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
            .separator(',')
            .unwrap()
            .scales(Scales::short())
            .precision(Decimals(3))
    }
}

impl std::str::FromStr for Formatter {
    type Err = parse::ParseError;
    fn from_str(s: &str) -> std::result::Result<Self, ParseError> {
        parse::parse_formatter(s)
    }
}

// Eq and Hash have specialised impls as the _state_ of the buffer should not impact equality
// checking
impl PartialEq for Formatter {
    #[allow(clippy::suspicious_operation_groupings)]
    fn eq(&self, other: &Self) -> bool {
        self.convert == other.convert
            && self.precision == other.precision
            && self.thou_sep == other.thou_sep
            // need to use the other suffix len.
            && self.suffix[..self.suffix_len] == other.suffix[..other.suffix_len]
            && self.strbuf[..self.start] == other.strbuf[..other.start]
            && self.scales == other.scales
    }
}

impl Eq for Formatter {}

impl Hash for Formatter {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.strbuf[..self.start].hash(hasher);
        self.thou_sep.hash(hasher);
        self.precision.hash(hasher);
        self.scales.hash(hasher);
        self.suffix[..self.suffix_len].hash(hasher);
        self.convert.hash(hasher);
    }
}

/// Returns `(reduced, exponent)`.
fn reduce_to_sn(n: f64) -> (f64, i32) {
    if n == 0.0 || n == -0.0 {
        (0.0, 0)
    } else {
        let abs = n.abs();
        let mut e = abs.log10().trunc() as i32;
        if abs < 1.0 {
            e -= 1;
        }
        let n = n * 10_f64.powi(-e);
        (n, e)
    }
}

// ########### ERROR #####################################################################
/// Errors when configuring a [`Formatter`].
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Prefix is longer than supported length.
    InvalidPrefix(String),
    /// Separator is not a byte long.
    InvalidSeparator(char),
    /// Suffix is longer than supported length.
    InvalidSuffix(String),
    /// Unit is longer than supported length.
    InvalidUnit(&'static str),
    /// Scaling base is 0.
    ZeroBase,
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            InvalidPrefix(prefix) => write!(
                f,
                "Invalid prefix `{}`. Prefix is longer than the supported {} bytes",
                prefix, PREFIX_LIM
            ),
            InvalidSeparator(sep) => write!(
                f,
                "Invalid separator `{}`. Separator can only be one byte long",
                sep
            ),
            InvalidSuffix(suffix) => write!(
                f,
                "Invalid suffix `{}`. Suffix is longer than the supported {} bytes",
                suffix, SUFFIX_LIM
            ),
            InvalidUnit(unit) => write!(
                f,
                "Invalid unit `{}`. Unit is longer than the supported {} bytes",
                unit, UNITS_LIM
            ),
            ZeroBase => write!(f, "Invalid scale base, base must be greater than zero"),
        }
    }
}

// ########### PRECISION #################################################################
/// Number precision.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
#[allow(missing_docs)]
pub enum Precision {
    Significance(u8),
    Decimals(u8),
    Unspecified,
}

// ########### SCALES ####################################################################
/// Scale numbers.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Scales {
    base: u16,
    units: Vec<&'static str>,
}

impl Scales {
    /// Create a new scale.
    ///
    /// If a unit is longer than the supported length, an error will be returned.
    pub fn new(base: u16, units: Vec<&'static str>) -> std::result::Result<Self, Error> {
        if base == 0 {
            return Err(Error::ZeroBase);
        }

        for unit in &units {
            if unit.len() > UNITS_LIM {
                return Err(Error::InvalidUnit(unit));
            }
        }
        Ok(Self { base, units })
    }

    /// Create a scale which is dummy and does not scale.
    pub fn none() -> Self {
        Self {
            base: std::u16::MAX,
            units: Vec::new(),
        }
    }

    /// The default scaling method.
    ///
    /// Based on a [short scale](https://en.wikipedia.org/wiki/Long_and_short_scales)
    /// the scaling uses base `1000`. The units are meant to be used to denote _magnitude_ of the
    /// number, so the empty base is empty.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::default()
    ///             .scales(Scales::short())
    ///             .precision(Precision::Decimals(1));
    /// assert_eq!(f.fmt(12.34e0), "12.3");
    /// assert_eq!(f.fmt(12.34e3), "12.3 K");
    /// assert_eq!(f.fmt(12.34e6), "12.3 M");
    /// assert_eq!(f.fmt(12.34e9), "12.3 B");
    /// assert_eq!(f.fmt(12.34e12), "12.3 T");
    /// assert_eq!(f.fmt(12.34e15), "12.3 P");
    /// assert_eq!(f.fmt(12.34e18), "12.3 E");
    /// assert_eq!(f.fmt(12.34e21), "12.3 Z");
    /// assert_eq!(f.fmt(12.34e24), "12.3 Y");
    /// assert_eq!(f.fmt(12.34e27), "12,339.9 Y");
    /// ```
    pub fn short() -> Self {
        Scales {
            base: 1000,
            units: vec!["", " K", " M", " B", " T", " P", " E", " Z", " Y"],
        }
    }

    /// Create a metric SI scale.
    ///
    /// The [SI scale](https://en.wikipedia.org/wiki/International_System_of_Units#Prefixes)
    /// steps with base `1000`. It is intended for use as a units prefix, so the empty base
    /// contains a space.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::new().scales(Scales::metric());
    /// assert_eq!(f.fmt(123456.0), "123.456 k");
    /// assert_eq!(f.fmt(123456789.0), "123.456789 M");
    /// ```
    pub fn metric() -> Self {
        Scales {
            base: 1000,
            units: vec![" ", " k", " M", " G", " T", " P", " E", " Z", " Y"],
        }
    }

    /// Create a binary scale.
    ///
    /// The [binary scale](https://en.wikipedia.org/wiki/Binary_prefix)
    /// steps with base `1024`. It is intended for use as a units prefix, so the empty base
    /// contains a space.
    ///
    /// # Example
    /// ```rust
    /// # use numfmt::*;
    /// let mut f = Formatter::new().scales(Scales::binary());
    /// assert_eq!(f.fmt(1024.0 * 1024.0), "1.0 Mi");
    /// assert_eq!(f.fmt(3.14 * 1024.0 * 1024.0), "3.14 Mi");
    /// ```
    pub fn binary() -> Self {
        Scales {
            base: 1024,
            units: vec![" ", " ki", " Mi", " Gi", " Ti", " Pi", " Ei", " Zi", " Yi"],
        }
    }

    /// The set base.
    pub fn base(&self) -> u16 {
        self.base
    }

    /// The set units.
    pub fn units(&self) -> &[&'static str] {
        self.units.as_slice()
    }

    /// Extract the `(base, units)`.
    pub fn into_inner(self) -> (u16, Vec<&'static str>) {
        (self.base, self.units)
    }

    /// Scale a number and return the scaled number with the unit.
    pub fn scale(&self, mut num: f64) -> (f64, &'static str) {
        let base = self.base as f64;
        let mut u = "";
        let mut n2 = num;
        // use n2 as a delayed write to not downsize num on last entry numbers
        for unit in &self.units {
            num = n2;
            u = unit;
            if num.abs() >= base {
                n2 = num / base;
            } else {
                break;
            }
        }
        (num, u)
    }
}
