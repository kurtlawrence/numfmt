Fast and friendly Rust number formatting.

# Format numbers.

Provides a [`Formatter`] to format decimal numbers with various methods. Formatting is
performance focused, it is generally faster than `std` with more features. There is also a
[string parser](#parsing) which can use a string to define a [`Formatter`] following a specific
grammar.

# Procedure
Formatting is done through the [`Formatter::fmt`] which follows the procedure:
1. Scale the number with the defined [`Scales`],
2. Check if _scaled number_ is above or below the [scientific notation
   cutoffs](#scientific-notation),
3. Add defined thousands separator,
4. Stop at defined [`Precision`],
5. Applies valid prefix, suffix, and unit decorations.

# Usage
## Default use
[`Default::default`] provides a general use default formatter with the following properties:
- [`Scales::short`] scaling,
- `,` thousands separator,
- 3 decimal places

```rust
# use numfmt::*;
let mut f = Formatter::default();
assert_eq!(f.fmt(0.0), "0");
assert_eq!(f.fmt(12345.6789), "12.345 K");
assert_eq!(f.fmt(0.00012345), "1.234e-4");
assert_eq!(f.fmt(123456e22), "1,234.559 Y");
```

## Custom use
The [`Formatter`] has many different options to customise how the number should be formatted.
The example below shows how a currency format would be developed:
```rust
# use numfmt::*;
let mut f = Formatter::new() // start with blank representation
    .separator(',').unwrap()
    .prefix("AU$").unwrap()
    .precision(Precision::Decimals(2));

assert_eq!(f.fmt(0.52), "AU$0.52");
assert_eq!(f.fmt(1234.567), "AU$1,234.56");
assert_eq!(f.fmt(12345678900.0), "AU$12,345,678,900.0");
```

# Scientific Notation
Scientific notation kicks in when the scaled number is greater than 12 integer digits
(123,456,789,000) or less than 3 leading zeros (0.0001234). The number _always_ has a leading
integer digit and has a default of **7 significant figures**.

# Precision
Precision, either with number of decimals or significant figures can be specified with
[`Precision`].
```rust
# use numfmt::*;
let mut f = Formatter::new();
assert_eq!(f.fmt(1234.56789), "1234.56789");

f = f.precision(Precision::Decimals(2));
assert_eq!(f.fmt(1234.56789), "1234.56");

f = f.precision(Precision::Significance(5));
assert_eq!(f.fmt(1234.56789), "1234.5");
```

# Performance
Formatting is generally faster than `std`'s `f64::to_string` implementation. When constructing
a [`Formatter`] there is an allocation for the buffer, and an allocation for any scales.
Reusing a [`Formatter`] is recommended to avoid unnecessary allocations. The `cached` row shows
the better performance reusing a formatter.

| Time (ns)        | 0.0 | 0.1234 | 2.718281828459045 | 1.797693148623157e307 |
| ---------------- | --- | ------ | ----------------- | --------------------- |
| numfmt - default | 35  | 115    | 153               | 195                   |
| numfmt - cached  | 2   | 75     | 89                | 126                   |
| std              | 35  | 96     | 105               | 214                   |

# Example - File size formatter
Using a combination of a scale, suffix, and precision, a file size printer can be constructed:
```rust
# use numfmt::*;
let mut f = Formatter::new()
                .scales(Scales::binary())
                .precision(Precision::Significance(3))
                .suffix("B").unwrap();

assert_eq!(f.fmt(123_f64), "123 B");
assert_eq!(f.fmt(1234_f64), "1.20 kiB");
assert_eq!(f.fmt(1_048_576_f64), "1.0 MiB");
assert_eq!(f.fmt(123456789876543_f64), "112 TiB");
```

# Parsing
A grammar is defined that can parse into a [`Formatter`]. This string representation can be
used as a user input for formatting numbers. The grammar is defined by a _prefix_, the number
format enclosed in brackets, and then the _suffix_.
```text
prefix[[.#|~#|.*][%|s|b|n][/<char>]]suffix
^----^ ^--------^^-------^^-------^ ^----^
prefix precision scale    separator suffix
```
> Each component is optional, including the number format. All formats are applied to the
_default_ [`Formatter`] so an empty format results in the default _formatter_.

## Prefix and Suffix
The prefix and suffix are bound to the supported lengths, and can have any character in them.
To use `[]` characters, a double bracket must be used.

### Example
```rust
# use numfmt::*;
let mut f: Formatter;
f = "".parse().unwrap();
assert_eq!(f.fmt(1.234), "1.234");

f = "prefix ".parse().unwrap();
assert_eq!(f.fmt(1.234), "prefix 1.234");

f = "[] suffix".parse().unwrap();
assert_eq!(f.fmt(1.234), "1.234 suffix");

f = "[[prefix [] suffix]]".parse().unwrap();
assert_eq!(f.fmt(1.234), "[prefix 1.234 suffix]");
```

## Precision
Precision is defined using a `.` for decimals, or a `~` for significant figures, followed by
a number. A maximum of 255 is supported. There is a special case: `.*` which removes any
default precision and uses [`Precision::Unspecified`].

### Example
```rust
# use numfmt::*;
let mut f: Formatter;
f = "[.2]".parse().unwrap(); // use two decimal places
assert_eq!(f.fmt(1.2345), "1.23");

f = "[.0]".parse().unwrap(); // use zero decimal places
assert_eq!(f.fmt(10.234), "10");

f = "[.*]".parse().unwrap(); // arbitrary precision
assert_eq!(f.fmt(1.234), "1.234");
assert_eq!(f.fmt(12.2), "12.2");

f = "[~3]".parse().unwrap(); // 3 significant figures
assert_eq!(f.fmt(1.234), "1.23");
assert_eq!(f.fmt(10.234), "10.2");
```

## Scale
Scale uses a character to denote what scaling should be used. By default the SI scaling is
used. The following characters are supported:
- `s` for SI scaling ([`Scales::short`]),
- `%` for percentage scaling ([`Formatter::percentage`]),
- `m` for metric scaling ([`Scales::metric`]),
- `b` for binary scaling ([`Scales::binary`]),
- `n` for no scaling ([`Scales::none`])

### Example
```rust
# use numfmt::*;
let mut f: Formatter;
f = "".parse().unwrap(); // default si scaling used
assert_eq!(f.fmt(12345.0), "12.345 K");

f = "[n]".parse().unwrap(); // turn off scaling
assert_eq!(f.fmt(12345.0), "12,345.0");

f = "[%.2]".parse().unwrap(); // format as percentages with 2 decimal places
assert_eq!(f.fmt(0.234), "23.40%");

f = "[b]".parse().unwrap(); // use a binary scaler
assert_eq!(f.fmt(3.14 * 1024.0 * 1024.0), "3.14 Mi");
```

## Separator
A separator character can be specified by using a forward slash `/` followed by a character.
The parser uses the _next character verbatim_, unless that character is `]` in which case the
separator is set to `None`. The default separator is a comma.

### Example
```rust
# use numfmt::*;
let mut f: Formatter;
f = "[n]".parse().unwrap(); // turn off scaling to see separator
assert_eq!(f.fmt(12345.0), "12,345.0");

f = "[n/]".parse().unwrap(); // use no separator
assert_eq!(f.fmt(12345.0), "12345.0");

f = "[n/.]".parse().unwrap(); // use a period
assert_eq!(f.fmt(12345.0), "12.345.0");

f = "[n/_]".parse().unwrap(); // use a underscroll
assert_eq!(f.fmt(12345.0), "12_345.0");

f = "[n/ ]".parse().unwrap(); // use a space
assert_eq!(f.fmt(12345.0), "12 345.0");
```

## Composing formats
There have been examples of composing formats already. The `prefix[num]suffix` order must be
adhered to, but the ordering within the number format is arbitrary. It is recommended to keep it
consistent with _precision, scaling, separator_ as this assists with readability and lowers the
risk of malformed formats (which will error on the parsing phase).

### Various composed examples
```rust
# use numfmt::*;
let mut f: Formatter;

// Percentages to two decimal places
f = "[.2%]".parse().unwrap();
assert_eq!(f.fmt(0.012345), "1.23%");

// Currency to zero decimal places
// notice the `n` for no scaling
f = "$[.0n] USD".parse().unwrap();
assert_eq!(f.fmt(123_456_789.12345), "$123,456,789 USD");

// Formatting file sizes
f = "[~3b]B".parse().unwrap();
assert_eq!(f.fmt(123_456_789.0), "117 MiB");

// Units to 1 decimal place
f = "[.1n] m/s".parse().unwrap();
assert_eq!(f.fmt(12345.68), "12,345.6 m/s");
```
