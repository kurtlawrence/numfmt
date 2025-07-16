
use super::*;
use std::f64::*;

#[test]
fn nan_and_inf() {
    let mut f = Formatter::new();
    assert_eq!(f.fmt(INFINITY), "∞");
    assert_eq!(f.fmt(NEG_INFINITY), "-∞");
    assert_eq!(f.fmt(NAN), "NaN");
}

#[test]
fn invalid_sep() {
    let f = Formatter::new().separator('ß');
    assert_eq!(f, Err(Error::InvalidSeparator('ß')));
}

#[test]
fn no_sep() {
    let mut f = Formatter::default();
    assert_eq!(f.thou_sep, Some(b','));
    f = f.separator(None).unwrap();
    assert_eq!(f.thou_sep, None);
}

#[test]
fn sn_reduction() {
    let f = reduce_to_sn;
    assert_eq!(f(0.0), (0.0, 0));
    assert_eq!(f(1.23), (1.23, 0));
    assert_eq!(f(12.34), (1.234, 1));
    assert_eq!(f(1234.567), (1.234567, 3));
    assert_eq!(f(1234.567e13), (1.234567, 16));
    assert_eq!(f(0.0123), (1.23, -2));
    assert_eq!(f(0.123), (1.23, -1));
    assert_eq!(f(0.0012345), (1.2345, -3));
    assert_eq!(f(0.00123e-12), (1.23, -15));
    assert_eq!(f(0.00123e12), (1.23, 9));
    assert_eq!(f(1234e-12), (1.234, -9));
    // negatives
    assert_eq!(f(-0.0), (-0.0, 0));
    assert_eq!(f(-1.23), (-1.23, 0));
    assert_eq!(f(-12.34), (-1.234, 1));
    assert_eq!(f(-1234.567), (-1.234567, 3));
    assert_eq!(f(-1234.567e13), (-1.234567, 16));
    assert_eq!(f(-0.0123), (-1.23, -2));
    assert_eq!(f(-0.123), (-1.23, -1));
    assert_eq!(f(-0.0012345), (-1.2345, -3));
    assert_eq!(f(-0.00123e-12), (-1.23, -15));
    assert_eq!(f(-0.00123e12), (-1.23, 9));
    assert_eq!(f(-1234e-12), (-1.234, -9));
}

#[test]
fn sn_tests() {
    let mut f = Formatter::new().scales(Scales::none());
    assert_eq!(f.fmt(123.4567e43), "1.234567e45");
    assert_eq!(f.fmt(123.4567e-43), "1.234567e-41");
    assert_eq!(f.fmt(-123.4567e-43), "-1.234567e-41");
    assert_eq!(f.fmt(-123.4567e43), "-1.234567e45");
    assert_eq!(f.fmt(0.000000007894), "7.893999e-9");
    assert_eq!(f.fmt(123454023590854.0), "1.234540e14");
    assert_eq!(f.fmt(123.456789e99), "1.234567e101");
}

#[test]
fn separator_tests() {
    // do not use a scaler for these tests
    let mut f = Formatter::new()
        .separator(',')
        .unwrap()
        .scales(Scales::none());
    assert_eq!(f.fmt(123456789_f64), "123,456,789.0");
    assert_eq!(f.fmt(12345678_f64), "12,345,678.0");
    assert_eq!(f.fmt(1234567_f64), "1,234,567.0");
    assert_eq!(f.fmt(123456_f64), "123,456.0");
    assert_eq!(f.fmt(1234_f64), "1,234.0");
    assert_eq!(f.fmt(123_f64), "123.0");
    assert_eq!(f.fmt(0.0), "0");
    assert_eq!(f.fmt(0.1234), "0.1234");
    assert_eq!(f.fmt(-123.0), "-123.0");
    assert_eq!(f.fmt(-1234.0), "-1,234.0");
    assert_eq!(f.fmt(-1234567.0), "-1,234,567.0");
    assert_eq!(f.fmt(-123456789101.0), "-123,456,789,101.0");
}

#[test]
fn test_scaling() {
    let s = Scales::short();
    assert_eq!(s.scale(123.0), (123.0, ""));
    assert_eq!(s.scale(-123.0), (-123.0, ""));
    assert_eq!(s.scale(1234.0), (1.234, " K"));
    assert_eq!(s.scale(-1234.0), (-1.234, " K"));
    assert_eq!(s.scale(-123456.0), (-123.456, " K"));
    assert_eq!(s.scale(-12345678.0), (-12.345678, " M"));

    let s = Scales::binary();
    assert_eq!(s.scale(123.0), (123.0, " "));
    assert_eq!(s.scale(1024.0 * 1024.0), (1.0, " Mi"));

    let s = Scales::new(2, vec!["x1", "x2", "x4", "x8", "x16"]).unwrap();
    assert_eq!(s.scale(20.0), (1.25, "x16"));
    assert_eq!(s.scale(64.0), (4.0, "x16")); // check it uses maximum if over

    let s = Scales::none();
    assert_eq!(s.scale(-1_000_000f64), (-1_000_000f64, ""));
}

#[test]
fn scaling_inside_fmtr() {
    let mut f = Formatter::default().precision(Unspecified);
    assert_eq!(f.fmt(12345678.0), "12.345678 M");
    assert_eq!(f.fmt(-12345.0), "-12.345 K");
    assert_eq!(f.fmt(-123.0), "-123.0");
    assert_eq!(f.fmt(-0.00123), "-0.00123");
}

#[test]
fn prefix() {
    let mut f = Formatter::new()
        .separator(',')
        .unwrap()
        .prefix("$")
        .unwrap();
    assert_eq!(f.fmt(123456.0), "$123,456.0");
    assert_eq!(f.fmt(0.01234), "$0.01234");
}

#[test]
fn suffix() {
    let mut f = Formatter::new()
        .separator(',')
        .unwrap()
        .suffix("%")
        .unwrap();
    assert_eq!(f.fmt(123456.0), "123,456.0%");
    assert_eq!(f.fmt(0.1234), "0.1234%");
}

#[test]
fn buf_lim_testing() {
    let mut f = Formatter::new()
        .build_scales(1, vec!["_ten chars"])
        .unwrap()
        .separator(',')
        .unwrap()
        .prefix("__ chars _")
        .unwrap()
        .suffix("a suffix !")
        .unwrap();
    assert_eq!(
        f.fmt(-123456789.0123456789),
        "__ chars _-123,456,789.01234567_ten charsa suffix !"
    );
}

#[test]
fn decimals_test() {
    let mut f = Formatter::new().precision(Decimals(6));
    assert_eq!(f.fmt(1234.5), "1234.5");
    assert_eq!(f.fmt(123.456789111), "123.456789");

    f = Formatter::default()
        .scales(Scales::none())
        .precision(Decimals(0));
    assert_eq!(f.fmt(1123.456), "1,123");
    assert_eq!(f.fmt(12345678.90123), "12,345,678");

    f = Formatter::default().precision(Decimals(1));
    assert_eq!(f.fmt(0.001234), "1.2e-3");
    f = Formatter::default().precision(Significance(1));
    assert_eq!(f.fmt(0.001234), "1e-3");
}

#[test]
fn significance_test() {
    let mut f = Formatter::default().precision(Significance(2));
    assert_eq!(f.fmt(1234.0), "1.2 K");
    assert_eq!(f.fmt(1.02), "1.0");
}

#[test]
fn currency_test() {
    let mut f = Formatter::currency("$").unwrap();
    assert_eq!(f.fmt(12345.6789), "$12,345.67");
    assert_eq!(f.fmt(1234_f64), "$1,234.0");

    let f = Formatter::currency("invalid length prefix");
    assert_eq!(
        f,
        Err(Error::InvalidPrefix("invalid length prefix".to_string()))
    );
}

#[test]
fn percentage_tests() {
    let mut f = Formatter::percentage();
    assert_eq!(f.fmt(0.678912), "67.8912%");
    assert_eq!(f.fmt(1.23), "123.0%");
    assert_eq!(f.fmt(1.2), "120.0%");
}

#[test]
fn failures() {
    use Error::*;
    let invalid = "invalid length prefix";

    let f = Formatter::new().prefix(invalid);
    assert_eq!(f, Err(InvalidPrefix(invalid.to_string())));
    assert_eq!(
        &f.unwrap_err().to_string(),
        "Invalid prefix `invalid length prefix`. Prefix is longer than the supported 12 bytes"
    );

    let f = Formatter::new().suffix(invalid);
    assert_eq!(f, Err(InvalidSuffix(invalid.to_string())));
    assert_eq!(
        &f.unwrap_err().to_string(),
        "Invalid suffix `invalid length prefix`. Suffix is longer than the supported 12 bytes"
    );

    let f = Formatter::new().build_scales(1000, vec![invalid]);
    assert_eq!(f, Err(InvalidUnit(invalid)));
    assert_eq!(
        &f.unwrap_err().to_string(),
        "Invalid unit `invalid length prefix`. Unit is longer than the supported 12 bytes"
    );

    let f = Formatter::new().build_scales(0, vec![""]);
    assert_eq!(f, Err(ZeroBase));
    assert_eq!(
        &f.unwrap_err().to_string(),
        "Invalid scale base, base must be greater than zero"
    );

    let f = Formatter::new().separator('😃');
    assert_eq!(f, Err(InvalidSeparator('😃')));
    assert_eq!(
        &f.unwrap_err().to_string(),
        "Invalid separator `😃`. Separator can only be one byte long"
    );
}

#[test]
fn getters() {
    let s = Scales::new(12, vec!["", "one"]).unwrap();
    assert_eq!(s.base(), 12);
    assert_eq!(s.units(), &["", "one"]);
    let (base, units) = s.into_inner();
    assert_eq!(base, 12);
    assert_eq!(units, &["", "one"]);
}

#[test]
fn eq_and_hashing() {
    let f1 = Formatter::default()
        .prefix("Hi")
        .unwrap()
        .suffix("Bye")
        .unwrap();
    let f2 = Formatter::new()
        .separator(',')
        .unwrap()
        .prefix("Hi")
        .unwrap()
        .suffix("Bye")
        .unwrap()
        .scales(Scales::short())
        .precision(Decimals(3));
    let f3 = Formatter::new();

    assert_eq!(f1, f2);
    assert_ne!(f1, f3);
    assert_ne!(f2, f3);

    let mut h = std::collections::hash_map::DefaultHasher::new();
    f1.hash(&mut h);
    let h1 = h.finish();

    let mut h = std::collections::hash_map::DefaultHasher::new();
    f2.hash(&mut h);
    let h2 = h.finish();

    let mut h = std::collections::hash_map::DefaultHasher::new();
    f3.hash(&mut h);
    let h3 = h.finish();

    assert_eq!(h1, h2);
    assert_ne!(h1, h3);
    assert_ne!(h2, h3);
}

#[test]
fn panicking_number() {
    let mut fmtr = Formatter::default()
        .precision(Precision::Unspecified)
        .scales(Scales::none());

    let s = fmtr.fmt(0.00316114);
    assert_eq!(s, "0.0031611399999999999");

    let s = fmtr.fmt(2_f64.powi(67));
    assert_eq!(s, "1.475739e20");
}

#[test]
fn panicking_number2() {
    let mut f = Formatter::default();

    let s = f.fmt(-0.0025053862329988824);
    assert_eq!(s, "-0.002");
}

#[test]
fn eu_testing() {
    let mut f: Formatter = "[,2]".parse().unwrap();
    let s = f.fmt(1.23);
    assert_eq!(s, "1,23");

    let mut f: Formatter = "[n/.]".parse().unwrap();
    let s = f.fmt(12345.0);
    assert_eq!(s, "12.345,0");
}
