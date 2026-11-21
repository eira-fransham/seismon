//! C `printf`-style formatting for FTEQW's `sprintf`.
//!
//! FTEQW passes each directive to the C library, so this follows C's rules for flags, width and
//! precision. The arguments are QuakeC values: numbers are floats (there are no integers in this
//! VM), and `%v` formats a vector as three `%g`s.

/// An argument to [`sprintf`].
#[derive(Debug, Clone, PartialEq)]
pub enum PrintfArg<'a> {
    Float(f32),
    Vector([f32; 3]),
    String(&'a [u8]),
}

impl PrintfArg<'_> {
    fn float(&self) -> f64 {
        match self {
            Self::Float(f) => f64::from(*f),
            Self::Vector(v) => f64::from(v[0]),
            Self::String(_) => 0.,
        }
    }

    fn vector(&self) -> [f32; 3] {
        match self {
            Self::Float(f) => [*f, 0., 0.],
            Self::Vector(v) => *v,
            Self::String(_) => [0.; 3],
        }
    }

    fn bytes(&self) -> &[u8] {
        match self {
            Self::String(s) => s,
            _ => b"",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Flags {
    alternate: bool,
    zero_pad: bool,
    left: bool,
    space: bool,
    plus: bool,
}

/// Pads `body` (whose sign or `0x` prefix is `prefix`) to `width`.
fn pad(out: &mut Vec<u8>, prefix: &[u8], body: &[u8], width: usize, flags: Flags, zero_ok: bool) {
    let len = prefix.len() + body.len();
    let padding = width.saturating_sub(len);
    if flags.left {
        out.extend_from_slice(prefix);
        out.extend_from_slice(body);
        out.extend(std::iter::repeat_n(b' ', padding));
    } else if flags.zero_pad && zero_ok {
        out.extend_from_slice(prefix);
        out.extend(std::iter::repeat_n(b'0', padding));
        out.extend_from_slice(body);
    } else {
        out.extend(std::iter::repeat_n(b' ', padding));
        out.extend_from_slice(prefix);
        out.extend_from_slice(body);
    }
}

/// The sign prefix of a signed number.
fn sign(negative: bool, flags: Flags) -> &'static [u8] {
    if negative {
        b"-"
    } else if flags.plus {
        b"+"
    } else if flags.space {
        b" "
    } else {
        b""
    }
}

/// `value` (which is non-negative) in C's `%e` format, without a sign.
fn exponential(value: f64, precision: usize, upper: bool, alternate: bool) -> String {
    let formatted = format!("{value:.precision$e}");
    let (mantissa, exponent) = formatted.split_once('e').unwrap_or((&formatted, "0"));
    let exponent: i32 = exponent.parse().unwrap_or(0);
    let mut out = String::from(mantissa);
    if alternate && precision == 0 {
        out.push('.');
    }
    out.push(if upper { 'E' } else { 'e' });
    out.push(if exponent < 0 { '-' } else { '+' });
    out.push_str(&format!("{:02}", exponent.unsigned_abs()));
    out
}

/// Removes trailing zeros (and then a trailing `.`) from the fractional part of `s`, for `%g`.
fn strip_trailing_zeros(s: &mut String) {
    let exponent_at = s.find(['e', 'E']).unwrap_or(s.len());
    let (number, exponent) = s.split_at(exponent_at);
    if !number.contains('.') {
        return;
    }
    let number = number.trim_end_matches('0').trim_end_matches('.');
    *s = format!("{number}{exponent}");
}

/// `value` (which is non-negative and finite) in C's format for the floating-point conversion
/// `conv`, without a sign.
fn format_float(value: f64, conv: u8, precision: Option<usize>, alternate: bool) -> String {
    let upper = conv.is_ascii_uppercase();
    match conv.to_ascii_lowercase() {
        b'f' => {
            let precision = precision.unwrap_or(6);
            let mut out = format!("{value:.precision$}");
            if alternate && precision == 0 {
                out.push('.');
            }
            out
        }
        b'e' => exponential(value, precision.unwrap_or(6), upper, alternate),
        _ => {
            // `%g`: `%e` or `%f`, depending on the exponent.
            let precision = match precision.unwrap_or(6) {
                0 => 1,
                p => p,
            };
            let exponent = if value == 0. {
                0
            } else {
                let formatted = format!("{value:.*e}", precision - 1);
                formatted.split_once('e').and_then(|(_, e)| e.parse::<i32>().ok()).unwrap_or(0)
            };

            let mut out = if exponent < -4 || exponent >= precision as i32 {
                exponential(value, precision - 1, upper, alternate)
            } else {
                let decimals = (precision as i32 - 1 - exponent).max(0) as usize;
                let mut out = format!("{value:.decimals$}");
                if alternate && !out.contains('.') {
                    out.push('.');
                }
                out
            };
            if !alternate {
                strip_trailing_zeros(&mut out);
            }
            out
        }
    }
}

/// Formats `value` with the floating-point conversion `conv`.
fn push_float(
    out: &mut Vec<u8>,
    value: f64,
    conv: u8,
    width: usize,
    precision: Option<usize>,
    flags: Flags,
) {
    let negative = value.is_sign_negative() && !(value == 0. && !value.is_sign_negative());
    let magnitude = value.abs();
    let body = if magnitude.is_nan() {
        if conv.is_ascii_uppercase() { "NAN".to_owned() } else { "nan".to_owned() }
    } else if magnitude.is_infinite() {
        if conv.is_ascii_uppercase() { "INF".to_owned() } else { "inf".to_owned() }
    } else {
        format_float(magnitude, conv, precision, flags.alternate)
    };
    let zero_ok = magnitude.is_finite();
    pad(out, sign(negative && !value.is_nan(), flags), body.as_bytes(), width, flags, zero_ok);
}

/// Formats `value` (truncated to an integer) with the integer conversion `conv`.
fn push_int(
    out: &mut Vec<u8>,
    value: f64,
    conv: u8,
    width: usize,
    precision: Option<usize>,
    flags: Flags,
) {
    let value = value as i64;
    let (prefix, digits): (Vec<u8>, String) = match conv {
        b'd' | b'i' | b'I' => (sign(value < 0, flags).to_vec(), value.unsigned_abs().to_string()),
        b'u' => (Vec::new(), (value as u64).to_string()),
        b'o' => {
            let digits = format!("{:o}", value as u64);
            let prefix = if flags.alternate && !digits.starts_with('0') { b"0".to_vec() } else {
                Vec::new()
            };
            (prefix, digits)
        }
        b'x' | b'p' => (
            if flags.alternate && value != 0 { b"0x".to_vec() } else { Vec::new() },
            format!("{:x}", value as u64),
        ),
        _ => (
            if flags.alternate && value != 0 { b"0X".to_vec() } else { Vec::new() },
            format!("{:X}", value as u64),
        ),
    };

    // A precision is the minimum number of digits, and disables zero padding.
    let digits = match precision {
        Some(0) if value == 0 => String::new(),
        Some(precision) => format!("{digits:0>precision$}"),
        None => digits,
    };
    pad(out, &prefix, digits.as_bytes(), width, flags, precision.is_none());
}

/// Parses a decimal number at the start of `s`.
fn parse_number(s: &[u8]) -> Option<(usize, usize)> {
    let len = s.iter().take_while(|c| c.is_ascii_digit()).count();
    if len == 0 {
        return None;
    }
    let value = std::str::from_utf8(&s[..len]).ok()?.parse().ok()?;
    Some((value, len))
}

/// Formats `args` with the C format string `format`, like FTEQW's `PF_sprintf`. `args` are the
/// arguments after the format string. Directives can choose their argument (`%2$s`) and take
/// their width or precision from an argument (`%*d`). An invalid directive ends the string.
pub fn sprintf(format: &[u8], args: &[PrintfArg<'_>]) -> Vec<u8> {
    let mut out = Vec::new();
    let mut next_arg = 0;
    let mut pos = 0;
    let arg = |index: usize| args.get(index).cloned().unwrap_or(PrintfArg::Float(0.));

    while pos < format.len() {
        let c = format[pos];
        if c != b'%' {
            out.push(c);
            pos += 1;
            continue;
        }
        pos += 1;
        if format.get(pos) == Some(&b'%') {
            out.push(b'%');
            pos += 1;
            continue;
        }

        let mut flags = Flags::default();
        let mut width = None;
        let mut precision = None;
        let mut this_arg = None;

        // `%N$`, or a width (possibly with a leading `0` flag).
        if let Some((number, len)) = parse_number(&format[pos..]) {
            if format.get(pos + len) == Some(&b'$') {
                this_arg = Some(number.saturating_sub(1));
                pos += len + 1;
            } else {
                if format[pos] == b'0' {
                    flags.zero_pad = true;
                }
                if number != 0 {
                    width = Some(number);
                }
                pos += len;
            }
        }

        if width.is_none() {
            loop {
                match format.get(pos) {
                    Some(b'#') => flags.alternate = true,
                    Some(b'0') => flags.zero_pad = true,
                    Some(b'-') => flags.left = true,
                    Some(b' ') => flags.space = true,
                    Some(b'+') => flags.plus = true,
                    _ => break,
                }
                pos += 1;
            }

            if format.get(pos) == Some(&b'*') {
                pos += 1;
                let index = match parse_number(&format[pos..]) {
                    Some((number, len)) if format.get(pos + len) == Some(&b'$') => {
                        pos += len + 1;
                        number.saturating_sub(1)
                    }
                    Some(_) => break,
                    None => {
                        next_arg += 1;
                        next_arg - 1
                    }
                };
                let w = arg(index).float() as i64;
                if w < 0 {
                    flags.left = true;
                }
                width = Some(w.unsigned_abs() as usize);
            } else if let Some((number, len)) = parse_number(&format[pos..]) {
                width = Some(number);
                pos += len;
            }
        }

        if format.get(pos) == Some(&b'.') {
            pos += 1;
            if format.get(pos) == Some(&b'*') {
                pos += 1;
                let index = match parse_number(&format[pos..]) {
                    Some((number, len)) if format.get(pos + len) == Some(&b'$') => {
                        pos += len + 1;
                        number.saturating_sub(1)
                    }
                    Some(_) => break,
                    None => {
                        next_arg += 1;
                        next_arg - 1
                    }
                };
                // A negative precision is as if there were none.
                precision = usize::try_from(arg(index).float() as i64).ok();
            } else if let Some((number, len)) = parse_number(&format[pos..]) {
                precision = Some(number);
                pos += len;
            } else {
                // `%.` without a precision is invalid.
                break;
            }
        }

        // Length modifiers make no difference to QuakeC values.
        while matches!(format.get(pos), Some(b'h' | b'l' | b'L' | b'q' | b'j' | b'z' | b't')) {
            pos += 1;
        }

        let Some(&conv) = format.get(pos) else {
            break;
        };
        pos += 1;

        let index = this_arg.unwrap_or_else(|| {
            next_arg += 1;
            next_arg - 1
        });
        let value = arg(index);
        let width = width.unwrap_or(0);

        match conv {
            b'd' | b'i' | b'I' | b'o' | b'u' | b'x' | b'X' => {
                push_int(&mut out, value.float(), conv, width, precision, flags)
            }
            b'p' | b'P' => {
                flags.zero_pad = true;
                let width = if width == 0 { 8 } else { width };
                let conv = if conv == b'p' { b'x' } else { b'X' };
                push_int(&mut out, value.float(), conv, width, precision, flags)
            }
            b'e' | b'E' | b'f' | b'F' | b'g' | b'G' => {
                push_float(&mut out, value.float(), conv, width, precision, flags)
            }
            b'v' | b'V' => {
                let conv = if conv == b'v' { b'g' } else { b'G' };
                for (i, component) in value.vector().into_iter().enumerate() {
                    if i > 0 {
                        out.push(b' ');
                    }
                    push_float(&mut out, f64::from(component), conv, width, precision, flags);
                }
            }
            b'c' => {
                let c = value.float() as u32 as u8;
                flags.zero_pad = false;
                pad(&mut out, b"", &[c], width, flags, false);
            }
            b's' | b'S' => {
                let mut s = value.bytes().to_vec();
                if conv == b'S' {
                    let mut quoted = vec![b'"'];
                    for &c in &s {
                        match c {
                            b'"' => quoted.extend_from_slice(b"\\\""),
                            b'\\' => quoted.extend_from_slice(b"\\\\"),
                            b'\n' => quoted.extend_from_slice(b"\\n"),
                            c => quoted.push(c),
                        }
                    }
                    quoted.push(b'"');
                    s = quoted;
                }
                if let Some(precision) = precision {
                    s.truncate(precision);
                }
                flags.zero_pad = false;
                pad(&mut out, b"", &s, width, flags, false);
            }
            _ => {
                bevy::log::warn!(
                    "sprintf: invalid format string: {}",
                    String::from_utf8_lossy(format)
                );
                break;
            }
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::{PrintfArg as A, sprintf};

    fn fmt(format: &str, args: &[A<'_>]) -> String {
        String::from_utf8(sprintf(format.as_bytes(), args)).unwrap()
    }

    #[test]
    fn integers() {
        assert_eq!(fmt("%d %i", &[A::Float(3.7), A::Float(-2.)]), "3 -2");
        assert_eq!(fmt("[%5d|%-5d|%05d]", &[A::Float(42.), A::Float(42.), A::Float(-42.)]), "[   42|42   |-0042]");
        assert_eq!(fmt("%+d % d", &[A::Float(1.), A::Float(1.)]), "+1  1");
        assert_eq!(fmt("%x %X %#x %o", &[A::Float(255.), A::Float(255.), A::Float(255.), A::Float(8.)]), "ff FF 0xff 10");
        assert_eq!(fmt("%.3d", &[A::Float(5.)]), "005");
        assert_eq!(fmt("%p", &[A::Float(255.)]), "000000ff");
    }

    #[test]
    fn floats() {
        assert_eq!(fmt("%f", &[A::Float(1.5)]), "1.500000");
        assert_eq!(fmt("%.2f", &[A::Float(1.23456)]), "1.23");
        assert_eq!(fmt("%8.3f|", &[A::Float(-1.)]), "  -1.000|");
        assert_eq!(fmt("%e", &[A::Float(1234.5)]), "1.234500e+03");
        assert_eq!(fmt("%g %g %g", &[A::Float(0.5), A::Float(100000.), A::Float(1000000.)]), "0.5 100000 1e+06");
        assert_eq!(fmt("%g", &[A::Float(0.0001)]), "0.0001");
        assert_eq!(fmt("%g", &[A::Float(0.00001)]), "1e-05");
        assert_eq!(fmt("%g", &[A::Float(0.)]), "0");
    }

    #[test]
    fn vectors_strings_and_chars() {
        assert_eq!(fmt("%v", &[A::Vector([1., 2.5, -3.])]), "1 2.5 -3");
        assert_eq!(fmt("[%5s|%-5s|%.2s]", &[A::String(b"ab"), A::String(b"ab"), A::String(b"abcd")]), "[   ab|ab   |ab]");
        assert_eq!(fmt("%S", &[A::String(b"a\"b")]), "\"a\\\"b\"");
        assert_eq!(fmt("%c%c", &[A::Float(104.), A::Float(105.)]), "hi");
    }

    #[test]
    fn argument_selection() {
        assert_eq!(fmt("%2$s %1$s", &[A::String(b"a"), A::String(b"b")]), "b a");
        assert_eq!(fmt("%*d|", &[A::Float(4.), A::Float(7.)]), "   7|");
        assert_eq!(fmt("%.*f", &[A::Float(1.), A::Float(2.25)]), "2.2");
        // Missing arguments are zero.
        assert_eq!(fmt("%d %s.", &[]), "0 .");
        assert_eq!(fmt("100%%", &[]), "100%");
    }
}
