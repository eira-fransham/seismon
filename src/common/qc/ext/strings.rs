//! String builtins (`strlen`, `substring`, `sprintf` etc.), as in FTEQW's `pr_bgcmd.c`. QuakeC
//! strings are bytes, so these work on bytes rather than UTF-8 (like FTEQW without `utf8_enable`).

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{InteropError, NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::{QCString, builtin_argc, builtin_vector_arg};
use sha1::Digest as _;

use super::{
    AnyArg,
    printf::{PrintfArg, sprintf},
    qc_string,
};
use crate::common::qc::{
    storage::ref_entity,
    stub::{Any, Ent, S, string},
    text, with_vm_ctx,
};

/// The first `argc - skip` of `args` (the variadic string arguments after `skip` fixed ones),
/// concatenated.
fn var_string(args: &[&S], skip: usize) -> Vec<u8> {
    let count = builtin_argc().saturating_sub(skip).max(1).min(args.len());
    args[..count].iter().flat_map(|s| string(s)).copied().collect()
}

/// Parses a number at the start of `s` like C's `atof`, returning 0 if there isn't one.
pub fn atof(s: &[u8]) -> f32 {
    let s = s.trim_ascii_start();
    let (negative, rest) = match s.first() {
        Some(b'-') => (true, &s[1..]),
        Some(b'+') => (false, &s[1..]),
        _ => (false, s),
    };

    // Hexadecimal, as glibc's `atof` accepts.
    if let Some(hex) = rest.strip_prefix(b"0x").or_else(|| rest.strip_prefix(b"0X")) {
        let len = hex.iter().take_while(|c| c.is_ascii_hexdigit()).count();
        if len > 0 {
            let value = u64::from_str_radix(std::str::from_utf8(&hex[..len]).unwrap(), 16)
                .map_or(f64::INFINITY, |v| v as f64);
            return (if negative { -value } else { value }) as f32;
        }
    }

    let mut len = rest.iter().take_while(|c| c.is_ascii_digit()).count();
    if rest.get(len) == Some(&b'.') {
        len += 1;
        len += rest[len..].iter().take_while(|c| c.is_ascii_digit()).count();
    }
    if len == 0 || rest[..len] == *b"." {
        return 0.;
    }
    // An exponent, if it's complete.
    if matches!(rest.get(len), Some(b'e' | b'E')) {
        let mut end = len + 1;
        if matches!(rest.get(end), Some(b'+' | b'-')) {
            end += 1;
        }
        let digits = rest[end.min(rest.len())..].iter().take_while(|c| c.is_ascii_digit()).count();
        if digits > 0 {
            len = end + digits;
        }
    }

    let value: f64 =
        std::str::from_utf8(&rest[..len]).ok().and_then(|s| s.parse().ok()).unwrap_or(0.);
    (if negative { -value } else { value }) as f32
}

/// Parses an integer at the start of `s` in `radix` like C's `strtol`, returning 0 if there isn't
/// one.
fn strtol(s: &[u8], radix: u32) -> i64 {
    let s = s.trim_ascii_start();
    let (negative, mut rest) = match s.first() {
        Some(b'-') => (true, &s[1..]),
        Some(b'+') => (false, &s[1..]),
        _ => (false, s),
    };
    if radix == 16 {
        rest = rest.strip_prefix(b"0x").or_else(|| rest.strip_prefix(b"0X")).unwrap_or(rest);
    }
    let mut value: i64 = 0;
    for &c in rest {
        let Some(digit) = char::from(c).to_digit(radix) else {
            break;
        };
        value = value.saturating_mul(i64::from(radix)).saturating_add(i64::from(digit));
    }
    if negative { -value } else { value }
}

/// `PF_stov`: parses a vector like `'1 2 3'` (the quotes are optional).
pub fn stov(s: &[u8]) -> Vec3 {
    let mut out = [0.; 3];
    let mut s = s.strip_prefix(b"'").unwrap_or(s);
    for value in &mut out {
        s = s.trim_ascii_start();
        *value = atof(s);
        if *value == 0. && !matches!(s.first(), Some(b'-' | b'+' | b'0'..=b'9')) {
            break;
        }
        let len = s.iter().take_while(|&&c| c != b' ' && c != b'\t' && c != b'\'').count();
        s = &s[len..];
        if s.first() == Some(&b'\'') {
            break;
        }
    }
    Vec3::from_array(out)
}

/// `PF_substring`: `length` bytes of `s` from `start`, where a negative `start` counts from the
/// end and a negative `length` leaves that many bytes (minus one) off the end.
pub fn substring(s: &[u8], start: f32, length: f32) -> &[u8] {
    let len = s.len() as i64;
    let mut start = start as i64;
    let mut length = length as i64;
    if start < 0 {
        start += len;
    }
    if length < 0 {
        length = len - start + (length + 1);
    }
    let start = start.max(0);
    if start >= len || length <= 0 {
        return b"";
    }
    let end = (start + length).min(len);
    &s[start as usize..end as usize]
}

/// `PF_strstrofs`: the offset of `needle` in `haystack` at or after `start`, or -1.
pub fn strstrofs(haystack: &[u8], needle: &[u8], start: i64) -> f32 {
    if start < 0 || start as usize > haystack.len() {
        return -1.;
    }
    let start = start as usize;
    if needle.is_empty() {
        return start as f32;
    }
    haystack[start..]
        .windows(needle.len())
        .position(|window| window == needle)
        .map_or(-1., |i| (start + i) as f32)
}

/// Compares like C's `strncmp` (or `strncasecmp`): the comparison result of the first difference
/// in the first `len` bytes (all of them with `None`).
fn compare(a: &[u8], b: &[u8], len: Option<usize>, case_insensitive: bool) -> f32 {
    let (a, b) = match len {
        Some(len) => (&a[..len.min(a.len())], &b[..len.min(b.len())]),
        None => (a, b),
    };
    let lower = |c: &u8| if case_insensitive { c.to_ascii_lowercase() } else { *c };
    match a.iter().map(lower).cmp(b.iter().map(lower)) {
        std::cmp::Ordering::Less => -1.,
        std::cmp::Ordering::Equal => 0.,
        std::cmp::Ordering::Greater => 1.,
    }
}

/// `strncmp` and `strncasecmp` with FTEQW's optional length and offsets. An offset past the end
/// of the string is the end. Unlike FTEQW's `strncmp` (but like DarkPlaces and FTEQW's
/// `strncasecmp`), the second offset is used.
fn compare_n(a: &S, b: &S, len: f32, a_ofs: f32, b_ofs: f32, case_insensitive: bool) -> f32 {
    let (a, b) = (string(a), string(b));
    let argc = builtin_argc();
    if argc <= 2 {
        return compare(a, b, None, case_insensitive);
    }
    let offset = |s: &[u8], ofs: f32, present: bool| {
        let ofs = if present { ofs as i64 } else { 0 };
        if ofs < 0 || ofs as usize > s.len() { s.len() } else { ofs as usize }
    };
    let a = &a[offset(a, a_ofs, argc > 3)..];
    let b = &b[offset(b, b_ofs, argc > 4)..];
    compare(a, b, Some(len.max(0.) as usize), case_insensitive)
}

/// `PF_strconv`'s conversion of digits: `conv` is 0 (same), 1 (white), 2 (red), 3 (special) or
/// 4 (red special).
fn conv_number(c: u8, base: u8, conv: i32) -> u8 {
    let offset = c - base;
    let base = match conv {
        1 => b'0',
        2 => b'0' + 128,
        3 => b'0' - 30,
        4 => b'0' + 128 - 30,
        _ => base,
    };
    offset.wrapping_add(base)
}

/// `PF_strconv`'s conversion of letters.
fn conv_alpha(c: u8, base_case: u8, base_color: u8, case: i32, color: i32, index: usize) -> u8 {
    let offset = c - base_case - base_color;
    let base_color = match color {
        1 => 0,
        2 => 128,
        5 | 6 => {
            if (index & 1) == usize::from(color == 6) {
                128
            } else {
                0
            }
        }
        _ => base_color,
    };
    let base_case = match case {
        1 => b'a',
        2 => b'A',
        _ => base_case,
    };
    offset.wrapping_add(base_case).wrapping_add(base_color)
}

/// `PF_strconv`: converts the case (`case`: 0 same, 1 lower, 2 upper) and colour (`alpha` and
/// `numbers`: 0 same, 1 white, 2 red, and see [`conv_number`] and [`conv_alpha`]) of `s`.
pub fn strconv(case: i32, alpha: i32, numbers: i32, s: &[u8]) -> Vec<u8> {
    s.iter()
        .enumerate()
        .map(|(i, &c)| match c {
            b'0'..=b'9' => conv_number(c, b'0', numbers),
            0xb0..=0xb9 => conv_number(c, b'0' + 128, numbers),
            0x92..=0x9b => conv_number(c, b'0' + 128 - 30, numbers),
            0x12..=0x1b => conv_number(c, b'0' - 30, numbers),
            b'a'..=b'z' => conv_alpha(c, b'a', 0, case, alpha, i),
            b'A'..=b'Z' => conv_alpha(c, b'A', 0, case, alpha, i),
            0xe1..=0xfa => conv_alpha(c, b'a', 128, case, alpha, i),
            0xc1..=0xda => conv_alpha(c, b'A', 128, case, alpha, i),
            c if (c & 127) < 16 || alpha == 0 => c,
            c if c < 128 => match alpha {
                2 => c + 128,
                _ => c,
            },
            c => match alpha {
                1 => c - 128,
                _ => c,
            },
        })
        .collect()
}

/// `PF_strpad`: `s` padded with spaces to `pad` bytes, on the left if `pad` is negative.
pub fn strpad(pad: i64, s: &[u8]) -> Vec<u8> {
    let width = pad.unsigned_abs() as usize;
    let padding = width.saturating_sub(s.len());
    let mut out = Vec::with_capacity(s.len() + padding);
    if pad < 0 {
        out.extend(std::iter::repeat_n(b' ', padding));
        out.extend_from_slice(s);
    } else {
        out.extend_from_slice(s);
        out.extend(std::iter::repeat_n(b' ', padding));
    }
    out
}

/// `PF_strreplace`: `subject` with every `search` replaced with `replace`.
pub fn strreplace(
    search: &[u8],
    replace: &[u8],
    subject: &[u8],
    case_insensitive: bool,
) -> Vec<u8> {
    if search.is_empty() {
        return subject.to_vec();
    }
    let mut out = Vec::with_capacity(subject.len());
    let mut pos = 0;
    while pos < subject.len() {
        let rest = &subject[pos..];
        let matches = rest.len() >= search.len()
            && if case_insensitive {
                rest[..search.len()].eq_ignore_ascii_case(search)
            } else {
                rest[..search.len()] == *search
            };
        if matches {
            out.extend_from_slice(replace);
            pos += search.len();
        } else {
            out.push(subject[pos]);
            pos += 1;
        }
    }
    out
}

/// `PF_strtrim`: `s` without leading or trailing spaces, tabs or newlines.
pub fn strtrim(s: &[u8]) -> &[u8] {
    let is_space = |c: &u8| matches!(c, b' ' | b'\t' | b'\n' | b'\r');
    let start = s.iter().position(|c| !is_space(c)).unwrap_or(s.len());
    let end = s.iter().rposition(|c| !is_space(c)).map_or(start, |i| i + 1);
    &s[start..end]
}

/// Quake's CRC-16 (CCITT, as in `crc.c`).
pub fn crc16(data: &[u8]) -> u16 {
    let mut crc: u16 = 0xffff;
    for &byte in data {
        crc ^= u16::from(byte) << 8;
        for _ in 0..8 {
            crc = if crc & 0x8000 != 0 { (crc << 1) ^ 0x1021 } else { crc << 1 };
        }
    }
    crc
}

/// The hash of `data` with `digest` (`MD4`, `SHA1`, `SHA224`, `SHA256`, `SHA384`, `SHA512` or
/// `CRC16`) in hexadecimal, or `None` for an unknown digest.
pub fn digest_hex(digest: &[u8], data: &[u8]) -> Option<String> {
    fn hex<B: AsRef<[u8]>>(bytes: B) -> String {
        bytes.as_ref().iter().map(|b| format!("{b:02x}")).collect()
    }

    let digest = str::from_utf8(digest).ok()?;

    macro_rules! match_digest {
        ($(($digest_kind:expr, $digest_fn:expr),)*) => {
            $(
                if $digest_kind.eq_ignore_ascii_case(digest) {
                    return Some(hex(&$digest_fn(data)));
                }
            )*
        };
    }

    match_digest![
        ("MD4", md4::Md4::digest),
        ("SHA1", sha1::Sha1::digest),
        ("SHA224", sha2::Sha224::digest),
        ("SHA256", sha2::Sha256::digest),
        ("SHA384", sha2::Sha384::digest),
        ("SHA512", sha2::Sha512::digest),
        ("CRC16", |data| format!("{:04x}", crc16(data))),
    ];

    None
}

/// `PF_uri_escape`: percent-encodes everything but RFC 3986's unreserved characters.
pub fn uri_escape(s: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len());
    for &c in s {
        if c.is_ascii_alphanumeric() || matches!(c, b'.' | b'-' | b'_' | b'~') {
            out.push(c);
        } else {
            out.extend_from_slice(format!("%{c:02X}").as_bytes());
        }
    }
    out
}

/// `PF_uri_unescape`: decodes percent-encoding. Invalid escapes and escaped NULs are kept as they
/// are.
pub fn uri_unescape(s: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len());
    let mut pos = 0;
    while pos < s.len() {
        if s[pos] == b'%'
            && let Some(hex) = s.get(pos + 1..pos + 3)
            && let Ok(byte) = u8::from_str_radix(std::str::from_utf8(hex).unwrap_or("x"), 16)
            && byte != 0
        {
            out.push(byte);
            pos += 3;
        } else {
            out.push(s[pos]);
            pos += 1;
        }
    }
    out
}

/// `PF_strftime`: the current time formatted with C's `strftime` format `format`.
fn strftime(local: bool, format: &[u8]) -> Vec<u8> {
    use std::fmt::Write as _;

    let format = String::from_utf8_lossy(format);
    let format = match &*format {
        "%R" => "%H:%M",
        "%F" => "%Y-%m-%d",
        other => other,
    };
    let items = chrono::format::StrftimeItems::new(format);
    let mut out = String::new();
    let result = if local {
        write!(out, "{}", chrono::Local::now().format_with_items(items))
    } else {
        write!(out, "{}", chrono::Utc::now().format_with_items(items))
    };
    if result.is_err() {
        warn!("strftime: invalid format {format:?}");
        out.clear();
    }
    out.into_bytes()
}

/// `PF_itof`/`PF_utof` with a bit range: `count` bits of `value` from bit `shift`.
fn bits(value: f32, shift: f32, count: f32) -> f32 {
    if builtin_argc() <= 1 {
        return value;
    }
    let mut value = value as i64 as u32;
    value = value.checked_shr(shift as u32).unwrap_or(0);
    let count = count as u32;
    if count != 32 {
        value &= 1u32.checked_shl(count).map_or(u32::MAX, |bit| bit - 1);
    }
    value as f32
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("strlen", |s: S| string(&s).len() as f32)
        .register("strcat", |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| {
            qc_string(var_string(&[&a, &b, &c, &d, &e, &f, &g, &h], 0))
        })
        // Strings are reference counted, so "zoned" strings are just copies.
        .register("strzone", |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| {
            qc_string(var_string(&[&a, &b, &c, &d, &e, &f, &g, &h], 0))
        })
        .register("strunzone", |_s: S| {})
        .register("substring", |s: S, start: f32, length: f32| {
            qc_string(substring(string(&s), start, length))
        })
        .register("stof", |s: S| atof(string(&s)))
        .register("stov", |s: S| stov(string(&s)).to_array())
        .register("etos", |e: Ent| {
            with_vm_ctx(|ctx| {
                let entity = ref_entity(&e)?;
                let number = if entity == ctx.root {
                    0
                } else {
                    ctx.storage
                        .edict(ctx.world, entity)
                        .or_else(|| ctx.storage.freed_edict(ctx.world, entity))
                        .unwrap_or(0)
                };
                Ok(qc_string(format!("entity {number}")))
            })
        })
        .register("strstrofs", |s: S, sub: S, start: f32| {
            let start = if builtin_argc() > 2 { start as i64 } else { 0 };
            strstrofs(string(&s), string(&sub), start)
        })
        .register("str2chr", |s: S, index: f32| {
            let s = string(&s);
            let mut index = if builtin_argc() > 1 { index as i64 } else { 0 };
            if index < 0 {
                index += s.len() as i64;
            }
            usize::try_from(index).ok().and_then(|i| s.get(i)).map_or(0., |&c| f32::from(c))
        })
        .register("chr2str", |a: f32, b: f32, c: f32, d: f32, e: f32, f: f32, g: f32, h: f32| {
            let chars = [a, b, c, d, e, f, g, h];
            let count = builtin_argc().min(chars.len());
            qc_string(chars[..count].iter().map(|&c| c as u32 as u8).collect::<Vec<_>>())
        })
        .register("strconv", |case: f32, alpha: f32, numbers: f32, a: S, b: S, c: S, d: S, e: S| {
            let s = var_string(&[&a, &b, &c, &d, &e], 3);
            qc_string(strconv(case as i32, alpha as i32, numbers as i32, &s))
        })
        .register("strpad", |pad: f32, a: S, b: S, c: S, d: S, e: S, f: S, g: S| {
            let s = var_string(&[&a, &b, &c, &d, &e, &f, &g], 1);
            qc_string(strpad(pad as i64, &s))
        })
        .register("infoadd", |info: S, key: S, a: S, b: S, c: S, d: S, e: S, f: S| {
            let value = var_string(&[&a, &b, &c, &d, &e, &f], 2);
            qc_string(text::info_set(string(&info), string(&key), &value))
        })
        .register("infoget", |info: S, key: S| {
            qc_string(text::info_get(string(&info), string(&key)).unwrap_or_default())
        })
        .register("strncmp", |a: S, b: S, len: f32, a_ofs: f32, b_ofs: f32| {
            compare_n(&a, &b, len, a_ofs, b_ofs, false)
        })
        .register("strcasecmp", |a: S, b: S| compare(string(&a), string(&b), None, true))
        .register("strncasecmp", |a: S, b: S, len: f32, a_ofs: f32, b_ofs: f32| {
            compare_n(&a, &b, len, a_ofs, b_ofs, true)
        })
        .register("strlennocol", |s: S| text::visible_len(string(&s)) as f32)
        .register("strdecolorize", |s: S| qc_string(text::decolorize(string(&s))))
        .register("strtolower", |s: S| qc_string(string(&s).to_ascii_lowercase()))
        .register("strtoupper", |s: S| qc_string(string(&s).to_ascii_uppercase()))
        .register("strreplace", |search: S, replace: S, subject: S| {
            qc_string(strreplace(string(&search), string(&replace), string(&subject), false))
        })
        .register("strireplace", |search: S, replace: S, subject: S| {
            qc_string(strreplace(string(&search), string(&replace), string(&subject), true))
        })
        .register("strtrim", |s: S| qc_string(strtrim(string(&s))))
        .register("strftime", |local: f32, a: S, b: S, c: S, d: S, e: S, f: S, g: S| {
            qc_string(strftime(local != 0., &var_string(&[&a, &b, &c, &d, &e, &f, &g], 1)))
        })
        .register("crc16", |insensitive: f32, a: S, b: S, c: S, d: S, e: S, f: S, g: S| {
            let mut s = var_string(&[&a, &b, &c, &d, &e, &f, &g], 1);
            if insensitive != 0. {
                s.make_ascii_lowercase();
            }
            f32::from(crc16(&s))
        })
        .register("digest_hex", |digest: S, a: S, b: S, c: S, d: S, e: S, f: S, g: S| {
            let data = var_string(&[&a, &b, &c, &d, &e, &f, &g], 1);
            match digest_hex(string(&digest), &data) {
                Some(hex) => qc_string(hex),
                None => {
                    warn!(
                        "digest_hex: unknown digest {:?}",
                        String::from_utf8_lossy(string(&digest))
                    );
                    V(QCString::null())
                }
            }
        })
        .register("uri_escape", |s: S| qc_string(uri_escape(string(&s))))
        .register("uri_unescape", |s: S| qc_string(uri_unescape(string(&s))))
        .register("stoi", |s: S| strtol(string(&s), 10) as i32 as f32)
        .register("stoh", |s: S| strtol(string(&s), 16) as u32 as f32)
        .register("itos", |i: f32| qc_string(format!("{}", i as i32)))
        .register("htos", |i: f32| qc_string(format!("{:08x}", i as i64 as u32)))
        .register("ftoi", |f: f32| f.trunc())
        .register("ftou", |f: f32| if f <= 0. { 0. } else { f.trunc() })
        .register("itof", bits)
        .register("utof", bits)
        .register(
            "sprintf",
            |format: S, a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any| {
                let decoded = [a, b, c, d, e, f, g]
                    .iter()
                    .take(builtin_argc().saturating_sub(1))
                    .map(AnyArg::decode)
                    .collect::<Result<Vec<_>, InteropError>>()?;

                with_vm_ctx(|ctx| {
                    let args = decoded
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| match arg {
                            AnyArg::String(s) => PrintfArg::String(s.as_bytes()),
                            // Entities are formatted as their numbers, like in DarkPlaces.
                            AnyArg::Entity(e) => PrintfArg::Float(if *e == ctx.root {
                                0.
                            } else {
                                ctx.storage.edict(ctx.world, *e).unwrap_or(0) as f32
                            }),
                            other => match builtin_vector_arg(i + 1) {
                                Some(v) => PrintfArg::Vector(v.to_array()),
                                None => PrintfArg::Float(other.float()),
                            },
                        })
                        .collect::<Vec<_>>();
                    Ok(qc_string(sprintf(string(&format), &args)))
                })
            },
        );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atof_parses_prefixes() {
        assert_eq!(atof(b"  12.5abc"), 12.5);
        assert_eq!(atof(b"-3"), -3.);
        assert_eq!(atof(b"1e3"), 1000.);
        assert_eq!(atof(b"1e"), 1.);
        assert_eq!(atof(b".5"), 0.5);
        assert_eq!(atof(b"0x10"), 16.);
        assert_eq!(atof(b"abc"), 0.);
        assert_eq!(atof(b""), 0.);
    }

    #[test]
    fn stov_parses_vectors() {
        assert_eq!(stov(b"'1 2 3'"), Vec3::new(1., 2., 3.));
        assert_eq!(stov(b"4 -5.5"), Vec3::new(4., -5.5, 0.));
        assert_eq!(stov(b"x 1 2"), Vec3::ZERO);
    }

    #[test]
    fn substring_handles_negatives() {
        assert_eq!(substring(b"hello", 1., 3.), b"ell");
        assert_eq!(substring(b"hello", -3., 2.), b"ll");
        assert_eq!(substring(b"hello", 1., -1.), b"ello");
        assert_eq!(substring(b"hello", 1., -2.), b"ell");
        assert_eq!(substring(b"hello", 9., 2.), b"");
        assert_eq!(substring(b"hello", 3., 99.), b"lo");
    }

    #[test]
    fn strstrofs_finds_substrings() {
        assert_eq!(strstrofs(b"abcabc", b"bc", 0), 1.);
        assert_eq!(strstrofs(b"abcabc", b"bc", 2), 4.);
        assert_eq!(strstrofs(b"abc", b"x", 0), -1.);
        assert_eq!(strstrofs(b"abc", b"a", 4), -1.);
    }

    #[test]
    fn comparisons() {
        assert_eq!(compare(b"abc", b"abd", None, false), -1.);
        assert_eq!(compare(b"abc", b"abd", Some(2), false), 0.);
        assert_eq!(compare(b"ABC", b"abc", None, true), 0.);
        assert_eq!(compare(b"b", b"a", None, false), 1.);
    }

    #[test]
    fn strconv_converts_case_and_colour() {
        assert_eq!(strconv(2, 0, 0, b"abC1"), b"ABC1");
        assert_eq!(strconv(1, 0, 0, b"ABc"), b"abc");
        assert_eq!(strconv(0, 2, 2, b"a1"), [b'a' + 128, b'1' + 128]);
        assert_eq!(strconv(0, 1, 1, &[b'a' + 128, b'1' + 128]), b"a1");
    }

    #[test]
    fn padding_trimming_and_replacing() {
        assert_eq!(strpad(5, b"ab"), b"ab   ");
        assert_eq!(strpad(-5, b"ab"), b"   ab");
        assert_eq!(strpad(1, b"abc"), b"abc");
        assert_eq!(strtrim(b" \tab c\n"), b"ab c");
        assert_eq!(strtrim(b"   "), b"");
        assert_eq!(strreplace(b"a", b"xy", b"banana", false), b"bxynxynxy");
        assert_eq!(strreplace(b"A", b"x", b"banana", true), b"bxnxnx");
        assert_eq!(strreplace(b"", b"x", b"ab", false), b"ab");
    }

    #[test]
    fn hashes() {
        // CRC-16/CCITT-FALSE's check value.
        assert_eq!(crc16(b"123456789"), 0x29b1);
        assert_eq!(digest_hex(b"md4", b"abc").unwrap(), "a448017aaf21d8525fc10ae87aa6729d");
        assert_eq!(
            digest_hex(b"SHA1", b"abc").unwrap(),
            "a9993e364706816aba3e25717850c26c9cd0d89d"
        );
        assert_eq!(
            digest_hex(b"sha256", b"abc").unwrap(),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        assert_eq!(digest_hex(b"crc16", b"123456789").unwrap(), "29b1");
        assert!(digest_hex(b"md5", b"abc").is_none());
    }

    #[test]
    fn uri_escaping() {
        assert_eq!(uri_escape(b"a b/c~"), b"a%20b%2Fc~");
        assert_eq!(uri_unescape(b"a%20b%2fc%zz%"), b"a b/c%zz%");
        assert_eq!(uri_unescape(b"%00"), b"%00");
    }

    #[test]
    fn integers_parse_like_c() {
        assert_eq!(strtol(b" -42x", 10), -42);
        assert_eq!(strtol(b"0x1F", 16), 31);
        assert_eq!(strtol(b"ff", 16), 255);
        assert_eq!(strtol(b"x", 10), 0);
    }
}
