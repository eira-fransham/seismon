//! Text handling shared by builtins: Quake's tokenizers, wildcard matching, colour codes and
//! infostrings.
//!
//! QuakeC strings are bytes (Quake's character set, not UTF-8), so everything here works on bytes.

use bevy::math::Vec3;

/// A token from [`tokenize`], with where it was in the source string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub text: Vec<u8>,
    /// The offset of the start of the token in the source (including the opening quote of a
    /// quoted token).
    pub start: usize,
    /// The offset just after the end of the token in the source (including the closing quote).
    pub end: usize,
}

/// The characters that are always tokens by themselves for `COM_Parse`.
const SINGLE_CHARS: &[u8] = b"{})(':";

/// Parses the next token from `s`, starting at `pos`, like Quake's `COM_Parse`: whitespace and
/// `//` comments are skipped, `"`-quoted strings are one token (without the quotes), and unless
/// `console`, each of `{})(':` is a token by itself.
///
/// With `console`, this is `Cmd_TokenizeString`'s parsing instead, which has no single-character
/// tokens.
///
/// Returns `None` at the end of the string.
pub fn com_parse(s: &[u8], mut pos: usize, console: bool) -> Option<Token> {
    loop {
        while s.get(pos).is_some_and(|&c| c <= b' ') {
            pos += 1;
        }
        if s.get(pos..pos + 2) == Some(b"//") {
            while s.get(pos).is_some_and(|&c| c != b'\n') {
                pos += 1;
            }
            continue;
        }
        break;
    }

    let start = pos;
    let &first = s.get(pos)?;

    if first == b'"' {
        pos += 1;
        let text_start = pos;
        while s.get(pos).is_some_and(|&c| c != b'"') {
            pos += 1;
        }
        let text = s[text_start..pos].to_vec();
        // Skip the closing quote, if there is one.
        let end = (pos + 1).min(s.len());
        return Some(Token { text, start, end });
    }

    let is_single = |c: u8| !console && SINGLE_CHARS.contains(&c);
    if is_single(first) {
        return Some(Token { text: vec![first], start, end: pos + 1 });
    }

    while s.get(pos).is_some_and(|&c| c > b' ' && !is_single(c) && (!console || c != b'"')) {
        pos += 1;
    }

    Some(Token { text: s[start..pos].to_vec(), start, end: pos })
}

/// Splits `s` into tokens with [`com_parse`] (FTEQW's `tokenize`), or with the console's rules if
/// `console` (`tokenize_console`).
pub fn tokenize(s: &[u8], console: bool) -> Vec<Token> {
    let mut out = Vec::new();
    let mut pos = 0;
    while let Some(token) = com_parse(s, pos, console) {
        pos = token.end;
        out.push(token);
    }
    out
}

/// Splits `s` at every occurrence of any of `separators` (`tokenizebyseparator`). Separators are
/// tried in order at each position, and empty separators are ignored. An empty string has no
/// tokens.
pub fn tokenize_by_separator(s: &[u8], separators: &[&[u8]]) -> Vec<Token> {
    let mut out = Vec::new();
    if s.is_empty() {
        return out;
    }
    let mut start = 0;
    let mut pos = 0;

    while pos < s.len() {
        if let Some(sep) =
            separators.iter().find(|sep| !sep.is_empty() && s[pos..].starts_with(sep))
        {
            out.push(Token { text: s[start..pos].to_vec(), start, end: pos });
            pos += sep.len();
            start = pos;
        } else {
            pos += 1;
        }
    }
    out.push(Token { text: s[start..].to_vec(), start, end: s.len() });

    out
}

/// Whether `s` matches the wildcard `pattern`, where `*` matches any sequence (including an empty
/// one) and `?` matches any single character.
pub fn wildcmp(pattern: &[u8], s: &[u8], case_insensitive: bool) -> bool {
    let eq = |a: u8, b: u8| {
        if case_insensitive { a.eq_ignore_ascii_case(&b) } else { a == b }
    };

    let (mut p, mut i) = (0, 0);
    // Where to resume after the last `*`: (pattern position, string position).
    let mut star = None;

    while i < s.len() {
        match pattern.get(p) {
            Some(b'*') => {
                star = Some((p + 1, i));
                p += 1;
            }
            Some(&c) if c == b'?' || eq(c, s[i]) => {
                p += 1;
                i += 1;
            }
            _ => match star {
                Some((star_p, star_i)) => {
                    p = star_p;
                    i = star_i + 1;
                    star = Some((star_p, star_i + 1));
                }
                None => return false,
            },
        }
    }

    pattern[p.min(pattern.len())..].iter().all(|&c| c == b'*')
}

/// The colours of the `^0`..`^9` colour codes.
const COLOR_CODES: [Vec3; 10] = [
    Vec3::new(0., 0., 0.),
    Vec3::new(1., 0., 0.),
    Vec3::new(0., 1., 0.),
    Vec3::new(1., 1., 0.),
    Vec3::new(0., 0., 1.),
    Vec3::new(0., 1., 1.),
    Vec3::new(1., 0., 1.),
    Vec3::new(1., 1., 1.),
    Vec3::new(0.5, 0.5, 0.5),
    Vec3::new(0.5, 0.5, 0.5),
];

/// A visible character of a string with colour codes.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ColoredChar {
    pub byte: u8,
    /// The colour set by the last colour code, or `None` before any.
    pub color: Option<Vec3>,
}

/// The length of the colour code at the start of `s`, and the colour it sets (`None` for `^^`,
/// which is an escaped `^`).
fn color_code(s: &[u8]) -> Option<(usize, Option<Vec3>)> {
    if s.first() != Some(&b'^') {
        return None;
    }

    match *s.get(1)? {
        b'^' => Some((2, None)),
        digit @ b'0'..=b'9' => Some((2, Some(COLOR_CODES[usize::from(digit - b'0')]))),
        b'x' => {
            let hex = |c: u8| char::from(c).to_digit(16).map(|d| d as f32 / 15.);
            let (r, g, b) = (hex(*s.get(2)?)?, hex(*s.get(3)?)?, hex(*s.get(4)?)?);
            Some((5, Some(Vec3::new(r, g, b))))
        }
        _ => None,
    }
}

/// The visible characters of `s` and their colours, interpreting FTEQW/DarkPlaces colour codes:
/// `^0`..`^9`, `^xRGB` (hexadecimal) and `^^` (a literal `^`).
pub fn colored_chars(s: &[u8]) -> Vec<ColoredChar> {
    let mut out = Vec::with_capacity(s.len());
    let mut color = None;
    let mut pos = 0;

    while pos < s.len() {
        match color_code(&s[pos..]) {
            Some((len, Some(code))) => {
                color = Some(code);
                pos += len;
            }
            Some((len, None)) => {
                out.push(ColoredChar { byte: b'^', color });
                pos += len;
            }
            None => {
                out.push(ColoredChar { byte: s[pos], color });
                pos += 1;
            }
        }
    }

    out
}

/// `s` without colour codes (`strdecolorize`).
pub fn decolorize(s: &[u8]) -> Vec<u8> {
    colored_chars(s).into_iter().map(|c| c.byte).collect()
}

/// The number of visible characters in `s` (`strlennocol`).
pub fn visible_len(s: &[u8]) -> usize {
    colored_chars(s).len()
}

/// The value of `key` in the infostring `info` (`\key\value\key\value...`).
pub fn info_get<'a>(info: &'a [u8], key: &[u8]) -> Option<&'a [u8]> {
    let mut parts = info.strip_prefix(b"\\").unwrap_or(info).split(|&c| c == b'\\');
    while let Some(k) = parts.next() {
        let v = parts.next().unwrap_or_default();
        if k == key {
            return Some(v);
        }
    }
    None
}

/// `info` with `key` set to `value` (`infoadd`). An empty value removes the key. Backslashes and
/// quotes can't be in keys or values, so they are removed.
pub fn info_set(info: &[u8], key: &[u8], value: &[u8]) -> Vec<u8> {
    let clean = |s: &[u8]| s.iter().copied().filter(|&c| c != b'\\' && c != b'"').collect();
    let (key, value): (Vec<u8>, Vec<u8>) = (clean(key), clean(value));

    let mut out = Vec::new();
    let mut parts = info.strip_prefix(b"\\").unwrap_or(info).split(|&c| c == b'\\');
    while let Some(k) = parts.next() {
        let v = parts.next().unwrap_or_default();
        if k.is_empty() || k == key.as_slice() {
            continue;
        }
        out.push(b'\\');
        out.extend_from_slice(k);
        out.push(b'\\');
        out.extend_from_slice(v);
    }

    if !value.is_empty() && !key.is_empty() {
        out.push(b'\\');
        out.extend_from_slice(&key);
        out.push(b'\\');
        out.extend_from_slice(&value);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn texts(tokens: &[Token]) -> Vec<&[u8]> {
        tokens.iter().map(|t| t.text.as_slice()).collect()
    }

    #[test]
    fn tokenize_splits_like_com_parse() {
        let tokens = tokenize(b"  foo \"bar baz\"{qux}// comment\n'a:b'", false);
        assert_eq!(
            texts(&tokens),
            [&b"foo"[..], b"bar baz", b"{", b"qux", b"}", b"'", b"a", b":", b"b", b"'"]
        );
        assert_eq!((tokens[1].start, tokens[1].end), (6, 15));
    }

    #[test]
    fn tokenize_console_has_no_single_char_tokens() {
        let tokens = tokenize(b"bind {x} \"+attack; wait\" // c", true);
        assert_eq!(texts(&tokens), [&b"bind"[..], b"{x}", b"+attack; wait"]);
    }

    #[test]
    fn tokenize_by_separator_keeps_empty_tokens() {
        let tokens = tokenize_by_separator(b"a,,b;c", &[b",", b";"]);
        assert_eq!(texts(&tokens), [&b"a"[..], b"", b"b", b"c"]);
        assert!(tokenize_by_separator(b"", &[b","]).is_empty());
        assert_eq!(texts(&tokenize_by_separator(b"a,", &[b","])), [&b"a"[..], b""]);
    }

    #[test]
    fn wildcards() {
        assert!(wildcmp(b"*.wav", b"sound/foo.wav", false));
        assert!(wildcmp(b"sv_*", b"sv_gravity", false));
        assert!(wildcmp(b"a?c", b"abc", false));
        assert!(!wildcmp(b"a?c", b"ac", false));
        assert!(wildcmp(b"*", b"", false));
        assert!(wildcmp(b"a*b*c", b"aXXbYYc", false));
        assert!(!wildcmp(b"a*b*c", b"aXXbYY", false));
        assert!(wildcmp(b"FOO*", b"foobar", true));
        assert!(!wildcmp(b"FOO*", b"foobar", false));
    }

    #[test]
    fn colour_codes() {
        assert_eq!(decolorize(b"^1red^7 ^^x^xF00y"), b"red ^xy");
        assert_eq!(visible_len(b"^1ab^2c"), 3);
        let chars = colored_chars(b"a^1b");
        assert_eq!(chars[0].color, None);
        assert_eq!(chars[1].color, Some(Vec3::new(1., 0., 0.)));
        // Incomplete codes are literal.
        assert_eq!(decolorize(b"^x1^"), b"^x1^");
    }

    #[test]
    fn infostrings() {
        let info = info_set(b"", b"name", b"player");
        assert_eq!(info, b"\\name\\player");
        let info = info_set(&info, b"team", b"red");
        assert_eq!(info_get(&info, b"name"), Some(&b"player"[..]));
        assert_eq!(info_get(&info, b"team"), Some(&b"red"[..]));
        assert_eq!(info_get(&info, b"skin"), None);
        let info = info_set(&info, b"name", b"");
        assert_eq!(info, b"\\team\\red");
    }
}
