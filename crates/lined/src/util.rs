use std::{borrow::Cow, io};
use unicode_width::*;

pub fn last_prompt_line_width<S: AsRef<str>>(s: S) -> usize {
    let last_prompt_line_width = handle_prompt(s.as_ref());
    remove_codes(last_prompt_line_width).width()
}

pub fn find_longest_common_prefix<T: Clone + Eq>(among: &[Vec<T>]) -> Option<Vec<T>> {
    if among.is_empty() {
        return None;
    } else if among.len() == 1 {
        return Some(among[0].clone());
    }

    for s in among {
        if s.is_empty() {
            return None;
        }
    }

    let shortest_word = among.iter().min_by_key(|x| x.len()).unwrap();

    let mut end = shortest_word.len();
    while end > 0 {
        let prefix = &shortest_word[..end];

        let mut failed = false;
        for s in among {
            if !s.starts_with(prefix) {
                failed = true;
                break;
            }
        }

        if !failed {
            return Some(prefix.into());
        }

        end -= 1;
    }

    None
}

pub enum AnsiState {
    Norm,
    Esc,
    Csi,
    Osc,
}

pub fn remove_codes(input: &str) -> Cow<str> {
    if input.contains('\x1B') {
        let mut clean = String::new();

        let mut s = AnsiState::Norm;
        for c in input.chars() {
            match s {
                AnsiState::Norm => match c {
                    '\x1B' => s = AnsiState::Esc,
                    _ => clean.push(c),
                },
                AnsiState::Esc => match c {
                    '[' => s = AnsiState::Csi,
                    ']' => s = AnsiState::Osc,
                    _ => s = AnsiState::Norm,
                },
                AnsiState::Csi if c.is_ascii_alphabetic() => s = AnsiState::Norm,
                AnsiState::Osc if c == '\x07' => s = AnsiState::Norm,
                _ => (),
            }
        }

        Cow::Owned(clean)
    } else {
        Cow::Borrowed(input)
    }
}

/// Returns the last prompt line.
pub fn handle_prompt(full_prompt: &str) -> &str {
    if let Some(index) = full_prompt.rfind('\n') {
        let (_, prompt) = full_prompt.split_at(index + 1);
        prompt
    } else {
        full_prompt
    }
}

pub fn terminal_width() -> io::Result<usize> {
    let (mut size_col, _) = ::termion::terminal_size()?;
    if size_col == 0 {
        size_col = 80;
    }
    Ok(size_col as usize)
}
