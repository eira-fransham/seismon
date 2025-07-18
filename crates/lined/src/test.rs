use super::*;
use context;

use std::{
    env, fmt, fs,
    io::{self, BufRead, BufReader, Write},
};

#[derive(Default)]
pub struct TestTty {
    stdout: Vec<u8>,
}

pub enum NoStdin {}

impl Iterator for NoStdin {
    type Item = io::Result<termion::event::Key>;

    fn next(&mut self) -> Option<Self::Item> {
        unreachable!()
    }
}

impl Tty for TestTty {
    fn next_key(&mut self) -> Option<io::Result<Key>> {
        None
    }

    fn width(&self) -> io::Result<usize> {
        Ok(80)
    }
}

impl io::Write for TestTty {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        self.stdout.write_vectored(bufs)
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.stdout.write_all(buf)
    }

    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        self.stdout.write_fmt(fmt)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}

fn assert_cursor_pos(s: &str, cursor: usize, expected_pos: CursorPosition) {
    let buf = Buffer::from(s.to_owned());
    let words = context::get_buffer_words(&buf);
    let pos = CursorPosition::get(cursor, words);
    assert!(
        expected_pos == pos,
        "buffer: {:?}, cursor: {}, expected pos: {:?}, pos: {:?}",
        s,
        cursor,
        expected_pos,
        pos
    );
}

#[test]
fn test_get_cursor_position() {
    use CursorPosition::*;

    let tests = &[
        ("hi", 0, OnWordLeftEdge(0)),
        ("hi", 1, InWord(0)),
        ("hi", 2, OnWordRightEdge(0)),
        ("abc  abc", 4, InSpace(Some(0), Some(1))),
        ("abc  abc", 5, OnWordLeftEdge(1)),
        ("abc  abc", 6, InWord(1)),
        ("abc  abc", 8, OnWordRightEdge(1)),
        (" a", 0, InSpace(None, Some(0))),
        ("a ", 2, InSpace(Some(0), None)),
    ];

    for t in tests {
        assert_cursor_pos(t.0, t.1, t.2);
    }
}

fn assert_buffer_actions(start: &str, expected: &str, actions: &[Action]) {
    let mut buf = Buffer::from(start.to_owned());
    for a in actions {
        a.do_on(&mut buf);
    }

    assert_eq!(expected, String::from(buf));
}

#[test]
fn test_buffer_actions() {
    assert_buffer_actions(
        "",
        "h",
        &[
            Action::Insert {
                start: 0,
                text: "hi".chars().collect(),
            },
            Action::Remove {
                start: 1,
                text: ".".chars().collect(),
            },
        ],
    );
}

#[test]
fn test_history_indexing() {
    let mut h = History::new();
    h.push(Buffer::from("a")).unwrap();
    h.push(Buffer::from("b")).unwrap();
    h.push(Buffer::from("c")).unwrap();
    assert_eq!(h.len(), 3);
    assert_eq!(String::from(h.buffers[0].clone()), "a".to_string());
    assert_eq!(String::from(h.buffers[1].clone()), "b".to_string());
    assert_eq!(String::from(h.buffers[2].clone()), "c".to_string());
}

#[test]
fn test_in_memory_history_truncating() {
    let mut h = History::new();
    h.set_max_buffers_size(2);
    for _ in 0..4 {
        h.push(Buffer::from("a")).unwrap();
        h.push(Buffer::from("b")).unwrap();
    }
    h.commit_to_file();
    assert_eq!(h.len(), 2);
}

#[test]
fn test_in_file_history_truncating() {
    let mut tmp_file = env::temp_dir();
    tmp_file.push("liner_test_file123.txt");

    {
        let mut h = History::new();
        let _ = h.set_file_name_and_load_history(&tmp_file).unwrap();
        h.set_max_file_size(5);
        for bytes in b'a'..b'z' {
            h.push(Buffer::from(format!("{}", bytes as char))).unwrap();
        }
        h.commit_to_file();
    }

    let f = fs::File::open(&tmp_file).unwrap();
    let r = BufReader::new(f);
    let count = r.lines().count();
    assert_eq!(count, 5);

    fs::remove_file(tmp_file).unwrap();
}

static TEXT: &'static str = "a
b
c
d
";

#[test]
fn test_reading_from_file() {
    let mut tmp_file = env::temp_dir();
    tmp_file.push("liner_test_file456.txt");
    {
        let mut f = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(tmp_file.clone())
            .unwrap();
        f.write_all(TEXT.as_bytes()).unwrap();
    }
    let mut h = History::new();
    h.set_file_name_and_load_history(tmp_file).unwrap();
    assert_eq!(String::from(h.buffers[0].clone()), "a".to_string());
    assert_eq!(String::from(h.buffers[1].clone()), "b".to_string());
    assert_eq!(String::from(h.buffers[2].clone()), "c".to_string());
    assert_eq!(String::from(h.buffers[3].clone()), "d".to_string());
}
