extern crate regex;
extern crate termion;

use std::{
    borrow::Cow,
    env::{args, current_dir},
    io,
};

use lined::{
    Completer, Context, CursorPosition, EditorContext, Event, EventKind, FilenameCompleter,
    Highlighter, Prompt,
};
use regex::Regex;
use termion::color;

/// This prints out the text back onto the screen with `dodo` highlighted.
struct HighlightDodo;

impl Highlighter for HighlightDodo {
    fn highlight<'a>(&self, s: &'a str) -> Cow<'a, str> {
        let reg_exp = Regex::new("(?P<k>dodo)").unwrap();
        let format = format!("{}$k{}", color::Fg(color::Red), color::Fg(color::Reset));
        reg_exp.replace_all(s, format.as_str())
    }
}

struct NoCommentCompleter {
    inner: Option<FilenameCompleter>,
}

impl Completer for NoCommentCompleter {
    fn completions<'a>(&'a mut self, start: &'a str) -> impl Iterator<Item = Cow<'a, str>> + 'a {
        if let Some(inner) = &mut self.inner {
            itertools::Either::Left(inner.completions(start))
        } else {
            itertools::Either::Right(std::iter::empty())
        }
    }

    fn on_event<C: EditorContext>(&mut self, event: Event<C>) {
        if let EventKind::BeforeComplete = event.kind {
            let (_, pos) = event.editor.words_and_cursor_position();

            // Figure out of we are completing a command (the first word) or a filename.
            let filename = match pos {
                CursorPosition::InWord(i) => i > 0,
                CursorPosition::InSpace(Some(_), _) => true,
                CursorPosition::InSpace(None, _) => false,
                CursorPosition::OnWordLeftEdge(i) => i >= 1,
                CursorPosition::OnWordRightEdge(i) => i >= 1,
            };

            if filename {
                let completer = FilenameCompleter::new(Some(current_dir().unwrap()));
                self.inner = Some(completer);
            } else {
                self.inner = None;
            }
        }
    }
}

fn main() {
    let mut con = Context::new().unwrap();
    let mut completer = NoCommentCompleter { inner: None };

    let history_file = match args().nth(1) {
        Some(file_name) => {
            println!("History file: {file_name}");
            file_name
        }
        None => {
            eprintln!("No history file provided. Ending example early.");
            return;
        }
    };

    con.history.set_file_name_and_load_history(history_file).unwrap();

    loop {
        let res = con.read_line_with_highlighter(
            Prompt::from("[prompt]$ "),
            &mut completer,
            HighlightDodo,
        );

        match res {
            Ok(res) => {
                match res.as_str() {
                    "emacs" => {
                        con.key_bindings = lined::KeyBindings::Emacs;
                        println!("emacs mode");
                    }
                    "vi" => {
                        con.key_bindings = lined::KeyBindings::Vi;
                        println!("vi mode");
                    }
                    "exit" | "" => {
                        println!("exiting...");
                        break;
                    }
                    _ => {}
                }

                if res.is_empty() {
                    break;
                }

                con.history.push(res.into()).unwrap();
            }
            Err(e) => {
                match e.kind() {
                    // ctrl-c pressed
                    io::ErrorKind::Interrupted => {}
                    // ctrl-d pressed
                    io::ErrorKind::UnexpectedEof => {
                        println!("exiting...");
                        break;
                    }
                    _ => {
                        // Ensure that all writes to the history file
                        // are written before exiting.
                        panic!("error: {e:?}")
                    }
                }
            }
        }
    }
    // Ensure that all writes to the history file are written before exiting.
    con.history.commit_to_file();
}
