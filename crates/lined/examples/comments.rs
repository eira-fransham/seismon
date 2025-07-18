extern crate liner;
extern crate regex;
extern crate termion;

use std::env::{args, current_dir};
use std::io;

use liner::{
    Completer, Context, CursorPosition, EditorContext, Event, EventKind, FilenameCompleter, Prompt,
};
use regex::Regex;
use termion::color;

// This prints out the text back onto the screen
fn highlight_dodo(s: &str) -> String {
    let reg_exp = Regex::new("(?P<k>dodo)").unwrap();
    let format = format!("{}$k{}", color::Fg(color::Red), color::Fg(color::Reset));
    reg_exp.replace_all(s, format.as_str()).to_string()
}

struct CommentCompleter {
    inner: Option<FilenameCompleter>,
}

impl Completer for CommentCompleter {
    fn completions(&mut self, start: &str) -> Vec<String> {
        if let Some(inner) = &mut self.inner {
            inner.completions(start)
        } else {
            Vec::new()
        }
    }

    fn on_event<C: EditorContext>(&mut self, event: Event<C>) {
        if let EventKind::BeforeComplete = event.kind {
            let (_, pos) = event.editor.get_words_and_cursor_position();

            // Figure out of we are completing a command (the first word) or a filename.
            let filename = match pos {
                // If we are inside of a word(i is the index inside of the text, and if that
                // position is over zero, we return true
                CursorPosition::InWord(i) => i > 0,
                // If we are in a space like this `cat | cart` or cat |
                // checks if there is a word to our left(indicated by there being Some value)
                CursorPosition::InSpace(Some(_), _) => true,
                // Checks if there is no word to our left(indicated by there being None value)
                CursorPosition::InSpace(None, _) => false,
                // If we are on the left edge of a word, and the position of the cursor is
                // greater than or equal to 1, return true
                CursorPosition::OnWordLeftEdge(i) => i >= 1,
                // If we are on the right edge of the word
                CursorPosition::OnWordRightEdge(i) => i >= 1,
            };

            // If we are not in a word with pos over zero, or in a space with text beforehand,
            // or on the left edge of a word with pos >= to 1, or on the Right edge of a word
            // under the same condition, then
            // This condition is only false under the predicate that we are in a space with no
            // word to the left
            if filename {
                let completer = FilenameCompleter::new(Some(current_dir().unwrap()));
                self.inner = Some(completer);
            } else {
                // Delete the completer
                self.inner = None;
            }
        }
    }
}

fn main() {
    let mut con = Context::new().unwrap();
    let mut completer = CommentCompleter { inner: None };

    let history_file = match args().nth(1) {
        Some(file_name) => {
            println!("History file: {}", file_name);
            file_name
        }
        None => {
            eprintln!("No history file provided. Ending example early.");
            return;
        }
    };

    con.history
        .set_file_name_and_load_history(history_file)
        .unwrap();

    loop {
        // Reads the line, the first arg is the prompt, the second arg is a function called on every bit of text leaving liner, and the third is called on every key press
        // Basically highlight_dodo(read_line()), where on every keypress, the lambda is called
        let res = con.read_line(
            Prompt::from("[prompt]\n% "),
            Some(Box::new(highlight_dodo)),
            &mut completer,
        );

        // We are out of the lambda, and res is the result from read_line which is an Into<String>
        match res {
            Ok(res) => {
                match res.as_str() {
                    "emacs" => {
                        con.key_bindings = liner::KeyBindings::Emacs;
                        println!("emacs mode");
                    }
                    "vi" => {
                        con.key_bindings = liner::KeyBindings::Vi;
                        println!("vi mode");
                    }
                    "exit" | "" => {
                        println!("exiting...");
                        break;
                    }
                    // If all else fails, do nothing
                    _ => {}
                }

                // If we typed nothing, don't continue down to pushing to history
                if res.is_empty() {
                    break;
                }

                con.history.push(res.into()).unwrap();
            }
            // If there was an error, get what type it was(remember, we still are in the match{}
            // from waaay above)
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
                        // are written before exiting due to error.
                        panic!("error: {:?}", e)
                    }
                }
            }
        }

        // End loop
    }

    // Ensure that all writes to the history file are written before exiting.
    con.history.commit_to_file();
}
