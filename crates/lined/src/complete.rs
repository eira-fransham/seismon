use crate::EditorContext;

use super::event::Event;
use std::{borrow::Cow, path::PathBuf};

/// Trait representing how to autocomplete unfinished inputs.
pub trait Completer {
    /// Get the list of possible completions for the given prefix.
    ///
    /// > *NOTE*: This should return a sorted, deduplicated iterator.
    fn completions<'a>(&'a mut self, start: &'a str) -> impl Iterator<Item = Cow<'a, str>> + 'a;
    /// Respond to an editor event.
    fn on_event<C: EditorContext>(&mut self, _event: Event<C>) {}
}

/// Simple completer that just takes a set of prefixes.
pub struct BasicCompleter {
    prefixes: Vec<String>,
}

impl BasicCompleter {
    /// Create a new [`BasicCompleter`] from an iterator of prefixes.
    pub fn new<I>(prefixes: I) -> BasicCompleter
    where
        I: IntoIterator,
        I::Item: Into<String>, {
        let mut prefixes = prefixes.into_iter().map(Into::into).collect::<Vec<String>>();
        prefixes.sort();
        prefixes.dedup();

        BasicCompleter { prefixes }
    }
}

impl Completer for BasicCompleter {
    fn completions<'a>(&'a mut self, start: &'a str) -> impl Iterator<Item = Cow<'a, str>> + 'a {
        self.prefixes.iter().filter(move |s| s.starts_with(start)).map(Into::into)
    }
}

/// A completer based on filenames in a given directory.
pub struct FilenameCompleter {
    working_dir: Option<PathBuf>,
    case_sensitive: bool,
}

impl FilenameCompleter {
    /// Create a new [`FilenameCompleter`] in a given working directory (or `None` to use the
    /// current working directory from the environment).
    pub fn new<T: Into<PathBuf>>(working_dir: Option<T>) -> Self {
        FilenameCompleter { working_dir: working_dir.map(|p| p.into()), case_sensitive: true }
    }

    /// Create a new [`FilenameCompleter`] that optionally uses case-sensitive filename matching
    /// based on the `case_sensitive` argument.
    pub fn with_case_sensitivity<T: Into<PathBuf>>(
        working_dir: Option<T>,
        case_sensitive: bool,
    ) -> Self {
        FilenameCompleter { working_dir: working_dir.map(|p| p.into()), case_sensitive }
    }
}

impl Completer for FilenameCompleter {
    fn completions<'a>(
        &'a mut self,
        mut start: &'a str,
    ) -> impl Iterator<Item = Cow<'a, str>> + 'a {
        // XXX: this function is really bad, TODO rewrite

        let start_owned: String = if start.starts_with('\"') || start.starts_with('\'') {
            start = &start[1..];
            if !start.is_empty() {
                start = &start[..start.len() - 1];
            }
            start.into()
        } else {
            start.replace(r"\ ", " ")
        };

        let start_path = PathBuf::from(start_owned.as_str());

        let full_path = match self.working_dir {
            Some(ref wd) => {
                let mut fp = PathBuf::from(wd);
                fp.push(start_owned.as_str());
                fp
            }
            None => PathBuf::from(start_owned.as_str()),
        };

        let p;
        let start_name;
        let completing_dir;
        match full_path.parent() {
            // XXX non-unix separaor
            Some(parent)
                if !start.is_empty()
                    && !start_owned.ends_with('/')
                    && !full_path.ends_with("..") =>
            {
                p = parent;
                start_name = if self.case_sensitive {
                    full_path.file_name().unwrap().to_string_lossy().into_owned()
                } else {
                    full_path.file_name().unwrap().to_string_lossy().to_lowercase()
                };
                completing_dir = false;
            }
            _ => {
                p = full_path.as_path();
                start_name = "".into();
                completing_dir =
                    start.is_empty() || start.ends_with('/') || full_path.ends_with("..");
            }
        }

        let read_dir = match p.read_dir() {
            Ok(x) => x,
            Err(_) => return vec![].into_iter(),
        };

        let mut matches = vec![];
        for dir in read_dir {
            let dir = match dir {
                Ok(x) => x,
                Err(_) => continue,
            };
            let file_name = dir.file_name();
            let file_name = if self.case_sensitive {
                file_name.to_string_lossy().to_string()
            } else {
                file_name.to_string_lossy().to_lowercase()
            };

            if start_name.is_empty() || file_name.starts_with(&*start_name) {
                let mut a = start_path.clone();
                if !a.is_absolute() {
                    a = PathBuf::new();
                } else if !completing_dir && !a.pop() {
                    return vec![].into_iter();
                }

                a.push(dir.file_name());
                let mut s = a.to_string_lossy();
                if dir.path().is_dir() {
                    let mut string = s.into_owned();
                    string.push('/');
                    s = string.into();
                }

                let mut b = PathBuf::from(&start_owned);
                if !completing_dir {
                    b.pop();
                }
                b.push(s.as_ref());

                matches.push(b.to_string_lossy().replace(" ", r"\ ").into());
            }
        }

        matches.into_iter()
    }
}
