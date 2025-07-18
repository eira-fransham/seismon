use std::{cmp, io, mem};
use termion::event::Key;

use crate::buffer::Buffer;
use crate::KeyMap;
use crate::{Editor, EditorContext};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CharMovement {
    RightUntil,
    RightAt,
    LeftUntil,
    LeftAt,
    Repeat,
    ReverseRepeat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MoveType {
    Inclusive,
    Exclusive,
}

/// The editing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Insert,
    Normal,
    Replace,
    Delete(usize),
    MoveToChar(CharMovement),
    G,
    Tilde,
}

struct ModeStack(Vec<Mode>);

impl ModeStack {
    fn with_insert() -> Self {
        ModeStack(vec![Mode::Insert])
    }

    /// Get the current mode.
    ///
    /// If the stack is empty, we are in normal mode.
    fn mode(&self) -> Mode {
        self.0.last().cloned().unwrap_or(Mode::Normal)
    }

    /// Empty the stack and return to normal mode.
    fn clear(&mut self) {
        self.0.clear()
    }

    /// Push the given mode on to the stack.
    fn push(&mut self, m: Mode) {
        self.0.push(m)
    }

    fn pop(&mut self) -> Mode {
        self.0.pop().unwrap_or(Mode::Normal)
    }
}

fn is_movement_key(key: Key) -> bool {
    match key {
        Key::Char('h')
        | Key::Char('l')
        | Key::Left
        | Key::Right
        | Key::Char('w')
        | Key::Char('W')
        | Key::Char('b')
        | Key::Char('B')
        | Key::Char('e')
        | Key::Char('E')
        | Key::Char('g')
        | Key::Backspace
        | Key::Char(' ')
        | Key::Home
        | Key::End
        | Key::Char('^')
        | Key::Char('$')
        | Key::Char('t')
        | Key::Char('f')
        | Key::Char('T')
        | Key::Char('F')
        | Key::Char(';')
        | Key::Char(',') => true,
        _ => false,
    }
}

#[derive(PartialEq)]
enum ViMoveMode {
    Keyword,
    Whitespace,
}

#[derive(PartialEq, Clone, Copy)]
enum ViMoveDir {
    Left,
    Right,
}

impl ViMoveDir {
    pub fn advance(self, cursor: &mut usize, max: usize) -> bool {
        self.move_cursor(cursor, max, self)
    }

    pub fn go_back(self, cursor: &mut usize, max: usize) -> bool {
        match self {
            ViMoveDir::Right => self.move_cursor(cursor, max, ViMoveDir::Left),
            ViMoveDir::Left => self.move_cursor(cursor, max, ViMoveDir::Right),
        }
    }

    fn move_cursor(self, cursor: &mut usize, max: usize, dir: ViMoveDir) -> bool {
        if dir == ViMoveDir::Right && *cursor == max {
            return false;
        }

        if dir == ViMoveDir::Left && *cursor == 0 {
            return false;
        }

        match dir {
            ViMoveDir::Right => *cursor += 1,
            ViMoveDir::Left => *cursor -= 1,
        };
        true
    }
}

/// All alphanumeric characters and _ are considered valid for keywords in vi by default.
fn is_vi_keyword(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

fn move_word<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word(ed, ViMoveMode::Keyword, ViMoveDir::Right, count)
}

fn move_word_ws<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word(ed, ViMoveMode::Whitespace, ViMoveDir::Right, count)
}

fn move_to_end_of_word_back<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word(ed, ViMoveMode::Keyword, ViMoveDir::Left, count)
}

fn move_to_end_of_word_ws_back<C: EditorContext>(
    ed: &mut Editor<C>,
    count: usize,
) -> io::Result<()> {
    vi_move_word(ed, ViMoveMode::Whitespace, ViMoveDir::Left, count)
}

fn vi_move_word<C: EditorContext>(
    ed: &mut Editor<C>,
    move_mode: ViMoveMode,
    direction: ViMoveDir,
    count: usize,
) -> io::Result<()> {
    enum State {
        Whitespace,
        Keyword,
        NonKeyword,
    }

    let mut cursor = ed.cursor();
    'repeat: for _ in 0..count {
        let buf = ed.current_buffer();
        let mut state = match buf.char_after(cursor) {
            None => break,
            Some(c) => match c {
                c if c.is_whitespace() => State::Whitespace,
                c if is_vi_keyword(c) => State::Keyword,
                _ => State::NonKeyword,
            },
        };

        while direction.advance(&mut cursor, buf.num_chars()) {
            let c = match buf.char_after(cursor) {
                Some(c) => c,
                _ => break 'repeat,
            };

            match state {
                State::Whitespace => match c {
                    c if c.is_whitespace() => {}
                    _ => break,
                },
                State::Keyword => match c {
                    c if c.is_whitespace() => state = State::Whitespace,
                    c if move_mode == ViMoveMode::Keyword && !is_vi_keyword(c) => break,
                    _ => {}
                },
                State::NonKeyword => match c {
                    c if c.is_whitespace() => state = State::Whitespace,
                    c if move_mode == ViMoveMode::Keyword && is_vi_keyword(c) => break,
                    _ => {}
                },
            }
        }
    }

    ed.move_cursor_to(cursor)
}

fn move_to_end_of_word<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word_end(ed, ViMoveMode::Keyword, ViMoveDir::Right, count)
}

fn move_to_end_of_word_ws<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word_end(ed, ViMoveMode::Whitespace, ViMoveDir::Right, count)
}

fn move_word_back<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word_end(ed, ViMoveMode::Keyword, ViMoveDir::Left, count)
}

fn move_word_ws_back<C: EditorContext>(ed: &mut Editor<C>, count: usize) -> io::Result<()> {
    vi_move_word_end(ed, ViMoveMode::Whitespace, ViMoveDir::Left, count)
}

fn vi_move_word_end<C: EditorContext>(
    ed: &mut Editor<C>,
    move_mode: ViMoveMode,
    direction: ViMoveDir,
    count: usize,
) -> io::Result<()> {
    enum State {
        Whitespace,
        EndOnWord,
        EndOnOther,
        EndOnWhitespace,
    }

    let mut cursor = ed.cursor();
    'repeat: for _ in 0..count {
        let buf = ed.current_buffer();
        let mut state = State::Whitespace;

        while direction.advance(&mut cursor, buf.num_chars()) {
            let c = match buf.char_after(cursor) {
                Some(c) => c,
                _ => break 'repeat,
            };

            match state {
                State::Whitespace => match c {
                    // skip initial whitespace
                    c if c.is_whitespace() => {}
                    // if we are in keyword mode and found a keyword, stop on word
                    c if move_mode == ViMoveMode::Keyword && is_vi_keyword(c) => {
                        state = State::EndOnWord;
                    }
                    // not in keyword mode, stop on whitespace
                    _ if move_mode == ViMoveMode::Whitespace => {
                        state = State::EndOnWhitespace;
                    }
                    // in keyword mode, found non-whitespace non-keyword, stop on anything
                    _ => {
                        state = State::EndOnOther;
                    }
                },
                State::EndOnWord if !is_vi_keyword(c) => {
                    direction.go_back(&mut cursor, buf.num_chars());
                    break;
                }
                State::EndOnWhitespace if c.is_whitespace() => {
                    direction.go_back(&mut cursor, buf.num_chars());
                    break;
                }
                State::EndOnOther if c.is_whitespace() || is_vi_keyword(c) => {
                    direction.go_back(&mut cursor, buf.num_chars());
                    break;
                }
                _ => {}
            }
        }
    }

    ed.move_cursor_to(cursor)
}

fn find_char(buf: &Buffer, start: usize, ch: char, count: usize) -> Option<usize> {
    assert!(count > 0);
    buf.chars()
        .enumerate()
        .skip(start)
        .filter(|&(_, &c)| c == ch)
        .nth(count - 1)
        .map(|(i, _)| i)
}

fn find_char_rev(buf: &Buffer, start: usize, ch: char, count: usize) -> Option<usize> {
    assert!(count > 0);
    let rstart = buf.num_chars() - start;
    buf.chars()
        .enumerate()
        .rev()
        .skip(rstart)
        .filter(|&(_, &c)| c == ch)
        .nth(count - 1)
        .map(|(i, _)| i)
}

/// Vi keybindings for `Editor`.
///
/// ```no_run
/// use liner::{Context, KeyBindings};
/// let mut context = Context::new().unwrap();
/// context.key_bindings = KeyBindings::Vi;
/// ```
pub struct Vi {
    mode_stack: ModeStack,
    current_command: Vec<Key>,
    last_command: Vec<Key>,
    current_insert: Option<Key>,
    last_insert: Option<Key>,
    count: u32,
    secondary_count: u32,
    last_count: u32,
    movement_reset: bool,
    last_char_movement: Option<(char, CharMovement)>,
}

impl Default for Vi {
    fn default() -> Self {
        Vi {
            mode_stack: ModeStack::with_insert(),
            current_command: Vec::new(),
            last_command: Vec::new(),
            current_insert: None,
            // we start vi in insert mode
            last_insert: Some(Key::Char('i')),
            count: 0,
            secondary_count: 0,
            last_count: 0,
            movement_reset: false,
            last_char_movement: None,
        }
    }
}

impl Vi {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the current mode.
    fn mode(&self) -> Mode {
        self.mode_stack.mode()
    }

    fn set_mode<C: EditorContext>(&mut self, mode: Mode, ed: &mut Editor<C>) -> io::Result<()> {
        use self::Mode::*;
        self.set_mode_preserve_last(mode, ed)?;
        if mode == Insert {
            self.last_count = 0;
            self.last_command.clear();
        }
        Ok(())
    }

    fn set_editor_mode<C: EditorContext>(&self, ed: &mut Editor<C>) -> io::Result<()> {
        use crate::editor::ViPromptMode;
        use Mode::*;
        if let Some(mode) = match self.mode() {
            Insert => Some(ViPromptMode::Insert),
            Normal => Some(ViPromptMode::Normal),
            _ => None, // Leave the last one
        } {
            ed.set_vi_mode(mode);
            ed.display()
        } else {
            Ok(())
        }
    }

    fn set_mode_preserve_last<C: EditorContext>(
        &mut self,
        mode: Mode,
        mut ed: &mut Editor<C>,
    ) -> io::Result<()> {
        use self::Mode::*;

        ed.no_eol = mode == Normal;
        self.movement_reset = mode != Insert;
        self.mode_stack.push(mode);
        self.set_editor_mode(&mut ed)?;

        if mode == Insert || mode == Tilde {
            ed.current_buffer_mut().start_undo_group();
        }
        Ok(())
    }

    fn pop_mode_after_movement<C: EditorContext>(
        &mut self,
        move_type: MoveType,
        mut ed: &mut Editor<C>,
    ) -> io::Result<()> {
        use self::Mode::*;
        use self::MoveType::*;

        let original_mode = self.mode_stack.pop();
        let last_mode = {
            // after popping, if mode is delete or change, pop that too. This is used for movements
            // with sub commands like 't' (MoveToChar) and 'g' (G).
            match self.mode() {
                Delete(_) => self.mode_stack.pop(),
                _ => original_mode,
            }
        };

        ed.no_eol = self.mode() == Mode::Normal;
        self.movement_reset = self.mode() != Mode::Insert;

        if let Delete(start_pos) = last_mode {
            // perform the delete operation
            match move_type {
                Exclusive => ed.delete_until(start_pos)?,
                Inclusive => ed.delete_until_inclusive(start_pos)?,
            }

            // update the last state
            mem::swap(&mut self.last_command, &mut self.current_command);
            self.last_insert = self.current_insert;
            self.last_count = self.count;

            // reset our counts
            self.count = 0;
            self.secondary_count = 0;
        }

        // in normal mode, count goes back to 0 after movement
        if original_mode == Normal {
            self.count = 0;
        }

        self.set_editor_mode(&mut ed)
    }

    fn pop_mode<C: EditorContext>(&mut self, mut ed: &mut Editor<C>) -> io::Result<()> {
        use self::Mode::*;

        let last_mode = self.mode_stack.pop();
        ed.no_eol = self.mode() == Normal;
        self.movement_reset = self.mode() != Insert;

        if last_mode == Insert || last_mode == Tilde {
            ed.current_buffer_mut().end_undo_group();
        }

        if last_mode == Tilde {
            ed.display().unwrap();
        }
        self.set_editor_mode(&mut ed)
    }

    /// Return to normal mode.
    fn normal_mode_abort<C: EditorContext>(&mut self, mut ed: &mut Editor<C>) -> io::Result<()> {
        self.mode_stack.clear();
        ed.no_eol = true;
        self.count = 0;
        self.set_editor_mode(&mut ed)
    }

    /// When doing a move, 0 should behave the same as 1 as far as the count goes.
    fn move_count(&self) -> usize {
        match self.count {
            0 => 1,
            _ => self.count as usize,
        }
    }

    /// Get the current count or the number of remaining chars in the buffer.
    fn move_count_left<C: EditorContext>(&self, ed: &Editor<C>) -> usize {
        cmp::min(ed.cursor(), self.move_count())
    }

    /// Get the current count or the number of remaining chars in the buffer.
    fn move_count_right<C: EditorContext>(&self, ed: &Editor<C>) -> usize {
        cmp::min(
            ed.current_buffer().num_chars() - ed.cursor(),
            self.move_count(),
        )
    }

    fn repeat<C: EditorContext>(&mut self, ed: &mut Editor<C>) -> io::Result<()> {
        self.last_count = self.count;
        let keys = mem::replace(&mut self.last_command, Vec::new());

        if let Some(insert_key) = self.last_insert {
            // enter insert mode if necessary
            self.handle_key_core(insert_key, ed)?;
        }

        for k in &keys {
            self.handle_key_core(*k, ed)?;
        }

        if self.last_insert.is_some() {
            // leave insert mode
            self.handle_key_core(Key::Esc, ed)?;
        }

        // restore the last command
        self.last_command = keys;

        Ok(())
    }

    fn handle_key_common<C: EditorContext>(
        &mut self,
        key: Key,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        match key {
            Key::Ctrl('l') => ed.clear(),
            Key::Left => ed.move_cursor_left(1),
            Key::Right => ed.move_cursor_right(1),
            Key::Up => ed.move_up(),
            Key::Down => ed.move_down(),
            Key::Home => ed.move_cursor_to_start_of_line(),
            Key::End => ed.move_cursor_to_end_of_line(),
            Key::Backspace => ed.delete_before_cursor(),
            Key::Delete => ed.delete_after_cursor(),
            Key::Null => Ok(()),
            _ => Ok(()),
        }
    }

    fn handle_key_insert<C: EditorContext>(
        &mut self,
        key: Key,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        match key {
            Key::Esc | Key::Ctrl('[') => {
                // perform any repeats
                if self.count > 0 {
                    self.last_count = self.count;
                    for _ in 1..self.count {
                        let keys = mem::replace(&mut self.last_command, Vec::new());
                        for k in keys {
                            self.handle_key_core(k, ed)?;
                        }
                    }
                    self.count = 0;
                }
                // cursor moves to the left when switching from insert to normal mode
                ed.move_cursor_left(1)?;
                self.pop_mode(ed)?;
                Ok(())
            }
            Key::Char(c) => {
                if self.movement_reset {
                    ed.current_buffer_mut().end_undo_group();
                    ed.current_buffer_mut().start_undo_group();
                    self.last_command.clear();
                    self.movement_reset = false;
                    // vim behaves as if this was 'i'
                    self.last_insert = Some(Key::Char('i'));
                }
                self.last_command.push(key);
                ed.insert_after_cursor(c)
            }
            // delete and backspace need to be included in the command buffer
            Key::Backspace | Key::Delete => {
                if self.movement_reset {
                    ed.current_buffer_mut().end_undo_group();
                    ed.current_buffer_mut().start_undo_group();
                    self.last_command.clear();
                    self.movement_reset = false;
                    // vim behaves as if this was 'i'
                    self.last_insert = Some(Key::Char('i'));
                }
                self.last_command.push(key);
                self.handle_key_common(key, ed)
            }
            // if this is a movement while in insert mode, reset the repeat count
            Key::Left | Key::Right | Key::Home | Key::End => {
                self.count = 0;
                self.movement_reset = true;
                self.handle_key_common(key, ed)
            }
            // up and down require even more special handling
            Key::Up => {
                self.count = 0;
                self.movement_reset = true;
                ed.current_buffer_mut().end_undo_group();
                ed.move_up()?;
                ed.current_buffer_mut().start_undo_group();
                Ok(())
            }
            Key::Down => {
                self.count = 0;
                self.movement_reset = true;
                ed.current_buffer_mut().end_undo_group();
                ed.move_down()?;
                ed.current_buffer_mut().start_undo_group();
                Ok(())
            }
            _ => self.handle_key_common(key, ed),
        }
    }

    fn handle_key_normal<C: EditorContext>(
        &mut self,
        key: Key,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        use self::CharMovement::*;
        use self::Mode::*;
        use self::MoveType::*;

        match key {
            Key::Esc => {
                self.count = 0;
                Ok(())
            }
            Key::Char('i') => {
                self.last_insert = Some(key);
                self.set_mode(Insert, ed)?;
                Ok(())
            }
            Key::Char('a') => {
                self.last_insert = Some(key);
                self.set_mode(Insert, ed)?;
                ed.move_cursor_right(1)
            }
            Key::Char('A') => {
                self.last_insert = Some(key);
                self.set_mode(Insert, ed)?;
                ed.move_cursor_to_end_of_line()
            }
            Key::Char('I') => {
                self.last_insert = Some(key);
                self.set_mode(Insert, ed)?;
                ed.move_cursor_to_start_of_line()
            }
            Key::Char('s') => {
                self.last_insert = Some(key);
                self.set_mode(Insert, ed)?;
                let pos = ed.cursor() + self.move_count_right(ed);
                ed.delete_until(pos)?;
                self.last_count = self.count;
                self.count = 0;
                Ok(())
            }
            Key::Char('r') => {
                self.set_mode(Mode::Replace, ed)?;
                Ok(())
            }
            Key::Char('d') | Key::Char('c') => {
                self.current_command.clear();

                if key == Key::Char('d') {
                    // handle special 'd' key stuff
                    self.current_insert = None;
                    self.current_command.push(key);
                } else {
                    // handle special 'c' key stuff
                    self.current_insert = Some(key);
                    self.current_command.clear();
                    self.set_mode(Insert, ed)?;
                }

                let start_pos = ed.cursor();
                self.set_mode(Mode::Delete(start_pos), ed)?;
                self.secondary_count = self.count;
                self.count = 0;
                Ok(())
            }
            Key::Char('D') => {
                // update the last command state
                self.last_insert = None;
                self.last_command.clear();
                self.last_command.push(key);
                self.count = 0;
                self.last_count = 0;

                ed.delete_all_after_cursor()
            }
            Key::Char('C') => {
                // update the last command state
                self.last_insert = None;
                self.last_command.clear();
                self.last_command.push(key);
                self.count = 0;
                self.last_count = 0;

                self.set_mode_preserve_last(Insert, ed)?;
                ed.delete_all_after_cursor()
            }
            Key::Char('.') => {
                // repeat the last command
                self.count = match (self.count, self.last_count) {
                    // if both count and last_count are zero, use 1
                    (0, 0) => 1,
                    // if count is zero, use last_count
                    (0, _) => self.last_count,
                    // otherwise use count
                    (_, _) => self.count,
                };
                self.repeat(ed)
            }
            Key::Char('h') | Key::Left | Key::Backspace => {
                let count = self.move_count_left(ed);
                ed.move_cursor_left(count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('l') | Key::Right | Key::Char(' ') => {
                let count = self.move_count_right(ed);
                ed.move_cursor_right(count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('k') | Key::Up => {
                ed.move_up()?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('j') | Key::Down => {
                ed.move_down()?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('t') => {
                self.set_mode(Mode::MoveToChar(RightUntil), ed)?;
                Ok(())
            }
            Key::Char('T') => {
                self.set_mode(Mode::MoveToChar(LeftUntil), ed)?;
                Ok(())
            }
            Key::Char('f') => {
                self.set_mode(Mode::MoveToChar(RightAt), ed)?;
                Ok(())
            }
            Key::Char('F') => {
                self.set_mode(Mode::MoveToChar(LeftAt), ed)?;
                Ok(())
            }
            Key::Char(';') => self.handle_key_move_to_char(key, Repeat, ed),
            Key::Char(',') => self.handle_key_move_to_char(key, ReverseRepeat, ed),
            Key::Char('w') => {
                let count = self.move_count();
                move_word(ed, count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('W') => {
                let count = self.move_count();
                move_word_ws(ed, count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('e') => {
                let count = self.move_count();
                move_to_end_of_word(ed, count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('E') => {
                let count = self.move_count();
                move_to_end_of_word_ws(ed, count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('b') => {
                let count = self.move_count();
                move_word_back(ed, count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('B') => {
                let count = self.move_count();
                move_word_ws_back(ed, count)?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('g') => {
                self.set_mode(Mode::G, ed)?;
                Ok(())
            }
            // if count is 0, 0 should move to start of line
            Key::Char('0') if self.count == 0 => {
                ed.move_cursor_to_start_of_line()?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char(i @ '0'..='9') => {
                let i = i.to_digit(10).unwrap();
                // count = count * 10 + i
                self.count = self.count.saturating_mul(10).saturating_add(i);
                Ok(())
            }
            Key::Char('^') => {
                ed.move_cursor_to_start_of_line()?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('$') => {
                ed.move_cursor_to_end_of_line()?;
                self.pop_mode_after_movement(Exclusive, ed)
            }
            Key::Char('x') | Key::Delete => {
                // update the last command state
                self.last_insert = None;
                self.last_command.clear();
                self.last_command.push(key);
                self.last_count = self.count;

                let pos = ed.cursor() + self.move_count_right(ed);
                ed.delete_until(pos)?;
                self.count = 0;
                Ok(())
            }
            Key::Char('~') => {
                // update the last command state
                self.last_insert = None;
                self.last_command.clear();
                self.last_command.push(key);
                self.last_count = self.count;

                self.set_mode(Tilde, ed)?;
                for _ in 0..self.move_count_right(ed) {
                    let c = ed.current_buffer().char_after(ed.cursor()).unwrap();
                    if c.is_lowercase() {
                        ed.delete_after_cursor()?;
                        for c in c.to_uppercase() {
                            ed.insert_after_cursor(c)?;
                        }
                    } else if c.is_uppercase() {
                        ed.delete_after_cursor()?;
                        for c in c.to_lowercase() {
                            ed.insert_after_cursor(c)?;
                        }
                    } else {
                        ed.move_cursor_right(1)?;
                    }
                }
                self.pop_mode(ed)?;
                Ok(())
            }
            Key::Char('u') => {
                let count = self.move_count();
                self.count = 0;
                for _ in 0..count {
                    if !ed.undo()? {
                        break;
                    }
                }
                Ok(())
            }
            Key::Ctrl('r') => {
                let count = self.move_count();
                self.count = 0;
                for _ in 0..count {
                    let did = ed.redo()?;
                    if !did {
                        break;
                    }
                }
                Ok(())
            }
            _ => self.handle_key_common(key, ed),
        }
    }

    fn handle_key_replace<C: EditorContext>(
        &mut self,
        key: Key,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        match key {
            Key::Char(c) => {
                // make sure there are enough chars to replace
                if self.move_count_right(ed) == self.move_count() {
                    // update the last command state
                    self.last_insert = None;
                    self.last_command.clear();
                    self.last_command.push(Key::Char('r'));
                    self.last_command.push(key);
                    self.last_count = self.count;

                    // replace count characters
                    ed.current_buffer_mut().start_undo_group();
                    for _ in 0..self.move_count_right(ed) {
                        ed.delete_after_cursor()?;
                        ed.insert_after_cursor(c)?;
                    }
                    ed.current_buffer_mut().end_undo_group();

                    ed.move_cursor_left(1)?;
                }
                self.pop_mode(ed)?;
            }
            // not a char
            _ => {
                self.normal_mode_abort(ed)?;
            }
        };

        // back to normal mode
        self.count = 0;
        Ok(())
    }

    fn handle_key_delete_or_change<C: EditorContext>(
        &mut self,
        key: Key,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        match (key, self.current_insert) {
            // check if this is a movement key
            (key, _) if is_movement_key(key) | (key == Key::Char('0') && self.count == 0) => {
                // set count
                self.count = match (self.count, self.secondary_count) {
                    (0, 0) => 0,
                    (_, 0) => self.count,
                    (0, _) => self.secondary_count,
                    _ => {
                        // secondary_count * count
                        self.secondary_count.saturating_mul(self.count)
                    }
                };

                // update the last command state
                self.current_command.push(key);

                // execute movement
                self.handle_key_normal(key, ed)
            }
            // handle numeric keys
            (Key::Char('0'..='9'), _) => self.handle_key_normal(key, ed),
            (Key::Char('c'), Some(Key::Char('c'))) | (Key::Char('d'), None) => {
                // updating the last command buffer doesn't really make sense in this context.
                // Repeating 'dd' will simply erase and already erased line. Any other commands
                // will then become the new last command and the user will need to press 'dd' again
                // to clear the line. The same largely applies to the 'cc' command. We update the
                // last command here anyway ¯\_(ツ)_/¯
                self.current_command.push(key);

                // delete the whole line
                self.count = 0;
                self.secondary_count = 0;
                ed.move_cursor_to_start_of_line()?;
                ed.delete_all_after_cursor()?;

                // return to the previous mode
                self.pop_mode(ed)?;
                Ok(())
            }
            // not a delete or change command, back to normal mode
            _ => {
                self.normal_mode_abort(ed)?;
                Ok(())
            }
        }
    }

    fn handle_key_move_to_char<C: EditorContext>(
        &mut self,
        key: Key,
        movement: CharMovement,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        use self::CharMovement::*;
        use self::MoveType::*;

        let count = self.move_count();
        self.count = 0;

        let (key, movement) = match (key, movement, self.last_char_movement) {
            // repeat the last movement
            (_, Repeat, Some((c, last_movement))) => (Key::Char(c), last_movement),
            // repeat the last movement in the opposite direction
            (_, ReverseRepeat, Some((c, LeftUntil))) => (Key::Char(c), RightUntil),
            (_, ReverseRepeat, Some((c, RightUntil))) => (Key::Char(c), LeftUntil),
            (_, ReverseRepeat, Some((c, LeftAt))) => (Key::Char(c), RightAt),
            (_, ReverseRepeat, Some((c, RightAt))) => (Key::Char(c), LeftAt),
            // repeat with no last_char_movement, invalid
            (_, Repeat, None) | (_, ReverseRepeat, None) => {
                self.normal_mode_abort(ed)?;
                return Ok(());
            }
            // pass valid keys through as is
            (Key::Char(c), _, _) => {
                // store last command info
                self.last_char_movement = Some((c, movement));
                self.current_command.push(key);
                (key, movement)
            }
            // all other combinations are invalid, abort
            _ => {
                self.normal_mode_abort(ed)?;
                return Ok(());
            }
        };

        match key {
            Key::Char(c) => {
                let move_type;
                match movement {
                    RightUntil => {
                        move_type = Inclusive;
                        match find_char(ed.current_buffer(), ed.cursor() + 1, c, count) {
                            Some(i) => ed.move_cursor_to(i - 1),
                            None => Ok(()),
                        }
                    }
                    RightAt => {
                        move_type = Inclusive;
                        match find_char(ed.current_buffer(), ed.cursor() + 1, c, count) {
                            Some(i) => ed.move_cursor_to(i),
                            None => Ok(()),
                        }
                    }
                    LeftUntil => {
                        move_type = Exclusive;
                        match find_char_rev(ed.current_buffer(), ed.cursor(), c, count) {
                            Some(i) => ed.move_cursor_to(i + 1),
                            None => Ok(()),
                        }
                    }
                    LeftAt => {
                        move_type = Exclusive;
                        match find_char_rev(ed.current_buffer(), ed.cursor(), c, count) {
                            Some(i) => ed.move_cursor_to(i),
                            None => Ok(()),
                        }
                    }
                    Repeat | ReverseRepeat => unreachable!(),
                }?;

                // go back to the previous mode
                self.pop_mode_after_movement(move_type, ed)
            }

            // can't get here due to our match above
            _ => unreachable!(),
        }
    }

    fn handle_key_g<C: EditorContext>(&mut self, key: Key, ed: &mut Editor<C>) -> io::Result<()> {
        use self::MoveType::*;

        let count = self.move_count();
        self.current_command.push(key);

        let res = match key {
            Key::Char('e') => {
                move_to_end_of_word_back(ed, count)?;
                self.pop_mode_after_movement(Inclusive, ed)
            }
            Key::Char('E') => {
                move_to_end_of_word_ws_back(ed, count)?;
                self.pop_mode_after_movement(Inclusive, ed)
            }

            // not a supported command
            _ => {
                self.normal_mode_abort(ed)?;
                Ok(())
            }
        };

        self.count = 0;
        res
    }
}

impl KeyMap for Vi {
    fn init<C: EditorContext>(&mut self, mut ed: &mut Editor<C>) -> io::Result<()> {
        // since we start in insert mode, we need to start an undo group
        ed.current_buffer_mut().start_undo_group();
        self.set_editor_mode(&mut ed)
    }

    fn handle_key_core<C: EditorContext>(
        &mut self,
        key: Key,
        ed: &mut Editor<C>,
    ) -> io::Result<()> {
        match self.mode() {
            Mode::Normal => self.handle_key_normal(key, ed),
            Mode::Insert => self.handle_key_insert(key, ed),
            Mode::Replace => self.handle_key_replace(key, ed),
            Mode::Delete(_) => self.handle_key_delete_or_change(key, ed),
            Mode::MoveToChar(movement) => self.handle_key_move_to_char(key, movement, ed),
            Mode::G => self.handle_key_g(key, ed),
            Mode::Tilde => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editor::Prompt;
    use crate::{Buffer, Completer, Context, Editor, KeyMap};
    use termion::event::Key;
    use termion::event::Key::*;

    fn simulate_keys<'a, C: EditorContext, M: KeyMap, I>(
        keymap: &mut M,
        ed: &mut Editor<C>,
        keys: I,
    ) -> bool
    where
        I: IntoIterator<Item = &'a Key>,
    {
        for k in keys {
            if keymap.handle_key(*k, ed, &mut EmptyCompleter).unwrap() {
                return true;
            }
        }

        false
    }

    struct EmptyCompleter;

    impl Completer for EmptyCompleter {
        fn completions(&mut self, _start: &str) -> Vec<String> {
            Vec::default()
        }
    }

    #[test]
    fn enter_is_done() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("done").unwrap();
        assert_eq!(ed.cursor(), 4);

        assert!(simulate_keys(&mut map, &mut ed, [Char('\n'),].iter()));

        assert_eq!(ed.cursor(), 4);
        assert_eq!(String::from(ed), "done");
    }

    #[test]
    fn move_cursor_left() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("let").unwrap();
        assert_eq!(ed.cursor(), 3);

        simulate_keys(&mut map, &mut ed, [Left, Char('f')].iter());

        assert_eq!(ed.cursor(), 3);
        assert_eq!(String::from(ed), "left");
    }

    #[test]
    fn cursor_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("right").unwrap();
        assert_eq!(ed.cursor(), 5);

        simulate_keys(&mut map, &mut ed, [Left, Left, Right].iter());

        assert_eq!(ed.cursor(), 4);
    }

    #[test]
    fn move_cursor_start_end() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        let test_str = "let there be tests";
        ed.insert_str_after_cursor(test_str).unwrap();
        assert_eq!(ed.cursor(), test_str.len());

        simulate_keys(&mut map, &mut ed, [Esc, Char('^')].iter());
        assert_eq!(ed.cursor(), 0);

        simulate_keys(&mut map, &mut ed, [Char('^')].iter());
        assert_eq!(ed.cursor(), 0);

        simulate_keys(&mut map, &mut ed, [Char('$')].iter());
        assert_eq!(ed.cursor(), test_str.len() - 1);

        simulate_keys(&mut map, &mut ed, [Char('$')].iter());
        assert_eq!(ed.cursor(), test_str.len() - 1);
    }

    #[test]
    fn vi_initial_insert() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
            ]
            .iter(),
        );

        assert_eq!(ed.cursor(), 6);
        assert_eq!(String::from(ed), "insert");
    }

    #[test]
    fn vi_left_right_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(&mut map, &mut ed, [Left].iter());
        assert_eq!(ed.cursor(), 3);
        simulate_keys(&mut map, &mut ed, [Right].iter());
        assert_eq!(ed.cursor(), 4);

        // switching from insert mode moves the cursor left
        simulate_keys(&mut map, &mut ed, [Esc, Left].iter());
        assert_eq!(ed.cursor(), 2);
        simulate_keys(&mut map, &mut ed, [Right].iter());
        assert_eq!(ed.cursor(), 3);

        simulate_keys(&mut map, &mut ed, [Char('h')].iter());
        assert_eq!(ed.cursor(), 2);
        simulate_keys(&mut map, &mut ed, [Char('l')].iter());
        assert_eq!(ed.cursor(), 3);
    }

    #[test]
    /// Shouldn't be able to move past the last char in vi normal mode
    fn vi_no_eol() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(&mut map, &mut ed, [Esc].iter());
        assert_eq!(ed.cursor(), 3);

        simulate_keys(&mut map, &mut ed, [Right, Right].iter());
        assert_eq!(ed.cursor(), 3);

        // in insert mode, we can move past the last char, but no further
        simulate_keys(&mut map, &mut ed, [Char('i'), Right, Right].iter());
        assert_eq!(ed.cursor(), 4);
    }

    #[test]
    /// Cursor moves left when exiting insert mode.
    fn vi_switch_from_insert() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(&mut map, &mut ed, [Esc].iter());
        assert_eq!(ed.cursor(), 3);

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Char('i'),
                Esc,
                Char('i'),
                //Ctrl+[ is the same as escape
                Ctrl('['),
                Char('i'),
                Esc,
                Char('i'),
                Ctrl('['),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 0);
    }

    #[test]
    fn vi_normal_history_cursor_eol() {
        let mut context = Context::test();
        context.history.push("data history".into()).unwrap();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(&mut map, &mut ed, [Up].iter());
        assert_eq!(ed.cursor(), 12);

        // in normal mode, make sure we don't end up past the last char
        simulate_keys(&mut map, &mut ed, [Ctrl('['), Up].iter());
        assert_eq!(ed.cursor(), 11);
    }

    #[test]
    fn vi_normal_history() {
        let mut context = Context::test();
        context.history.push("data second".into()).unwrap();
        context.history.push("skip1".into()).unwrap();
        context.history.push("data one".into()).unwrap();
        context.history.push("skip2".into()).unwrap();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(&mut map, &mut ed, [Up].iter());
        assert_eq!(ed.cursor(), 8);

        // in normal mode, make sure we don't end up past the last char
        simulate_keys(&mut map, &mut ed, [Ctrl('['), Char('k')].iter());
        assert_eq!(ed.cursor(), 10);
    }

    #[test]
    fn vi_search_history() {
        // Test incremental search as well as vi binding in search mode.
        let mut context = Context::test();
        context.history.push("data pat second".into()).unwrap();
        context.history.push("skip1".into()).unwrap();
        context.history.push("data pat one".into()).unwrap();
        context.history.push("skip2".into()).unwrap();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("pat").unwrap();
        assert_eq!(ed.cursor(), 3);
        simulate_keys(&mut map, &mut ed, [Ctrl('r'), Right].iter());
        assert_eq!(ed.cursor(), 12);

        //simulate_keys(&mut map,     &mut ed, [Ctrl('['), Char('u'), Char('i')].iter());
        ed.delete_all_before_cursor().unwrap();
        assert_eq!(ed.cursor(), 0);
        //ed.insert_str_after_cursor("pat").unwrap();
        //assert_eq!(ed.cursor(), 3);
        simulate_keys(
            &mut map,
            &mut ed,
            [
                Ctrl('r'),
                Char('p'),
                Char('a'),
                Char('t'),
                Ctrl('['),
                Char('k'),
                Ctrl('f'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 14);

        simulate_keys(&mut map, &mut ed, [Ctrl('['), Char('u'), Char('i')].iter());
        assert_eq!(ed.cursor(), 0);
        simulate_keys(
            &mut map,
            &mut ed,
            [Ctrl('s'), Char('p'), Char('a'), Char('t'), Ctrl('f')].iter(),
        );
        assert_eq!(ed.cursor(), 15);

        ed.delete_all_before_cursor().unwrap();
        assert_eq!(ed.cursor(), 0);
        ed.insert_str_after_cursor("pat").unwrap();
        assert_eq!(ed.cursor(), 3);
        simulate_keys(
            &mut map,
            &mut ed,
            [Ctrl('s'), Ctrl('['), Char('j'), Right].iter(),
        );
        assert_eq!(ed.cursor(), 11);
    }

    #[test]
    fn vi_normal_delete() {
        let mut context = Context::test();
        context.history.push("history".into()).unwrap();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Delete, Char('x')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "ta");
    }
    #[test]
    fn vi_substitute_command() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(
            &mut map,
            &mut ed,
            [
                //ctrl+[ is the same as Esc
                Ctrl('['),
                Char('0'),
                Char('s'),
                Char('s'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "sata");
    }

    #[test]
    fn substitute_with_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data").unwrap();
        assert_eq!(ed.cursor(), 4);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('2'), Char('s'), Char('b'), Char('e')].iter(),
        );
        assert_eq!(String::from(ed), "beta");
    }

    #[test]
    fn substitute_with_count_repeat() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("data data").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('2'),
                Char('s'),
                Char('b'),
                Char('e'),
                //The same as Esc
                Ctrl('['),
                Char('4'),
                Char('l'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "beta beta");
    }

    #[test]
    /// make sure our count is accurate
    fn vi_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(&mut map, &mut ed, [Esc].iter());
        assert_eq!(map.count, 0);

        simulate_keys(&mut map, &mut ed, [Char('1')].iter());
        assert_eq!(map.count, 1);

        simulate_keys(&mut map, &mut ed, [Char('1')].iter());
        assert_eq!(map.count, 11);

        // switching to insert mode and back to edit mode should reset the count
        simulate_keys(&mut map, &mut ed, [Char('i'), Esc].iter());
        assert_eq!(map.count, 0);

        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// make sure large counts don't overflow
    fn vi_count_overflow() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        // make sure large counts don't overflow our u32
        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
                Char('9'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// make sure large counts ending in zero don't overflow
    fn vi_count_overflow_zero() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        // make sure large counts don't overflow our u32
        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('1'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
                Char('0'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// Esc should cancel the count
    fn vi_count_cancel() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('1'), Char('0'), Esc].iter());
        assert_eq!(map.count, 0);
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test insert with a count
    fn vi_count_simple() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                //same as Esc
                Ctrl('['),
                Char('3'),
                Char('i'),
                Char('t'),
                Char('h'),
                Char('i'),
                Char('s'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "thisthisthis");
    }

    #[test]
    /// test dot command
    fn vi_dot_command() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Char('i'), Char('f'), Esc, Char('.'), Char('.')].iter(),
        );
        assert_eq!(String::from(ed), "iiifff");
    }

    #[test]
    /// test dot command with repeat
    fn vi_dot_command_repeat() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Char('i'), Char('f'), Esc, Char('3'), Char('.')].iter(),
        );
        assert_eq!(String::from(ed), "iifififf");
    }

    #[test]
    /// test dot command with repeat
    fn vi_dot_command_repeat_multiple() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Char('i'), Char('f'), Esc, Char('3'), Char('.'), Char('.')].iter(),
        );
        assert_eq!(String::from(ed), "iififiifififff");
    }

    #[test]
    /// test dot command with append
    fn vi_dot_command_append() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('a'),
                Char('i'),
                Char('f'),
                Esc,
                Char('.'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "ififif");
    }

    #[test]
    /// test dot command with append and repeat
    fn vi_dot_command_append_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('a'),
                Char('i'),
                Char('f'),
                Esc,
                Char('3'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "ifififif");
    }

    #[test]
    /// test dot command with movement
    fn vi_dot_command_movement() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('a'),
                Char('d'),
                Char('t'),
                Char(' '),
                Left,
                Left,
                Char('a'),
                Esc,
                Right,
                Right,
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "data ");
    }

    #[test]
    /// test move_count function
    fn move_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        assert_eq!(map.move_count(), 1);
        map.count = 1;
        assert_eq!(map.move_count(), 1);
        map.count = 99;
        assert_eq!(map.move_count(), 99);
    }

    #[test]
    /// make sure the count is reset if movement occurs
    fn vi_count_movement_reset() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('3'),
                Char('i'),
                Char('t'),
                Char('h'),
                Char('i'),
                Char('s'),
                Left,
                Esc,
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "this");
    }

    #[test]
    /// test movement with counts
    fn movement_with_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("right").unwrap();
        assert_eq!(ed.cursor(), 5);

        simulate_keys(&mut map, &mut ed, [Esc, Char('3'), Left].iter());

        assert_eq!(ed.cursor(), 1);
    }

    #[test]
    /// test movement with counts, then insert (count should be reset before insert)
    fn movement_with_count_then_insert() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("right").unwrap();
        assert_eq!(ed.cursor(), 5);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('3'), Left, Char('i'), Char(' '), Esc].iter(),
        );
        assert_eq!(String::from(ed), "r ight");
    }

    #[test]
    /// make sure we only attempt to repeat for as many chars are in the buffer
    fn count_at_buffer_edge() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('3'), Char('r'), Char('x')].iter(),
        );
        // the cursor should not have moved and no change should have occured
        assert_eq!(ed.cursor(), 6);
        assert_eq!(String::from(ed), "replace");
    }

    #[test]
    /// test basic replace
    fn basic_replace() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(&mut map, &mut ed, [Esc, Char('r'), Char('x')].iter());
        assert_eq!(ed.cursor(), 6);
        assert_eq!(String::from(ed), "replacx");
    }

    #[test]
    fn replace_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('3'), Char('r'), Char(' ')].iter(),
        );
        // cursor should be on the last replaced char
        assert_eq!(ed.cursor(), 2);
        assert_eq!(String::from(ed), "   lace");
    }

    #[test]
    /// make sure replace won't work if there aren't enough chars
    fn replace_with_count_eol() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('3'), Char('r'), Char('x')].iter(),
        );
        // the cursor should not have moved and no change should have occured
        assert_eq!(ed.cursor(), 6);
        assert_eq!(String::from(ed), "replace");
    }

    #[test]
    /// make sure normal mode is enabled after replace
    fn replace_then_normal() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('r'), Char('x'), Char('0')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "replacx");
    }

    #[test]
    /// test replace with dot
    fn dot_replace() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('r'),
                Char('x'),
                Char('.'),
                Char('.'),
                Char('7'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "xxxxxxx");
    }

    #[test]
    /// test replace with dot
    fn dot_replace_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('2'),
                Char('r'),
                Char('x'),
                Char('.'),
                Char('.'),
                Char('.'),
                Char('.'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "xxxxxxx");
    }

    #[test]
    /// test replace with dot at eol
    fn dot_replace_eol() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("test").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('3'),
                Char('r'),
                Char('x'),
                Char('.'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "xxxt");
    }

    #[test]
    /// test replace with dot at eol multiple times
    fn dot_replace_eol_multiple() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("this is a test").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('3'),
                Char('r'),
                Char('x'),
                Char('$'),
                Char('.'),
                Char('4'),
                Char('h'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "xxxs is axxxst");
    }

    #[test]
    /// verify our move count
    fn move_count_right() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);
        assert_eq!(map.move_count_right(&ed), 0);
        map.count = 10;
        assert_eq!(map.move_count_right(&ed), 0);

        map.count = 0;

        simulate_keys(&mut map, &mut ed, [Esc, Left].iter());
        assert_eq!(map.move_count_right(&ed), 1);
    }

    #[test]
    /// verify our move count
    fn move_count_left() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);
        assert_eq!(map.move_count_left(&ed), 1);
        map.count = 10;
        assert_eq!(map.move_count_left(&ed), 7);

        map.count = 0;

        simulate_keys(&mut map, &mut ed, [Esc, Char('0')].iter());
        assert_eq!(map.move_count_left(&ed), 0);
    }

    #[test]
    /// test delete with dot
    fn dot_x_delete() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace").unwrap();
        assert_eq!(ed.cursor(), 7);

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('2'), Char('x'), Char('.')].iter(),
        );
        assert_eq!(String::from(ed), "ace");
    }

    #[test]
    /// test deleting a line
    fn delete_line() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('d'), Char('d')].iter());
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test for normal mode after deleting a line
    fn delete_line_normal() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('d'),
                Char('d'),
                Char('i'),
                Char('n'),
                Char('e'),
                Char('w'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 2);
        assert_eq!(String::from(ed), "new");
    }

    #[test]
    /// test aborting a delete (and change)
    fn delete_abort() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("don't delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('d'),
                Esc,
                Char('d'),
                Char('c'),
                Char('c'),
                Char('d'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 11);
        assert_eq!(String::from(ed), "don't delete");
    }

    #[test]
    /// test deleting a single char to the left
    fn delete_char_left() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('d'), Char('h')].iter());
        assert_eq!(ed.cursor(), 4);
        assert_eq!(String::from(ed), "delee");
    }

    #[test]
    /// test deleting multiple chars to the left
    fn delete_chars_left() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('3'), Char('d'), Char('h')].iter(),
        );
        assert_eq!(ed.cursor(), 2);
        assert_eq!(String::from(ed), "dee");
    }

    #[test]
    /// test deleting a single char to the right
    fn delete_char_right() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('d'), Char('l')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "elete");
    }

    #[test]
    /// test deleting multiple chars to the right
    fn delete_chars_right() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('3'), Char('d'), Char('l')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "ete");
    }

    #[test]
    /// test repeat with delete
    fn delete_and_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('d'), Char('l'), Char('.')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "lete");
    }

    #[test]
    /// test delete until end of line
    fn delete_until_end() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('d'), Char('$')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test delete until end of line
    fn delete_until_end_shift_d() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('0'), Char('D')].iter());
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test delete until start of line
    fn delete_until_start() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('$'), Char('d'), Char('0')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "e");
    }

    #[test]
    /// test a compound count with delete
    fn delete_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('2'), Char('d'), Char('2'), Char('l')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "te");
    }

    #[test]
    /// test a compound count with delete and repeat
    fn delete_with_count_and_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete delete").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('2'),
                Char('d'),
                Char('2'),
                Char('l'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "elete");
    }

    #[test]
    fn move_to_end_of_word_simple() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here are").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor(" som").unwrap();
        let end_pos = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos);
    }

    #[test]
    fn move_to_end_of_word_comma() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ar").unwrap();
        let start_pos = ed.cursor();
        ed.insert_after_cursor('e').unwrap();
        let end_pos1 = ed.cursor();
        ed.insert_str_after_cursor(", som").unwrap();
        let end_pos2 = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos1);
        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos2);
    }

    #[test]
    fn move_to_end_of_word_nonkeywords() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ar").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor("e,,,").unwrap();
        let end_pos1 = ed.cursor();
        ed.insert_str_after_cursor(",som").unwrap();
        let end_pos2 = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos1);
        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos2);
    }

    #[test]
    fn move_to_end_of_word_whitespace() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        assert_eq!(ed.cursor(), 0);
        ed.insert_str_after_cursor("here are").unwrap();
        let start_pos = ed.cursor();
        assert_eq!(ed.cursor(), 8);
        ed.insert_str_after_cursor("      som").unwrap();
        assert_eq!(ed.cursor(), 17);
        ed.insert_str_after_cursor("e words").unwrap();
        assert_eq!(ed.cursor(), 24);
        ed.move_cursor_to(start_pos).unwrap();
        assert_eq!(ed.cursor(), 8);

        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), 17);
    }

    #[test]
    fn move_to_end_of_word_whitespace_nonkeywords() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ar").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor("e   ,,,").unwrap();
        let end_pos1 = ed.cursor();
        ed.insert_str_after_cursor(", som").unwrap();
        let end_pos2 = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos1);
        super::move_to_end_of_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos2);
    }

    #[test]
    fn move_to_end_of_word_ws_simple() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here are").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor(" som").unwrap();
        let end_pos = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos);
    }

    #[test]
    fn move_to_end_of_word_ws_comma() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ar").unwrap();
        let start_pos = ed.cursor();
        ed.insert_after_cursor('e').unwrap();
        let end_pos1 = ed.cursor();
        ed.insert_str_after_cursor(", som").unwrap();
        let end_pos2 = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos1);
        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos2);
    }

    #[test]
    fn move_to_end_of_word_ws_nonkeywords() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ar").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor("e,,,,som").unwrap();
        let end_pos = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();
        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos);
    }

    #[test]
    fn move_to_end_of_word_ws_whitespace() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here are").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor("      som").unwrap();
        let end_pos = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos);
    }

    #[test]
    fn move_to_end_of_word_ws_whitespace_nonkeywords() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ar").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor("e   ,,,").unwrap();
        let end_pos1 = ed.cursor();
        ed.insert_str_after_cursor(", som").unwrap();
        let end_pos2 = ed.cursor();
        ed.insert_str_after_cursor("e words").unwrap();
        ed.move_cursor_to(start_pos).unwrap();

        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos1);
        super::move_to_end_of_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), end_pos2);
    }

    #[test]
    fn move_word_simple() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ").unwrap();
        let pos1 = ed.cursor();
        ed.insert_str_after_cursor("are ").unwrap();
        let pos2 = ed.cursor();
        ed.insert_str_after_cursor("some words").unwrap();
        ed.move_cursor_to_start_of_line().unwrap();

        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
    }

    #[test]
    fn move_word_whitespace() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("   ").unwrap();
        let pos1 = ed.cursor();
        ed.insert_str_after_cursor("word").unwrap();
        let pos2 = ed.cursor();
        ed.move_cursor_to_start_of_line().unwrap();

        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
    }

    #[test]
    fn move_word_nonkeywords() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("..=").unwrap();
        let pos1 = ed.cursor();
        ed.insert_str_after_cursor("word").unwrap();
        let pos2 = ed.cursor();
        ed.move_cursor_to_start_of_line().unwrap();

        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
    }

    #[test]
    fn move_word_whitespace_nonkeywords() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("..=   ").unwrap();
        let pos1 = ed.cursor();
        ed.insert_str_after_cursor("..=").unwrap();
        let pos2 = ed.cursor();
        ed.insert_str_after_cursor("word").unwrap();
        let pos3 = ed.cursor();
        ed.move_cursor_to_start_of_line().unwrap();

        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos3);
    }

    #[test]
    fn move_word_and_back() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ").unwrap();
        let pos1 = ed.cursor();
        ed.insert_str_after_cursor("are ").unwrap();
        let pos2 = ed.cursor();
        ed.insert_str_after_cursor("some").unwrap();
        let pos3 = ed.cursor();
        ed.insert_str_after_cursor("..= ").unwrap();
        let pos4 = ed.cursor();
        ed.insert_str_after_cursor("words").unwrap();
        let pos5 = ed.cursor();

        // make sure move_word() and move_word_back() are reflections of eachother

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos3);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos4);
        super::move_word(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos5);

        super::move_word_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos4);
        super::move_word_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos3);
        super::move_word_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
        super::move_word_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), 0);

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos4);
        super::move_word_ws(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos5);

        super::move_word_ws_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos4);
        super::move_word_ws_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos2);
        super::move_word_ws_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws_back(&mut ed, 1).unwrap();
        assert_eq!(ed.cursor(), 0);
    }

    #[test]
    fn move_word_and_back_with_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here ").unwrap();
        ed.insert_str_after_cursor("are ").unwrap();
        let pos1 = ed.cursor();
        ed.insert_str_after_cursor("some").unwrap();
        let pos2 = ed.cursor();
        ed.insert_str_after_cursor("..= ").unwrap();
        ed.insert_str_after_cursor("words").unwrap();
        let pos3 = ed.cursor();

        // make sure move_word() and move_word_back() are reflections of eachother
        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word(&mut ed, 3).unwrap();
        assert_eq!(ed.cursor(), pos2);
        super::move_word(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), pos3);

        super::move_word_back(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), pos2);
        super::move_word_back(&mut ed, 3).unwrap();
        assert_eq!(ed.cursor(), 0);

        ed.move_cursor_to_start_of_line().unwrap();
        super::move_word_ws(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), pos3);

        super::move_word_ws_back(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), pos1);
        super::move_word_ws_back(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), 0);
    }

    #[test]
    fn move_to_end_of_word_ws_whitespace_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();

        ed.insert_str_after_cursor("here are").unwrap();
        let start_pos = ed.cursor();
        ed.insert_str_after_cursor("      som").unwrap();
        ed.insert_str_after_cursor("e word").unwrap();
        let end_pos = ed.cursor();
        ed.insert_str_after_cursor("s and some").unwrap();

        ed.move_cursor_to(start_pos).unwrap();
        super::move_to_end_of_word_ws(&mut ed, 2).unwrap();
        assert_eq!(ed.cursor(), end_pos);
    }

    #[test]
    /// test delete word
    fn delete_word() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("delete some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('d'), Char('w')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "some words");
    }

    #[test]
    /// test changing a line
    fn change_line() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('c'),
                Char('c'),
                Char('d'),
                Char('o'),
                Char('n'),
                Char('e'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 4);
        assert_eq!(String::from(ed), "done");
    }

    #[test]
    /// test deleting a single char to the left
    fn change_char_left() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('c'), Char('h'), Char('e'), Esc].iter(),
        );
        assert_eq!(ed.cursor(), 4);
        assert_eq!(String::from(ed), "chanee");
    }

    #[test]
    /// test deleting multiple chars to the left
    fn change_chars_left() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('3'), Char('c'), Char('h'), Char('e')].iter(),
        );
        assert_eq!(ed.cursor(), 3);
        assert_eq!(String::from(ed), "chee");
    }

    #[test]
    /// test deleting a single char to the right
    fn change_char_right() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('c'), Char('l'), Char('s')].iter(),
        );
        assert_eq!(ed.cursor(), 1);
        assert_eq!(String::from(ed), "shange");
    }

    #[test]
    /// test changing multiple chars to the right
    fn change_chars_right() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('3'),
                Char('c'),
                Char('l'),
                Char('s'),
                Char('t'),
                Char('r'),
                Char('a'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 3);
        assert_eq!(String::from(ed), "strange");
    }

    #[test]
    /// test repeat with change
    fn change_and_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('c'),
                Char('l'),
                Char('s'),
                Esc,
                Char('l'),
                Char('.'),
                Char('l'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 2);
        assert_eq!(String::from(ed), "sssnge");
    }

    #[test]
    /// test change until end of line
    fn change_until_end() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('c'),
                Char('$'),
                Char('o'),
                Char('k'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 1);
        assert_eq!(String::from(ed), "ok");
    }

    #[test]
    /// test change until end of line
    fn change_until_end_shift_c() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('C'), Char('o'), Char('k')].iter(),
        );
        assert_eq!(ed.cursor(), 2);
        assert_eq!(String::from(ed), "ok");
    }

    #[test]
    /// test change until end of line
    fn change_until_end_from_middle_shift_c() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('2'),
                Char('l'),
                Char('C'),
                Char(' '),
                Char('o'),
                Char('k'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "ch ok");
    }

    #[test]
    /// test change until start of line
    fn change_until_start() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('$'),
                Char('c'),
                Char('0'),
                Char('s'),
                Char('t'),
                Char('r'),
                Char('a'),
                Char('n'),
                Char('g'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 6);
        assert_eq!(String::from(ed), "strange");
    }

    #[test]
    /// test a compound count with change
    fn change_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('2'),
                Char('c'),
                Char('2'),
                Char('l'),
                Char('s'),
                Char('t'),
                Char('r'),
                Char('a'),
                Char('n'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 4);
        assert_eq!(String::from(ed), "strange");
    }

    #[test]
    /// test a compound count with change and repeat
    fn change_with_count_and_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change change").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('2'),
                Char('c'),
                Char('2'),
                Char('l'),
                Char('o'),
                Esc,
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "ochange");
    }

    #[test]
    /// test change word
    fn change_word() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('c'),
                Char('w'),
                Char('t'),
                Char('w'),
                Char('e'),
                Char('a'),
                Char('k'),
                Char(' '),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "tweak some words");
    }

    #[test]
    /// make sure the count is properly reset
    fn test_count_reset_around_insert_and_delete() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("these are some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('d'),
                Char('3'),
                Char('w'),
                Char('i'),
                Char('w'),
                Char('o'),
                Char('r'),
                Char('d'),
                Char('s'),
                Char(' '),
                Esc,
                Char('l'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "words words words");
    }

    #[test]
    /// make sure t command does nothing if nothing was found
    fn test_t_not_found() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('t'), Char('z')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
    }

    #[test]
    /// make sure t command moves the cursor
    fn test_t_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('t'), Char('d')].iter(),
        );
        assert_eq!(ed.cursor(), 3);
    }

    #[test]
    /// make sure t command moves the cursor
    fn test_t_movement_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg d").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('2'), Char('t'), Char('d')].iter(),
        );
        assert_eq!(ed.cursor(), 8);
    }

    #[test]
    /// test normal mode after char movement
    fn test_t_movement_then_normal() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('t'), Char('d'), Char('l')].iter(),
        );
        assert_eq!(ed.cursor(), 4);
    }

    #[test]
    /// test delete with char movement
    fn test_t_movement_with_delete() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('d'), Char('t'), Char('d')].iter(),
        );
        assert_eq!(ed.cursor(), 0);
        assert_eq!(String::from(ed), "defg");
    }

    #[test]
    /// test change with char movement
    fn test_t_movement_with_change() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('c'),
                Char('t'),
                Char('d'),
                Char('z'),
                Char(' '),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(ed.cursor(), 1);
        assert_eq!(String::from(ed), "z defg");
    }

    #[test]
    /// make sure f command moves the cursor
    fn test_f_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('f'), Char('d')].iter(),
        );
        assert_eq!(ed.cursor(), 4);
    }

    #[test]
    /// make sure T command moves the cursor
    fn test_cap_t_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('$'), Char('T'), Char('d')].iter(),
        );
        assert_eq!(ed.cursor(), 5);
    }

    #[test]
    /// make sure F command moves the cursor
    fn test_cap_f_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('$'), Char('F'), Char('d')].iter(),
        );
        assert_eq!(ed.cursor(), 4);
    }

    #[test]
    /// make sure ; command moves the cursor
    fn test_semi_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc abc").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('f'), Char('c'), Char(';')].iter(),
        );
        assert_eq!(ed.cursor(), 6);
    }

    #[test]
    /// make sure , command moves the cursor
    fn test_comma_movement() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc abc").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('f'), Char('c'), Char('$'), Char(',')].iter(),
        );
        assert_eq!(ed.cursor(), 2);
    }

    #[test]
    /// test delete with semi (;)
    fn test_semi_delete() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc abc").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('f'), Char('c'), Char('d'), Char(';')].iter(),
        );
        assert_eq!(ed.cursor(), 1);
        assert_eq!(String::from(ed), "ab");
    }

    #[test]
    /// test delete with semi (;) and repeat
    fn test_semi_delete_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc abc abc abc").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('f'),
                Char('c'),
                Char('d'),
                Char(';'),
                Char('.'),
                Char('.'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "ab");
    }

    #[test]
    /// test find_char
    fn test_find_char() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcdefg").unwrap();
        assert_eq!(super::find_char(ed.current_buffer(), 0, 'd', 1), Some(3));
    }

    #[test]
    /// test find_char with non-zero start
    fn test_find_char_with_start() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcabc").unwrap();
        assert_eq!(super::find_char(ed.current_buffer(), 1, 'a', 1), Some(3));
    }

    #[test]
    /// test find_char with count
    fn test_find_char_with_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcabc").unwrap();
        assert_eq!(super::find_char(ed.current_buffer(), 0, 'a', 2), Some(3));
    }

    #[test]
    /// test find_char not found
    fn test_find_char_not_found() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcdefg").unwrap();
        assert_eq!(super::find_char(ed.current_buffer(), 0, 'z', 1), None);
    }

    #[test]
    /// test find_char_rev
    fn test_find_char_rev() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcdefg").unwrap();
        assert_eq!(
            super::find_char_rev(ed.current_buffer(), 6, 'd', 1),
            Some(3)
        );
    }

    #[test]
    /// test find_char_rev with non-zero start
    fn test_find_char_rev_with_start() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcabc").unwrap();
        assert_eq!(
            super::find_char_rev(ed.current_buffer(), 5, 'c', 1),
            Some(2)
        );
    }

    #[test]
    /// test find_char_rev with count
    fn test_find_char_rev_with_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcabc").unwrap();
        assert_eq!(
            super::find_char_rev(ed.current_buffer(), 6, 'c', 2),
            Some(2)
        );
    }

    #[test]
    /// test find_char_rev not found
    fn test_find_char_rev_not_found() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        ed.insert_str_after_cursor("abcdefg").unwrap();
        assert_eq!(super::find_char_rev(ed.current_buffer(), 6, 'z', 1), None);
    }

    #[test]
    /// undo with counts
    fn test_undo_with_counts() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abcdefg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('x'), Char('x'), Char('x'), Char('3'), Char('u')].iter(),
        );
        assert_eq!(String::from(ed), "abcdefg");
    }

    #[test]
    /// redo with counts
    fn test_redo_with_counts() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abcdefg").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('x'),
                Char('x'),
                Char('x'),
                Char('u'),
                Char('u'),
                Char('u'),
            ]
            .iter(),
        );
        // Ctrl-r taken by incremental search so do this manually.
        ed.redo().unwrap();
        ed.redo().unwrap();
        assert_eq!(String::from(ed), "abcde");
    }

    #[test]
    /// test change word with 'gE'
    fn change_word_ge_ws() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("change some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('c'),
                Char('g'),
                Char('E'),
                Char('e'),
                Char('t'),
                Char('h'),
                Char('i'),
                Char('n'),
                Char('g'),
                Esc,
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "change something");
    }

    #[test]
    /// test undo in groups
    fn undo_insert() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test undo in groups
    fn undo_insert2() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('i'),
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test undo in groups
    fn undo_insert_with_history() {
        let mut context = Context::test();
        context
            .history
            .push(Buffer::from("insert something"))
            .unwrap();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('i'),
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Up,
                Char('h'),
                Char('i'),
                Char('s'),
                Char('t'),
                Char('o'),
                Char('r'),
                Char('y'),
                Down,
                Char(' '),
                Char('t'),
                Char('e'),
                Char('x'),
                Char('t'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "insert");
    }

    #[test]
    /// test undo in groups
    fn undo_insert_with_history2() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('i'),
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Up,
                Esc,
                Down,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "");
    }

    #[test]
    /// test undo in groups
    fn undo_insert_with_movement_reset() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('i'),
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                // movement reset will get triggered here
                Left,
                Right,
                Char(' '),
                Char('t'),
                Char('e'),
                Char('x'),
                Char('t'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "insert");
    }

    #[test]
    /// test undo in groups
    fn undo_3x() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("rm some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('3'), Char('x'), Char('u')].iter(),
        );
        assert_eq!(String::from(ed), "rm some words");
    }

    #[test]
    /// test undo in groups
    fn undo_insert_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Esc,
                Char('3'),
                Char('i'),
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "insert");
    }

    #[test]
    /// test undo in groups
    fn undo_insert_with_repeat() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Char('i'),
                Char('n'),
                Char('s'),
                Char('e'),
                Char('r'),
                Char('t'),
                Esc,
                Char('3'),
                Char('.'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "insert");
    }

    #[test]
    /// test undo in groups
    fn undo_s_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('0'),
                Char('8'),
                Char('s'),
                Char('o'),
                Char('k'),
                Esc,
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "replace some words");
    }

    #[test]
    /// test undo in groups
    fn undo_multiple_groups() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [
                Esc,
                Char('A'),
                Char(' '),
                Char('h'),
                Char('e'),
                Char('r'),
                Char('e'),
                Esc,
                Char('0'),
                Char('8'),
                Char('s'),
                Char('o'),
                Char('k'),
                Esc,
                Char('2'),
                Char('u'),
            ]
            .iter(),
        );
        assert_eq!(String::from(ed), "replace some words");
    }

    #[test]
    /// test undo in groups
    fn undo_r_command_with_count() {
        let mut context = Context::test();

        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("replace some words").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('8'), Char('r'), Char(' '), Char('u')].iter(),
        );
        assert_eq!(String::from(ed), "replace some words");
    }

    #[test]
    /// test tilde
    fn tilde_basic() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("tilde").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('~')].iter());
        assert_eq!(String::from(ed), "tildE");
    }

    #[test]
    /// test tilde
    fn tilde_basic2() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("tilde").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('~'), Char('~')].iter());
        assert_eq!(String::from(ed), "tilde");
    }

    #[test]
    /// test tilde
    fn tilde_move() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("tilde").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('~'), Char('~')].iter(),
        );
        assert_eq!(String::from(ed), "TIlde");
    }

    #[test]
    /// test tilde
    fn tilde_repeat() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("tilde").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('~'), Char('.')].iter());
        assert_eq!(String::from(ed), "tilde");
    }

    #[test]
    /// test tilde
    fn tilde_count() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("tilde").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('1'), Char('0'), Char('~')].iter(),
        );
        assert_eq!(String::from(ed), "TILDE");
    }

    #[test]
    /// test tilde
    fn tilde_count_short() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("TILDE").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('2'), Char('~')].iter(),
        );
        assert_eq!(String::from(ed), "tiLDE");
    }

    #[test]
    /// test tilde
    fn tilde_nocase() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("ti_lde").unwrap();

        simulate_keys(
            &mut map,
            &mut ed,
            [Esc, Char('0'), Char('6'), Char('~')].iter(),
        );
        assert_eq!(String::from(ed), "TI_LDE");
    }

    #[test]
    /// ctrl-h should act as backspace
    fn ctrl_h() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("not empty").unwrap();

        let res = map.handle_key(Ctrl('h'), &mut ed, &mut EmptyCompleter);
        assert_eq!(res.is_ok(), true);
        assert_eq!(ed.current_buffer().to_string(), "not empt".to_string());
    }

    #[test]
    /// repeat char move with no last char
    fn repeat_char_move_no_char() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = Vi::new();
        map.init(&mut ed).unwrap();
        ed.insert_str_after_cursor("abc defg").unwrap();

        simulate_keys(&mut map, &mut ed, [Esc, Char('$'), Char(';')].iter());
        assert_eq!(ed.cursor(), 7);
    }
}
