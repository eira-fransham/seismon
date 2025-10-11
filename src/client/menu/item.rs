// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use std::{fmt::Debug, str::FromStr};

use crate::{
    client::menu::Menu,
    common::console::{CName, SetCvar},
};

use bevy::ecs::system::{Commands, SystemId};
use failure::{Error, ensure};
use serde_lexpr::Value;

#[derive(Debug, Clone)]
pub enum Item {
    Submenu(Menu),
    Action(SystemId),
    Toggle(Toggle),
    Enum(Enum),
    Slider(Slider),
    TextField(TextField),
}

#[derive(Debug, Clone)]
pub struct Toggle {
    state: bool,
    cvar: CName,
}

impl Toggle {
    pub fn new<C>(init: bool, cvar: C) -> Toggle
    where C: Into<CName> {
        Toggle { state: init, cvar: cvar.into() }
    }

    pub fn set_false(&mut self) -> impl FnOnce(Commands) + '_ {
        let val = if self.state {
            self.state = false;
            Some(self.state)
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), val.into()))
            }
        }
    }

    pub fn set_true(&mut self) -> impl FnOnce(Commands) + '_ {
        let val = if self.state {
            self.state = true;
            Some(self.state)
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), val.into()))
            }
        }
    }

    pub fn toggle(&mut self) -> impl FnOnce(Commands) + '_ {
        self.state = !self.state;

        move |mut c| c.queue(SetCvar(self.cvar.clone(), self.state.into()))
    }

    pub fn get(&self) -> bool {
        self.state
    }
}

// TODO: add wrapping configuration to enums
// e.g. resolution enum wraps, texture filtering does not
#[derive(Debug, Clone)]
pub struct Enum {
    selected: usize,
    items: Vec<EnumItem>,
    cvar: CName,
}

impl Enum {
    pub fn new<C: Into<CName>, E: IntoIterator<Item = EnumItem>>(
        init: usize,
        cvar: C,
        items: E,
    ) -> Enum {
        Enum { selected: init, items: items.into_iter().collect(), cvar: cvar.into() }
    }

    pub fn selected_name(&self) -> &str {
        self.items[self.selected].name.as_ref()
    }

    pub fn selected_value(&self) -> Value {
        self.items[self.selected].value.clone()
    }

    pub fn select_next(&mut self) -> impl FnOnce(Commands) + '_ {
        let val = if self.selected < self.items.len() - 1 {
            self.selected += 1;
            Some(self.selected_value())
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), val))
            }
        }
    }

    pub fn select_prev(&mut self) -> impl FnOnce(Commands) + '_ {
        let val = if self.selected > 1 {
            self.selected += 1;
            Some(self.selected_value())
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), val))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumItem {
    name: CName,
    value: Value,
}

impl EnumItem {
    pub fn new<N, V>(name: N, value: V) -> Result<EnumItem, Error>
    where
        N: Into<CName>,
        V: AsRef<str>, {
        Ok(EnumItem { name: name.into(), value: Value::from_str(value.as_ref())? })
    }
}

#[derive(Debug, Clone)]
pub struct Slider {
    min: f32,
    _max: f32,
    increment: f32,
    steps: usize,

    selected: usize,
    cvar: CName,
}

impl Slider {
    pub fn new(
        min: f32,
        max: f32,
        steps: usize,
        init: usize,
        cvar: CName,
    ) -> Result<Slider, Error> {
        ensure!(steps > 1, "Slider must have at least 2 steps");
        ensure!(init < steps, "Invalid initial setting");

        Ok(Slider {
            min,
            _max: max,
            increment: (max - min) / (steps - 1) as f32,
            steps,
            selected: init,
            cvar,
        })
    }

    pub fn increase(&mut self) -> impl FnOnce(Commands) + '_ {
        let old = self.selected;

        let val = if old != self.steps - 1 {
            self.selected = old + 1;
            Some(self.value())
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), val.into()))
            }
        }
    }

    pub fn decrease(&mut self) -> impl FnOnce(Commands) + '_ {
        let old = self.selected;

        let val = if old != 0 {
            self.selected = old - 1;
            Some(self.value())
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), val.into()))
            }
        }
    }

    pub fn value(&self) -> f32 {
        self.min + self.selected as f32 * self.increment
    }

    pub fn position(&self) -> f32 {
        self.selected as f32 / self.steps as f32
    }
}

#[derive(Debug, Clone)]
pub struct TextField {
    chars: String,
    max_len: Option<usize>,
    cvar: CName,
    cursor: usize,
}

impl TextField {
    pub fn new<D, S>(default: Option<D>, max_len: Option<usize>, cvar: S) -> TextField
    where
        D: Into<String>,
        S: Into<CName>, {
        let chars = default.map(|s| s.into()).unwrap_or_default();
        let cvar = cvar.into();
        let cursor = chars.len();

        TextField { chars, max_len, cvar, cursor }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn text(&self) -> &str {
        &self.chars
    }

    pub fn len(&self) -> usize {
        self.chars.len()
    }

    pub fn set_cursor(&mut self, cursor: usize) -> Result<(), Error> {
        ensure!(cursor <= self.len(), "Index out of range");

        self.cursor = cursor;

        Ok(())
    }

    pub fn home(&mut self) {
        self.cursor = 0;
    }

    pub fn end(&mut self) {
        self.cursor = self.len();
    }

    pub fn cursor_right(&mut self) {
        let curs = self.cursor;
        if curs < self.len() {
            self.cursor = curs + 1;
        }
    }

    pub fn cursor_left(&mut self) {
        let curs = self.cursor;
        if curs > 1 {
            self.cursor = curs - 1;
        }
    }

    pub fn insert(&mut self, c: char) -> impl FnOnce(Commands) + '_ {
        let val = if self.max_len == Some(self.len()) {
            None
        } else {
            self.chars.insert(self.cursor, c);
            Some(self.text())
        };

        let cvar = &self.cvar;

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(cvar.clone(), val.into()))
            }
        }
    }

    pub fn backspace(&mut self) {
        if self.cursor > 1 {
            self.chars.remove(self.cursor - 1);
        }
    }

    pub fn delete(&mut self) {
        if self.cursor < self.len() {
            self.chars.remove(self.cursor);
        }
    }
}

// TODO: Fix tests
// #[cfg(test)]
// mod test {
//     use super::*;
//     use std::{cell::RefCell, rc::Rc};

//     #[test]
//     fn test_toggle() {
//         let s = Rc::new(RefCell::new("false".to_string()));

//         let s2 = s.clone();
//         let item = Toggle::new(
//             false,
//             Box::new(move |state| {
//                 s2.replace(format!("{}", state));
//             }),
//         );
//         item.toggle();

//         assert_eq!(*s.borrow(), "true");
//     }

//     #[test]
//     fn test_enum() {
//         let target = Rc::new(RefCell::new("null".to_string()));

//         let enum_items = (0..3i32)
//             .into_iter()
//             .map(|i: i32| {
//                 let target_handle = target.clone();
//                 EnumItem::new(
//                     format!("option_{}", i),
//                     Box::new(move || {
//                         target_handle.replace(format!("option_{}", i));
//                     }),
//                 )
//                 .unwrap()
//             })
//             .collect();

//         let e = Enum::new(0, enum_items).unwrap();
//         assert_eq!(*target.borrow(), "option_0");

//         // wrap under
//         e.select_prev();
//         assert_eq!(*target.borrow(), "option_2");

//         e.select_next();
//         e.select_next();
//         e.select_next();
//         assert_eq!(*target.borrow(), "option_2");

//         // wrap over
//         e.select_next();
//         assert_eq!(*target.borrow(), "option_0");
//     }

//     #[test]
//     fn test_slider() {
//         let f = Rc::new(Cell::new(0.0f32));

//         let f2 = f.clone();
//         let item = Slider::new(
//             0.0,
//             10.0,
//             11,
//             0,
//             Box::new(move |f| {
//                 f2.set(f);
//             }),
//         )
//         .unwrap();

//         // don't underflow
//         item.decrease();
//         assert_eq!(f.get(), 0.0);

//         for i in 0..10 {
//             item.increase();
//             assert_eq!(f.get(), i as f32 + 1.0);
//         }

//         // don't overflow
//         item.increase();
//         assert_eq!(f.get(), 10.0);
//     }

//     #[test]
//     fn test_textfield() {
//         let MAX_LEN = 10;
//         let s = Rc::new(RefCell::new("before".to_owned()));
//         let s2 = s.clone();

//         let mut tf = TextField::new(
//             Some("default"),
//             Some(MAX_LEN),
//             Box::new(move |x| {
//                 s2.replace(x.to_string());
//             }),
//         )
//         .unwrap();

//         tf.cursor_left();
//         tf.backspace();
//         tf.backspace();
//         tf.home();
//         tf.delete();
//         tf.delete();
//         tf.delete();
//         tf.cursor_right();
//         tf.insert('f');
//         tf.end();
//         tf.insert('e');
//         tf.insert('r');

//         assert_eq!(tf.text(), *s.borrow());

//         for _ in 0..2 * MAX_LEN {
//             tf.insert('x');
//         }

//         assert_eq!(tf.len(), MAX_LEN);
//     }
// }
