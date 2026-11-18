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

use bevy_mod_commands::{CName, Registry, SetCvar};

use crate::client::menu::Menu;

use anyhow::ensure;
use bevy::ecs::system::{Commands, SystemId};
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
    where
        C: Into<CName>,
    {
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
                c.queue(SetCvar(self.cvar.clone(), toggle_value(val)))
            }
        }
    }

    pub fn set_true(&mut self) -> impl FnOnce(Commands) + '_ {
        let val = if !self.state {
            self.state = true;
            Some(self.state)
        } else {
            None
        };

        move |mut c| {
            if let Some(val) = val {
                c.queue(SetCvar(self.cvar.clone(), toggle_value(val)))
            }
        }
    }

    pub fn toggle(&mut self) -> impl FnOnce(Commands) + '_ {
        self.state = !self.state;

        move |mut c| c.queue(SetCvar(self.cvar.clone(), toggle_value(self.state)))
    }

    pub fn get(&self) -> bool {
        self.state
    }

    pub fn cvar(&self) -> &str {
        &self.cvar
    }

    /// Read the current value of the cvar backing this toggle, if it exists and is a number or
    /// boolean.
    pub fn read(&self, registry: &Registry) -> Option<bool> {
        read_toggle(registry, &self.cvar)
    }

    /// Update the state of this toggle from its cvar.
    pub fn sync_from(&mut self, registry: &Registry) {
        if let Some(state) = self.read(registry) {
            self.state = state;
        }
    }
}

/// Toggles are stored as `0`/`1`, like in Quake, so that other code can read them as numbers.
fn toggle_value(state: bool) -> Value {
    Value::from(i64::from(state))
}

fn read_toggle(registry: &Registry, cvar: &str) -> Option<bool> {
    let cvar = registry.get_cvar(cvar)?;
    let value = cvar.value();

    value.as_bool().or_else(|| value.as_f64().map(|v| v != 0.))
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
        let val = if self.selected > 0 {
            self.selected -= 1;
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

    pub fn cvar(&self) -> &str {
        &self.cvar
    }

    /// The index of the item matching the current value of the cvar, if any.
    pub fn read(&self, registry: &Registry) -> Option<usize> {
        let value = registry.get_cvar(&self.cvar)?.value();

        self.items.iter().position(|item| item.value == *value)
    }

    /// The name of the item matching the current value of the cvar, falling back to the
    /// selected item.
    pub fn read_name(&self, registry: &Registry) -> &str {
        self.read(registry).map_or_else(|| self.selected_name(), |i| self.items[i].name.as_ref())
    }

    /// Update the selected item from the cvar.
    pub fn sync_from(&mut self, registry: &Registry) {
        if let Some(selected) = self.read(registry) {
            self.selected = selected;
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumItem {
    name: CName,
    value: Value,
}

impl EnumItem {
    pub fn new<N, V>(name: N, value: V) -> anyhow::Result<EnumItem>
    where
        N: Into<CName>,
        V: AsRef<str>,
    {
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
    ) -> anyhow::Result<Slider> {
        ensure!(steps > 1, "Slider must have at least 2 steps");
        ensure!(init < steps, "Invalid initial setting");
        ensure!(min != max, "Slider must have a non-empty range");

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

    /// The position of the slider handle, from `0` to `1`.
    pub fn position(&self) -> f32 {
        self.selected as f32 / (self.steps - 1) as f32
    }

    /// The position of the slider handle for the given value, from `0` to `1`. `min` may be
    /// greater than `max`, in which case the slider is inverted (e.g. brightness/gamma).
    pub fn position_of(&self, value: f32) -> f32 {
        ((value - self.min) / (self.increment * (self.steps - 1) as f32)).clamp(0., 1.)
    }

    /// The step closest to the given value.
    fn step_of(&self, value: f32) -> usize {
        (self.position_of(value) * (self.steps - 1) as f32).round() as usize
    }

    pub fn cvar(&self) -> &str {
        &self.cvar
    }

    /// Read the current value of the cvar backing this slider, if it exists and is a number.
    pub fn read(&self, registry: &Registry) -> Option<f32> {
        registry.read_cvar::<f32>(&self.cvar).ok()
    }

    /// Update the selected step from the cvar.
    pub fn sync_from(&mut self, registry: &Registry) {
        if let Some(value) = self.read(registry) {
            self.selected = self.step_of(value);
        }
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
        S: Into<CName>,
    {
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

    pub fn set_cursor(&mut self, cursor: usize) -> anyhow::Result<()> {
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
        if curs > 0 {
            self.cursor = curs - 1;
        }
    }

    pub fn insert(&mut self, c: char) -> impl FnOnce(Commands) + '_ {
        let val = if self.max_len == Some(self.len()) {
            None
        } else {
            self.chars.insert(self.cursor, c);
            self.cursor += 1;
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
        if self.cursor > 0 {
            self.chars.remove(self.cursor - 1);
            self.cursor -= 1;
        }
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn delete(&mut self) {
        if self.cursor < self.len() {
            self.chars.remove(self.cursor);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn toggle_set_true_and_false() {
        let mut toggle = Toggle::new(false, "t");

        let _ = toggle.set_false();
        assert!(!toggle.get());
        let _ = toggle.set_true();
        assert!(toggle.get());
        let _ = toggle.set_true();
        assert!(toggle.get());
        let _ = toggle.set_false();
        assert!(!toggle.get());
        let _ = toggle.toggle();
        assert!(toggle.get());
    }

    #[test]
    fn enum_stays_in_bounds() {
        let items = (0..3).map(|i| EnumItem::new(format!("option_{i}"), i.to_string()).unwrap());
        let mut e = Enum::new(0, "e", items);

        let _ = e.select_prev();
        assert_eq!(e.selected_name(), "option_0");
        let _ = e.select_next();
        assert_eq!(e.selected_name(), "option_1");
        let _ = e.select_prev();
        assert_eq!(e.selected_name(), "option_0");

        for _ in 0..5 {
            let _ = e.select_next();
        }
        assert_eq!(e.selected_name(), "option_2");
    }

    #[test]
    fn slider_positions() {
        let mut slider = Slider::new(0., 10., 11, 0, "s".into()).unwrap();

        let _ = slider.decrease();
        assert_eq!(slider.value(), 0.);
        assert_eq!(slider.position(), 0.);

        for i in 0..10 {
            let _ = slider.increase();
            assert_eq!(slider.value(), i as f32 + 1.);
        }

        let _ = slider.increase();
        assert_eq!(slider.value(), 10.);
        assert_eq!(slider.position(), 1.);

        assert_eq!(slider.position_of(5.), 0.5);
        assert_eq!(slider.position_of(-5.), 0.);
        assert_eq!(slider.position_of(50.), 1.);
        assert_eq!(slider.step_of(4.4), 4);
    }

    #[test]
    fn inverted_slider() {
        let slider = Slider::new(1., 0.5, 11, 0, "gamma".into()).unwrap();

        assert_eq!(slider.value(), 1.);
        assert_eq!(slider.position_of(1.), 0.);
        assert_eq!(slider.position_of(0.5), 1.);
        assert_eq!(slider.position_of(0.75), 0.5);
        assert_eq!(slider.step_of(0.75), 5);
    }

    #[test]
    fn slider_rejects_empty_range() {
        assert!(Slider::new(1., 1., 11, 0, "s".into()).is_err());
    }

    #[test]
    fn text_field_editing() {
        let mut tf = TextField::new(Some("abc"), Some(5), "t");

        tf.home();
        tf.cursor_left();
        assert_eq!(tf.cursor(), 0);
        tf.backspace();
        assert_eq!(tf.text(), "abc");

        tf.cursor_right();
        tf.backspace();
        assert_eq!(tf.text(), "bc");
        assert_eq!(tf.cursor(), 0);

        let _ = tf.insert('x');
        assert_eq!(tf.text(), "xbc");
        assert_eq!(tf.cursor(), 1);

        tf.end();
        tf.delete();
        let _ = tf.insert('y');
        let _ = tf.insert('z');
        let _ = tf.insert('!');
        assert_eq!(tf.text(), "xbcyz");
    }
}
