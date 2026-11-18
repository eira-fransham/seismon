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

mod item;
pub mod render;

use anyhow::bail;
use bevy::ecs::{
    resource::Resource,
    system::{Commands, IntoSystem},
    world::World,
};

use bevy_mod_commands::{CName, Registry};

pub use self::item::{Enum, EnumItem, Item, Slider, TextField, Toggle};

#[derive(Default, Clone, Copy, Debug)]
pub enum MenuState {
    /// Menu is inactive.
    #[default]
    Inactive,

    /// Menu is active. `index` indicates the currently selected element.
    Active { index: usize },

    /// A submenu of this menu is active. `index` indicates the active submenu.
    InSubMenu { index: usize },
}

#[derive(Default, Debug, Clone)]
/// Specifies how the menu body should be rendered.
pub enum MenuBodyView {
    /// The menu body is rendered using a predefined bitmap.
    Predefined {
        /// The path to the bitmap.
        path: imstr::ImString,
    },
    /// The menu body is rendered dynamically based on its contents.
    #[default]
    Dynamic,
}

#[derive(Default, Debug, Clone)]
pub struct MenuView {
    pub draw_plaque: bool,
    pub title_path: imstr::ImString,
    pub body: MenuBodyView,
}

impl MenuView {
    /// Returns true if the Quake plaque should be drawn to the left of the menu.
    pub fn draw_plaque(&self) -> bool {
        self.draw_plaque
    }

    /// Returns the path to the menu title bitmap.
    pub fn title_path(&self) -> &str {
        &self.title_path
    }

    /// Returns a MenuBodyView which specifies how to render the menu body.
    pub fn body(&self) -> &MenuBodyView {
        &self.body
    }
}

#[derive(Default, Debug, Resource, Clone)]
pub struct Menu {
    items: Vec<NamedMenuItem>,
    state: MenuState,
    view: MenuView,
    generation: u64,
}

/// A deferred effect of a menu input (e.g. setting a cvar or running an action).
pub type MenuAction<'a> = Box<dyn FnOnce(Commands) + 'a>;

impl Menu {
    /// Returns a reference to the active submenu of this menu and its parent.
    fn active_submenu_and_parent(&self) -> anyhow::Result<(&Menu, Option<&Menu>)> {
        let mut m = self;
        let mut m_parent = None;

        while let MenuState::InSubMenu { index } = m.state {
            match m.items[index].item {
                Item::Submenu(ref s) => {
                    m_parent = Some(m);
                    m = s;
                }
                _ => bail!("Menu state points to invalid submenu"),
            }
        }

        Ok((m, m_parent))
    }

    /// Return a reference to the active submenu of this menu
    pub fn active_submenu(&self) -> anyhow::Result<&Menu> {
        let (m, _) = self.active_submenu_and_parent()?;
        Ok(m)
    }

    /// Return a reference to the active submenu of this menu
    pub fn active_submenu_mut(&mut self) -> anyhow::Result<&mut Menu> {
        let mut m = self;

        while let MenuState::InSubMenu { index } = &mut m.state {
            match &mut m.items[*index].item {
                Item::Submenu(s) => {
                    m = s;
                }
                _ => bail!("Menu state points to invalid submenu"),
            }
        }

        Ok(m)
    }

    /// Select the next element of this Menu.
    pub fn select_next(&mut self) -> anyhow::Result<()> {
        let m = self.active_submenu_mut()?;

        if let MenuState::Active { index } = m.state {
            m.state = MenuState::Active { index: (index + 1) % m.items.len() };
        } else {
            bail!("Selected menu is inactive (invariant violation)");
        }

        Ok(())
    }

    /// Select the previous element of this Menu.
    pub fn select_prev(&mut self) -> anyhow::Result<()> {
        let m = self.active_submenu_mut()?;

        if let MenuState::Active { index } = m.state {
            m.state = MenuState::Active {
                index: index.checked_sub(1).map(|i| i % m.items.len()).unwrap_or(m.items.len() - 1),
            };
        } else {
            bail!("Selected menu is inactive (invariant violation)");
        }

        Ok(())
    }

    /// Return a reference to the currently selected menu item.
    pub fn selected(&self) -> anyhow::Result<&Item> {
        let m = self.active_submenu()?;

        if let MenuState::Active { index } = m.state {
            Ok(&m.items[index].item)
        } else {
            bail!("Active menu in invalid state (invariant violation)")
        }
    }

    /// Activate the currently selected menu item.
    ///
    /// If this item is a `Menu`, sets the active (sub)menu's state to
    /// `MenuState::InSubMenu` and the selected submenu's state to
    /// `MenuState::Active`.
    ///
    /// If this item is an `Action`, executes the function contained in the
    /// `Action`.
    ///
    /// If this item is a toggle it is flipped, and if it is a slider or enum it is moved to the
    /// right (like `M_AdjustSliders(1)` in Quake).
    ///
    /// Otherwise, this has no effect.
    pub fn activate(&mut self) -> anyhow::Result<MenuAction<'_>> {
        if matches!(self.selected(), Ok(Item::Submenu(_))) {
            self.generation += 1;
        }

        let m = self.active_submenu_mut()?;

        let MenuState::Active { index } = m.state else {
            return Ok(Box::new(|_| {}));
        };

        if let Item::Submenu(submenu) = &mut m.items[index].item {
            m.state = MenuState::InSubMenu { index };
            submenu.state = MenuState::Active { index: 0 };

            return Ok(Box::new(|_| {}));
        }

        match &mut m.items[index].item {
            Item::Action(action) => {
                let action = *action;
                Ok(Box::new(move |mut c: Commands| c.run_system(action)))
            }
            Item::Toggle(toggle) => Ok(Box::new(toggle.toggle())),
            Item::Enum(e) => Ok(Box::new(e.select_next())),
            Item::Slider(slider) => Ok(Box::new(slider.increase())),
            Item::Submenu(_) | Item::TextField(_) => Ok(Box::new(|_| {})),
        }
    }

    pub fn left(&mut self) -> anyhow::Result<MenuAction<'_>> {
        let m = self.active_submenu_mut()?;

        let MenuState::Active { index } = m.state else {
            return Ok(Box::new(|_| {}));
        };

        Ok(match &mut m.items[index].item {
            Item::Enum(e) => Box::new(e.select_prev()),
            Item::Slider(slider) => Box::new(slider.decrease()),
            Item::TextField(text) => {
                text.cursor_left();
                Box::new(|_| {})
            }
            Item::Toggle(toggle) => Box::new(toggle.set_false()),
            Item::Submenu(_) | Item::Action(_) => Box::new(|_| {}),
        })
    }

    pub fn right(&mut self) -> anyhow::Result<MenuAction<'_>> {
        let m = self.active_submenu_mut()?;

        let MenuState::Active { index } = m.state else {
            return Ok(Box::new(|_| {}));
        };

        Ok(match &mut m.items[index].item {
            Item::Enum(e) => Box::new(e.select_next()),
            Item::Slider(slider) => Box::new(slider.increase()),
            Item::TextField(text) => {
                text.cursor_right();
                Box::new(|_| {})
            }
            Item::Toggle(toggle) => Box::new(toggle.set_true()),
            Item::Submenu(_) | Item::Action(_) => Box::new(|_| {}),
        })
    }

    /// Update the state of the items in the active submenu from their cvars, so that adjusting
    /// them starts from the current value even if it was changed elsewhere (e.g. the console).
    pub fn sync_from(&mut self, registry: &Registry) -> anyhow::Result<()> {
        for item in &mut self.active_submenu_mut()?.items {
            match &mut item.item {
                Item::Toggle(toggle) => toggle.sync_from(registry),
                Item::Enum(e) => e.sync_from(registry),
                Item::Slider(slider) => slider.sync_from(registry),
                Item::Submenu(_) | Item::Action(_) | Item::TextField(_) => {}
            }
        }

        Ok(())
    }

    /// Return `true` if the root menu is active, `false` otherwise.
    pub fn at_root(&self) -> bool {
        matches!(self.state, MenuState::Active { .. })
    }

    /// Deactivate the active menu and activate its parent
    pub fn back(&mut self) -> anyhow::Result<()> {
        let MenuState::InSubMenu { index } = self.state else {
            bail!("Cannot back out of root menu!");
        };

        let Item::Submenu(child) = &mut self.items[index].item else {
            bail!("Menu state points to invalid submenu");
        };

        if let MenuState::InSubMenu { .. } = child.state {
            child.back()?;
        } else {
            child.state = MenuState::Inactive;
            self.state = MenuState::Active { index };
        }

        self.generation += 1;

        Ok(())
    }

    /// Return to the root menu, with the first item selected.
    pub fn reset(&mut self) {
        while !self.at_root() {
            if self.back().is_err() {
                break;
            }
        }

        self.state = MenuState::Active { index: 0 };
        self.generation += 1;
    }

    /// The index of the selected item in the active submenu.
    pub fn cursor(&self) -> Option<usize> {
        match self.active_submenu().ok()?.state {
            MenuState::Active { index } => Some(index),
            _ => None,
        }
    }

    /// A counter that changes every time the active submenu changes, so that the renderer
    /// knows when to rebuild the page.
    pub fn generation(&self) -> u64 {
        self.generation
    }

    /// Iterate over this menu and all of its submenus, recursively.
    pub fn all_menus(&self) -> Box<dyn Iterator<Item = &Menu> + '_> {
        Box::new(std::iter::once(self).chain(self.items.iter().flat_map(|item| match &item.item {
            Item::Submenu(submenu) => submenu.all_menus(),
            _ => Box::new(std::iter::empty()),
        })))
    }

    pub fn items(&self) -> impl Iterator<Item = &NamedMenuItem> + '_ {
        self.items.iter()
    }

    pub fn state(&self) -> MenuState {
        self.state
    }

    pub fn view(&self) -> &MenuView {
        &self.view
    }
}

pub struct MenuBuilder<'a> {
    world: &'a mut World,
    items: Vec<NamedMenuItem>,
}

impl<'a> MenuBuilder<'a> {
    pub fn new(world: &'a mut World) -> Self {
        MenuBuilder { world, items: Default::default() }
    }

    pub fn world(&mut self) -> &mut World {
        self.world
    }

    pub fn build(mut self, view: MenuView) -> Menu {
        // deactivate all child menus
        for item in self.items.iter_mut() {
            if let Item::Submenu(m) = &mut item.item {
                m.state = MenuState::Inactive;
            }
        }

        Menu { items: self.items, state: MenuState::Active { index: 0 }, view, generation: 0 }
    }

    pub fn add_submenu<S>(
        mut self,
        name: S,
        submenu: impl FnOnce(MenuBuilder<'_>) -> anyhow::Result<Menu>,
    ) -> anyhow::Result<Self>
    where
        S: Into<CName>,
    {
        let submenu = submenu(MenuBuilder::new(&mut *self.world))?;
        self.items.push(NamedMenuItem::new(name, Item::Submenu(submenu)));
        Ok(self)
    }

    pub fn add_action<N, S, M>(mut self, name: N, action: S) -> Self
    where
        N: Into<CName>,
        S: IntoSystem<(), (), M> + 'static,
    {
        let action_id = self.world.register_system(action);
        self.items.push(NamedMenuItem::new(name, Item::Action(action_id)));
        self
    }

    pub fn add_toggle<N, S>(mut self, name: N, init: bool, cvar: S) -> Self
    where
        N: Into<CName>,
        S: Into<CName>,
    {
        self.items.push(NamedMenuItem::new(name, Item::Toggle(Toggle::new(init, cvar))));
        self
    }

    pub fn add_enum<S, C, E>(mut self, name: S, cvar: C, init: usize, items: E) -> Self
    where
        S: Into<CName>,
        C: Into<CName>,
        E: FnOnce(EnumBuilder) -> Vec<EnumItem>,
    {
        self.items.push(NamedMenuItem::new(
            name,
            Item::Enum(Enum::new(init, cvar, items(EnumBuilder::new()))),
        ));
        self
    }

    pub fn add_slider<N, S>(
        mut self,
        name: N,
        min: f32,
        max: f32,
        steps: usize,
        init: usize,
        cvar: S,
    ) -> anyhow::Result<Self>
    where
        N: Into<CName>,
        S: Into<CName>,
    {
        self.items.push(NamedMenuItem::new(
            name,
            Item::Slider(Slider::new(min, max, steps, init, cvar.into())?),
        ));
        Ok(self)
    }

    pub fn add_text_field<N, D, S>(
        mut self,
        name: N,
        default: Option<D>,
        max_len: Option<usize>,
        cvar: S,
    ) -> anyhow::Result<Self>
    where
        N: Into<CName>,
        D: Into<String>,
        S: Into<CName>,
    {
        self.items.push(NamedMenuItem::new(
            name,
            Item::TextField(TextField::new(default, max_len, cvar)),
        ));
        Ok(self)
    }
}

#[derive(Default)]
pub struct EnumBuilder {
    items: Vec<EnumItem>,
}

impl EnumBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with<N, S>(mut self, name: N, val: S) -> anyhow::Result<Self>
    where
        N: Into<CName>,
        S: AsRef<str>,
    {
        self.items.push(EnumItem::new(name, val)?);

        Ok(self)
    }

    pub fn build(self) -> Vec<EnumItem> {
        self.items
    }
}

#[derive(Debug, Clone)]
pub struct NamedMenuItem {
    name: CName,
    item: Item,
}

impl NamedMenuItem {
    fn new<S>(name: S, item: Item) -> NamedMenuItem
    where
        S: Into<CName>,
    {
        let name = name.into();
        NamedMenuItem { name, item }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn item(&self) -> &Item {
        &self.item
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn view() -> MenuView {
        MenuView::default()
    }

    fn nested(world: &mut World) -> Menu {
        MenuBuilder::new(world)
            .add_toggle("a", false, "a")
            .add_submenu("b", |b| {
                Ok(b.add_toggle("b0", false, "b0")
                    .add_submenu("c", |c| Ok(c.add_toggle("c0", false, "c0").build(view())))?
                    .build(view()))
            })
            .unwrap()
            .build(view())
    }

    #[test]
    fn back_returns_to_parent() {
        let mut world = World::new();
        let mut menu = nested(&mut world);

        assert!(menu.at_root());
        assert!(menu.back().is_err());

        menu.select_next().unwrap();
        let _ = menu.activate().unwrap();
        assert_eq!(menu.cursor(), Some(0));
        menu.select_next().unwrap();
        let _ = menu.activate().unwrap();
        assert_eq!(menu.active_submenu().unwrap().items().next().unwrap().name(), "c0");

        let generation = menu.generation();
        menu.back().unwrap();
        assert!(menu.generation() > generation);
        assert!(!menu.at_root());
        assert_eq!(menu.active_submenu().unwrap().items().next().unwrap().name(), "b0");
        assert_eq!(menu.cursor(), Some(1));

        menu.back().unwrap();
        assert!(menu.at_root());
        assert_eq!(menu.cursor(), Some(1));
    }

    #[test]
    fn reset_returns_to_root() {
        let mut world = World::new();
        let mut menu = nested(&mut world);

        menu.select_next().unwrap();
        let _ = menu.activate().unwrap();
        menu.select_next().unwrap();
        let _ = menu.activate().unwrap();

        menu.reset();
        assert!(menu.at_root());
        assert_eq!(menu.cursor(), Some(0));
    }

    #[test]
    fn all_menus_walks_submenus() {
        let mut world = World::new();
        assert_eq!(nested(&mut world).all_menus().count(), 3);
    }
}
