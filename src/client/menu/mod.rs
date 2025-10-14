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

use bevy::ecs::{
    resource::Resource,
    system::{Commands, IntoSystem, SystemId},
    world::World,
};
use failure::{Error, bail};

use crate::common::console::CName;

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
}

impl Menu {
    /// Returns a reference to the active submenu of this menu and its parent.
    fn active_submenu_and_parent(&self) -> Result<(&Menu, Option<&Menu>), Error> {
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
    pub fn active_submenu(&self) -> Result<&Menu, Error> {
        let (m, _) = self.active_submenu_and_parent()?;
        Ok(m)
    }

    /// Return a reference to the active submenu of this menu
    pub fn active_submenu_mut(&mut self) -> Result<&mut Menu, Error> {
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

    /// Returns a reference to the active submenu of this menu and its parent.
    fn active_submenu_parent_mut(&mut self) -> Result<Option<&mut Menu>, Error> {
        let MenuState::InSubMenu { mut index } = self.active_submenu()?.state else {
            return Ok(Some(self));
        };
        let Item::Submenu(m) = &mut self.items[index].item else {
            bail!("Menu state points to invalid submenu");
        };
        let mut m = m;

        loop {
            match &mut m.items[index].item {
                Item::Submenu(s) => {
                    m = s;
                    if let MenuState::InSubMenu { index: new_index } = m.state {
                        index = new_index;
                    } else {
                        return Ok(Some(m));
                    }
                }
                _ => bail!("Menu state points to invalid submenu"),
            }
        }
    }

    /// Select the next element of this Menu.
    pub fn select_next(&mut self) -> Result<(), Error> {
        let m = self.active_submenu_mut()?;

        if let MenuState::Active { index } = m.state {
            m.state = MenuState::Active { index: (index + 1) % m.items.len() };
        } else {
            bail!("Selected menu is inactive (invariant violation)");
        }

        Ok(())
    }

    /// Select the previous element of this Menu.
    pub fn select_prev(&mut self) -> Result<(), Error> {
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
    pub fn selected(&self) -> Result<&Item, Error> {
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
    /// Otherwise, this has no effect.
    pub fn activate(&mut self) -> Result<impl FnOnce(Commands), Error> {
        fn run(action: Option<SystemId>) -> impl FnOnce(Commands) {
            move |mut c: Commands| {
                if let Some(action) = action {
                    c.run_system(action);
                }
            }
        }

        let m = self.active_submenu_mut()?;

        if let MenuState::Active { index } = m.state {
            match &mut m.items[index].item {
                Item::Submenu(submenu) => {
                    m.state = MenuState::InSubMenu { index };
                    submenu.state = MenuState::Active { index: 0 };

                    Ok(run(None))
                }

                Item::Action(action) => {
                    let action = *action;
                    Ok(run(Some(action)))
                }

                _ => Ok(run(None)),
            }
        } else {
            Ok(run(None))
        }
    }

    pub fn left(&mut self) -> Result<impl FnOnce(Commands) + '_, Error> {
        let m = self.active_submenu_mut()?;

        Ok(move |c: Commands| {
            if let MenuState::Active { index } = m.state {
                match &mut m.items[index].item {
                    Item::Enum(e) => (e.select_prev())(c),
                    Item::Slider(slider) => (slider.decrease())(c),
                    Item::TextField(text) => text.cursor_left(),
                    Item::Toggle(toggle) => (toggle.set_false())(c),
                    _ => {}
                }
            }
        })
    }

    pub fn right(&mut self) -> Result<impl FnOnce(Commands) + '_, Error> {
        let m = self.active_submenu_mut()?;

        Ok(move |c: Commands| {
            if let MenuState::Active { index } = m.state {
                match &mut m.items[index].item {
                    Item::Enum(e) => (e.select_next())(c),
                    Item::Slider(slider) => (slider.increase())(c),
                    Item::TextField(text) => text.cursor_right(),
                    Item::Toggle(toggle) => (toggle.set_true())(c),
                    _ => {}
                }
            }
        })
    }

    /// Return `true` if the root menu is active, `false` otherwise.
    pub fn at_root(&self) -> bool {
        matches!(self.state, MenuState::Active { .. })
    }

    /// Deactivate the active menu and activate its parent
    pub fn back(&mut self) -> Result<(), Error> {
        if self.at_root() {
            bail!("Cannot back out of root menu!");
        }

        let m = self.active_submenu_mut()?;
        m.state = MenuState::Inactive;

        match self.active_submenu_parent_mut()? {
            Some(mp) => {
                let s = mp.state;
                match s {
                    MenuState::InSubMenu { index } => mp.state = MenuState::Active { index },
                    _ => unreachable!(),
                };
            }

            None => unreachable!(),
        }

        Ok(())
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

        Menu { items: self.items, state: MenuState::Active { index: 0 }, view }
    }

    pub fn add_submenu<S>(
        mut self,
        name: S,
        submenu: impl FnOnce(MenuBuilder<'_>) -> Result<Menu, Error>,
    ) -> Result<Self, Error>
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
        S: IntoSystem<(), (), M> + 'static, {
        let action_id = self.world.register_system(action);
        self.items.push(NamedMenuItem::new(name, Item::Action(action_id)));
        self
    }

    pub fn add_toggle<N, S>(mut self, name: N, init: bool, cvar: S) -> Self
    where
        N: Into<CName>,
        S: Into<CName>, {
        self.items.push(NamedMenuItem::new(name, Item::Toggle(Toggle::new(init, cvar))));
        self
    }

    pub fn add_enum<S, C, E>(mut self, name: S, cvar: C, init: usize, items: E) -> Self
    where
        S: Into<CName>,
        C: Into<CName>,
        E: FnOnce(EnumBuilder) -> Vec<EnumItem>, {
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
    ) -> Result<Self, Error>
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
    ) -> Result<Self, Error>
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

    pub fn with<N, S>(mut self, name: N, val: S) -> Result<Self, Error>
    where
        N: Into<CName>,
        S: AsRef<str>, {
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
    where S: Into<CName> {
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

// #[cfg(test)]
// mod test {
//     use super::*;
//     use std::{cell::Cell, rc::Rc};

//     fn view() -> MenuView {
//         MenuView {
//             draw_plaque: false,
//             title_path: "path".to_string(),
//             body: MenuBodyView::Dynamic,
//         }
//     }

//     fn is_inactive(state: &MenuState) -> bool {
//         match state {
//             MenuState::Inactive => true,
//             _ => false,
//         }
//     }

//     fn is_active(state: &MenuState) -> bool {
//         match state {
//             MenuState::Active { .. } => true,
//             _ => false,
//         }
//     }

//     fn is_insubmenu(state: &MenuState) -> bool {
//         match state {
//             MenuState::InSubMenu { .. } => true,
//             _ => false,
//         }
//     }

//     #[test]
//     fn test_menu_builder() {
//         let action_target = Rc::new(Cell::new(false));
//         let action_target_handle = action_target.clone();

//         let _m = MenuBuilder::new()
//             .add_action("action", Box::new(move || action_target_handle.set(true)))
//             .build(view());

//         // TODO
//     }

//     #[test]
//     fn test_menu_active_submenu() {
//         let menu = MenuBuilder::new()
//             .add_submenu(
//                 "menu_1",
//                 MenuBuilder::new()
//                     .add_action("action_1", Box::new(|| ()))
//                     .build(view()),
//             )
//             .add_submenu(
//                 "menu_2",
//                 MenuBuilder::new()
//                     .add_action("action_2", Box::new(|| ()))
//                     .build(view()),
//             )
//             .build(view());

//         let m = &menu;
//         let m1 = match m.items[0].item {
//             Item::Submenu(ref m1i) => m1i,
//             _ => unreachable!(),
//         };
//         let m2 = match m.items[1].item {
//             Item::Submenu(ref m2i) => m2i,
//             _ => unreachable!(),
//         };

//         assert!(is_active(&m.state.get()));
//         assert!(is_inactive(&m1.state.get()));
//         assert!(is_inactive(&m2.state.get()));

//         // enter m1
//         m.activate().unwrap();
//         assert!(is_insubmenu(&m.state.get()));
//         assert!(is_active(&m1.state.get()));
//         assert!(is_inactive(&m2.state.get()));

//         // exit m1
//         m.back().unwrap();
//         assert!(is_active(&m.state.get()));
//         assert!(is_inactive(&m1.state.get()));
//         assert!(is_inactive(&m2.state.get()));

//         // enter m2
//         m.next().unwrap();
//         m.activate().unwrap();
//         assert!(is_insubmenu(&m.state.get()));
//         assert!(is_inactive(&m1.state.get()));
//         assert!(is_active(&m2.state.get()));
//     }
// }
