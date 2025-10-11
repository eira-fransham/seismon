// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

pub mod commands;
pub mod console;
pub mod game;

use bevy::{
    ecs::resource::Resource, input::keyboard::KeyboardInput, prelude::*,
    render::extract_resource::ExtractResource,
};

use self::{game::GameInput, systems::InputEventReader};

pub struct SeismonInputPlugin;

impl Plugin for SeismonInputPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.init_resource::<InputFocus>()
            .init_resource::<GameInput>()
            .init_resource::<InputEventReader<KeyboardInput>>()
            .add_systems(
                Update,
                (
                    systems::game_input
                        .run_if(resource_exists_and_equals::<InputFocus>(InputFocus::Game)),
                    systems::console_input
                        .run_if(resource_exists_and_equals::<InputFocus>(InputFocus::Console)),
                    systems::menu_input
                        .run_if(resource_exists_and_equals::<InputFocus>(InputFocus::Menu)),
                    systems::reset_mouse_delta.run_if(resource_exists_and_changed::<InputFocus>),
                )
                    .run_if(systems::window_is_focused),
            );

        commands::register_commands(app);
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Resource, ExtractResource)]
pub enum InputFocus {
    Game,
    #[default]
    Console,
    Menu,
}

pub mod systems {
    use bevy::{
        ecs::event::EventCursor,
        input::{ButtonState, keyboard::KeyboardInput, mouse::MouseMotion},
        prelude::*,
        window::PrimaryWindow,
    };

    use crate::{
        client::menu::Menu,
        common::console::{ConsoleInput, ConsoleOutput, Registry, RunCmd, to_terminal_key},
    };

    use super::game::{AnyInput, Binding, BindingValidState, GameInput, Trigger};

    pub fn window_is_focused(windows: Query<&Window, With<PrimaryWindow>>) -> bool {
        let Ok(window) = windows.single() else {
            return false;
        };
        if !window.focused {
            return false;
        }

        true
    }

    #[derive(Resource)]
    pub struct InputEventReader<E: Event> {
        reader: EventCursor<E>,
    }

    impl<E: Event> Default for InputEventReader<E> {
        fn default() -> Self {
            Self { reader: default() }
        }
    }

    pub fn reset_mouse_delta(mut run_cmds: EventWriter<RunCmd<'static>>) {
        run_cmds.write(RunCmd("mousedelta".into(), Box::new(["#(0 0)".into()])));
    }

    pub fn game_input(
        mut reader: ResMut<InputEventReader<KeyboardInput>>,
        keyboard_events: Res<Events<KeyboardInput>>,
        keyboard_input: Res<ButtonInput<KeyCode>>,
        mut mouse_events: EventReader<MouseMotion>,
        mut run_cmds: EventWriter<RunCmd<'static>>,
        input: Res<GameInput>,
    ) {
        for key in reader.reader.read(&keyboard_events) {
            // TODO: Make this work better if we have arguments - currently we clone the arguments
            // every time TODO: Error handling
            if let Ok(Some(binding)) = input.binding(key.logical_key.clone()) {
                run_cmds.write_batch(binding.commands.iter().filter_map(|cmd| {
                    match (cmd.0.trigger, key.state) {
                        (Some(Trigger::Positive) | None, ButtonState::Pressed)
                            if keyboard_input.just_pressed(key.key_code) =>
                        {
                            Some(cmd.clone())
                        }
                        (Some(Trigger::Positive) | None, ButtonState::Released)
                            if keyboard_input.just_released(key.key_code) =>
                        {
                            cmd.clone().invert()
                        }
                        (Some(Trigger::Negative), _) => unreachable!(
                            "Binding found to a negative edge! TODO: Do we want to support this?"
                        ),
                        _ => None,
                    }
                }));
            }
        }

        let mouse_total: Vec2 = mouse_events.read().map(|motion| motion.delta).sum();
        let mouse_x = mouse_total.x;
        let mouse_y = mouse_total.y;

        match format!("mousedelta ({mouse_x} {mouse_y})").parse() {
            Ok(cmd) => {
                run_cmds.write(cmd);
            }
            Err(e) => error!("Invalid mouse movement: {mouse_total} ({e})"),
        }
    }

    // TODO: Should use a proper input manager
    #[allow(clippy::too_many_arguments)]
    pub fn console_input(
        mut reader: ResMut<InputEventReader<KeyboardInput>>,
        keyboard_events: Res<Events<KeyboardInput>>,
        button_state: Res<ButtonInput<KeyCode>>,
        mut run_cmds: EventWriter<RunCmd<'static>>,
        input: Res<GameInput>,
        mut console_in: ResMut<ConsoleInput>,
        mut console_out: ResMut<ConsoleOutput>,
        time: Res<Time<Virtual>>,
        registry: Res<Registry>,
    ) {
        // TODO: Use a thread_local vector instead of reallocating
        let mut keys = Vec::new();
        for key in reader.reader.read(&keyboard_events) {
            let KeyboardInput { logical_key, state, .. } = key;

            if AnyInput::from(logical_key.clone()) == AnyInput::ESCAPE {
                run_cmds.write("toggleconsole".into());
                return;
            }

            if let Ok(Some(Binding { commands, valid: BindingValidState::Any })) =
                input.binding(logical_key.clone())
            {
                run_cmds.write_batch(commands.iter().filter_map(|cmd| {
                    match (cmd.0.trigger, state) {
                        (Some(Trigger::Positive) | None, ButtonState::Pressed) => Some(cmd.clone()),
                        (Some(Trigger::Positive) | None, ButtonState::Released) => {
                            cmd.clone().invert()
                        }
                        (Some(Trigger::Negative), _) => unreachable!(
                            "Binding found to a negative edge! TODO: Do we want to support this?"
                        ),
                    }
                }));
            } else {
                keys.push(key);
            }
        }

        for exec in console_in.update(
            keys.iter()
                .filter_map(|KeyboardInput { logical_key: key, state, .. }| {
                    if *state == ButtonState::Pressed {
                        Some(to_terminal_key(key, &button_state))
                    } else {
                        None
                    }
                })
                .flatten(),
            registry.all_names(),
        ) {
            match exec {
                Ok(cmd) => {
                    console_out.print(ConsoleInput::PROMPT, &*time);
                    console_out.println(&cmd, &*time);

                    let cmd = RunCmd::parse(&cmd);

                    match cmd {
                        Ok(cmd) => {
                            run_cmds.write(cmd.into_owned());
                        }
                        Err(e) => warn!("Console error: {}", e),
                    }
                }
                Err(e) => warn!("Console error: {}", e),
            }
        }
    }

    pub fn menu_input(
        mut reader: ResMut<InputEventReader<KeyboardInput>>,
        keyboard_events: Res<Events<KeyboardInput>>,
        mut commands: Commands,
        mut run_cmds: EventWriter<RunCmd<'static>>,
        mut menu: ResMut<Menu>,
        input: Res<GameInput>,
    ) {
        // TODO: Use a thread_local vector instead of reallocating
        for key in reader.reader.read(&keyboard_events) {
            let KeyboardInput { logical_key, state, .. } = key;

            if let Ok(Some(Binding { commands, valid: BindingValidState::Any })) =
                input.binding(logical_key.clone())
            {
                run_cmds.write_batch(commands.iter().filter_map(|cmd| {
                    match (cmd.0.trigger, state) {
                        (Some(Trigger::Positive) | None, ButtonState::Pressed) => Some(cmd.clone()),
                        (Some(Trigger::Positive) | None, ButtonState::Released) => {
                            cmd.clone().invert()
                        }
                        (Some(Trigger::Negative), _) => unreachable!(
                            "Binding found to a negative edge! TODO: Do we want to support this?"
                        ),
                    }
                }));

                continue;
            }

            let KeyboardInput { logical_key: key, state: ButtonState::Pressed, .. } = key else {
                continue;
            };

            let input = AnyInput::from(key.clone());

            // TODO: Make this actually respect the `togglemenu` keybinding
            if input == AnyInput::ESCAPE {
                if menu.at_root() {
                    run_cmds.write("togglemenu".into());
                } else {
                    menu.back().expect("TODO: Handle menu failures");
                }
            } else if input == AnyInput::ENTER {
                let func = menu.activate().expect("TODO: Handle menu failures");
                func(commands.reborrow());
            } else if input == AnyInput::UPARROW {
                menu.select_prev().expect("TODO: Handle menu failures");
            } else if input == AnyInput::DOWNARROW {
                menu.select_next().expect("TODO: Handle menu failures");
            } else if input == AnyInput::LEFTARROW {
                let func = menu.left().expect("TODO: Handle menu failures");
                func(commands.reborrow());
            } else if input == AnyInput::RIGHTARROW {
                let func = menu.right().expect("TODO: Handle menu failures");
                func(commands.reborrow());
            }
        }
    }
}
