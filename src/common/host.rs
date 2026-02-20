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

use std::time::{Duration, Instant};

use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoopWindowTarget},
};

pub trait Program: Sized {
    fn handle_event<T>(
        &mut self,
        event: Event<T>,
        _target: &EventLoopWindowTarget<T>,
        control_flow: &mut ControlFlow,
    ) -> Control;

    fn frame(&mut self, frame_duration: Duration);
}

pub struct Host<P>
where
    P: Program,
{
    program: P,

    init_time: Instant,
    prev_frame_time: Instant,
}

pub enum Control {
    Continue,
    Exit,
}

pub mod cvars {
    use bevy::app::App;

    use crate::common::console::{Cvar, RegisterCmdExt as _};

    pub fn register_cvars(app: &mut App) {
        app.cvar("host_maxfps", Cvar::new("72"), "sets the maximum desired frames per second");
    }
}

impl<P> Host<P>
where
    P: Program,
{
    pub fn handle_event<T>(
        &mut self,
        event: Event<T>,
        _target: &EventLoopWindowTarget<T>,
        control_flow: &mut ControlFlow,
    ) -> Control {
        match event {
            Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => Control::Exit,

            Event::AboutToWait => {
                // self.frame();
                Control::Continue
            }
            Event::Suspended | Event::Resumed => Control::Continue,
            Event::LoopExiting => {
                // TODO:
                // - host_writeconfig
                // - others...
                Control::Exit
            }

            e => self.program.handle_event(e, _target, control_flow),
        }
    }

    pub fn program(&self) -> &P {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut P {
        &mut self.program
    }

    // pub fn frame(&mut self) {
    //     // TODO: make sure this doesn't cause weirdness with e.g. leap seconds
    //     let new_frame_time = Utc::now();
    //     self.prev_frame_duration = new_frame_time.signed_duration_since(self.prev_frame_time);

    //     // if the time elapsed since the last frame is too low, don't run this one yet
    //     let prev_frame_duration = self.prev_frame_duration;
    //     if !self.check_frame_duration(prev_frame_duration) {
    //         // avoid busy waiting if we're running at a really high framerate
    //         std::thread::sleep(std::time::Duration::from_millis(1));
    //         return;
    //     }

    //     // we're running this frame, so update the frame time
    //     self.prev_frame_time = new_frame_time;

    //     self.program.frame(self.prev_frame_duration);
    // }

    pub fn uptime(&self) -> Duration {
        self.init_time - self.prev_frame_time
    }
}
