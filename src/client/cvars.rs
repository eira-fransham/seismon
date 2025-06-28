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

use bevy::app::App;

use crate::common::console::{Cvar, RegisterCmdExt};

pub fn register_cvars(app: &mut App) {
    app.cvar(
        "cl_anglespeedkey",
        "1.5",
        "sets the speed that the direction keys change the view angle",
    );
    app.cvar(
        "cl_backspeed",
        Cvar::new("200").archive(),
        "the base speed you move when pressing +back",
    );
    app.cvar(
        "cl_bob",
        "0.02",
        "Controls how much your weapon and view bobs moves when you walk",
    );
    app.cvar(
        "cl_bobcycle",
        "0.6",
        "the frequency at which the viewmodel bobs",
    );
    app.cvar(
        "cl_bobup",
        "0.5",
        "adjusts how much your viewmodel & weapon bobs up when running",
    );
    app.cvar(
        "_cl_color",
        Cvar::new("0").archive(),
        "the client's colors, as set by the color command - use cl_color instead",
    );
    app.cvar("cl_crossx", "0", "the x offset of the crosshair");
    app.cvar("cl_crossy", "0", "the y offset of the crosshair");
    app.cvar(
        "cl_forwardspeed",
        Cvar::new("400").archive(),
        "the base speed you move when pressing +forward",
    );
    app.cvar(
        "cl_movespeedkey",
        "2.0",
        "the speed multiplier when pressing the run key",
    );
    app.cvar(
        "_cl_name",
        Cvar::new("player").archive(),
        "the player's name - use the name command instead",
    );
    app.cvar(
        "cl_nolerp",
        "0",
        "disables/enables location/angle interpolation",
    );
    app.cvar(
        "cl_pitchspeed",
        "150",
        "sets the speed of your look when looking up or down with mouse or keyboard",
    );
    app.cvar(
        "cl_rollangle",
        "2.0",
        "the angle that the camera tilts to when the player moves to a side",
    );
    app.cvar(
        "cl_rollspeed",
        "200",
        "the speed threshold at which the camera starts to tilt",
    );
    app.cvar("cl_drawhud", "1", "whether to draw the HUD");
    app.cvar(
        "cl_shownet",
        "0",
        "toggle the display of current net status",
    );
    app.cvar(
        "cl_sidespeed",
        "350",
        "the base speed you move when pressing +left and +right",
    );
    app.cvar(
        "viewsize",
        "100",
        concat!(
            "set display size (in Seismon, this is ignored, but it will still be used to ",
            "hide the hud"
        ),
    );
    app.cvar(
        "cl_upspeed",
        "200",
        "the base speed you move when pressing +moveup and +movedown",
    );
    // TODO: How is this different from `cl_anglespeedkey`?
    app.cvar(
        "cl_yawspeed",
        "140",
        "sets the speed that the direction keys change the view angle",
    );
    app.cvar(
        "cl_hud",
        "3",
        "0: no hud, 1: transparent hud, 2: standard hud, 3: standard hud with ammo",
    )
    .alias("hudstyle", "cl_hud");
    app.cvar(
        "fov",
        "90",
        "sets the camera's field of view angle (in degrees)",
    );
    // TODO: What is the difference between this and `cl_skipCrosshair`?
    app.cvar("crosshair", "1", "whether to draw the crosshair");
    app.cvar(
        "m_pitch",
        Cvar::new("0.022").archive(),
        "sets the mouse vertical sensitivity multiplier",
    );
    app.cvar(
        "m_yaw",
        Cvar::new("0.022").archive(),
        "sets the mouse horizontal sensitivity multiplier",
    );
    app.cvar(
        "sensitivity",
        Cvar::new("3").archive(),
        "sets the mouse sensitivity",
    );
    app.cvar(
        "v_idlescale",
        "0",
        "Toggles whether the the view remains idle",
    );
    app.cvar("v_ipitch_cycle", "1", "");
    app.cvar("v_ipitch_level", "0.3", "");
    app.cvar("v_iroll_cycle", "0.5", "");
    app.cvar("v_iroll_level", "0.1", "");
    app.cvar("v_iyaw_cycle", "2", "");
    app.cvar("v_iyaw_level", "0.3", "");
    app.cvar(
        "v_kickpitch",
        "0.6",
        "sets the vertical distance the camera moves when the player takes damage",
    );
    app.cvar(
        "v_kickroll",
        "0.6",
        "sets the amount player view roll changes when player takes damage",
    );
    app.cvar(
        "v_kicktime",
        "0.5",
        "sets the duration that the pitch and roll are adjusted when player takes damage",
    );
    app.cvar(
        "scr_centertime",
        "2",
        "sets the duration that center text remains on the screen",
    );
    app.cvar("sv_gravity", "800", "sets the server's gravity");
}
