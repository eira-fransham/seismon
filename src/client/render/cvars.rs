// Copyright © 2020 Cormac O'Brien.
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

use bevy::prelude::*;

use crate::common::console::RegisterCmdExt;

pub fn register_cvars(app: &mut App) {
    // TODO: Implement this
    app.cvar(
        "r_lightmap",
        "0",
        "only render the lightmap, and not the main texture",
    )
    // TODO: Re-implement MSAA
    .cvar(
        "r_msaa_samples",
        "1",
        "set the multi-sampled anti-aliasing sample count",
    )
    .cvar(
        "r_sky_scollspeed",
        "32",
        "Skybox texture scroll speed (in texels)",
    )
    .cvar(
        "post_blendmode",
        "normal",
        "Sets the blend mode for postprocess color shift",
    )
    .cvar(
        "post_colorspace",
        "oklab",
        "Sets the colorspace for postprocess color shift",
    );
}
