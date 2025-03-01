# Seismon

### An extensible, modern Quake engine

#### Click here for a preview video:

[![Preview video](https://img.youtube.com/vi/Hy5QStHHv8A/0.jpg)](https://www.youtube.com/watch?v=Hy5QStHHv8A)

Seismon is a Quake engine written in Rust, based on the [Bevy](https://bevyengine.org/) framework. It is extensible,
written in a modern ECS style, has a modern deferred and pipelined rendering system, and includes a modern audio
framework. The engine is built of a set of plugins, and much of the work could be extracted to work in other games,
especially the console. Work to extract these pieces out is ongoing.

To my knowledge, this is the first full from-scratch rewrite of Quake 1, i.e. not built on top of the Quake 1 source code
release.

Bevy has a lot of features related to rendering, audio and state management that in my [previous Quake-related project](https://github.com/eira-fransham/goeld)
had to be implement manually - specifically, tonemapping/HDR and pipelined rendering. The audio system has also been completely
overhauled, and I even implemented a rewrite of Bevy's default audio framework to allow adding custom DSP effects using
[`fundsp`](https://github.com/SamiPerttu/fundsp). The way that it is currently written is very different from
[`bevy_fundsp`](https://github.com/harudagondi/bevy_fundsp), which is essentially just a helper for writing DSP output
to a buffer and then sending that buffer to Bevy's normal audio systems. All audio is now heirarchically organised into
mixers, with each mixer having control over the internal audio processing. The use-case for this could be that game audio
could have reverb applied while the menu audio could be left unchanged. Mixers are just normal components and can be
accessed as such. The project for the fork of `bevy_audio` is at [`bevy-mod-dynamicaudio`](https://github.com/eira-fransham/bevy-mod-dynamicaudio).
The audio effects in the build of the game at time of writing are a subtle reverb and filter delay, but most importantly
I have added a limiter so that the game audio doesn't completly blow the speakers out when there are more than a couple of
sounds playing at once.

Based on [Richter](https://github.com/cormac-obrien/richter) by Cormac O'Brien, and still shares a lot of its DNA. It should
be at feature parity with Richter but as refactoring work is ongoing some things may be broken temporarily. Started as a quick
project to make Richter run on macOS, but ended up with some 15,000 lines added and 10,000 lines removed.

![Preview of demo playback](content/preview.gif)

### Goals

The ultimate goal is for the renderer, client-server interactions, server, input and console to be separate modules that can
be mixed and matched without requiring all of them. As Quake already has a client-server model even in singleplayer games,
once the client is its own separate system that only communicates with the server through regular networking methods it
should be possible to write game logic in Rust (and therefore any scripting layer that integrates with Bevy, such as Lua)
and still have regular Quake clients connect to it, instead of being restricted to QuakeC.

These goals are partially completed, as the audio, rendering and input handling are already separate plugins, although
there are still some remaining interdependence issues.

### Status

The console and post-processing are done using regular Bevy primitives, with the console being rendered using `bevy-ui`.
The world and client updates are still handled with a centralised struct instead of components, making it impossible for
regular systems to interact with it. The console and keybinding system has been updated to be much more extensible, and
command implementations are just regular systems which can access any resource or component. All rendering is done through
the Bevy rendergraph, although the rendering code itself is still mostly written by hand using wgpu, albeit in a much more
extensible way than the original Richter implementation.

Hosting a server has preliminary support - the server can run all the initialisation code in QuakeC, and it can
load into any level from the original game. At the time of writing, input is unimplemented, but most physics routines and
thinking are implemented. Clients can send movement information, but that information is not used yet. The server can
communicate with the client locally, although remote clients are still unimplemented. At time of writing there seems to be
a bug where enemies are either not appearing or being immediately killed. Work on the server is the current highest priority
task. Here is an example of the client connected to the local Seismon server running `e1m2`:

![Preview of running server](content/seismon-server.gif)

`map` works the first time, although there are still bugs when changing maps (presumably due to state being incorrectly carried
over between map runs), and `changelevel` is still unimplemented.

Networking is untested since beginning the rewrite, and I've been only using demos as a testcase. It is a priority to get this
working again once the client update code is ported to use the ECS. I haven't touched most of the networking code, so in
theory it should still work or only require minor changes.

I've implemented mod support outside of the original `id1` directory, and so far all the mods that are designed to work with the
original Quake release work. I have tried Hipnotic, Rogue, Soul of Evil, and Xmen: Ravages of Apocalypse, and playing demos from
all of these games works. I have run the entirety of the "Quake Done Quickest" demos (`qdqst`) so can confirm that all maps from
the original game can be loaded and rendered correctly.

A host of bugs and limitations from the original Richter were fixed. Inputs are no longer handled by an enum and you can
define your own arbitrary `+action`/`-action` commands which can be bound, and arbitrary cvars which can have a system
attached which will run when the cvar is changed. Commands are also implemented as systems, and so can have access to global
state.

There are still a couple of small pieces of code that use nightly Rust, but I hope to fix those soon.

### Help needed

See [issues](https://github.com/eira-fransham/seismon/issues) for an up-to-date list of what needs help implementing.

### Running

```
cd /path/to/quake
# To run Quake 1 (id1 folder)
cargo +nightly run --release --manifest-path /path/to/seismon --bin quake-client
# To run other games
cargo +nightly run --release --manifest-path /path/to/seismon --bin quake-client -- --game [GAME_NAME]
```

#### Feature checklist

- Networking
  - [x] NetQuake network protocol implementation (`sv_protocol 15`)
    - [x] Connection protocol implemented
    - [x] All in-game server commands handled
    - [x] Carryover between levels
  - [ ] FitzQuake extended protocol support (`sv_protocol 666`)
- Rendering
  - [x] Deferred dynamic lighting
  - [x] Particle effects
  - [x] Pipelined rendering
  - [x] Customizable UI
  - Brush model (`.bsp`) rendering
    - Textures
      - [x] Static textures
      - [x] Animated textures
      - [x] Alternate animated textures
      - [x] Liquid texture warping
      - [x] Sky texture scrolling
    - [x] Lightmaps
    - [x] Occlusion culling
  - Alias model (`.mdl`) rendering
    - [x] Keyframe animation
      - [x] Static keyframes
      - [x] Animated keyframes
    - [ ] Keyframe interpolation
    - [ ] Ambient lighting
    - [x] Viewmodel rendering
  - UI
    - [x] Console
    - [x] HUD
    - [x] Level intermissions
    - [x] On-screen messages
    - [x] Menus
- Sound
  - [x] Loading and playback
  - [x] Entity sound
  - [x] Ambient sound
  - [x] Spatial attenuation
  - [ ] Stereo spatialization (almost complete)
  - [x] Music
  - [x] Global effects, particularly lookahead-enabled limiting to prevent audio clipping
- Console
  - [x] Line editing
  - [x] History browsing
  - [x] Cvar modification
  - [x] Command execution
  - [x] Quake script file execution
- Demos
  - [x] Demo playback
  - [ ] Demo recording
- File formats
  - [x] BSP loader
  - [x] MDL loader
  - [x] SPR loader
  - [x] PAK archive extraction
  - [x] WAD archive extraction

## Legal

This software is released under the terms of the MIT License (see LICENSE.txt).

This project is in no way affiliated with id Software LLC, Bethesda Softworks LLC, or ZeniMax Media
Inc. Information regarding the Quake trademark can be found at Bethesda's [legal information
page](https://bethesda.net/en/document/legal-information).

Due to licensing restrictions, the data files necessary to run Quake cannot be distributed with this
package. `pak0.pak`, which contains the files for the first episode ("shareware Quake"), can be
retrieved from id's FTP server at `ftp://ftp.idsoftware.com/idstuff/quake`. The full game can be
purchased from a number of retailers including Steam and GOG.
