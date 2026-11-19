# Seismon

### An extensible, modern Quake engine

#### Click here for a preview video:

[![Preview video](https://img.youtube.com/vi/mBE5v9SwUV0/0.jpg)](https://youtu.be/mBE5v9SwUV0)

Seismon is a Quake engine written in Rust, based on the [Bevy](https://bevyengine.org/) framework. It is extensible,
written in a modern ECS style, has a modern deferred and pipelined rendering system, and includes a modern audio
framework. The engine is built as a set of plugins, and much of the work could be extracted to work in other games.

To my knowledge, this is the first full from-scratch rewrite of Quake 1, i.e. not forked from the Quake 1 source code
release.

### Installation

```sh
cargo install --frozen --git https://github.com/eira-fransham/seismon.git
```

### Running

Run the original Quake or other total conversions such as LibreQuake:

```sh
seismon --base-dir /path/to/quake
```

Run a mod:

```sh
seismon --base-dir /path/to/quake --game xmenroa
```

Run some commands at startup:

```sh
seismon --base-dir /path/to/quake +skill 2 +map e1m1
```

### Goals

The goal of this engine is to have the most-extensible Quake engine in the world. While engines
such as FTEQW are very powerful, they are still limited by the fact that any new features need
to be special-cased and built into the engine. As Bevy is incredibly modular, the intention is
to allow new features to be added easily by pulling in external Bevy crates. With
`bevy_mod_scripting`, code in Lua can access the ECS, which even opens up the possibility of
implementing builtins or new core engine features without recompiling the engine.

### Status

The game boots and can be played normally. Demos will play at startup, and you can open the
menu and start a new single-player game. Entering gates to transition between levels, as well
as the intermission screen at the end of levels, is fully implemented. I have yet to do a
full playthrough of the game, but I've played far enough to feel reasonably confident that
a full playthrough could be possible.

The console works much the same as the original game. You can run commands such as `map`,
explicitly invoke actions such as `+mlook`, set cvars like `sensitivity 2`, and bind
keys like `bind space +jump`. You can see the documentation for a command, action or
cvar with `help somecommand`.

### Help needed

See [issues](https://github.com/eira-fransham/seismon/issues) for a reasonably-up-to-date list of what needs help implementing.

### Feature checklist

- Networking
  - [x] NetQuake network protocol implementation (`sv_protocol 15`)
    - [x] Connection protocol implemented
    - [x] All in-game server commands handled
    - [x] Carryover between levels
  - [ ] FitzQuake extended protocol support (`sv_protocol 666`)
- Server
  - [x] Initialisation routines
  - [x] All opcodes implemented
  - [x] All builtins implemented
  - [x] Input handling
  - [x] Basic client connection handling
  - [x] Full client connection
  - [x] Single-player
  - [ ] External clients
  - [ ] Extended QuakeC builtins, supporting mods built for engines like FTEQW, QSS, DarkPlaces etc
- Rendering
  - [x] Deferred dynamic lighting
  - [x] Particle effects
  - [x] Pipelined rendering
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
    - [x] Ambient lighting
    - [x] Viewmodel rendering
  - UI
    - [x] Console
    - [x] HUD
    - [x] Level intermissions
    - [x] On-screen messages
    - [x] Menus
    - [ ] CSQC (client-side QuakeC)
- Use Bevy libraries instead of hand-rolling code
  - [x] Use `bevy_seedling` for audio
  - [x] Use `bevy_hanabi` for particles
  - [x] Use `bevy_ui` for UI
  - [x] Use standard Bevy rendering for the world/models
  - [x] Use `bevy_mod_scripting` for the QuakeC
  - [ ] Support for rollback netcode using `matchbox`
  - [ ] Integrate an optional physics engine
- Sound
  - [x] Loading and playback
  - [x] Entity sound
  - [x] Ambient sound
  - [x] Spatial attenuation
  - [x] Stereo spatialization
  - [x] Music
  - [x] Global effects, particularly lookahead-enabled limiting to prevent audio clipping
- Console
  - [x] Line editing
  - [x] History browsing
  - [x] Cvar modification
  - [x] Shell-style completion
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
