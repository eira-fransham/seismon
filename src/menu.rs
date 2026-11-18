use bevy::{
    app::AppExit,
    ecs::{
        message::{MessageWriter, Messages},
        system::ResMut,
    },
    log::warn,
};
use bevy_mod_commands::{Registry, RunCmd};
use seismon::client::menu::{Menu, MenuBodyView, MenuBuilder, MenuView};

pub fn build_main_menu(builder: MenuBuilder) -> anyhow::Result<Menu> {
    Ok(builder
        .add_submenu("Single Player", build_menu_sp)?
        .add_submenu("Multiplayer", build_menu_mp)?
        .add_submenu("Options", build_menu_options)?
        .add_action("Help/Ordering", || ())
        .add_action("Quit", |mut quit: ResMut<Messages<AppExit>>| {
            quit.write(AppExit::Success);
        })
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/ttl_main.lmp".into(),
            body: MenuBodyView::Predefined { path: "gfx/mainmenu.lmp".into() },
        }))
}

fn build_menu_sp(builder: MenuBuilder) -> anyhow::Result<Menu> {
    Ok(builder
        .add_action("New Game", |mut commands: MessageWriter<RunCmd<'static>>| {
            commands.write("map start".parse().unwrap());
        })
        .add_action("Load", || unimplemented!())
        .add_action("Save", || unimplemented!())
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/ttl_sgl.lmp".into(),
            body: MenuBodyView::Predefined { path: "gfx/sp_menu.lmp".into() },
        }))
}

fn build_menu_mp(builder: MenuBuilder) -> anyhow::Result<Menu> {
    Ok(builder
        .add_submenu("Join a Game", build_menu_mp_join)?
        .add_action("New Game", || unimplemented!())
        .add_action("Setup", || unimplemented!())
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/p_multi.lmp".into(),
            body: MenuBodyView::Predefined { path: "gfx/mp_menu.lmp".into() },
        }))
}

fn build_menu_mp_join(builder: MenuBuilder) -> anyhow::Result<Menu> {
    Ok(builder
        .add_submenu("TCP", build_menu_mp_join_tcp)?
        // .add_textbox // description
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/p_multi.lmp".into(),
            body: MenuBodyView::Predefined { path: "gfx/mp_menu.lmp".into() },
        }))
}

fn build_menu_mp_join_tcp(builder: MenuBuilder) -> anyhow::Result<Menu> {
    // Join Game - TCP/IP          // title
    //
    //  Address: 127.0.0.1         // label
    //
    //  Port     [26000]           // text field
    //
    //  Search for local games...  // menu
    //
    //  Join game at:              // label
    //  [                        ] // text field
    Ok(builder
        // .add
        // TODO
        .add_toggle("placeholder", false, "scratch1")
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/p_multi.lmp".into(),
            body: MenuBodyView::Dynamic,
        }))
}

fn build_menu_options(builder: MenuBuilder) -> anyhow::Result<Menu> {
    // Ranges match `M_AdjustSliders` in `menu.c`.
    Ok(builder
        // TODO: "Customize controls"
        .add_action("Go to console", |mut commands: MessageWriter<RunCmd<'static>>| {
            commands.write("toggleconsole".into());
        })
        .add_action("Reset to defaults", |mut cvars: ResMut<Registry>| {
            for cvar in [
                "viewsize",
                "gamma",
                "sensitivity",
                "bgmvolume",
                "volume",
                "lookspring",
                "lookstrafe",
            ] {
                if let Err(e) = cvars.reset_cvar(cvar) {
                    warn!("{}", e);
                }
            }
        })
        .add_slider("Screen size", 30., 120., 10, 7, "viewsize")?
        // Lower gamma is brighter, so the slider is inverted.
        .add_slider("Brightness", 1., 0.5, 11, 0, "gamma")?
        .add_slider("Mouse Speed", 1., 11., 21, 4, "sensitivity")?
        .add_slider("CD Music Volume", 0., 1., 11, 10, "bgmvolume")?
        .add_slider("Sound Volume", 0., 1., 11, 7, "volume")?
        // TODO: "Always Run" (`cl_forwardspeed`/`cl_backspeed` 200 <-> 400) and "Invert Mouse"
        // (negate `m_pitch`) need toggles that aren't backed by a single boolean cvar.
        .add_toggle("Lookspring", false, "lookspring")
        .add_toggle("Lookstrafe", false, "lookstrafe")
        // TODO: "Video Options"
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/p_option.lmp".into(),
            body: MenuBodyView::Dynamic,
        }))
}
