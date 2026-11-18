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
    Ok(builder
        // .add_submenu("Customize controls", unimplemented!())
        .add_action("Go to console", |mut commands: MessageWriter<RunCmd<'static>>| {
            commands.write("toggleconsole".into());
        })
        // TODO
        .add_action("Reset to defaults", |mut cvars: ResMut<Registry>| {
            for cvar in [
                "r_renderscale",
                "r_screensize",
                "gamma",
                "cl_sensitivity",
                "bgmvolume",
                "volume",
                "cl_alwaysrun",
                "invertmouse",
                "lookspring",
                "lookstrafe",
            ] {
                if let Err(e) = cvars.reset_cvar(cvar) {
                    warn!("{}", e);
                }
            }
        })
        .add_slider("Render scale", 0.25, 1.0, 2, 0, "r_renderscale")?
        .add_slider("Screen Size", 0.0, 1.0, 10, 9, "r_screensize")?
        .add_slider("Brightness", 0.5, 1.5, 10, 5, "r_gamma")?
        .add_slider("Mouse Speed", 0.0, 1.0, 10, 9, "cl_sensitivity")?
        .add_slider("CD music volume", 0.0, 1.0, 10, 9, "bgmvolume")?
        .add_slider("Sound volume", 0.0, 1.0, 10, 9, "volume")?
        // TODO
        .add_toggle("Always run", true, "cl_alwaysrun")
        .add_toggle("Invert mouse", false, "invertmouse")
        .add_toggle("Lookspring", false, "lookspring")
        .add_toggle("Lookstrafe", false, "lookstrafe")
        // .add_submenu("Video options", unimplemented!())
        .build(MenuView {
            draw_plaque: true,
            title_path: "gfx/p_option.lmp".into(),
            body: MenuBodyView::Dynamic,
        }))
}
