use bevy::{
    ecs::system::{Commands, In},
    time::{Fixed, Time},
};

use bevy_mod_commands::RegisterCmdExt;

pub fn register_cvars<A: RegisterCmdExt>(app: &mut A) {
    // TODO: X-Men: Ravages of Apocalypse uses `temp1` as a cvar, we should have some way to
    // explicitly cvars       that haven't been declared.
    app.cvar("sv_paused", "0", "1 if the server is paused, 0 otherwise")
        // TODO: Have a way to have cvars be basic getter/setter pairs so we can implement `deathmatch` and `coop`
        //       cvars in terms of this.
        .cvar("coop", "0", "1 for a co-op game, 0 otherwise")
        .cvar("deathmatch", "0", "Nonzero for a deathmatch game (2: weapons stay, items don't respawn)")
        .cvar(
            "teamplay",
            "0",
            "0: no teams, 1: friendly fire disabled, 2: friendly fire enabled",
        )
        .cvar("noexit", "0", "1 to prevent players from leaving the level in deathmatch")
        .cvar("samelevel", "0", "1 to restart the same level instead of going to the next one")
        .cvar("temp1", "0", "Used internally for QuakeC")
        .cvar("skill", "1", "0: easy, 1: normal, 2: hard, 3: nightmare")
        .cvar("sv_gravity", "800", "Gravity strength")
        .cvar(
            "sv_stepheight",
            "18",
            "Max step size (for players and monsters)",
        )
        .cvar("fraglimit", "0", "End game when a player reaches this number of frags")
        .cvar("timelimit", "0", "End game when the level has been running for this amount of time (in seconds)")
        .cvar("sv_aim", "2", "Maximum auto-aim distance")
        .cvar("sv_maxvelocity", "2000", "Maximum velocity of entities")
        .cvar("sv_maxvelocity", "2000", "Maximum velocity of entities")
        .cvar("sv_maxspeed", "320", "Set the ground speed that the player will try to walk at (not the maximum possible speed)")
        .cvar("sv_maxairspeed", "30", "Set the air speed that the player will try to move at (not the maximum possible speed)")
        .cvar("sv_stopspeed", "100", "Set the player's grounded friction")
        .cvar("sv_accelerate", "10", "Set the player's acceleration (on the ground and in the air)")
        .cvar("sv_wateraccelerate", "10", "Set the player's water acceleration")
        .cvar("sv_friction", "4", "Set the player's air friction")
        .cvar("sv_waterfriction", "4", "Set the player's water friction")
        .cvar("sv_seed", "0", "Seed for the server's random number generator, applied when a level starts")
        .cvar("edgefriction", "2", "Friction multiplier when the player is about to walk off an edge")
        .cvar_on_set(
            "sys_tickrate",
            "0.05",
            |In(new_tickrate), mut commands: Commands| {
                commands.insert_resource(Time::<Fixed>::from_seconds(
                    serde_lexpr::from_value(&new_tickrate).unwrap_or(0.05),
                ));
            },
            "Tickrate of server (how often the server updates)",
        );
}
