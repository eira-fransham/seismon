//! Spawning the entities in a map's entity lump, ported from `ED_LoadFromFile`/`ED_ParseEdict` in
//! `pr_edict.c`.

use bevy::prelude::*;
use bevy_mod_scripting_qcvm::{QCString, qcvm::Type};
use snafu::Report;

use crate::{
    common::parse,
    server::{
        ServerCtx,
        error::ServerError,
        qc::{
            self, LevelVm,
            fields::{QcTables, QcValue},
        },
    },
};

/// Not spawned on skill 0.
pub const SPAWNFLAG_NOT_EASY: i32 = 256;
/// Not spawned on skill 1.
pub const SPAWNFLAG_NOT_MEDIUM: i32 = 512;
/// Not spawned on skill 2 or 3.
pub const SPAWNFLAG_NOT_HARD: i32 = 1024;
/// Not spawned in deathmatch.
pub const SPAWNFLAG_NOT_DEATHMATCH: i32 = 2048;

/// Whether an entity with `spawnflags` is filtered out on `skill` (already rounded and clamped to
/// `0..=3`), as in `ED_LoadFromFile`.
fn inhibited(spawnflags: i32, skill: i32, deathmatch: bool) -> bool {
    if deathmatch {
        spawnflags & SPAWNFLAG_NOT_DEATHMATCH != 0
    } else {
        let flag = match skill {
            0 => SPAWNFLAG_NOT_EASY,
            1 => SPAWNFLAG_NOT_MEDIUM,
            _ => SPAWNFLAG_NOT_HARD,
        };
        spawnflags & flag != 0
    }
}

/// Parses a float from the map like `atof`, except that anything unparseable is 0 (rather than
/// the longest parseable prefix).
fn map_float(val: &str) -> f32 {
    val.trim().parse().unwrap_or_default()
}

impl ServerCtx<'_> {
    /// Spawns every entity in the entity lump `entmap`, as `ED_LoadFromFile` does.
    ///
    /// The first entity is written onto worldspawn (edict 0). Every later entity is allocated, has
    /// its keys set, and is freed if it's filtered out by `skill`/`deathmatch` or has no spawn
    /// function. Otherwise, its spawn function (named by its `classname`) is called.
    pub fn spawn_map_entities(&mut self, entmap: &str) -> Result<(), ServerError> {
        let skill = (self.registry().read_cvar::<f32>("skill").unwrap_or(1.) + 0.5) as i32;
        let skill = skill.clamp(0, 3);
        // Already zeroed when `coop` is set.
        let deathmatch = self.globals().deathmatch != 0.;

        let entity_list = parse::entities(entmap)
            .map_err(|e| ServerError::with_msg(format!("Failed parsing entity lump: {e}")))?;

        let mut inhibited_count = 0;
        for (i, keys) in entity_list.enumerate() {
            let is_world = i == 0;
            // Worldspawn isn't cleared, so the model, solid and movetype set by the server are
            // kept, as in Quake.
            let ent = if is_world { self.worldspawn() } else { self.alloc_uninitialized()? };

            if let Err(e) = self.parse_map_entity(ent, keys) {
                error!("Failed parsing map entity {i}: {}", Report::from_error(e));
                if !is_world {
                    self.free(ent)?;
                }
                continue;
            }

            let info = self.info(ent)?;
            let spawnflags = info.spawnflags as i32;
            let classname = info.classname.clone();

            if inhibited(spawnflags, skill, deathmatch) {
                if is_world {
                    warn!("Worldspawn has spawnflags {spawnflags} which would inhibit it");
                } else {
                    self.free(ent)?;
                    inhibited_count += 1;
                    continue;
                }
            }

            if classname.is_null() || classname.as_bytes().is_empty() {
                warn!("No classname for edict {}", self.edict(ent)?);
                if !is_world {
                    self.free(ent)?;
                }
                continue;
            }

            let has_spawn_fn = self
                .world
                .resource::<LevelVm>()
                .0
                .vm
                .functions()
                .get_by_name(&*classname.0)
                .is_ok();
            if !has_spawn_fn {
                warn!("No spawn function for {}", classname.to_string_lossy());
                if !is_world {
                    self.free(ent)?;
                }
                continue;
            }

            let worldspawn = self.worldspawn();
            let mut globals = self.globals_mut();
            globals.self_ = ent;
            globals.world = worldspawn;
            // QuakeC may have linked the entity or stored references to it before failing, so it
            // isn't freed.
            if let Err(e) = qc::call(self.world, classname.0.clone()) {
                error!(
                    "Failed spawning {}: {}",
                    classname.to_string_lossy(),
                    Report::from_error(e)
                );
            }
        }

        info!("{inhibited_count} entities inhibited");

        Ok(())
    }

    /// Writes the keys of a map entity onto `ent`, as `ED_ParseEdict` does.
    ///
    /// Bad map data (unknown keys, unparseable values) is warned about and skipped.
    ///
    /// ## Special cases
    ///
    /// There are two cases where the keys do not directly correspond to entity fields:
    ///
    /// - `angle`: This allows QuakeEd to write a single value instead of a set of Euler angles. The
    ///   value should be interpreted as the second component of the `angles` field.
    /// - `light`: This is simply an alias for `light_lev`.
    fn parse_map_entity<'a, I: IntoIterator<Item = (&'a str, &'a str)>>(
        &mut self,
        ent: Entity,
        keys: I,
    ) -> Result<(), ServerError> {
        let tables = self.world.resource::<QcTables>().clone();

        let set = |ctx: &mut Self, name: &str, value: QcValue| {
            tables
                .fields
                .set(ctx.world, ent, name, value)
                .map_err(|e| ServerError::with_msg(e.to_string()))
        };

        for (key, val) in keys {
            // Quake trims trailing spaces from key names (but not values).
            let key = key.trim_end_matches(' ');
            debug!(".{} = {}", key, val);
            match key {
                // ignore keys starting with an underscore
                k if k.starts_with('_') => {}

                "angle" => {
                    // this is referred to in the original source as "anglehack" -- essentially,
                    // only the yaw (Y) value is given. see
                    // https://github.com/id-Software/Quake/blob/master/WinQuake/pr_edict.c#L826-L834
                    set(self, "angles", QcValue::Vector(Vec3::new(0.0, map_float(val), 0.0)))?;
                }

                "light" => {
                    // more fun hacks brought to you by Carmack & Friends
                    set(self, "light_lev", QcValue::Float(map_float(val)))?;
                }

                k => {
                    let Some(kind) = tables.fields.kind(k) else {
                        warn!("'{k}' is not a field");
                        continue;
                    };

                    let value = match kind {
                        Type::String => QcValue::String(if val.is_empty() {
                            // The string table had the empty string as string 0, i.e. null.
                            QCString::null()
                        } else {
                            QCString::from(unescape_map_string(val).as_ref())
                        }),
                        Type::Float => QcValue::Float(map_float(val)),
                        Type::Vector => QcValue::Vector(parse::vec3(val)),
                        Type::Entity => {
                            let Some(entity) =
                                val.trim().parse::<u16>().ok().and_then(|id| self.edicts().get(id))
                            else {
                                warn!("{k}: {val:?} is not an allocated edict");
                                continue;
                            };
                            QcValue::Entity(entity)
                        }
                        Type::Function => {
                            warn!("{k}: function fields in maps are not supported");
                            continue;
                        }
                        ty => {
                            warn!("{k}: fields of type {ty} can't be set by maps");
                            continue;
                        }
                    };

                    set(self, k, value)?;
                }
            }
        }

        Ok(())
    }
}

/// Unescapes a string value from the map's entity lump, as Quake's `ED_NewString` does: `\n`
/// becomes a newline, and a backslash followed by anything else becomes a lone backslash.
fn unescape_map_string(val: &str) -> std::borrow::Cow<'_, str> {
    if !val.contains('\\') {
        return val.into();
    }

    let mut out = String::with_capacity(val.len());
    let mut chars = val.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some(_) => out.push('\\'),
            // A trailing backslash is kept.
            None => out.push('\\'),
        }
    }
    out.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unescape_map_string_matches_ed_new_string() {
        assert_eq!(unescape_map_string("plain"), "plain");
        assert_eq!(unescape_map_string(r"a\nb"), "a\nb");
        assert_eq!(unescape_map_string(r"a\xb"), "a\\b");
        assert_eq!(unescape_map_string("a\\"), "a\\");
    }

    #[test]
    fn map_float_is_zero_when_unparseable() {
        assert_eq!(map_float("90"), 90.);
        assert_eq!(map_float(" 90 "), 90.);
        assert_eq!(map_float("abc"), 0.);
        assert_eq!(map_float(""), 0.);
    }

    #[test]
    fn inhibited_by_skill() {
        for (skill, flag) in
            [(0, SPAWNFLAG_NOT_EASY), (1, SPAWNFLAG_NOT_MEDIUM), (2, SPAWNFLAG_NOT_HARD)]
        {
            assert!(inhibited(flag, skill, false), "skill {skill}");
            for other in [SPAWNFLAG_NOT_EASY, SPAWNFLAG_NOT_MEDIUM, SPAWNFLAG_NOT_HARD] {
                if other != flag {
                    assert!(!inhibited(other, skill, false), "skill {skill}, flag {other}");
                }
            }
            // Skill flags are ignored in deathmatch.
            assert!(!inhibited(flag, skill, true));
            assert!(!inhibited(SPAWNFLAG_NOT_DEATHMATCH, skill, false));
        }
        // Nightmare uses the hard flag.
        assert!(inhibited(SPAWNFLAG_NOT_HARD, 3, false));
        assert!(!inhibited(SPAWNFLAG_NOT_MEDIUM, 3, false));
    }

    #[test]
    fn inhibited_in_deathmatch() {
        assert!(inhibited(SPAWNFLAG_NOT_DEATHMATCH, 1, true));
        assert!(inhibited(SPAWNFLAG_NOT_DEATHMATCH | SPAWNFLAG_NOT_EASY, 0, true));
        assert!(!inhibited(0, 1, true));
    }
}
