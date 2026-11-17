//! Edict numbers: QuakeC's (and the network protocol's) numbering of entities.
//!
//! The server uses Bevy [`Entity`]s internally. Edict numbers are only used where their order is
//! observable (the physics loop, `find`, `findradius`, `checkclient`) and on the wire.

use std::ops::{Bound, RangeBounds};

use bevy::prelude::*;

/// For now, we just use the same limit as Quake.
pub const MAX_ENTITIES: usize = 600;

/// The edict number of a QuakeC entity.
#[derive(Component, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Edict(pub u16);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Slot {
    Vacant,
    /// Reserved for a client that hasn't spawned yet.
    Reserved,
    Occupied(Entity),
}

/// The mapping between edict numbers and entities.
///
/// Slot 0 is worldspawn and slots `1..=max_clients` are reserved for clients. Other entities are
/// allocated the lowest vacant slot.
#[derive(Resource, Debug)]
pub struct EdictTable {
    slots: Vec<Slot>,
    /// Slots which have been freed or overwritten since the last call to
    /// [`EdictTable::take_recycled`]. Any per-client networking state for these is stale.
    recycled: Vec<u16>,
    /// The edict numbers that removed entities had, for debugging.
    freed: hashbrown::HashMap<Entity, u16>,
}

impl EdictTable {
    pub fn new(worldspawn: Entity, num_players: usize) -> Self {
        let slots = std::iter::once(Slot::Occupied(worldspawn))
            .chain(std::iter::repeat_n(Slot::Reserved, num_players))
            .chain(std::iter::repeat(Slot::Vacant))
            .take(MAX_ENTITIES)
            .collect();

        Self { slots, recycled: Vec::new(), freed: Default::default() }
    }

    /// The entity with edict number `edict`, if it is allocated.
    pub fn get(&self, edict: u16) -> Option<Entity> {
        match self.slots.get(edict as usize)? {
            Slot::Occupied(entity) => Some(*entity),
            Slot::Vacant | Slot::Reserved => None,
        }
    }

    /// The worldspawn entity.
    pub fn worldspawn(&self) -> Entity {
        self.get(0).expect("Worldspawn should always exist")
    }

    /// Finds the slot to allocate a new entity in.
    pub fn vacant_slot(&mut self) -> u16 {
        let slot = match self.slots.iter().position(|slot| *slot == Slot::Vacant) {
            Some(slot) => slot,
            None => {
                self.slots.push(Slot::Vacant);
                self.slots.len() - 1
            }
        };

        slot.try_into().expect("Too many edicts")
    }

    /// Finds the first slot reserved for a client.
    pub fn reserved_slot(&self) -> Option<u16> {
        self.slots.iter().position(|slot| *slot == Slot::Reserved).map(|slot| slot as u16)
    }

    /// Puts `entity` in slot `edict`, which should come from [`EdictTable::vacant_slot`] or
    /// [`EdictTable::reserved_slot`].
    pub fn occupy(&mut self, edict: u16, entity: Entity) {
        let slot = &mut self.slots[edict as usize];
        if let Slot::Occupied(_) = slot {
            self.recycled.push(edict);
        }
        *slot = Slot::Occupied(entity);
    }

    /// Frees slot `edict`. Does nothing if it is already free.
    pub fn free(&mut self, edict: u16) {
        let Some(slot) = self.slots.get_mut(edict as usize) else {
            return;
        };

        let Slot::Occupied(entity) = *slot else {
            return;
        };

        self.freed.insert(entity, edict);
        *slot = Slot::Vacant;
        self.recycled.push(edict);
    }

    /// The edict number that a removed entity had.
    pub fn freed_edict(&self, entity: Entity) -> Option<u16> {
        self.freed.get(&entity).copied()
    }

    /// Drains the list of edicts whose slots have been freed or overwritten since the last call.
    pub fn take_recycled(&mut self) -> impl Iterator<Item = u16> + '_ {
        self.recycled.drain(..)
    }

    /// All allocated entities, in edict order.
    pub fn iter(&self) -> impl Iterator<Item = (u16, Entity)> + '_ {
        self.range(..)
    }

    /// The allocated entities with edict numbers in `range`, in edict order.
    pub fn range<R>(&self, range: R) -> impl Iterator<Item = (u16, Entity)> + '_
    where
        R: RangeBounds<usize>,
    {
        let start = match range.start_bound() {
            Bound::Unbounded => 0,
            Bound::Excluded(bound) => *bound + 1,
            Bound::Included(bound) => *bound,
        };
        let end = match range.end_bound() {
            Bound::Unbounded => self.slots.len(),
            Bound::Excluded(bound) => *bound,
            Bound::Included(bound) => *bound + 1,
        }
        .min(self.slots.len());

        self.slots.get(start.min(end)..end).unwrap_or_default().iter().enumerate().filter_map(
            move |(i, slot)| match slot {
                Slot::Occupied(entity) => Some(((start + i) as u16, *entity)),
                Slot::Vacant | Slot::Reserved => None,
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entity(i: u32) -> Entity {
        Entity::from_raw_u32(i).unwrap()
    }

    #[test]
    fn allocation_matches_quake() {
        let mut table = EdictTable::new(entity(1), 2);

        // World is slot 0 and clients are reserved, so the first entity is after them.
        let a = table.vacant_slot();
        assert_eq!(a, 3);
        table.occupy(a, entity(10));
        let b = table.vacant_slot();
        assert_eq!(b, 4);
        table.occupy(b, entity(11));

        // Freed slots are reused lowest-first, and reported as recycled.
        table.free(a);
        assert_eq!(table.vacant_slot(), 3);
        assert_eq!(table.take_recycled().collect::<Vec<_>>(), [3]);

        // Reserved slots are used for clients.
        let client = table.reserved_slot().unwrap();
        assert_eq!(client, 1);
        table.occupy(client, entity(12));
        assert_eq!(table.reserved_slot(), Some(2));

        assert_eq!(
            table.iter().collect::<Vec<_>>(),
            [(0, entity(1)), (1, entity(12)), (4, entity(11))]
        );
        assert_eq!(table.range(2..).collect::<Vec<_>>(), [(4, entity(11))]);
        assert_eq!(table.get(3), None);
        assert_eq!(table.get(2), None);
    }
}
