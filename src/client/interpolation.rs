use std::mem;

use bevy::{
    animation::animatable::Animatable,
    app::{App, Last},
    ecs::{
        component::{Component, Mutable},
        entity::Entity,
        query::{Changed, Or, With, Without},
        reflect::ReflectComponent,
        schedule::IntoScheduleConfigs as _,
        system::{Commands, Query, Res},
    },
    reflect::Reflect,
    time::Time,
};

pub trait InterpolateApp {
    fn interpolate_component<C, T>(&mut self) -> &mut Self
    where
        C: Default + Clone + Component<Mutability = Mutable> + Animatable,
        T: Default + Send + Sync + 'static;
}

impl InterpolateApp for App {
    fn interpolate_component<C, T>(&mut self) -> &mut Self
    where
        C: Default + Clone + Component<Mutability = Mutable> + Animatable,
        T: Default + Send + Sync + 'static,
    {
        self.add_systems(
            Last,
            (no_interpolate::<C>, (interpolate::<C, T>, next_to_prev::<C>).chain()),
        )
    }
}

#[derive(Clone, Component, Reflect)]
#[reflect(Component)]
#[repr(transparent)]
struct Prev<C>(Next<C>)
where
    C: Default + Component;

#[derive(Clone, Component, Reflect)]
#[reflect(Component)]
#[repr(transparent)]
struct OldNext<C>(Next<C>)
where
    C: Default + Component;

#[derive(Clone, Component, Reflect)]
#[reflect(Component)]
#[require(C)]
pub struct Next<C>
where
    C: Default + Component,
{
    pub component: C,
    pub elapsed_secs: f64,
}

#[derive(Clone, Component, Reflect)]
#[reflect(Component)]
pub struct NoInterpolation;

fn no_interpolate<C>(
    components: Query<(&Next<C>, &mut C), Or<(With<NoInterpolation>, Without<Prev<C>>)>>,
) where
    C: Default + Clone + Component<Mutability = Mutable> + Animatable,
{
    for (next, mut cur) in components {
        *cur = next.component.clone();
    }
}

fn interpolate<C, T>(
    components: Query<(&Prev<C>, &Next<C>, &mut C), Without<NoInterpolation>>,
    time: Res<Time<T>>,
) where
    C: Default + Clone + Component<Mutability = Mutable> + Animatable,
    T: Default + Send + Sync + 'static,
{
    for (prev, next, mut cur) in components {
        let range = next.elapsed_secs - prev.0.elapsed_secs;
        let factor = ((time.elapsed_secs_f64() - prev.0.elapsed_secs) / range).clamp(0., 1.);

        *cur = C::interpolate(&prev.0.component, &next.component, factor as f32);
    }
}

fn next_to_prev<C>(
    components: Query<(Entity, Option<&mut OldNext<C>>, &mut Next<C>), Changed<Next<C>>>,
    mut commands: Commands,
) where
    C: Default + Clone + Component<Mutability = Mutable> + Animatable,
{
    for (ent, mut old_next, next) in components {
        if let Some(OldNext(old_next)) = old_next.as_deref_mut() {
            let old_next = mem::replace(old_next, next.clone());
            commands.entity(ent).insert(Prev(old_next));
        } else {
            commands.entity(ent).insert(OldNext(next.clone()));
        }
    }
}
