use std::{any::TypeId, fmt, marker::PhantomData};

use bevy::{
    animation::animatable::{Animatable as _, BlendInput},
    app::{App, Last, Plugin, PostUpdate},
    camera::Camera,
    color::{Color, LinearRgba},
    core_pipeline::{
        core_3d::graph::Node3d,
        fullscreen_material::{FullscreenMaterial, FullscreenMaterialPlugin},
    },
    ecs::{
        component::Component,
        entity::Entity,
        lifecycle::{Insert, Remove},
        observer::On,
        query::{Changed, With},
        relationship::RelationshipTarget,
        schedule::IntoScheduleConfigs as _,
        system::{Commands, ParallelCommands, Query},
    },
    render::{
        extract_component::ExtractComponent,
        render_graph::{InternedRenderLabel, RenderLabel as _},
        render_resource::ShaderType,
    },
    shader::ShaderRef,
};

use crate::client::systems::frame::ViewEntities;

#[derive(Default, Debug)]
#[non_exhaustive]
pub struct ColorShiftPlugin {}

impl Plugin for ColorShiftPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(FullscreenMaterialPlugin::<ComputedColorShift>::default())
            .add_systems(Last, (mark_color_shift_dirty, compute_color_shifts).chain())
            .init_color_shift::<Contents>()
            .init_color_shift::<Damage>()
            .init_color_shift::<Bonus>()
            .init_color_shift::<Powerup>();
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Contents {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Damage {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bonus {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Powerup {}

impl ColorShiftPass for Contents {
    const DECAY: f32 = 0.;
}

impl ColorShiftPass for Damage {
    const COLOR: Option<Color> = Some(Color::linear_rgb(1., 0., 0.));
    const DECAY: f32 = 0.;
}

impl ColorShiftPass for Bonus {
    const DECAY: f32 = 0.;
}

impl ColorShiftPass for Powerup {
    const DECAY: f32 = 0.;
}

pub trait ColorShiftPass: Send + Sync + 'static {
    const COLOR: Option<Color> = None;
    const DECAY: f32;
}

pub trait ColorShiftApp {
    fn init_color_shift<M>(&mut self) -> &mut Self
    where
        M: ColorShiftPass;
}

impl ColorShiftApp for App {
    fn init_color_shift<M>(&mut self) -> &mut Self
    where
        M: ColorShiftPass,
    {
        self.add_observer(spawn_color_shift_entity::<M>)
            .add_observer(despawn_color_shift_entity::<M>)
            .add_systems(PostUpdate, update_color_shift_entities::<M>)
    }
}

#[derive(Component, Debug)]
#[relationship(relationship_target = ColorShifts)]
struct ColorShiftFor {
    #[relationship]
    entity: Entity,
    type_id: Option<TypeId>,
}

#[derive(Component, Default, Debug, PartialEq)]
struct ErasedColorShift {
    color: Option<Color>,
    amount: f32,
}

#[derive(Component, Debug)]
#[relationship_target(relationship = ColorShiftFor, linked_spawn)]
struct ColorShifts(Vec<Entity>);

#[derive(Component)]
pub struct ColorShift<M> {
    /// The color to shift towards, or `None` to disable.
    pub color: Option<Color>,
    /// The ratio of existing color to shifted color. 0 = no change, 1 = maximum shift, but 1 does
    /// not necessarily mean that the screen is just block color (depends on blend mode).
    pub amount: f32,
    /// The amount that this amount will decay per second
    pub decay: f32,
    _phantom: PhantomData<M>,
}

impl<M> ColorShift<M>
where
    M: ColorShiftPass,
{
    pub fn new(color: Color, amount: f32) -> Self {
        Self { color: Some(color), amount, ..Default::default() }
    }
}

impl<M> ColorShift<M> {
    fn erase(&self) -> ErasedColorShift {
        let ColorShift { color, amount, .. } = *self;
        ErasedColorShift { color, amount }
    }
}

impl<M> fmt::Debug for ColorShift<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ColorShift")
            .field("color", &self.color)
            .field("amount", &self.amount)
            .field("decay", &self.decay)
            .finish()
    }
}

impl<M> Default for ColorShift<M>
where
    M: ColorShiftPass,
{
    fn default() -> Self {
        Self { color: M::COLOR, decay: M::DECAY, amount: 0., _phantom: PhantomData }
    }
}

#[derive(Component, ExtractComponent, Default, Clone, Copy, ShaderType, Debug)]
struct ComputedColorShift {
    color: LinearRgba,
}

impl FullscreenMaterial for ComputedColorShift {
    fn fragment_shader() -> ShaderRef {
        "assets/color-shift.wgsl".into()
    }

    fn node_edges() -> Vec<InternedRenderLabel> {
        vec![
            Node3d::Tonemapping.intern(),
            // The label is automatically generated from the name of the struct
            Self::node_label().intern(),
            Node3d::EndMainPassPostProcessing.intern(),
        ]
    }
}

#[derive(Component, Default, Clone, Copy, Debug)]
struct RebuildColorShiftMarker;

fn spawn_color_shift_entity<M>(
    trigger: On<Insert, ColorShift<M>>,
    mut commands: Commands,
    shifts: Query<&ColorShift<M>>,
) where
    M: Send + Sync + 'static,
{
    let Ok(shift) = shifts.get(trigger.entity) else {
        return;
    };

    commands.spawn((
        ColorShiftFor { entity: trigger.entity, type_id: Some(TypeId::of::<M>()) },
        shift.erase(),
    ));
}

fn despawn_color_shift_entity<M>(
    trigger: On<Remove, ColorShift<M>>,
    mut commands: Commands,
    color_shifts: Query<&ColorShifts>,
    shift_entities: Query<&ColorShiftFor>,
) where
    M: Send + Sync + 'static,
{
    let Ok(shifts) = color_shifts.get(trigger.entity) else {
        return;
    };

    let shift_entity = shifts.iter().find(|shift_entity| {
        shift_entities
            .get(*shift_entity)
            .is_ok_and(|shift| shift.type_id == Some(TypeId::of::<M>()))
    });

    if let Some(shift_entity) = shift_entity {
        commands.entity(shift_entity).despawn();
    }
}

fn update_color_shift_entities<M>(
    mut shift_entities: Query<(&ColorShiftFor, &mut ErasedColorShift)>,
    shifts: Query<&ColorShift<M>, Changed<ColorShift<M>>>,
) where
    M: Send + Sync + 'static,
{
    shift_entities.par_iter_mut().for_each(|(target, mut shift)| {
        let Ok(target) = shifts.get(target.entity) else {
            return;
        };

        let erased = target.erase();

        if *shift != erased {
            *shift = erased;
        }
    });
}

fn mark_color_shift_dirty(
    commands: ParallelCommands,
    shift_entities: Query<&ColorShiftFor, Changed<ErasedColorShift>>,
) {
    shift_entities.par_iter().for_each(|shift_for| {
        commands.command_scope(|mut commands| {
            commands.entity(shift_for.entity).try_insert_if_new(RebuildColorShiftMarker);
        });
    });
}

fn compute_color_shifts(
    commands: ParallelCommands,
    mut entities: Query<(Entity, &ColorShifts, &ViewEntities), With<RebuildColorShiftMarker>>,
    cameras: Query<(), With<Camera>>,
    shifts: Query<&ErasedColorShift>,
) {
    entities.par_iter_mut().for_each(|(root, shift_ents, view_ents)| {
        let colors = shift_ents
            .iter()
            .filter_map(|shift_ent| shifts.get(shift_ent).ok())
            .filter_map(|shift_ent| Some((shift_ent.color?, shift_ent.amount)))
            .map(|(color, amount)| BlendInput {
                weight: 1.,
                value: LinearRgba { alpha: amount, ..color.to_linear() },
                additive: true,
            });
        let color = LinearRgba::blend(
            std::iter::once(BlendInput { weight: 1., value: LinearRgba::NONE, additive: true })
                .chain(colors),
        );

        let computed = ComputedColorShift { color };

        commands.command_scope(|mut commands| {
            for view in view_ents.iter() {
                if cameras.get(view).is_ok() {
                    commands.entity(view).insert(computed);
                }
            }

            commands.entity(root).remove::<RebuildColorShiftMarker>();
        });
    });
}
