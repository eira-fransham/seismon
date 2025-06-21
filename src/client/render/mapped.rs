use std::{
    borrow::{Borrow, BorrowMut},
    marker::PhantomData,
};

pub trait Metadata: 'static {
    type Metadata<'a>: Copy;

    fn to_metadata(&self) -> Self::Metadata<'_>;
}

impl<A: 'static, B: 'static> Metadata for (A, B) {
    type Metadata<'a> = (&'a A, &'a B);

    fn to_metadata(&self) -> Self::Metadata<'_> {
        (&self.0, &self.1)
    }
}

pub struct Mapped<
    M,
    I,
    O = I,
    F = fn(<M as Metadata>::Metadata<'_>, &I) -> O,
    ICollection = Vec<I>,
    OCollection = Vec<O>,
> where
    M: Metadata,
{
    inputs: ICollection,
    outputs: OCollection,
    mapper: F,
    _marker: PhantomData<(fn(<M as Metadata>::Metadata<'_>, &I) -> O, I, O)>,
}

impl<I, O, M, F, ICollection, OCollection> Mapped<M, I, O, F, ICollection, OCollection>
where
    M: Metadata,
{
    pub fn new(inputs: ICollection, outputs: OCollection, mapper: F) -> Self {
        Self {
            inputs,
            outputs,
            mapper,
            _marker: PhantomData,
        }
    }

    pub fn set_inputs(&mut self, new_inputs: ICollection) {
        self.inputs = new_inputs;
    }
}

impl<I, O, M, F, ICollection, OCollection> Mapped<M, I, O, F, ICollection, OCollection>
where
    OCollection: Borrow<[O]>,
    M: Metadata,
{
    pub fn outputs(&self) -> &[O] {
        self.outputs.borrow()
    }
}

impl<I, O, M, F, ICollection, OCollection> Mapped<M, I, O, F, ICollection, OCollection>
where
    ICollection: Borrow<[I]>,
    OCollection: BorrowMut<[O]>,
    F: for<'a, 'b> FnMut(M::Metadata<'a>, &'b I) -> O,
    M: Metadata,
{
    pub fn update(&mut self, metadata: &M) {
        let metadata = metadata.to_metadata();
        for (input, output) in self.inputs.borrow().iter().zip(self.outputs.borrow_mut()) {
            *output = (self.mapper)(metadata, input);
        }
    }
}
