use crate::data::errors::Result;

pub trait Lang {
    type Ty;
    type Val;
    type Function;
    type Anf;
}

pub trait NaturalEmbedding<Fr: Lang> {
    type Val;
    fn embed(_: &<Fr as Lang>::Val) -> Result<Self::Val>;
    // fn embed_type(_: &<Fr as Lang>::Ty) -> Option<<Fr as Lang>::Ty>;
}

pub trait IsTyped<T: PartialEq> {
    fn is_prod(&self) -> bool;
    fn as_type(&self) -> T;
    fn is_type(&self, ty: &T) -> bool {
        self.as_type() == *ty
    }
}
