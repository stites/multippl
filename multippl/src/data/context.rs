// use crate::annotate::grammar::Var;
// use crate::data::output::{Output, SubstMap};
// use crate::data::WeightMap;
// use crate::grammar::SVal;
// use crate::uniquify::grammar::UniqueId;
// use rsdd::repr::bdd::BddPtr;
// use crate::data::HashMap;

// #[derive(Debug, Clone)]
// pub struct Context {
//     pub accept: BddPlan,
//     pub samples: BddPlan,
//     pub substitutions: SubstMap,
//     pub weightmap: WeightMap,
//     pub ssubstitutions: HashMap<UniqueId, Vec<SVal>>,
// }
// impl Context {
//     pub fn from_output(c: &Output) -> Self {
//         match c {
//             Exact(out) => {},
//             Sample(out)=> {},
//             Interop(out)=> {},
//         }

//         Context {
//             accept: c.accept,
//             samples: c.samples,
//             substitutions: c.substitutions.clone(),
//             weightmap: c.weightmap.clone(),
//             ssubstitutions: c.ssubstitutions.clone(),
//         }
//     }
// }
// impl Default for Context {
//     fn default() -> Self {
//         Context {
//             accept: BddPtr::PtrTrue,
//             samples: BddPtr::PtrTrue,
//             substitutions: Default::default(),
//             weightmap: Default::default(),
//             ssubstitutions: Default::default(),
//         }
//     }
// }
