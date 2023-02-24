fn leaf_variable(bdd: BddPtr) -> Option<VarLabel> {
    let n = bdd.into_node_safe()?;
    if (n.low == BddPtr::PtrTrue && n.high == BddPtr::PtrFalse)
        || (n.low == BddPtr::PtrFalse && n.high == BddPtr::PtrTrue)
    {
        Some(n.var)
    } else {
        None
    }
}

fn variables(bdd: BddPtr, order: VarOrder) -> Vec<VarLabel> {
    Fold::new(
        &mut |vs: Vec<Option<VarLabel>>, bdd| {
            let mut vs = vs.clone();
            vs.push(bdd.node.var_safe());
            vs
        },
        vec![],
        &|ret, lo_hi| match lo_hi {
            None => ret,
            Some((lo, hi)) => {
                let mut v = ret.clone();
                v.extend(lo);
                v.extend(hi);
                v
            }
        },
    )
    .mut_fold(&bdd)
    .into_iter()
    .filter_map(|x| x)
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tracing_test::*;

    #[test]
    #[ignore]
    #[traced_test]
    fn test_bdd_manager() {
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(6);
        // x variable point to the x bdd ptr
        let x_var = mgr.var(VarLabel::new(0), true); // (0, F, T)
        let x_ptr = mgr.var(VarLabel::new(1), true); // (1, F, T)

        // x0 variable point to the x0 bdd ptr
        let x0_var = mgr.var(VarLabel::new(3), true); // (3, F, T)
        let x0_ptr = mgr.var(VarLabel::new(4), true); // (4, F, T)

        // y_ptr is a bdd pointer to a disjunction: y = x || x0
        let y_ptr = mgr.or(x0_ptr, x_ptr); // (1, (4, F, T), T)

        // .. and y is a variable pointing to this y_ptr BDD
        let y_var = mgr.var(VarLabel::new(2), true); // (2, F, T)

        // observe on the variables x and y: x || y
        let observation = mgr.or(x_var, y_var); // (0, (2, F, T), T)

        // now the goal is to substitute out x and y variables for their BDD pointers:
        let x = mgr.compose(observation, VarLabel::new(2), y_ptr);
        // should result in: (0, (2, F, T), T) -> (0, (1, (4, F, T), T), T)
        // but it results in (1, (4, F, T), T)
        println!("{}", x.print_bdd());
        // composing again should be idempotent: there is no VarLabel(2) in the BDD:
        let x = mgr.compose(observation, VarLabel::new(2), y_ptr);
        // but it results in !(1, (4, F, T), T)
        println!("{}", x.print_bdd());

        // observe bug 1: apply_substitutions becomes a negation
        // Comp. dist [(0, (2, F, T), T)]
        // substitutions:
        // - UniqueId(2)@(#2L2Ctrue): [(1, (4, F, T), T)]
        // - UniqueId(0)@(#0L0Ctrue): [(1, F, T)]
        // - UniqueId(3)@(#3L3Ctrue): [(4, F, T)]
        //
        // [substituting] notleaf: apply sub: UniqueId(2)@(1, (4, F, T), T)
        // [substituting] notleaf: transform: (0, (2, F, T), T) => (1, (4, F, T), T)
        // [substituting] notleaf: apply sub: UniqueId(2)@(1, (4, F, T), T)
        // [substituting] notleaf: transform: (1, (4, F, T), T) => !(1, T, (4, F, T))
        // [substituting] final: (0, (2, F, T), T) -> !(1, T, (4, F, T))
    }
}
