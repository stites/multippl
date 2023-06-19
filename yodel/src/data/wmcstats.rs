use std::cmp;

#[derive(Debug, Clone, Copy)]
pub struct WmcStats {
    pub dist: usize,
    pub accept: usize,
    pub dist_accept: usize,
    pub mgr_recursive_calls: usize,
}
impl WmcStats {
    pub fn largest_of(&self, o: &WmcStats) -> WmcStats {
        WmcStats {
            dist: cmp::max(self.dist, o.dist),
            accept: cmp::max(self.accept, o.accept),
            dist_accept: cmp::max(self.dist_accept, o.dist_accept),
            mgr_recursive_calls: cmp::max(self.mgr_recursive_calls, o.mgr_recursive_calls),
        }
    }
    pub fn empty() -> WmcStats {
        WmcStats {
            dist: 0,
            accept: 0,
            dist_accept: 0,
            mgr_recursive_calls: 0,
        }
    }
}
