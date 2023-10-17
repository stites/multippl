use rand::distributions::{Bernoulli, Distribution};

#[no_mangle]
pub extern "C" fn bern(p: f64) -> bool {
    println!("in rust");
    let d = Bernoulli::new(p).unwrap();
    d.sample(&mut rand::thread_rng())
}
