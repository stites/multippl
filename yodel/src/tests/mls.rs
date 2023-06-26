use super::*;
use crate::*;

#[test]
#[traced_test]
fn mls_0() {
    let p = program!(~lets![
       ~ "x" <- bern!(1/3);
       ~ "y" <- var!(~ "x") ;
       ~~ var!(~ "x")
    ]);
    // check_approx1("let binding", 1.0 / 3.0, &p, 15000); // done!
    println!("program1 pass");
    let p = program!(~lets![
       ~ "x" <- bern!(1/3);
       ~ "y" <- bern!(1/3);
       ~~ ite!(~ (b!(~@anf true) ) ? ( var!(~ "x") ) : ( var!(~ "y") ) )
    ]);
    // check_approx1("let binding", 1.0 / 3.0, &p, 15000);

    println!("program2 pass");
    let p = program!(~lets![
       ~ "x" <- exact!(flip!(1/3)) ;
       ~ "y" <- var!(~ "x") ;
       ~~ var!(~ "x")
    ]);
    check_approx1("let binding", 1.0 / 3.0, &p, 10);
    println!("program3 pass");
}
