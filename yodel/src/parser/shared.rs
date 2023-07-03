use tree_sitter::{Node, Parser, Tree, TreeCursor};

pub fn tree_parser(code: String) -> Option<Tree> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_yodel::language())
        .expect("Error loading yodel grammar");
    parser.parse(code, None)
}
// fn parse_program(src: &[u8], c: &mut TreeCursor, n: &Node) -> ProgramSugar {
//     ProgramSugar::Exact(parse_expr(src, c, n))
// }
// pub fn parse(code: &str) -> Option<ProgramSugar> {
//     let tree = tree_parser(code.to_string())?;
//     let expr = parse_tree(code.as_bytes(), tree);
//     Some(expr)
// }
pub fn parse_str(src: &[u8], n: &Node) -> String {
    let utf8 = n.clone().utf8_text(src).unwrap();
    String::from_utf8(utf8.into()).unwrap()
}

pub fn parse_vec<X>(
    src: &[u8],
    c: &mut TreeCursor,
    n: Node,
    parse_el: impl Fn(&[u8], &mut TreeCursor, &Node) -> X,
) -> Vec<X> {
    let mut xs = vec![];
    let mut _c = c.clone();
    let mut cs = n.named_children(&mut _c);

    for _ in 0..n.named_child_count() {
        let x = cs.next().unwrap();
        let x = parse_el(src, c, &x);
        xs.push(x);
    }
    xs
}
pub fn parse_float_h(src: &[u8], c: &mut TreeCursor, n: Node) -> f64 {
    let utf8 = n.utf8_text(src).unwrap();
    let fstr = String::from_utf8(utf8.into()).unwrap();
    fstr.parse::<f64>().unwrap()
}

pub fn parse_float(src: &[u8], c: &mut TreeCursor, n: Node) -> f64 {
    match n.named_child_count() {
        0 => parse_float_h(src, c, n),
        1 => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let fnode = cs.next().unwrap();
            parse_float_h(src, c, fnode)
        }
        2 => panic!("impossible"),
        3 => {
            // division operation
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let l = cs.next().unwrap();
            let l = parse_float(src, c, l);

            let op = cs.next().unwrap();
            let utf8 = op.utf8_text(src).unwrap();
            let op = String::from_utf8(utf8.into()).unwrap();
            assert!(
                op == "/".to_string(),
                "invalid program found!\nsexp: {}",
                n.to_sexp()
            );

            let r = cs.next().unwrap();
            let r = parse_float(src, c, r);

            l / r
        }
        _ => panic!("unexpected"),
    }
}
