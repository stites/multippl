module.exports = grammar({
  name: 'yodel',
  word: $ => $.identifier,
  rules: {
    source_file: $ => $.program,
    program: $ => choice(
      seq($.comment, $.program),
      seq("exact", '{', $.eexpr, '}', repeat($.comment),),
      seq("sample", '{', $.sexpr, '}', repeat($.comment),),
      seq($.sfun, $.program),
      seq($.efun, $.program),
    ),
    sarg: $ => seq( $.identifier, ':', $.sty ),
    sargs: $ => choice(
      seq('(', ')'),
      seq('(', $.sarg, ')'),
      seq('(', repeat(seq($.sarg, ',')), $.sarg, optional(','), ')')
    ),
    earg: $ => seq($.identifier, ':', $.ety),
    eargs: $ => choice(
      seq('(', ')'),
      seq('(', $.earg, ')'),
      seq('(', repeat(seq($.earg, ',')), $.earg, optional(','), ')')
    ),

    sfun: $ => seq('sample', 'fn', field("name", $.identifier), $.sargs, '->', $.sty, '{', $.sexpr, '}'),
    efun: $ => seq('exact', 'fn', field("name", $.identifier), $.eargs, '->', $.ety, '{', $.eexpr, '}'),

    eexpr: $ => choice(
      seq($.comment, $.eexpr),
      $.elet,
      $.eite,
      $.eflip,
      $.ediscrete,
      $.eiterate,
      $.eobserve,
      $.esample,
      // seq($._function_name, '(', $.anf ,')'),
      $.eann,
      $.efst,
      $.esnd,
//      $.eprj,
//      $.eprod,
      $.eapp,
      prec(6, $.eanf),
      prec(5, seq('(', $.eexpr, ')'))
    ),
    ety: $ => choice($.tyBool, $.tyFloat, $.tyInt, $.etyProd), // includes sugar of int
    tyBool: $ => 'Bool',
    tyFloat: $ => 'Float',

    eapp: $ => choice(
      seq(field("name", $.identifier), '(', ')'),
      seq(field("name", $.identifier), '(', $.eanf, ')'),
      seq(field("name", $.identifier), '(', repeat(seq($.eanf,  ',')), $.eanf, optional(','), ')'),
     ),

    etyProd: $ => choice(
      seq('(', $.ety, ',', $.ety, ')'),
      seq('(', $.ety, ',', repeat(seq($.ety, ',')), $.ety, optional(','), ')'),
    ),

    efst: $ => seq('fst', $.eanf),
    esnd: $ => seq('snd', $.eanf),

    // sanfprj: $ => seq($.identifier, '[', $.sanf, ']'), // vector access
    eanfprj: $ => choice(
        seq($.identifier, '[', $.eanf, ']'),
        // seq('prj', '(', $.eanf, ',', $.eanf, ')'),
    ),

    // eprod: $ => choice(
    //   seq('(', $.eanf, ',', $.eanf, ')'),
    //   seq('(', $.eanf, ',', repeat(seq($.eanf, ',')), $.eanf, ')'),
    // ),

    // _eanfprod_item: $ => choice(
    //   $.evalue, $.eanf,
    // ),


    eanfprod: $ => choice(
      seq('(', $.evalue, ',', $.eanf, optional(','), ')'),
      seq('(', $.eanf, ',', $.eanf, optional(','), ')'),
      // seq('(', $.eanf, ',', repeat(seq($.eanf, ',')), $.evalue, ')'),
      seq('(', $.eanf, ',', repeat(seq($.eanf, ',')), $.eanf, optional(','), ')'),
    ),

    elet: $ => choice(
      seq('let', field('var', $.identifier), '=', $.eexpr, 'in', $.eexpr),
      seq('let', field('var', $.identifier), ':', $.ety, '=', $.eexpr, 'in', $.eexpr),
      // seq('let', $.identifier, ':', $.ety, '=', $.eexpr,  ';', $.eexpr), // TODO
      // prec.left(10, seq('let', $.identifier, ':', $.ety, '=', $.eexpr, $.eexpr)), // TODO
    ),
    eite: $ =>
      prec.left(2, seq('if', $.eanf, 'then', $.eexpr, 'else', $.eexpr)),

    eflip: $ => seq('flip', $.eanf),
    ediscrete: $ => seq('discrete', '(', repeat(seq($.eanf, ',')), $.eanf,  ')'),
    eiterate: $ => seq('iterate', '(', $.identifier, ',',$.eanf, ',', $.eanf,  ')'),
    eobserve: $ => choice(
      seq('observe', $.eanf, 'in', $.eexpr),
    ),
    esample: $ => choice(
      seq('sample', '(', $.sexpr, ')' ),
      seq('sample', '{', $.sexpr, '}' ),
    ),

    bool: $ => choice('true', 'false'),
    bool_biop: $ => choice('||', '&&'),

    bool_unop: $ => '!',

    float: $ => /-?\d+\.(?:\d*|)/, // 0.3  0.3. 3. 0.
    // I think I need to disallow whitespace before continuing with this
    // https://gist.github.com/Aerijo/df27228d70c633e088b0591b8857eeef
    comment: $ => token(seq('//', /.*/)), // like this
    // comment: $ => token(seq('%', /.*/)),
    // comment: $ => token(seq("//", /[^\n]*/)), // like this

    int: $ => /\d+/,
    numeric_op: $ => choice('*', '/', '+', '-', '^'),
    compare_op: $ => choice('==', '<', '<=', '>', '>='),

    evalue: $ => choice(
      // $.evalueprod,
      $.bool,
      $.int,
      $.float,
    ),

    eann: $ => prec.right(5, seq($.eexpr, ':', $.ety)),

    eanfbinop: $ => choice(
      prec.left(2, seq($.eanf, $.numeric_op, $.eanf)),
      prec.left(3, seq($.eanf, $.bool_biop, $.eanf)),
      prec.left(4, seq($.eanf, $.compare_op, $.eanf)),
    ),
    eanfunop: $ => choice(
      prec.left(5, seq($.bool_unop, $.eanf)),
    ),
    eanf: $ => choice(
      prec(10, $.eanfprod),
      $.identifier,
      prec(6, $.evalue),
      $.eanfprj,
      $.eanfbinop,
      $.eanfunop,
      seq('(', $.eanf, ')'),
    ),

    identifier: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
    // _func: $ => seq('fun', $._function_name, '(', $.VAR, ')', ':', $._type, '{', $.eexpr, '}'),

    sexpr: $ => choice(
      seq($.comment, $.sexpr),
      // $.comment,
      // $.sbinding_section,
      $.smap,
      $.swhile,
      $.sfold,
      $.slam,
      $.sobserve,
      $.sexact,
      $.site,
      $.ssample,
      $.slet,
      $.sletsample,
      $.sseq,
      $.sanf,
      $.sapp,
      $.sann,

      seq('(', $.sexpr, ')')
    ),

    site: $ => choice(
      seq('if', $.sanf, '{', $.sexpr, '}', 'else', '{', $.sexpr, '}'),
      seq('if', $.sanf, '{', $.sexpr, '}', 'else', $.site ),
      seq('if', '(', $.sanf, ')', '{', $.sexpr, '}', 'else', '{', $.sexpr, '}'),
      seq('if', '(', $.sanf, ')', '{', $.sexpr, '}', 'else', $.site ),
      seq('if', $.sanf, 'then', $.sexpr, 'else', $.sexpr),
      seq('if', '(', $.sanf, ')', 'then', $.sexpr, 'else', $.sexpr),
    ),

    sapp: $ => choice(
      seq(field("name", $.identifier), '(', ')'),
      seq(field("name", $.identifier), '(', $.sanf, optional(','), ')'),
      seq(field("name", $.identifier), '(', repeat(seq($.sanf,  ',')), $.sanf, optional(','), ')'),
    ),



    // sbinding_section: $ => seq("do", repeat(choice($.slet, $.sseq_first)), $.sexpr),
    slet: $ => choice(
      // seq($.identifier, ':', $.sty, "<-", $.sexpr, ';', $.sexpr),
      seq(field("var", $.identifier), "<-", $.sexpr, ';', $.sexpr),
    ),
    sletsample: $ => choice(
      seq(field("var", $.identifier), "~", $.sexpr, ';', $.sexpr),
    ),
    sseq: $ => prec.left(-100, seq($.sexpr, ';', $.sexpr)),
    // sseq_first: $ => seq($.sexpr, ';',),
    smap: $ => seq('map', '(', $.identifier, '->', $.sexpr, ')', $.sanf),
    swhile: $ => choice(
      // seq('while', '(',  $.sanf, ')', '{', $.sexpr, '}'),
      seq('while', $.sanf, '{', $.sexpr, '}'),
    ),

    ssample: $ => seq('~', $.sexpr),
    sfold: $ => seq('fold', '(', $.sanf, '(', $.identifier, $.identifier, '->', $.sexpr, ')', $.sanf),

    slam: $ => choice(
      seq('(\\', $.identifier, '->', $.sexpr, ')'),
      seq('(\\', repeat(seq($.identifier, ',')), $.identifier, '->', $.sexpr, ')'),
    ),
    sobserve: $ => seq('observe', $.sanf, 'from', $.sanf, ';', $.sexpr),
    sexact: $ => choice(
      seq('exact', '(', $.eexpr, ')' ),
      seq('exact', '{', $.eexpr, '}' ),
    ),

    sanf: $ => choice(
      $.identifier,
      prec(6, $.svalue), // prefer values over anf

      $.sanfprj,
      $.sanfvec,
      $.sanfprod,

      $.sanfbern,
      $.sanfpoisson,
      $.sanfuniform,
      $.sanfnormal,
      $.sanfbeta,
      $.sanfdiscrete,
      $.sanfdirichlet,

      $.sanfbinop,
      $.sanfunop,

      prec(7, seq('(', $.sanf, ')')),
    ),

    sanfbinop: $ => choice(
      prec.left(6, seq($.sanf, $.numeric_op, $.sanf)),
      prec.left(6, seq('(', $.sanf, $.numeric_op, $.sanf, ')')),
      prec.left(3, seq($.sanf, $.bool_biop, $.sanf)),
      prec.left(3, seq('(', $.sanf, $.bool_biop, $.sanf, ')')),
      prec.left(4, seq($.sanf, $.compare_op, $.sanf)),
      prec.left(4, seq('(', $.sanf, $.compare_op, $.sanf, ')')),
    ),
    sanfunop: $ => choice(
      prec.left(5, seq($.bool_unop, $.sanf)),
    ),
    sann: $ => prec.right(4, seq($.sexpr, ':', $.sty)),
    svalue: $ => choice(
      $.bool,
      $.float,
      $.int,
      $.svec,
      $.sprod,

      $.sbern,
      $.spoisson,
      $.suniform,
      $.snormal,
      $.sbeta,
      $.sdiscrete,
      $.sdirichlet,
    ),
    sanfprj: $ => seq($.identifier, '[', $.sanf, ']'), // vector access
    sanfvec: $ => choice(
      seq('[', $.sanf, ']'),
      seq('[', repeat(seq($.sanf, ',')), $.sanf, optional(','), ']'),
    ),
    svec: $ => choice(
      seq('[', $.svalue, ']'),
      seq('[', repeat(seq($.svalue, ',')), $.svalue, optional(','), ']'),
    ),
    sanfprod: $ => choice(
      seq('(', $.sanf, ',', $.sanf, ')'),
      seq('(', $.sanf, ',', repeat(seq($.sanf, ',')), $.sanf, optional(','), ')'),
    ),
    sprod: $ => choice(
      seq('(', $.svalue, ',', $.svalue, ')'),
      seq('(', $.svalue, ',', repeat(seq($.svalue, ',')), $.svalue, optional(','), ')'),
    ),



    // // [x] && x [ 0 ]
    // AnfPrj(Box<Anf<X, Val>>, Box<Anf<X, Val>>),

    /// anf forms of distributions
    sanfbern: $ => seq('bern', '(', $.sanf, ')'),
    sanfpoisson: $ => seq('poisson', '(', $.sanf, ')'),
    sanfuniform: $ => seq('uniform', '(', $.sanf, ',', $.sanf, ')'),
    sanfnormal: $ => seq('normal', '(', $.sanf, ',', $.sanf, ')'),
    sanfbeta: $ => seq('beta', '(', $.sanf, ',', $.sanf, ')'),
    sanfdiscrete: $ => choice(
      seq('discrete', '(', $.sanf, ')'),
      seq('discrete', '(', repeat(seq($.sanf, ',')), $.sanf, optional(','), ')'),
    ),
    sanfdirichlet: $ => choice(
      seq('dirichlet', '(', $.sanf, ')'),
      seq('dirichlet', '(', repeat(seq($.sanf, ',')), $.sanf, optional(','), ')'),
    ),

    /// value forms of distributions
    sbern: $ => choice(
      seq('bern', $.svalue),
      seq('bern', '(', $.svalue, ')'),
    ),
    spoisson: $ => choice(
      seq('poisson', $.svalue),
      seq('poisson', '(', $.svalue, ')'),
    ),
    suniform: $ => choice(
      //seq('uniform', $.svalue, $.svalue),
      seq('uniform', '(', $.svalue, ',', $.svalue, ')'),
    ),
    snormal: $ => choice(
      //seq('normal', $.svalue, $.svalue),
      seq('normal', '(', $.svalue, ',', $.svalue, ')'),
    ),
    sbeta: $ => choice(
      //seq('beta', $.svalue, $.svalue),
      seq('beta', '(', $.svalue, ',', $.svalue, ')'),
    ),
    sdiscrete: $ => choice(
      seq('discrete', '(', $.svalue, ')'),
      seq('discrete', '(', repeat(seq($.svalue, ',')), $.svalue, ')'),
    ),
    sdirichlet: $ => choice(
      seq('dirichlet', '(', $.svalue, ')'),
      seq('dirichlet', '(', repeat(seq($.svalue, ',')), $.svalue, ')'),
    ),

    sty: $ => choice($.tyBool, $.tyFloat, $.tyInt, $.tyVec, $.tyDistribution, $.styProd),
    tyInt: $ => 'Int',
    tyDistribution: $ => 'Dist',
    tyVec: $ => seq('[', $.sty, ']'),
    styProd: $ => choice(
      seq('(', $.sty, ',', $.sty, ')'),
      seq('(', $.sty, ',', repeat(seq($.sty, ',')), $.sty, ')'),
    ),


  }
});

