module.exports = grammar({
  name: 'yodel',
  word: $ => $.identifier,
  rules: {

    // source_file: $ => seq(repeat($._function), $._expr)
    source_file: $ => $.program,

    program: $ => choice(
      seq("exact", '{', $._eexpr, '}'),
      seq("sample", '{', $._sexpr, '}'),
      seq($.sfun, $.program),
      seq($.efun, $.program),
    ),

    sfun: $ => choice(
      seq('fn', $.identifier, '@s', '(', $.identifier, ')', '{', $._sexpr, '}'),
      seq('fn', $.identifier, '@s', '(', repeat(seq($.identifier, ',')), $.identifier,')', '{', $._sexpr, '}'),
    ),
    efun: $ => choice(
      seq('fn', $.identifier, '@e', '(', $.identifier, ')', '{', $._eexpr, '}'),
      seq('fn', $.identifier, '@e', '(', repeat(seq($.identifier, ',')), $.identifier,')', '{', $._eexpr, '}'),
    ),
    _eexpr: $ => choice(
      $.elet,
      $.eite,
      $.eflip,
      $.ediscrete,
      $.eobserve,
      $.esample,
      // seq($._function_name, '(', $.anf ,')'),
      $.eann,
      $.efst,
      $.esnd,
      $.eprj,
      $.eprod,
      $.app,
      $.eanf,
      seq('(', $._eexpr, ')')
    ),
    ety: $ => choice($.tyBool, $.tyFloat, $.tyInt, $.tyProd), // includes sugar of int
    tyBool: $ => 'Bool',
    tyFloat: $ => 'Float',

    app: $ => choice(
      seq($.identifier, '(', $.sanf, ')'),
      seq($.identifier, '(', repeat(seq($.sanf,  ',')), $.sanf, ')'),
    ),

    tyProd: $ => choice(
      seq('(', $.ety, ',', $.ety, ')'),
      seq('(', $.ety, ',', repeat(seq($.ety, ',')), $.ety, ')'),
    ),

    efst: $ => seq('fst', $.eanf),
    esnd: $ => seq('snd', $.eanf),

    eprj: $ => choice(
        seq('prj',  $.index, $.eanf ),
        seq('prj', '(', $.index, ',', $.eanf, ')'),
    ),
    index: $ => /\d+/,

    eprod: $ => choice(
      seq('(', $.eanf, ',', $.eanf, ')'),
      seq('(', $.eanf, ',', repeat(seq($.eanf, ',')), $.eanf, ')'),
    ),


    elet: $ => choice(
      seq('let', $.identifier, '=', $._eexpr, 'in', $._eexpr),
      seq('let', $.identifier, ':', $.ety, '=', $._eexpr, 'in', $._eexpr),
      // seq('let', $.identifier, ':', $.ety, '=', $._eexpr,  ';', $._eexpr), // TODO
      // prec.left(10, seq('let', $.identifier, ':', $.ety, '=', $._eexpr, $._eexpr)), // TODO
    ),
    eite: $ =>
      prec.left(2, seq('if', $.eanf, 'then', $._eexpr, 'else', $._eexpr)),

    eflip: $ => seq('flip', $.eanf),
    ediscrete: $ => seq('discrete', '(', repeat(seq($.eanf, ",")), $.eanf,  ')'),
    eobserve: $ => choice(
      seq('observe', $.eanf),
      seq('observe', '(', $.eanf, ')'),
    ),
    esample: $ => choice(
      seq('sample', '(', $._sexpr, ')' ),
      seq('sample', '{', $._sexpr, '}' ),
    ),

    bool: $ => choice('true', 'false'),
    bool_biop: $ => choice('||', '&&'),

    bool_unop: $ => '!',

    float: $ => /\d+(?:\.\d*|)/, // 0.3  0.3. 3. 0.

    // exact floats allow for int-looking floats
    efloat: $ => /\d+(?:\.\d*|)/, // 0.3  0.3. 3 0.

    int: $ => /\d+/,
    numeric_op: $ => choice('*', '/', '+', '-'),

    _evalue: $ => choice(
      $.bool,
      $.float,
      $.int,
      prec(10, seq('(', $._evalue, ',', $._evalue, ')')),
      prec(10, seq('(', $._evalue, ',', repeat(seq($._evalue, ',')), $._evalue, ')')),
    ),

    eann: $ => prec.right(5, seq($._eexpr, ':', $.ety)),

    eanf: $ => choice(
      $.identifier,
      $._evalue,
      prec.left(2, seq($.eanf, $.numeric_op, $.eanf)),
      prec.left(2, seq('(', $.eanf, $.numeric_op, $.eanf, ')')),
      prec.left(3, seq($.eanf, $.bool_biop, $.eanf)),
      prec.left(3, seq('(', $.eanf, $.bool_biop, $.eanf, ')')),
      prec.left(5, seq($.bool_unop, $.eanf)),
    ),

    identifier: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
    // _func: $ => seq('fun', $._function_name, '(', $.VAR, ')', ':', $._type, '{', $._eexpr, '}'),

    _sexpr: $ => choice(
      // $.sbinding_section,
      $.slet,
      $.smap,
      $.sfold,
      $.app,
      $.slam,
      $.sobs,
      $.sexact,
      $.ssample,
      $.sanf,

      seq('(', $._sexpr, ')')
    ),

    // sbinding_section: $ => seq("do", repeat(choice($.slet, $.sseq_first)), $._sexpr),
    slet: $ => seq($.identifier, "<-", $._sexpr, ';', $._sexpr),
    sseq: $ => seq($._sexpr, ';', $._sexpr),
    // sseq_first: $ => seq($._sexpr, ';',),
    smap: $ => seq('map', '(', $.identifier, '->', $._sexpr, ')', $.sanf),
    ssample: $ => seq('~', $.sanf, ''),
    sfold: $ => seq('fold', '(', $.sanf, '(', $.identifier, $.identifier, '->', $._sexpr, ')', $.sanf),

    slam: $ => choice(
      seq('(\\', $.identifier, '->', $._sexpr, ')'),
      seq('(\\', repeat(seq($.identifier, ',')), $.identifier, '->', $._sexpr, ')'),
    ),
    sobs: $ => seq('observe', '(', $.sanf, ',', $.sanf, ')'),
    sexact: $ => choice(
      seq('exact', '(', $._eexpr, ')' ),
      seq('exact', '{', $._eexpr, '}' ),
    ),


    sanf: $ => choice(
      $.identifier,
      prec.left(6, seq($.sanf, $.numeric_op, $.sanf)),
      prec.left(6, seq('(', $.sanf, $.numeric_op, $.sanf, ')')),
      prec.left(3, seq($.sanf, $.bool_biop, $.sanf)),
      prec.left(3, seq('(', $.sanf, $.bool_biop, $.sanf, ')')),
      prec.left(5, seq($.bool_unop, $.sanf)),
      $._svalue,
    ),

    _svalue: $ => choice(
      $.bool,
      $.float,
      $.int,
      $.svec,

      $.sbern,
      $.spoisson,
      $.suniform,
      $.snormal,
      $.sbeta,
      $.sdiscrete,
      $.sdirichlet,
    ),
    svec: $ => choice(
      seq('[', $.sanf, ']'),
      seq('[', repeat(seq($.sanf, ',')), $.sanf, ']'),
    ),
    sbern: $ => seq('bern', '(', $.sanf, ')'),
    spoisson: $ => seq('poisson', '(', $.sanf, ')'),
    suniform: $ => seq('uniform', '(', $.sanf, ',', $.sanf, ')'),
    snormal: $ => seq('normal', '(', $.sanf, ',', $.sanf, ')'),
    sbeta: $ => seq('beta', '(', $.sanf, ',', $.sanf, ')'),
    sdiscrete: $ => choice(
      seq('discrete', '(', $.sanf, ')'),
      seq('discrete', '(', repeat(seq($.sanf, ',')), $.sanf, ')'),
    ),
    sdirichlet: $ => choice(
      seq('dirichlet', '(', $.sanf, ')'),
      seq('dirichlet', '(', repeat(seq($.sanf, ',')), $.sanf, ')'),
    ),

    sty: $ => choice($.tyBool, $.tyFloat, $.tyInt, $.tyVec, $.tyDistribution),
    tyInt: $ => 'Int',
    tyDistribution: $ => 'Dist',
    tyVec: $ => seq('[', $.sty, ']'),

  }
});

