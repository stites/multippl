module.exports = grammar({
  name: 'yodel',
  word: $ => $.identifier,
  rules: {

    // source_file: $ => seq(repeat($._function), $._expr)
    source_file: $ => $._expr,

    _expr: $ => choice(
      $.let_binding,
      $.ite_binding,
      $.flip,
      $.observe,
      $.sample,
      // seq($._function_name, '(', $.anf ,')'),
      $.anf,
      $.ann,
      $.fst,
      $.snd,
      $.prod,
      seq('(', $._expr, ')')
    ),
    ty: $ => choice($.tBool, $.tProd),
    tBool: $ => 'Bool',

    tProd: $ => choice(
      seq('(', $.ty, ',', $.ty, ')'),
      seq('(', $.ty, ',', repeat(seq($.ty, ',')), $.ty, ')'),
    ),

    fst: $ => seq('fst', $.anf),
    snd: $ => seq('snd', $.anf),
    prj: $ => seq('prj', $.index, $.anf),
    index: $ => /\d+/,

    prod: $ => choice(
      seq('(', $.anf, ',', $.anf, ')'),
      seq('(', $.anf, ',', repeat(seq($.anf, ',')), $.anf, ')'),
    ),


    let_binding: $ => choice(
      seq('let', $.identifier, '=', $._expr, 'in', $._expr),
      seq('let', $.identifier, ':', $.ty, '=', $._expr, 'in', $._expr),
      // seq('let', $.identifier, ':', $.ty, '=', $._expr,  ';', $._expr), // TODO
      // prec.left(10, seq('let', $.identifier, ':', $.ty, '=', $._expr, $._expr)), // TODO
    ),
    ite_binding: $ =>
      prec.left(2, seq('if', $.anf, 'then', $._expr, 'else', $._expr)),
    flip: $ => seq('flip', $._float),
    observe: $ => choice(
      seq('observe', $.anf),
      seq('observe', '(', $.anf, ')'),
    ),
    sample: $ => seq('sample', '(', $._expr, ')' ),

    bool: $ => choice('true', 'false'),
    bool_biop: $ => choice('||', '&&'),

    bool_unop: $ => '!',

    _float: $ => choice(
      $.float,
      seq($.float, $.float_op, $.float),
      seq('(', $._float, ')'),
    ),
    float: $ => /\d+(?:\.\d*|)/, // 0.3  0.3. 3 0.
    float_op: $ => choice('*', '/', '+', '-'),

    _value: $ => choice(
      $.bool,
      prec(10, seq('(', $._value, ',', $._value, ')')),
      prec(10, seq('(', $._value, ',', repeat(seq($._value, ',')), $._value, ')')),
    ),

    ann: $ => prec.right(5, seq($._expr, ':', $.ty)),
    anf: $ => choice(
      $.identifier,
      $._value,
      prec.left(3, seq($.anf, $.bool_biop, $.anf)),
      prec.left(5, seq($.bool_unop, $.anf)),
    ),

    identifier: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
    // _func: $ => seq('fun', $._function_name, '(', $.VAR, ')', ':', $._type, '{', $._expr, '}'),
  }
});

