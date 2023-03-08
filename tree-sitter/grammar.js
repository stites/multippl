module.exports = grammar({
  name: 'yodel',
  word: $ => $.identifier,
  rules: {

    // source_file: $ => seq(repeat($._function), $._expr)
    source_file: $ => $._expr,

    // _type:  $ => choice('Bool', seq('(', $._type, ',', $._type, ')')),
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
    tProd: $ => seq('(', $.ty, ',', $.ty, ')'),

    fst: $ => seq('fst', $.anf),
    snd: $ => seq('snd', $.anf),
    prod: $ => seq('(', $.anf, ',', $.anf, ')'),

    let_binding: $ => choice(
      prec.left(1, seq('let', $.identifier, ':', $.ty, '=', $._expr, 'in', $._expr)),
      prec.left(1, seq('let', $.identifier, ':', $.ty, '=', $._expr,       $._expr)),
      prec.left(1, seq('let', $.identifier, ':', $.ty, '=', $._expr,  ';', $._expr)),
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

    _value: $ => choice($.bool), // , seq('(', $._value, ',', $._value, ')')),

    ann: $ => prec.right(5, seq($._expr, ':', $.ty)),
    anf: $ => choice(
      $._value,
      prec.left(1, seq($.anf, $.bool_biop, $.anf)),
      prec.left(2, seq($.bool_unop, $.anf)),
    ),

    identifier: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
    // _func: $ => seq('fun', $._function_name, '(', $.VAR, ')', ':', $._type, '{', $._expr, '}'),
  }
});

