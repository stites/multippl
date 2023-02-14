module.exports = grammar({
  name: 'weight4me',
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
      $.fst,
      $.snd,
      $.prod,
    ),
    fst: $ => seq('fst', $.anf),
    snd: $ => seq('snd', $.anf),
    prod: $ => seq('(', $.anf, ',', $.anf, ')'),
    let_binding: $ => choice(
      seq('let', $.identifier, '=', $._expr, 'in', $._expr),
      prec.left(1, seq('let', $.identifier, '=', $._expr, $._expr)),
    ),
    ite_binding: $ => seq('if', $.anf, 'then', $._expr, 'else', $._expr),
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

    anf: $ => choice($.identifier, $._value, prec.left(1, seq($.anf, $.bool_biop, $.anf)), prec.left(2, seq($.bool_unop, $.anf))),

    identifier: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
    // _func: $ => seq('fun', $._function_name, '(', $.VAR, ')', ':', $._type, '{', $._expr, '}'),
  }
});

