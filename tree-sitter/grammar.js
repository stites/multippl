module.exports = grammar({
  name: 'BddIS',

  rules: {
    VAR: $ => /[a-zA-Z][a-zA-Z0-9]*/,
    PARAM: $ => /[0-9]+(?\.[0-9]*)/,

    // source_file: $ => seq(repeat($._function), $._expr)
    source_file: $ => $._expr,

    _type:  $ => choice('Bool', seq('(', $._type, ',', $._type, ')')),
    _value: $ => choice('true', 'false', seq('(', $._value, ',', $._value, ')')),
    _anf:   $ => choice($.VAR, $._value),
    _expr:  $ => choice(
      $._anf,
      seq($._fst, $._anf),
      seq($._snd, $._anf),
      seq('(', $._anf, ',', $._anf, ')'),
      seq('let', $.VAR, '=', $._expr, 'in', $._expr),
      seq('if', $._anf, 'then', $._expr, 'else', $._expr),
      // seq($._function_name, '(', $._anf ,')'),
      seq('flip', $.PARAM),
      seq('observe', $._anf),
      seq('sample', $._expr),
    ),
    // _func: $ => seq('fun', $._function_name, '(', $.VAR, ')', ':', $._type, '{', $._expr, '}'),
  }
});

