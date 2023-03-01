module.exports = grammar({
  name: 'formula',

  word: $ => $.var,
  rules: {
    source_file: $ => $._expr,
    _expr: $ => choice(
      $.true,
      $.false,
      $.var,
      $.neg,
      $.and,
      $.or,
      seq('(', $._expr, ')')
    ),
    false: $ => choice(
      'false',
      'F',
      'False',
    ),
    true: $ => choice(
      'true',
      'T',
      'True',
    ),
    neg: $ => choice(
      prec.left(5, seq('!', $._expr)),
      prec.left(5, seq('¬', $._expr)),
    ),
    and: $ => choice(
      prec.left(4, seq($._expr, '&', $._expr)),
      prec.left(4, seq($._expr, '&&', $._expr)),
      prec.left(4, seq($._expr, '∧', $._expr)),
    ),
    or: $ => choice(
      prec.left(3, seq($._expr, '|', $._expr)),
      prec.left(3, seq($._expr, '||', $._expr)),
      prec.left(3, seq($._expr, '∨', $._expr)),
    ),
    var: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
  }
});
