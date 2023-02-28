module.exports = grammar({
  name: 'formula',

  word: $ => $.var,
  rules: {
    source_file: $ => $._expr,
    _expr: $ => choice(
      $.var,
      $.and,
      $.or,
      $.neg,
      seq('(', $._expr, ')')
    ),
    neg: $ => choice(
      prec.left(1, seq('!', $._expr)),
      prec.left(1, seq('¬', $._expr)),
    ),
    and: $ => choice(
      prec.left(2, seq($._expr, '&', $._expr)),
      prec.left(2, seq($._expr, '&&', $._expr)),
      prec.left(2, seq($._expr, '∧', $._expr)),
    ),
    or: $ => choice(
      prec.left(3, seq($._expr, '|', $._expr)),
      prec.left(3, seq($._expr, '||', $._expr)),
      prec.left(3, seq($._expr, '∨', $._expr)),
    ),
    var: $ => /[a-zA-Z_][_a-zA-Z0-9]*/,
  }
});
