module.exports = grammar({
  name: 'formula',

  word: $ => $.var,
  rules: {
    source_file: $ => $._expr,
    _expr: $ => choice(
      $.var,
      $.neg,
      $.and,
      $.or,
      seq('(', $._expr, ')')
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
