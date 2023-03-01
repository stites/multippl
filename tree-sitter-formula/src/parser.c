#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 15
#define LARGE_STATE_COUNT 6
#define SYMBOL_COUNT 25
#define ALIAS_COUNT 0
#define TOKEN_COUNT 18
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 1

enum {
  sym_var = 1,
  anon_sym_LPAREN = 2,
  anon_sym_RPAREN = 3,
  anon_sym_false = 4,
  anon_sym_F = 5,
  anon_sym_False = 6,
  anon_sym_true = 7,
  anon_sym_T = 8,
  anon_sym_True = 9,
  anon_sym_BANG = 10,
  anon_sym_ = 11,
  anon_sym_AMP = 12,
  anon_sym_AMP_AMP = 13,
  anon_sym_2 = 14,
  anon_sym_PIPE = 15,
  anon_sym_PIPE_PIPE = 16,
  anon_sym_3 = 17,
  sym_source_file = 18,
  sym__expr = 19,
  sym_false = 20,
  sym_true = 21,
  sym_neg = 22,
  sym_and = 23,
  sym_or = 24,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_var] = "var",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_false] = "false",
  [anon_sym_F] = "F",
  [anon_sym_False] = "False",
  [anon_sym_true] = "true",
  [anon_sym_T] = "T",
  [anon_sym_True] = "True",
  [anon_sym_BANG] = "!",
  [anon_sym_] = "¬",
  [anon_sym_AMP] = "&",
  [anon_sym_AMP_AMP] = "&&",
  [anon_sym_2] = "∧",
  [anon_sym_PIPE] = "|",
  [anon_sym_PIPE_PIPE] = "||",
  [anon_sym_3] = "∨",
  [sym_source_file] = "source_file",
  [sym__expr] = "_expr",
  [sym_false] = "false",
  [sym_true] = "true",
  [sym_neg] = "neg",
  [sym_and] = "and",
  [sym_or] = "or",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_var] = sym_var,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_false] = anon_sym_false,
  [anon_sym_F] = anon_sym_F,
  [anon_sym_False] = anon_sym_False,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_T] = anon_sym_T,
  [anon_sym_True] = anon_sym_True,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_] = anon_sym_,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_AMP_AMP] = anon_sym_AMP_AMP,
  [anon_sym_2] = anon_sym_2,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_PIPE_PIPE] = anon_sym_PIPE_PIPE,
  [anon_sym_3] = anon_sym_3,
  [sym_source_file] = sym_source_file,
  [sym__expr] = sym__expr,
  [sym_false] = sym_false,
  [sym_true] = sym_true,
  [sym_neg] = sym_neg,
  [sym_and] = sym_and,
  [sym_or] = sym_or,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_var] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_F] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_False] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_T] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_True] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_2] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_3] = {
    .visible = true,
    .named = false,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__expr] = {
    .visible = false,
    .named = true,
  },
  [sym_false] = {
    .visible = true,
    .named = true,
  },
  [sym_true] = {
    .visible = true,
    .named = true,
  },
  [sym_neg] = {
    .visible = true,
    .named = true,
  },
  [sym_and] = {
    .visible = true,
    .named = true,
  },
  [sym_or] = {
    .visible = true,
    .named = true,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(1);
      if (lookahead == '!') ADVANCE(4);
      if (lookahead == '&') ADVANCE(6);
      if (lookahead == '(') ADVANCE(2);
      if (lookahead == ')') ADVANCE(3);
      if (lookahead == '|') ADVANCE(9);
      if (lookahead == 172) ADVANCE(5);
      if (lookahead == 8743) ADVANCE(8);
      if (lookahead == 8744) ADVANCE(11);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(12);
      END_STATE();
    case 1:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '&') ADVANCE(7);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_AMP_AMP);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_2);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '|') ADVANCE(10);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_PIPE_PIPE);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_3);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(sym_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(12);
      END_STATE();
    default:
      return false;
  }
}

static bool ts_lex_keywords(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (lookahead == 'F') ADVANCE(1);
      if (lookahead == 'T') ADVANCE(2);
      if (lookahead == 'f') ADVANCE(3);
      if (lookahead == 't') ADVANCE(4);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      END_STATE();
    case 1:
      ACCEPT_TOKEN(anon_sym_F);
      if (lookahead == 'a') ADVANCE(5);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(anon_sym_T);
      if (lookahead == 'r') ADVANCE(6);
      END_STATE();
    case 3:
      if (lookahead == 'a') ADVANCE(7);
      END_STATE();
    case 4:
      if (lookahead == 'r') ADVANCE(8);
      END_STATE();
    case 5:
      if (lookahead == 'l') ADVANCE(9);
      END_STATE();
    case 6:
      if (lookahead == 'u') ADVANCE(10);
      END_STATE();
    case 7:
      if (lookahead == 'l') ADVANCE(11);
      END_STATE();
    case 8:
      if (lookahead == 'u') ADVANCE(12);
      END_STATE();
    case 9:
      if (lookahead == 's') ADVANCE(13);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(14);
      END_STATE();
    case 11:
      if (lookahead == 's') ADVANCE(15);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(16);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(17);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_True);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(18);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_False);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_var] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [anon_sym_F] = ACTIONS(1),
    [anon_sym_False] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_T] = ACTIONS(1),
    [anon_sym_True] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_AMP_AMP] = ACTIONS(1),
    [anon_sym_2] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_PIPE_PIPE] = ACTIONS(1),
    [anon_sym_3] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(14),
    [sym__expr] = STATE(12),
    [sym_false] = STATE(12),
    [sym_true] = STATE(12),
    [sym_neg] = STATE(12),
    [sym_and] = STATE(12),
    [sym_or] = STATE(12),
    [sym_var] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_false] = ACTIONS(7),
    [anon_sym_F] = ACTIONS(7),
    [anon_sym_False] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_T] = ACTIONS(9),
    [anon_sym_True] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(11),
    [anon_sym_] = ACTIONS(11),
  },
  [2] = {
    [sym__expr] = STATE(13),
    [sym_false] = STATE(13),
    [sym_true] = STATE(13),
    [sym_neg] = STATE(13),
    [sym_and] = STATE(13),
    [sym_or] = STATE(13),
    [sym_var] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_false] = ACTIONS(7),
    [anon_sym_F] = ACTIONS(7),
    [anon_sym_False] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_T] = ACTIONS(9),
    [anon_sym_True] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(11),
    [anon_sym_] = ACTIONS(11),
  },
  [3] = {
    [sym__expr] = STATE(8),
    [sym_false] = STATE(8),
    [sym_true] = STATE(8),
    [sym_neg] = STATE(8),
    [sym_and] = STATE(8),
    [sym_or] = STATE(8),
    [sym_var] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_false] = ACTIONS(7),
    [anon_sym_F] = ACTIONS(7),
    [anon_sym_False] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_T] = ACTIONS(9),
    [anon_sym_True] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(11),
    [anon_sym_] = ACTIONS(11),
  },
  [4] = {
    [sym__expr] = STATE(10),
    [sym_false] = STATE(10),
    [sym_true] = STATE(10),
    [sym_neg] = STATE(10),
    [sym_and] = STATE(10),
    [sym_or] = STATE(10),
    [sym_var] = ACTIONS(17),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_false] = ACTIONS(7),
    [anon_sym_F] = ACTIONS(7),
    [anon_sym_False] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_T] = ACTIONS(9),
    [anon_sym_True] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(11),
    [anon_sym_] = ACTIONS(11),
  },
  [5] = {
    [sym__expr] = STATE(11),
    [sym_false] = STATE(11),
    [sym_true] = STATE(11),
    [sym_neg] = STATE(11),
    [sym_and] = STATE(11),
    [sym_or] = STATE(11),
    [sym_var] = ACTIONS(19),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_false] = ACTIONS(7),
    [anon_sym_F] = ACTIONS(7),
    [anon_sym_False] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_T] = ACTIONS(9),
    [anon_sym_True] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(11),
    [anon_sym_] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 2,
    ACTIONS(23), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(21), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [13] = 2,
    ACTIONS(27), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(25), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [26] = 2,
    ACTIONS(31), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(29), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [39] = 2,
    ACTIONS(35), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(33), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [52] = 2,
    ACTIONS(39), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(37), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [65] = 4,
    ACTIONS(43), 1,
      anon_sym_AMP,
    ACTIONS(47), 1,
      anon_sym_PIPE,
    ACTIONS(45), 2,
      anon_sym_AMP_AMP,
      anon_sym_2,
    ACTIONS(41), 4,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [82] = 5,
    ACTIONS(43), 1,
      anon_sym_AMP,
    ACTIONS(49), 1,
      ts_builtin_sym_end,
    ACTIONS(51), 1,
      anon_sym_PIPE,
    ACTIONS(45), 2,
      anon_sym_AMP_AMP,
      anon_sym_2,
    ACTIONS(53), 2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [100] = 5,
    ACTIONS(43), 1,
      anon_sym_AMP,
    ACTIONS(51), 1,
      anon_sym_PIPE,
    ACTIONS(55), 1,
      anon_sym_RPAREN,
    ACTIONS(45), 2,
      anon_sym_AMP_AMP,
      anon_sym_2,
    ACTIONS(53), 2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [118] = 1,
    ACTIONS(57), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(6)] = 0,
  [SMALL_STATE(7)] = 13,
  [SMALL_STATE(8)] = 26,
  [SMALL_STATE(9)] = 39,
  [SMALL_STATE(10)] = 52,
  [SMALL_STATE(11)] = 65,
  [SMALL_STATE(12)] = 82,
  [SMALL_STATE(13)] = 100,
  [SMALL_STATE(14)] = 118,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_false, 1),
  [23] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_false, 1),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_true, 1),
  [27] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_true, 1),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_neg, 2),
  [31] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_neg, 2),
  [33] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expr, 3),
  [35] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expr, 3),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_and, 3),
  [39] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_and, 3),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_or, 3),
  [43] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [47] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_or, 3),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [51] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [57] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_formula(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym_var,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
