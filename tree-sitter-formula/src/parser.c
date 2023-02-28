#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 13
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 17
#define ALIAS_COUNT 0
#define TOKEN_COUNT 12
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 1

enum {
  sym_var = 1,
  anon_sym_LPAREN = 2,
  anon_sym_RPAREN = 3,
  anon_sym_BANG = 4,
  anon_sym_ = 5,
  anon_sym_AMP = 6,
  anon_sym_AMP_AMP = 7,
  anon_sym_2 = 8,
  anon_sym_PIPE = 9,
  anon_sym_PIPE_PIPE = 10,
  anon_sym_3 = 11,
  sym_source_file = 12,
  sym__expr = 13,
  sym_neg = 14,
  sym_and = 15,
  sym_or = 16,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_var] = "var",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
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
  [sym_neg] = "neg",
  [sym_and] = "and",
  [sym_or] = "or",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_var] = sym_var,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
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
      ACCEPT_TOKEN(ts_builtin_sym_end);
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
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_var] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
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
    [sym_source_file] = STATE(12),
    [sym__expr] = STATE(10),
    [sym_neg] = STATE(10),
    [sym_and] = STATE(10),
    [sym_or] = STATE(10),
    [sym_var] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(7),
    [anon_sym_] = ACTIONS(7),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 4,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 1,
      sym_var,
    ACTIONS(7), 2,
      anon_sym_BANG,
      anon_sym_,
    STATE(11), 4,
      sym__expr,
      sym_neg,
      sym_and,
      sym_or,
  [17] = 4,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(11), 1,
      sym_var,
    ACTIONS(7), 2,
      anon_sym_BANG,
      anon_sym_,
    STATE(4), 4,
      sym__expr,
      sym_neg,
      sym_and,
      sym_or,
  [34] = 5,
    ACTIONS(15), 1,
      anon_sym_AMP,
    ACTIONS(19), 1,
      anon_sym_PIPE,
    ACTIONS(13), 2,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
    ACTIONS(17), 2,
      anon_sym_AMP_AMP,
      anon_sym_2,
    ACTIONS(21), 2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [53] = 4,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(23), 1,
      sym_var,
    ACTIONS(7), 2,
      anon_sym_BANG,
      anon_sym_,
    STATE(8), 4,
      sym__expr,
      sym_neg,
      sym_and,
      sym_or,
  [70] = 4,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      sym_var,
    ACTIONS(7), 2,
      anon_sym_BANG,
      anon_sym_,
    STATE(9), 4,
      sym__expr,
      sym_neg,
      sym_and,
      sym_or,
  [87] = 2,
    ACTIONS(29), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(27), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [100] = 4,
    ACTIONS(19), 1,
      anon_sym_PIPE,
    ACTIONS(33), 1,
      anon_sym_AMP,
    ACTIONS(21), 2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
    ACTIONS(31), 4,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
  [117] = 2,
    ACTIONS(37), 2,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(35), 6,
      ts_builtin_sym_end,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
      anon_sym_2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [130] = 5,
    ACTIONS(15), 1,
      anon_sym_AMP,
    ACTIONS(19), 1,
      anon_sym_PIPE,
    ACTIONS(39), 1,
      ts_builtin_sym_end,
    ACTIONS(17), 2,
      anon_sym_AMP_AMP,
      anon_sym_2,
    ACTIONS(21), 2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [148] = 5,
    ACTIONS(15), 1,
      anon_sym_AMP,
    ACTIONS(19), 1,
      anon_sym_PIPE,
    ACTIONS(41), 1,
      anon_sym_RPAREN,
    ACTIONS(17), 2,
      anon_sym_AMP_AMP,
      anon_sym_2,
    ACTIONS(21), 2,
      anon_sym_PIPE_PIPE,
      anon_sym_3,
  [166] = 1,
    ACTIONS(43), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 17,
  [SMALL_STATE(4)] = 34,
  [SMALL_STATE(5)] = 53,
  [SMALL_STATE(6)] = 70,
  [SMALL_STATE(7)] = 87,
  [SMALL_STATE(8)] = 100,
  [SMALL_STATE(9)] = 117,
  [SMALL_STATE(10)] = 130,
  [SMALL_STATE(11)] = 148,
  [SMALL_STATE(12)] = 166,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_neg, 2),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expr, 3),
  [29] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expr, 3),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_and, 3),
  [33] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_and, 3),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_or, 3),
  [37] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_or, 3),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [43] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
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
