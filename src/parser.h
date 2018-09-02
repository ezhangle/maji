#ifndef MAJI_PARSER_H
#define MAJI_PARSER_H

#include "lexer.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

struct parser
{
    struct lexer lexer;
    struct token *tokens;
    int current_token;
};

struct token parser_advance(struct parser *parser);
struct token parser_previous(struct parser *parser);
struct token parser_peek(struct parser *parser);

bool parser_check(struct parser *parser, enum token_kind kind);
bool parser_match(struct parser *parser, enum token_kind kind);
void parser_consume(struct parser *parser, enum token_kind kind);
bool parser_eof(struct parser *parser);

void parser_fatal(struct token token, const char *format, ...);

void parser_init(struct parser *parser, uint8_t *file);
void parser_destroy(struct parser *parser);

#endif
