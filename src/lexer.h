#ifndef MAJI_LEXER_H
#define MAJI_LEXER_H

#include "utf8_util.h"
#include <stdint.h>

struct lexer
{
    uint8_t *file;

    uint8_t *buffer;
    uint8_t *at;

    int column;
    int line;
};

static const char *token_kind_str[] =
{
    "CHAR_LITERAL",
    "INT_LITERAL",
    "FLOAT32_LITERAL",
    "FLOAT64_LITERAL",

    "OPEN_PAREN",
    "CLOSE_PAREN",

    "PLUS",
    "DASH",
    "STAR",
    "SLASH",

    "EOF",
    "UNKNOWN"
};
enum token_kind
{
    TOKEN_KIND_CHAR_LITERAL,
    TOKEN_KIND_INT_LITERAL,
    TOKEN_KIND_FLOAT32_LITERAL,
    TOKEN_KIND_FLOAT64_LITERAL,

    TOKEN_KIND_OPEN_PAREN,
    TOKEN_KIND_CLOSE_PAREN,

    TOKEN_KIND_PLUS,
    TOKEN_KIND_DASH,
    TOKEN_KIND_STAR,
    TOKEN_KIND_SLASH,

    TOKEN_KIND_EOF,
    TOKEN_KIND_UNKNOWN
};

struct token
{
    enum token_kind kind;

    uint8_t *text;
    int length;

    int column;
    int line;

    union
    {
        utf8_char uc;
        uint8_t c;
        int i;
        float f32;
        double f64;
    } as;
};

void lexer_init(struct lexer *lexer, uint8_t *file);
void lexer_destroy(struct lexer *lexer);

struct token lexer_get_token(struct lexer *lexer);
void lexer_print_token(struct token token, uint8_t *token_value_u8, int token_value_length);

#endif
