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

enum token_kind
{
    // NOTE(koekeishiya): Tokens with ascii value less than 127 use
    // the ascii value as token_kind as well
    TOKEN_KIND_IDENTIFIER = 128,
    TOKEN_KIND_INT_LITERAL,
    TOKEN_KIND_STRING_LITERAL,

    TOKEN_KIND_EQUAL,
    TOKEN_KIND_NOT_EQUAL,
    TOKEN_KIND_LT_EQUAL,
    TOKEN_KIND_GT_EQUAL,
    TOKEN_KIND_AND,
    TOKEN_KIND_OR,
    TOKEN_KIND_LSHIFT,
    TOKEN_KIND_RSHIFT,

    TOKEN_KIND_COLON_ASSIGN,
    TOKEN_KIND_ADD_ASSIGN,
    TOKEN_KIND_SUB_ASSIGN,
    TOKEN_KIND_MUL_ASSIGN,
    TOKEN_KIND_DIV_ASSIGN,
    TOKEN_KIND_MOD_ASSIGN,
    TOKEN_KIND_OR_ASSIGN,
    TOKEN_KIND_AND_ASSIGN,
    TOKEN_KIND_XOR_ASSIGN,
    TOKEN_KIND_NOT_ASSIGN,
    TOKEN_KIND_LSHIFT_ASSIGN,
    TOKEN_KIND_RSHIFT_ASSIGN,

    TOKEN_KIND_EOF,
    TOKEN_KIND_UNKNOWN
};

enum number_base
{
    NUMBER_BASE_NONE,
    NUMBER_BASE_CHAR,
    NUMBER_BASE_BINARY,
    NUMBER_BASE_OCTAL,
    NUMBER_BASE_DECIMAL,
    NUMBER_BASE_HEXADECIMAL
};

struct token
{
    enum token_kind kind;
    enum number_base base;

    uint8_t *text;
    int length;

    int column;
    int line;

    union {
        uint8_t c;
        utf8_char uc;
        uint8_t *string_val;
        uint8_t *name;
        uint64_t int_val;
        double float_val;
    } as;
};

void lexer_init(struct lexer *lexer, uint8_t *file);
void lexer_destroy(struct lexer *lexer);

struct token lexer_get_token(struct lexer *lexer);
void lexer_print_token(struct token token, uint8_t *token_value_u8, int token_value_length);

#endif
