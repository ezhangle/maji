#include "lexer.h"
#include "utf8_util.h"
#include "char_util.h"
#include "string_util.h"
#include "file_util.h"

static inline utf8_char
lexer_advance(struct lexer *lexer)
{
    utf8_char uc = utf8_next_char(&lexer->at);
    if (uc == '\n') {
        lexer->column = 0;
        ++lexer->line;
    }
    ++lexer->column;
    return uc;
}

static inline void
lexer_eat_whitespace(struct lexer *lexer)
{
    while (*lexer->at && is_whitespace(*lexer->at)) {
        lexer_advance(lexer);
    }
}

static inline void
lexer_eat_decimal(struct lexer *lexer)
{
    while (*lexer->at && is_digit(*lexer->at)) {
        lexer_advance(lexer);
    }
}

static inline void
lexer_eat_hexadecimal(struct lexer *lexer)
{
    while (*lexer->at && is_hexadecimal(*lexer->at)) {
        lexer_advance(lexer);
    }
}

static inline void
lexer_eat_binary(struct lexer *lexer)
{
    while (*lexer->at && is_binary(*lexer->at)) {
        lexer_advance(lexer);
    }
}

struct token lexer_get_token(struct lexer *lexer)
{
    lexer_eat_whitespace(lexer);

    int num_bytes;
    utf8_char current = utf8_get_char_and_bytes(lexer->at, &num_bytes);

    struct token token = {
        .text   = lexer->at,
        .length = num_bytes,
        .column = lexer->column,
        .line   = lexer->line,
        .as.uc  = current
    };

    lexer_advance(lexer);

    switch (current) {
    case '\0': token.kind = TOKEN_KIND_EOF;        break;

    case '+': token.kind = TOKEN_KIND_PLUS;        break;
    case '-': token.kind = TOKEN_KIND_DASH;        break;
    case '*': token.kind = TOKEN_KIND_STAR;        break;
    case '/': token.kind = TOKEN_KIND_SLASH;       break;
    case '(': token.kind = TOKEN_KIND_OPEN_PAREN;  break;
    case ')': token.kind = TOKEN_KIND_CLOSE_PAREN; break;

    default:
        if (is_digit(current)) {
            token.kind = TOKEN_KIND_INT_LITERAL;
            if (*lexer->at && *lexer->at == 'x') {
                lexer_advance(lexer);
                lexer_eat_hexadecimal(lexer);
                token.length = lexer->at - token.text;
                token.as.i = convert_string_count_to_int(token.text + 2, token.length - 2, 16);
            } else if (*lexer->at && *lexer->at == 'b') {
                lexer_advance(lexer);
                lexer_eat_binary(lexer);
                token.length = lexer->at - token.text;
                token.as.i = convert_string_count_to_int(token.text + 2, token.length - 2, 2);
            } else {
                lexer_eat_decimal(lexer);
                token.length = lexer->at - token.text;
                token.as.i = convert_string_count_to_int(token.text, token.length, 10);
            }
        } else {
            token.kind = TOKEN_KIND_UNKNOWN;
        }
        break;
    }

    return token;
}

void lexer_print_token(struct token token, uint8_t *token_value_u8, int token_value_length)
{
    char *token_value = (char *)token_value_u8;

    switch (token.kind) {
    case TOKEN_KIND_CHAR_LITERAL:
        snprintf(token_value, token_value_length, "%c", token.as.c);
        break;
    case TOKEN_KIND_INT_LITERAL:
        snprintf(token_value, token_value_length, "%d", token.as.i);
        break;
    case TOKEN_KIND_FLOAT32_LITERAL:
        snprintf(token_value, token_value_length, "%f", token.as.f32);
        break;
    case TOKEN_KIND_FLOAT64_LITERAL:
        snprintf(token_value, token_value_length, "%f", token.as.f64);
        break;
    case TOKEN_KIND_PLUS:
    case TOKEN_KIND_DASH:
    case TOKEN_KIND_STAR:
    case TOKEN_KIND_SLASH:
    case TOKEN_KIND_OPEN_PAREN:
    case TOKEN_KIND_CLOSE_PAREN:
        snprintf(token_value, token_value_length, "%c", token.as.c);
        break;
    case TOKEN_KIND_EOF:
        snprintf(token_value, token_value_length, "EOF");
        break;
    case TOKEN_KIND_UNKNOWN:
        snprintf(token_value, token_value_length, "%.*s", token.length, token.text);
        break;
    }
}

void lexer_init(struct lexer *lexer, uint8_t *file)
{
    lexer->buffer = read_file(file);
    lexer->at = lexer->buffer;
    lexer->file = file;
    lexer->column = 1;
    lexer->line = 1;
}

void lexer_destroy(struct lexer *lexer)
{
    free(lexer->buffer);
}
