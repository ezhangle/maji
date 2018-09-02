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

static inline void
lexer_eat_identifier(struct lexer *lexer)
{
    while (*lexer->at && is_identifier(*lexer->at)) {
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
        .as.uc  = current,
        .kind   = current
    };

    lexer_advance(lexer);

    switch (current) {
    case '\0': token.kind = TOKEN_KIND_EOF; break;

    case '(': case ')':
    case '[': case ']':
    case '{': case '}':
        break;

    case ':':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_COLON_ASSIGN;
        }
        break;
    case '+':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_ADD_ASSIGN;
        }
        break;
    case '-':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_SUB_ASSIGN;
        }
        break;
    case '*':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_MUL_ASSIGN;
        }
        break;
    case '/':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_DIV_ASSIGN;
        }
        break;
    case '%':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_MOD_ASSIGN;
        }
        break;

    case '|':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_OR_ASSIGN;
        }
        break;
    case '&':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_AND_ASSIGN;
        }
        break;
    case '^':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_XOR_ASSIGN;
        }
        break;
    case '~':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_NOT_ASSIGN;
        }
        break;
    case '<':
        if ((lexer->at[0] && lexer->at[0] == '<') &&
            (lexer->at[1] && lexer->at[1] == '=')) {
            lexer_advance(lexer);
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_LSHIFT_ASSIGN;
        }
        break;
    case '>':
        if ((lexer->at[0] && lexer->at[0] == '>') &&
            (lexer->at[1] && lexer->at[1] == '=')) {
            lexer_advance(lexer);
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_RSHIFT_ASSIGN;
        }
        break;

    default:
        if (is_digit(current)) {
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
            token.kind = TOKEN_KIND_INT_LITERAL;
        } else if (is_identifier(current)) {
            lexer_eat_identifier(lexer);
            token.length = lexer->at - token.text;
            token.as.name = intern_string_count(token.text, token.length);
            token.kind = TOKEN_KIND_IDENTIFIER;
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
    case TOKEN_KIND_IDENTIFIER:
        snprintf(token_value, token_value_length, "%s", token.as.name);
        break;
    case TOKEN_KIND_INT_LITERAL:
        snprintf(token_value, token_value_length, "%d", token.as.i);
        break;
    case TOKEN_KIND_COLON_ASSIGN:
    case TOKEN_KIND_ADD_ASSIGN:
    case TOKEN_KIND_SUB_ASSIGN:
    case TOKEN_KIND_MUL_ASSIGN:
    case TOKEN_KIND_DIV_ASSIGN:
    case TOKEN_KIND_MOD_ASSIGN:
    case TOKEN_KIND_OR_ASSIGN:
    case TOKEN_KIND_AND_ASSIGN:
    case TOKEN_KIND_XOR_ASSIGN:
    case TOKEN_KIND_NOT_ASSIGN:
    case TOKEN_KIND_LSHIFT_ASSIGN:
    case TOKEN_KIND_RSHIFT_ASSIGN:
        snprintf(token_value, token_value_length, "%.*s", token.length, token.text);
        break;
    case TOKEN_KIND_EOF:
        snprintf(token_value, token_value_length, "EOF");
        break;
    case TOKEN_KIND_UNKNOWN:
        snprintf(token_value, token_value_length, "UNKNOWN_TOKEN(%.*s)", token.length, token.text);
        break;
    default:
        snprintf(token_value, token_value_length, "%c", token.as.c);
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
