#include "lexer.h"
#include "utf8_util.h"
#include "char_util.h"
#include "string_util.h"
#include "file_util.h"

static const int char_int_table[] =
{
    ['0'] = 0x0, ['1'] = 0x1,
    ['2'] = 0x2, ['3'] = 0x3,
    ['4'] = 0x4, ['5'] = 0x5,
    ['6'] = 0x6, ['7'] = 0x7,
    ['8'] = 0x8, ['9'] = 0x9,
    ['a'] = 0xA, ['A'] = 0xA,
    ['b'] = 0xB, ['B'] = 0xB,
    ['c'] = 0xC, ['C'] = 0xC,
    ['d'] = 0xD, ['D'] = 0xD,
    ['e'] = 0xE, ['E'] = 0xE,
    ['f'] = 0xF, ['F'] = 0xF,
};

#define CHECK_DIGIT_FUNC(name) bool name(char c)
typedef CHECK_DIGIT_FUNC(check_digit_func);
static check_digit_func *check_digit[] =
{
    [2]  = is_binary,
    [8]  = is_octal,
    [10] = is_digit,
    [16] = is_hexadecimal,
};

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

static inline bool
lexer_eat_integer(struct lexer *lexer, struct token *token)
{
    bool overflow = false;
    int value = 0;
    int base = 10;

    if (*lexer->at && *lexer->at == 'x') {
        lexer_advance(lexer);
        base = 16;
    } else if (*lexer->at && *lexer->at == 'o') {
        lexer_advance(lexer);
        base = 8;
    } else if (*lexer->at && *lexer->at == 'b') {
        lexer_advance(lexer);
        base = 2;
    } else {
        //
        // NOTE(koekeishiya): We already traversed past the first
        // digit if the literal is in base10.. We move back one
        // so that our parsing below will work as expected.
        //
        --lexer->at;
    }

    while (*lexer->at && check_digit[base](*lexer->at)) {
        uint8_t c = lexer_advance(lexer);
        int digit = char_int_table[c];
        if (value > (INT_MAX - digit) / base) {
            overflow = true;
        }
        value = value * base + digit;
    }

    token->length = lexer->at - token->text;
    token->kind = TOKEN_KIND_INT_LITERAL;
    token->as.i = value;

    return overflow;
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

    case '=':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_EQUAL;
        }
        break;
    case '!':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_NOT_EQUAL;
        }
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
        } else if (*lexer->at && *lexer->at == '|') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_OR;
        }
        break;
    case '&':
        if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_AND_ASSIGN;
        } else if (*lexer->at && *lexer->at == '&') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_AND;
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
        if (*lexer->at && *lexer->at == '<') {
            lexer_advance(lexer);
            if (*lexer->at && *lexer->at == '=') {
                lexer_advance(lexer);
                token.length = lexer->at - token.text;
                token.kind = TOKEN_KIND_LSHIFT_ASSIGN;
            } else {
                token.length = lexer->at - token.text;
                token.kind = TOKEN_KIND_LSHIFT;
            }
        } else if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_LT_EQUAL;
        }
        break;
    case '>':
        if (*lexer->at && *lexer->at == '>') {
            lexer_advance(lexer);
            if (*lexer->at && *lexer->at == '=') {
                lexer_advance(lexer);
                token.length = lexer->at - token.text;
                token.kind = TOKEN_KIND_RSHIFT_ASSIGN;
            } else {
                token.length = lexer->at - token.text;
                token.kind = TOKEN_KIND_RSHIFT;
            }
        } else if (*lexer->at && *lexer->at == '=') {
            lexer_advance(lexer);
            token.length = lexer->at - token.text;
            token.kind = TOKEN_KIND_GT_EQUAL;
        }
        break;

    default:
        if (is_digit(current)) {
            bool overflow = lexer_eat_integer(lexer, &token);
            if (overflow) {
                printf("#%d:%d integer literal '%.*s' overflow (%d)!\n",
                       token.line, token.column, token.length, token.text, token.as.i);
            }
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
    case TOKEN_KIND_EOF:
        snprintf(token_value, token_value_length, "EOF");
        break;
    case TOKEN_KIND_UNKNOWN:
        snprintf(token_value, token_value_length, "UNKNOWN_TOKEN(%.*s)", token.length, token.text);
        break;
    default:
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
