#include "lexer.h"
#include "utf8_util.h"
#include "char_util.h"
#include "string_util.h"
#include "file_util.h"

#include <inttypes.h>

static const uint64_t char_int_table[] =
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
    [10] = is_decimal,
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

static inline void
lexer_eat_block_comment(struct lexer *lexer)
{
    int depth = 1;
    while (depth > 0 && lexer->at[0] && lexer->at[1]) {
        if ((lexer->at[0] == '/') &&
            (lexer->at[1] == '*')) {
            ++depth;
        }

        if ((lexer->at[0] == '*') &&
            (lexer->at[1] == '/')) {
            --depth;
        }

        if (depth > 0) lexer_advance(lexer);
    }

    if ((lexer->at[0] == '*') &&
        (lexer->at[1] == '/')) {
        lexer_advance(lexer);
        lexer_advance(lexer);
    } else {
        // TODO(koekeishiya): missing end of block comment
    }
}

static inline void
lexer_eat_line_comment(struct lexer *lexer)
{
    while (*lexer->at && *lexer->at != '\n') {
        lexer_advance(lexer);
    }
}

static inline void
lexer_eat_string(struct lexer *lexer)
{
    while (*lexer->at && *lexer->at != '"') {
        if (*lexer->at == '\\') {
            lexer_advance(lexer);
        }
        lexer_advance(lexer);
    }

    if (*lexer->at && *lexer->at == '"') {
        lexer_advance(lexer);
    } else {
        // TODO(koekeishiya): missing end of string literal
    }
}

static inline void
lexer_eat_char(struct lexer *lexer, struct token *token)
{
    // NOTE(koekeishiya): empty char literal is equal to zero
    if (*lexer->at && *lexer->at == '\'') {
        token->as.int_val = 0;
        lexer_advance(lexer);
    } else {
        uint8_t c = lexer_advance(lexer);
        if (c == '\\' && *lexer->at) {
            token->as.int_val = resolve_char(lexer_advance(lexer));
        } else if (c != '\\') {
            token->as.int_val = c;
        }
    }

    if (*lexer->at && *lexer->at == '\'') {
        lexer_advance(lexer);
    } else {
        // TODO(koekeishiya): missing end of char literal
    }

    token->length = lexer->at - token->text;
    token->kind = TOKEN_KIND_INT_LITERAL;
    token->base = NUMBER_BASE_CHAR;
}

static inline bool
lexer_eat_number(struct lexer *lexer, struct token *token)
{
    bool overflow = false;
    uint64_t value = 0;
    uint64_t base = 10;

    if (*token->text == '0') {
        if (*lexer->at && *lexer->at == 'x') {
            token->base = NUMBER_BASE_HEXADECIMAL;
            lexer_advance(lexer);
            base = 16;
        } else if (*lexer->at && *lexer->at == 'o') {
            token->base = NUMBER_BASE_OCTAL;
            lexer_advance(lexer);
            base = 8;
        } else if (*lexer->at && *lexer->at == 'b') {
            token->base = NUMBER_BASE_BINARY;
            lexer_advance(lexer);
            base = 2;
        }
    }

    if (base == 10) {

        //
        // NOTE(koekeishiya): We already traversed past the first digit.
        // We move back one so that our parsing below will work as expected.
        //

        --lexer->at;
        --lexer->column;
        token->base = NUMBER_BASE_DECIMAL;
    }

    while (*lexer->at && check_digit[base](*lexer->at)) {
        uint8_t c = lexer_advance(lexer);
        uint64_t digit = char_int_table[c];
        if (value > (UINT64_MAX - digit) / base) {
            overflow = true;
        }
        value = value * base + digit;
        token->length = lexer->at - token->text;
        token->as.int_val = value;
        token->kind = TOKEN_KIND_INT_LITERAL;
    }

    if (base == 10) {
        if (*lexer->at == 'e' || *lexer->at == '.') {
            lexer_advance(lexer);
            while (*lexer->at && check_digit[base](*lexer->at)) {
                lexer_advance(lexer);
            }
            token->length = lexer->at - token->text;
            uint8_t *float_literal = copy_string_count(token->text, token->length);
            overflow = sscanf((char*)float_literal, "%lf", &token->as.float_val) != 1;
            free(float_literal);
            token->kind = TOKEN_KIND_FLOAT_LITERAL;
        }
    }

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
        .kind   = current,
        .base   = NUMBER_BASE_NONE
    };

    lexer_advance(lexer);

    switch (current) {
    case '\0': token.kind = TOKEN_KIND_EOF; break;

    case ';':
    case '(': case ')':
    case '[': case ']':
    case '{': case '}':
        break;

    case '\'':
        lexer_eat_char(lexer, &token);
        break;
    case '"':
        lexer_eat_string(lexer);
        token.length = lexer->at - token.text;
        if (token.length <= 2) {
            token.as.string_val = u8"";
        } else {
            uint8_t *resolved_string = resolve_string_count(token.text + 1, token.length - 2);
            token.as.string_val = intern_string(resolved_string);
            free(resolved_string);
        }
        token.kind = TOKEN_KIND_STRING_LITERAL;
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
        } else if (*lexer->at && *lexer->at == '/') {
            lexer_advance(lexer);
            lexer_eat_line_comment(lexer);
            token = lexer_get_token(lexer);
        } else if (*lexer->at && *lexer->at == '*') {
            lexer_advance(lexer);
            lexer_eat_block_comment(lexer);
            token = lexer_get_token(lexer);
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
        if (is_decimal(current)) {
            bool overflow = lexer_eat_number(lexer, &token);
            if (overflow) {
                printf("#%d:%d number literal '%.*s' overflow!\n",
                       token.line, token.column, token.length, token.text);
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
        if (token.base == NUMBER_BASE_CHAR) {
            snprintf(token_value, token_value_length, "%c", (uint8_t)token.as.int_val);
        } else {
            snprintf(token_value, token_value_length, "%" PRIu64, token.as.int_val);
        }
        break;
    case TOKEN_KIND_FLOAT_LITERAL:
        snprintf(token_value, token_value_length, "%f", token.as.float_val);
        break;
    case TOKEN_KIND_STRING_LITERAL:
        snprintf(token_value, token_value_length, "%s", token.as.string_val);
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
