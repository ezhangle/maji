#include "parser.h"
#include "buffer.h"

#include <stdarg.h>
#include <assert.h>

struct token parser_advance(struct parser *parser)
{
    if (!parser_eof(parser)) {
        ++parser->current_token;
        buf_push(parser->tokens, lexer_get_token(&parser->lexer));
    }
    return parser_previous(parser);
}

struct token parser_previous(struct parser *parser)
{
    assert(parser->current_token > 0);
    return parser->tokens[parser->current_token - 1];
}

struct token parser_peek(struct parser *parser)
{
    assert(parser->current_token >= 0);
    return parser->tokens[parser->current_token];
}

bool parser_check(struct parser *parser, enum token_kind kind)
{
    if (parser_eof(parser)) return false;
    struct token token = parser_peek(parser);
    return token.kind == kind;
}

bool parser_match(struct parser *parser, enum token_kind kind)
{
    if (parser_check(parser, kind)) {
        parser_advance(parser);
        return true;
    }
    return false;
}

void parser_consume(struct parser *parser, enum token_kind kind)
{
    if (parser_check(parser, kind)) {
        parser_advance(parser);
        return;
    }

    uint8_t at_token_value[255];
    struct token at_token = parser_peek(parser);
    lexer_print_token(at_token, at_token_value, sizeof(at_token_value));

    parser_fatal(at_token, "expected token '%c', but got '%s'\n", kind, at_token_value);
}

bool parser_eof(struct parser *parser)
{
    struct token token = parser_peek(parser);
    return token.kind == TOKEN_KIND_EOF;
}

void parser_fatal(struct token token, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    printf("#%d:%d ", token.line, token.column);
    vprintf(format, args);
    va_end(args);
    exit(1);
}

void parser_init(struct parser *parser, uint8_t *file)
{
    parser->tokens = NULL;
    parser->current_token = 0;
    lexer_init(&parser->lexer, file);
    buf_push(parser->tokens, lexer_get_token(&parser->lexer));
}

void parser_destroy(struct parser *parser)
{
    buf_free(parser->tokens);
    lexer_destroy(&parser->lexer);
    memset(parser, 0, sizeof(struct parser));
}
