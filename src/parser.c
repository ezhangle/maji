#include "parser.h"
#include "buffer.h"

#include "ast.h"

#include <stdarg.h>
#include <assert.h>

static uint8_t *struct_keyword;
static uint8_t *enum_keyword;
static uint8_t *func_keyword;
static uint8_t *if_keyword;
static uint8_t *else_keyword;
static uint8_t *for_keyword;
static uint8_t *while_keyword;
static uint8_t *return_keyword;
static uint8_t *continue_keyword;
static uint8_t *break_keyword;
static uint8_t *cast_keyword;
static uint8_t *sizeof_keyword;

void parser_init_keywords(void)
{
    struct_keyword = intern_string(u8"struct");
    enum_keyword = intern_string(u8"enum");
    func_keyword = intern_string(u8"func");
    if_keyword = intern_string(u8"if");
    else_keyword = intern_string(u8"else");
    for_keyword = intern_string(u8"for");
    while_keyword = intern_string(u8"while");
    return_keyword = intern_string(u8"return");
    continue_keyword = intern_string(u8"continue");
    break_keyword = intern_string(u8"break");
    cast_keyword = intern_string(u8"cast");
    sizeof_keyword = intern_string(u8"sizeof");
}

struct ast_expr *parse_expr_operand(struct parser *parser)
{
    if (parser_match(parser, TOKEN_KIND_INT_LITERAL)) {
        struct token token = parser_previous(parser);
        if (token.base == NUMBER_BASE_CHAR) {
            return ast_expr_char(token.as.int_val);
        } else {
            return ast_expr_int(token.as.int_val, token.base);
        }
    } else if (parser_match(parser, TOKEN_KIND_FLOAT_LITERAL)) {
        return ast_expr_float(parser_previous(parser).as.float_val);
    } else if (parser_match(parser, TOKEN_KIND_STRING_LITERAL)) {
        return ast_expr_string(parser_previous(parser).as.string_val);
    } else if (parser_match_keyword(parser, cast_keyword)) {
        parser_consume(parser, '(');
        struct ast_typespec *type = parse_type(parser);
        parser_consume(parser, ')');
        return ast_expr_cast(type, parse_expr_unary(parser));
    } else if (parser_match_keyword(parser, sizeof_keyword)) {
        parser_consume(parser, '(');
        struct ast_typespec *type = parse_type(parser);
        parser_consume(parser, ')');
        return ast_expr_sizeof_type(type);
    } else if (parser_match(parser, TOKEN_KIND_IDENTIFIER)) {
        return ast_expr_identifier(parser_previous(parser).as.name);
    } else if (parser_match(parser, '(')) {
        struct ast_expr *expr = parse_expr(parser);
        parser_consume(parser, ')');
        return expr;
    } else {
        uint8_t at_token_value[255];
        struct token at_token = parser_peek(parser);
        lexer_print_token(at_token, at_token_value, sizeof(at_token_value));
        parser_fatal(parser, at_token, "unexpected token '%s' in expression", at_token_value);
        return NULL;
    }
}

static inline bool parser_match_base(struct parser *parser)
{
    return ((parser_match(parser, '(')) ||
            (parser_match(parser, '[')) ||
            (parser_match(parser, '.')));
}

struct ast_expr *parse_expr_base(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_operand(parser);
    while (parser_match_base(parser)) {
        enum token_kind kind = parser_previous(parser).kind;
        if (kind == '(') {
            struct ast_expr **args = NULL;
            if (!parser_check(parser, ')')) {
                buf_push(args, parse_expr(parser));
                while (parser_match(parser, ',')) {
                    buf_push(args, parse_expr(parser));
                }
            }
            parser_consume(parser, ')');
            expr = ast_expr_call(expr, args, buf_len(args));
        } else if (kind == '[') {
            struct ast_expr *index = parse_expr(parser);
            parser_consume(parser, ']');
            expr = ast_expr_index(expr, index);
        } else if (kind == '.') {
            parser_consume(parser, TOKEN_KIND_IDENTIFIER);
            expr = ast_expr_field(expr, parser_previous(parser).as.name);
        }
    }
    return expr;
}

static inline bool parser_match_unary(struct parser *parser)
{
    return ((parser_match(parser, '+')) ||
            (parser_match(parser, '-')) ||
            (parser_match(parser, '*')) ||
            (parser_match(parser, '&')) ||
            (parser_match(parser, '~')) ||
            (parser_match(parser, '!')) ||
            (parser_match(parser, TOKEN_KIND_INC)) ||
            (parser_match(parser, TOKEN_KIND_DEC)));
}

struct ast_expr *parse_expr_unary(struct parser *parser)
{
    if (parser_match_unary(parser)) {
        enum token_kind op = parser_previous(parser).kind;
        return ast_expr_unary(op, parse_expr_unary(parser));
    }
    return parse_expr_base(parser);
}

static inline bool parser_match_factor(struct parser *parser)
{
    return ((parser_match(parser, '*')) ||
            (parser_match(parser, '/')) ||
            (parser_match(parser, '%')) ||
            (parser_match(parser, '&')) ||
            (parser_match(parser, TOKEN_KIND_LSHIFT)) ||
            (parser_match(parser, TOKEN_KIND_RSHIFT)));
}

struct ast_expr *parse_expr_factor(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_unary(parser);
    while (parser_match_factor(parser)) {
        enum token_kind op = parser_previous(parser).kind;
        expr = ast_expr_binary(op, expr, parse_expr_unary(parser));
    }
    return expr;
}

static inline bool parser_match_term(struct parser *parser)
{
    return ((parser_match(parser, '+')) ||
            (parser_match(parser, '-')) ||
            (parser_match(parser, '|')) ||
            (parser_match(parser, '^')));
}

struct ast_expr *parse_expr_term(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_factor(parser);
    while (parser_match_term(parser)) {
        enum token_kind op = parser_previous(parser).kind;
        expr = ast_expr_binary(op, expr, parse_expr_factor(parser));
    }
    return expr;
}

static inline bool parser_match_cmp(struct parser *parser)
{
    return ((parser_match(parser, '<')) ||
            (parser_match(parser, TOKEN_KIND_LT_EQUAL)) ||
            (parser_match(parser, '>')) ||
            (parser_match(parser, TOKEN_KIND_GT_EQUAL)) ||
            (parser_match(parser, TOKEN_KIND_EQUAL)) ||
            (parser_match(parser, TOKEN_KIND_NOT_EQUAL)));
}

struct ast_expr *parse_expr_cmp(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_term(parser);
    while (parser_match_cmp(parser)) {
        enum token_kind op = parser_previous(parser).kind;
        expr = ast_expr_binary(op, expr, parse_expr_term(parser));
    }
    return expr;
}

struct ast_expr *parse_expr_and(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_cmp(parser);
    while (parser_match(parser, TOKEN_KIND_AND)) {
        expr = ast_expr_binary(TOKEN_KIND_AND, expr, parse_expr_cmp(parser));
    }
    return expr;
}

struct ast_expr *parse_expr_or(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_and(parser);
    while (parser_match(parser, TOKEN_KIND_OR)) {
        expr = ast_expr_binary(TOKEN_KIND_OR, expr, parse_expr_and(parser));
    }
    return expr;
}

struct ast_expr *parse_expr_ternary(struct parser *parser)
{
    struct ast_expr *expr = parse_expr_or(parser);
    if (parser_match(parser, '?')) {
        struct ast_expr *then_expr = parse_expr_ternary(parser);
        parser_consume(parser, ':');
        struct ast_expr *else_expr = parse_expr_ternary(parser);
        expr = ast_expr_ternary(expr, then_expr, else_expr);
    }
    return expr;
}

struct ast_expr *parse_expr(struct parser *parser)
{
    return parse_expr_ternary(parser);
}

struct ast_expr *parse_expr_paren(struct parser *parser)
{
    parser_consume(parser, '(');
    struct ast_expr *expr = parse_expr(parser);
    parser_consume(parser, ')');
    return expr;
}

static inline bool parser_match_assignment(struct parser *parser)
{
    return ((parser_match(parser, '=')) ||
            (parser_match(parser, TOKEN_KIND_ADD_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_SUB_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_MUL_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_DIV_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_MOD_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_OR_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_AND_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_XOR_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_NOT_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_LSHIFT_ASSIGN)) ||
            (parser_match(parser, TOKEN_KIND_RSHIFT_ASSIGN)));
}

struct ast_stmt *parse_stmt_base(struct parser *parser)
{
    struct ast_stmt *stmt = NULL;
    struct ast_expr *expr = parse_expr(parser);

    if (parser_match(parser, TOKEN_KIND_COLON_ASSIGN)) {
        if (expr->kind != AST_EXPR_IDENTIFIER) {
            parser_fatal(parser, parser_previous(parser), ":= must be preceeded by an identifier\n");
        }
        stmt = ast_stmt_init(expr->name, parse_expr(parser));
    } else if (parser_match(parser, TOKEN_KIND_CONST_ASSIGN)) {
        parser_fatal(parser, parser_previous(parser), ":: is not allowed in a statement context\n");
    } else if (parser_match_assignment(parser)) {
        enum token_kind op = parser_previous(parser).kind;
        stmt = ast_stmt_assign(op, expr, parse_expr(parser));
    } else if ((parser_match(parser, TOKEN_KIND_INC)) ||
               (parser_match(parser, TOKEN_KIND_DEC))) {
        enum token_kind op = parser_previous(parser).kind;
        stmt = ast_stmt_assign(op, expr, NULL);
    } else if (parser_match(parser, ':')) {
        if (expr->kind != AST_EXPR_IDENTIFIER) {
            parser_fatal(parser, parser_previous(parser), ": must be preceeded by an identifier in a statement context\n");
        }
        struct ast_typespec *type = parse_type(parser);
        struct ast_expr *init = NULL;
        if (parser_match(parser, '=')) {
            init = parse_expr(parser);
        }
        stmt = ast_stmt_decl(expr->name, type, init);
    } else {
        stmt = ast_stmt_expr(expr);
    }

    return stmt;
}

struct ast_stmt *parse_stmt_if(struct parser *parser)
{
    struct ast_expr *condition = parse_expr_paren(parser);
    struct ast_stmt_block then_block = parse_stmt_block(parser);
    struct ast_stmt_block else_block = {};
    struct ast_else_if *else_ifs = NULL;
    while (parser_match_keyword(parser, else_keyword)) {
        if (!parser_match_keyword(parser, if_keyword)) {
            else_block = parse_stmt_block(parser);
            break;
        }
        struct ast_expr *else_if_condition = parse_expr_paren(parser);
        struct ast_stmt_block else_if_block = parse_stmt_block(parser);
        buf_push(else_ifs, ((struct ast_else_if) {else_if_condition, else_if_block}));
    }
    return ast_stmt_if(condition, then_block, else_ifs, buf_len(else_ifs), else_block);
}

struct ast_stmt *parse_stmt_for(struct parser *parser)
{
    struct ast_stmt *init = NULL;
    struct ast_expr *condition = NULL;
    struct ast_stmt *next = NULL;

    parser_consume(parser, '(');
    if (!parser_check(parser, ';')) {
        init = parse_stmt_base(parser);
    }
    parser_consume(parser, ';');
    if (!parser_check(parser, ';')) {
        condition = parse_expr(parser);
    }
    parser_consume(parser, ';');
    if (!parser_check(parser, ')')) {
        next = parse_stmt_base(parser);
        if (next->kind == AST_STMT_INIT) {
            parser_fatal(parser, parser_previous(parser), "init statements not allowed in for statement's next clause");
        }
    }
    parser_consume(parser, ')');
    return ast_stmt_for(init, condition, next, parse_stmt_block(parser));
}

struct ast_stmt *parse_stmt_while(struct parser *parser)
{
    struct ast_expr *condition = parse_expr_paren(parser);
    return ast_stmt_while(condition, parse_stmt_block(parser));
}

struct ast_stmt *parse_stmt(struct parser *parser)
{
    if (parser_match_keyword(parser, if_keyword)) {
        return parse_stmt_if(parser);
    } else if (parser_match_keyword(parser, for_keyword)) {
        return parse_stmt_for(parser);
    } else if (parser_match_keyword(parser, while_keyword)) {
        return parse_stmt_while(parser);
    } else if (parser_check(parser, '{')) {
        return ast_stmt_block(parse_stmt_block(parser));
    } else if (parser_match_keyword(parser, return_keyword)) {
        struct ast_expr *expr = NULL;
        if (!parser_check(parser, ';')) {
            expr = parse_expr(parser);
        }
        parser_consume(parser, ';');
        return ast_stmt_return(expr);
    } else if (parser_match_keyword(parser, break_keyword)) {
        parser_consume(parser, ';');
        return ast_stmt_break();
    } else if (parser_match_keyword(parser, continue_keyword)) {
        parser_consume(parser, ';');
        return ast_stmt_continue();
    } else {
        struct ast_stmt *stmt = parse_stmt_base(parser);
        parser_consume(parser, ';');
        return stmt;
    }
}

struct ast_stmt_block parse_stmt_block(struct parser *parser)
{
    parser_consume(parser, '{');
    struct ast_stmt **statements = NULL;
    while (!parser_eof(parser) && !parser_check(parser, '}')) {
        struct ast_stmt *stmt = parse_stmt(parser);
        buf_push(statements, stmt);
    }
    parser_consume(parser, '}');
    return (struct ast_stmt_block) {statements, buf_len(statements)};
}

const uint8_t *parse_name(struct parser *parser)
{
    parser_consume(parser, TOKEN_KIND_IDENTIFIER);
    return parser_previous(parser).as.name;
}

struct ast_typespec *parse_type_func(struct parser *parser)
{
    parser_consume(parser, '(');

    struct ast_typespec **args = NULL;
    if (!parser_check(parser, ')')) {
        buf_push(args, parse_type(parser));
        while (parser_match(parser, ',')) {
            buf_push(args, parse_type(parser));
        }
    }
    parser_consume(parser, ')');

    struct ast_typespec *ret = NULL;
    if (parser_match(parser, TOKEN_KIND_ARROW)) {
        ret = parse_type(parser);
    }

    return ast_typespec_func(args, buf_len(args), ret);
}

struct ast_typespec *parse_type_base(struct parser *parser)
{
    if (parser_match(parser, TOKEN_KIND_IDENTIFIER)) {
        return ast_typespec_identifier(parser_previous(parser).as.name);
    } else if (parser_match_keyword(parser, func_keyword)) {
        return parse_type_func(parser);
    } else if (parser_match(parser, '(')) {
        return parse_type(parser);
    } else {
        uint8_t at_token_value[255];
        struct token at_token = parser_peek(parser);
        lexer_print_token(at_token, at_token_value, sizeof(at_token_value));
        parser_fatal(parser, at_token, "unexpected token %s in type\n", at_token_value);
        return NULL;
    }
}

struct ast_typespec *parse_type(struct parser *parser)
{
    struct ast_typespec *type = parse_type_base(parser);
    while ((parser_check(parser, '[')) ||
           (parser_check(parser, '*'))) {
        if (parser_match(parser, '[')) {
            struct ast_expr *expr = parse_expr(parser);
            parser_consume(parser, ']');
            type = ast_typespec_array(type, expr);
        } else {
            parser_consume(parser, '*');
            type = ast_typespec_pointer(type);
        }
    }
    return type;
}

struct ast_struct_item parse_decl_struct_item(struct parser *parser)
{
    const uint8_t *name = parse_name(parser);
    parser_consume(parser, ':');
    struct ast_typespec *type = parse_type(parser);
    parser_consume(parser, ';');
    return (struct ast_struct_item) {name, type};
}

struct ast_decl *parse_decl_struct(struct parser *parser, struct token identifier)
{
    parser_consume(parser, '{');
    struct ast_struct_item *items = NULL;
    while (!parser_eof(parser) && !parser_check(parser, '}')) {
        buf_push(items, parse_decl_struct_item(parser));
    }
    parser_consume(parser, '}');
    return ast_decl_struct(identifier.as.name, items, buf_len(items));
}

struct ast_enum_item parse_decl_enum_item(struct parser *parser)
{
    const uint8_t *name = parse_name(parser);
    struct ast_expr *expr = NULL;
    if (parser_match(parser, '=')) {
        expr = parse_expr(parser);
    }
    return (struct ast_enum_item) {name, expr};
}

struct ast_decl *parse_decl_enum(struct parser *parser, struct token identifier)
{
    parser_consume(parser, '{');
    struct ast_enum_item *items = NULL;
    if (!parser_check(parser, '}')) {
        buf_push(items, parse_decl_enum_item(parser));
        while (parser_match(parser, ',')) {
            buf_push(items, parse_decl_enum_item(parser));
        }
    }
    parser_consume(parser, '}');
    return ast_decl_enum(identifier.as.name, items, buf_len(items));
}

struct ast_func_param parse_decl_func_param(struct parser *parser)
{
    const uint8_t *name = parse_name(parser);
    parser_consume(parser, ':');
    struct ast_typespec *type = parse_type(parser);
    return (struct ast_func_param) {name, type};
}

struct ast_decl *parse_decl_func(struct parser *parser, struct token identifier)
{
    struct ast_func_param *params = NULL;
    if (!parser_check(parser, ')')) {
        buf_push(params, parse_decl_func_param(parser));
        while (parser_match(parser, ',')) {
            buf_push(params, parse_decl_func_param(parser));
        }
    }
    parser_consume(parser, ')');

    struct ast_typespec *ret = NULL;
    if (parser_match(parser, TOKEN_KIND_ARROW)) {
        ret = parse_type(parser);
    }

    if (parser_match(parser, TOKEN_KIND_FOREIGN)) {
        parser_consume(parser, TOKEN_KIND_STRING_LITERAL);
        const uint8_t *lib = parser_previous(parser).as.name;
        parser_consume(parser, ';');
        return ast_decl_func_foreign(identifier.as.name, params, buf_len(params), ret, lib);
    }

    struct ast_stmt_block block = parse_stmt_block(parser);
    return ast_decl_func(identifier.as.name, params, buf_len(params), ret, block);
}

struct ast_decl *parse_decl_const(struct parser *parser, struct token identifier)
{
    struct ast_expr *expr = parse_expr(parser);
    parser_consume(parser, ';');
    return ast_decl_const(identifier.as.name, expr);
}

struct ast_decl *parse_decl_var_infer(struct parser *parser, struct token identifier)
{
    struct ast_expr *expr = parse_expr(parser);
    parser_consume(parser, ';');
    return ast_decl_var(identifier.as.name, NULL, expr);
}

struct ast_decl *parse_decl_var(struct parser *parser, struct token identifier)
{
    struct ast_typespec *type = parse_type(parser);
    struct ast_expr *expr = NULL;
    if (parser_match(parser, '=')) {
        expr = parse_expr(parser);
    }
    parser_consume(parser, ';');
    return ast_decl_var(identifier.as.name, type, expr);
}

struct ast_decl *parse_decl(struct resolver *resolver)
{
    struct parser *parser = &resolver->parser;

    while (parser_match(parser, TOKEN_KIND_LOAD)) {
        parser_consume(parser, TOKEN_KIND_STRING_LITERAL);
        struct token at_token = parser_previous(parser);
        uint8_t *filename = at_token.as.name;
        parser_consume(parser, ';');

        uint8_t *directory = file_directory(parser->lexer.file);
        size_t directory_length = length_of_string(directory);
        size_t filename_length = length_of_string(filename);
        size_t file_length = directory_length + filename_length + 2;

        uint8_t *file = malloc(file_length * sizeof(uint8_t));
        snprintf((char*)file, file_length, "%s/%s", directory, filename);

        if (!file_exists(file)) {
            parser_fatal(parser, at_token, "\e[1;31merror:\e[0m file not found: '%s'\n", file);
        }

        buf_push(resolver->files, file);
        return NULL;
    }

    parser_consume(parser, TOKEN_KIND_IDENTIFIER);
    struct token identifier = parser_previous(parser);

    if (parser_match(parser, TOKEN_KIND_CONST_ASSIGN)) {
        if (parser_match_keyword(parser, struct_keyword)) {
            return parse_decl_struct(parser, identifier);
        } else if (parser_match_keyword(parser, enum_keyword)) {
            return parse_decl_enum(parser, identifier);
        } else if (parser_match(parser, '(')) {
            return parse_decl_func(parser, identifier);
        } else {
            return parse_decl_const(parser, identifier);
        }
    } else if (parser_match(parser, TOKEN_KIND_COLON_ASSIGN)) {
        return parse_decl_var_infer(parser, identifier);
    } else if (parser_match(parser, ':')) {
        return parse_decl_var(parser, identifier);
    } else {
        uint8_t at_token_value[255];
        struct token at_token = parser_peek(parser);
        lexer_print_token(at_token, at_token_value, sizeof(at_token_value));
        parser_fatal(parser, at_token, "expected token '::', ':=' or ':', but got '%s'\n", at_token_value);
        return NULL;
    }
}

bool parser_match_keyword(struct parser *parser, const uint8_t *keyword)
{
    if (parser_check(parser, TOKEN_KIND_IDENTIFIER)) {
        struct token identifier = parser_peek(parser);
        bool result = identifier.as.name == keyword;
        if (result) parser_advance(parser);
        return result;
    }
    return false;
}

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

    parser_fatal(parser, at_token, "expected token '%s', but got '%s'\n", token_kind_str[kind], at_token_value);
}

bool parser_eof(struct parser *parser)
{
    struct token token = parser_peek(parser);
    return token.kind == TOKEN_KIND_EOF;
}

void parser_fatal(struct parser *parser, struct token token, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    printf("\e[1;32m%s\e[0m \e[1;34m#%d:%d\e[0m ", parser->lexer.file, token.line, token.column);
    vprintf(format, args);
    va_end(args);
    exit(1);
}

void parser_init(struct parser *parser, uint8_t *file)
{
    memset(parser, 0, sizeof(struct parser));
    parser_init_keywords();
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
