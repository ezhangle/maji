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

bool parser_match_keyword(struct parser *parser, const uint8_t *keyword);
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

//
//
//

void parser_init_keywords(void);

struct ast_expr *parse_expr_operand(struct parser *parser);
struct ast_expr *parse_expr_base(struct parser *parser);
struct ast_expr *parse_expr_unary(struct parser *parser);
struct ast_expr *parse_expr_factor(struct parser *parser);
struct ast_expr *parse_expr_term(struct parser *parser);
struct ast_expr *parse_expr_cmp(struct parser *parser);
struct ast_expr *parse_expr_and(struct parser *parser);
struct ast_expr *parse_expr_or(struct parser *parser);
struct ast_expr *parse_expr_ternary(struct parser *parser);
struct ast_expr *parse_expr(struct parser *parser);
struct ast_expr *parse_expr_paren(struct parser *parser);

struct ast_stmt *parse_stmt_base(struct parser *parser);
struct ast_stmt *parse_stmt_if(struct parser *parser);
struct ast_stmt *parse_stmt_for(struct parser *parser);
struct ast_stmt *parse_stmt_while(struct parser *parser);
struct ast_stmt *parse_stmt(struct parser *parser);
struct ast_stmt_block parse_stmt_block(struct parser *parser);

const uint8_t *parse_name(struct parser *parser);
struct ast_typespec *parse_type_func(struct parser *parser);
struct ast_typespec *parse_type_base(struct parser *parser);
struct ast_typespec *parse_type(struct parser *parser);

struct ast_struct_item parse_decl_struct_item(struct parser *parser);
struct ast_decl *parse_decl_struct(struct parser *parser, struct token identifier);
struct ast_enum_item parse_decl_enum_item(struct parser *parser);
struct ast_decl *parse_decl_enum(struct parser *parser, struct token identifier);
struct ast_func_param parse_decl_func_param(struct parser *parser);
struct ast_decl *parse_decl_func(struct parser *parser, struct token identifier);
struct ast_decl *parse_decl_const(struct parser *parser, struct token identifier);
struct ast_decl *parse_decl_var_infer(struct parser *parser, struct token identifier);
struct ast_decl *parse_decl_var(struct parser *parser, struct token identifier);
struct ast_decl *parse_decl(struct parser *parser);

#endif
