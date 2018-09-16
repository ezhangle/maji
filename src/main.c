#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "lexer.h"
#include "lexer.c"

#include "ast.h"

#include "parser.h"
#include "parser.c"

static void ast_print_test(void)
{
    struct ast_expr *exprs[] = {
        ast_expr_binary('+', ast_expr_int(1), ast_expr_int(2)),
        ast_expr_unary('-', ast_expr_float(3.14)),
        ast_expr_ternary(ast_expr_identifier(u8"flag"), ast_expr_string(u8"true"), ast_expr_string(u8"false")),
        ast_expr_field(ast_expr_identifier(u8"input"), u8"type"),
        ast_expr_call(ast_expr_identifier(u8"fact"), (struct ast_expr*[]){ast_expr_int(42)}, 1),
        ast_expr_index(ast_expr_field(ast_expr_identifier(u8"input"), u8"events"), ast_expr_int(3)),
        ast_expr_cast(ast_typespec_pointer(ast_typespec_identifier(u8"int")), ast_expr_identifier(u8"void_ptr"))
    };

    for (struct ast_expr **it = exprs; it != exprs + sizeof(exprs)/sizeof(*exprs); it++) {
        ast_print_expr(*it);
        printf("\n");
    }

    struct ast_stmt *stmts[] = {
        ast_stmt_return(ast_expr_int(42)),
        ast_stmt_break(),
        ast_stmt_continue(),
        ast_stmt_block(
            (struct ast_stmt_block){
                (struct ast_stmt*[]){
                    ast_stmt_break(),
                    ast_stmt_continue()
                },
                2,
             }
        ),
        ast_stmt_expr(ast_expr_call(ast_expr_identifier(u8"print"), (struct ast_expr*[]){ast_expr_int(1), ast_expr_int(2)}, 2)),
        ast_stmt_init(u8"x", ast_expr_int(42)),
        ast_stmt_if(
            ast_expr_identifier(u8"flag1"),
            (struct ast_stmt_block){
                (struct ast_stmt*[]){
                    ast_stmt_return(ast_expr_int(1))
                },
                1,
            },
            (struct ast_else_if[]){
                {
                    ast_expr_identifier(u8"flag2"),
                    (struct ast_stmt_block){
                        (struct ast_stmt*[]){
                            ast_stmt_return(ast_expr_int(2))
                        },
                        1,
                    }
                }
            },
            1,
            (struct ast_stmt_block){
                (struct ast_stmt*[]){
                    ast_stmt_return(ast_expr_int(3))
                },
                1,
            }
        ),
        ast_stmt_while(
            ast_expr_identifier(u8"running"),
            (struct ast_stmt_block){
                (struct ast_stmt*[]){
                    ast_stmt_assign(TOKEN_KIND_ADD_ASSIGN, ast_expr_identifier(u8"i"), ast_expr_int(16)),
                },
                1,
            }
        ),
    };

    for (struct ast_stmt **it = stmts; it != stmts + sizeof(stmts)/sizeof(*stmts); it++) {
        ast_print_stmt(*it);
        printf("\n");
    }
}

int main(int argc, char **argv)
{
#if 0
    uint8_t *string = u8"1243 - 0x3ff * 0b111 とあ 0b11 + 0b1001 - 'b' * 2.555"
                        "// line comment\n"
                        "'\\n'\n"
                        "'\\\''\n"
                        "/* this is\n"
                        "a\n"
                        "    /* nested block */\n"
                        " comment */\n"
                        "''\n"
                        "0x2 + (5 * 4 / 0b10) - 0b1 + 0o10 + \"hello, \\nworld!\"\n";

    struct parser parser = {
        .lexer = (struct lexer) {
            .buffer = string,
            .at = string,
            .column = 1,
            .line = 1
        },
        .tokens = NULL,
        .current_token = 0,
    };
    buf_push(parser.tokens, lexer_get_token(&parser.lexer));

    while (!parser_eof(&parser)) {
        struct token token = parser_advance(&parser);
        uint8_t token_value[255];
        lexer_print_token(token, token_value, sizeof(token_value));
        printf("got token '%s'\n", token_value);
    }
#endif

    ast_print_test();
    return 0;
}
