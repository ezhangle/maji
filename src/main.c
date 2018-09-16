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

struct parser init_stream(const uint8_t *stream)
{
    struct parser parser = {
        .lexer = (struct lexer) {
            .buffer = (uint8_t*)stream,
            .at = (uint8_t*)stream,
            .column = 1,
            .line = 1
        },
        .tokens = NULL,
        .current_token = 0,
    };
    buf_push(parser.tokens, lexer_get_token(&parser.lexer));
    return parser;
}

void ast_parse_test(void)
{
    parser_init_keywords();
    const uint8_t *tests[] = {
        u8"x := b == 1 ? 1+2 : 3-4;",
        u8"Vector :: struct { x: float; y: float; }",
        u8"Color :: enum { RED = 3, GREEN, BLUE = 0 }",
        u8"Words :: enum { }",
        u8"fact :: (n: int) -> int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }",
        u8"fact :: (n: int) -> int { p := 1; for (i := 1; i <= n; ++i) { p *= i; } return p; }",
        u8"pi :: 3.14;",
        u8"v: int = 1;",
        u8"foo :: () { while (is_running) { printf(\"heh\"); } }",
        u8"w := 2;",
        u8"foo := a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0;",
        u8"main :: (argc: int, argv: int*[]) -> int { { str :: \"hello, world\"; len := strlen(str); } }",
    };
    for (const uint8_t **it = tests; it != tests + sizeof(tests)/sizeof(*tests); it++) {
        struct parser parser = init_stream(*it);
        struct ast_decl *decl = parse_decl(&parser);
        ast_print_decl(decl);
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

    // ast_print_test();
    ast_parse_test();

    return 0;
}
