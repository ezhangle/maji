#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "lexer.h"
#include "lexer.c"

#include "ast.h"

#include "parser.h"
#include "parser.c"

int main(int argc, char **argv)
{
    uint8_t *string = u8"1243 - 0x3ff * 0b111 とあ 0b11 + 0b1001 - 'b'"
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

    ast_print_expr(ast_expr_binary('+', ast_expr_int(2), ast_expr_binary('*', ast_expr_int(5), ast_expr_int(4))));
    printf("\n");
    ast_print_expr(ast_expr_cast(ast_typespec_pointer(ast_typespec_identifier(u8"int")), ast_expr_int(0xff0321b)));
    printf("\n");
    ast_print_expr(ast_expr_index(ast_expr_field(ast_expr_identifier(u8"input"), u8"events"), ast_expr_int(2)));
    printf("\n");

    return 0;
}
