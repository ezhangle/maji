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
    uint8_t *string = u8"1243 - 0x3ff * 0b111 とあ 0b11 + 0b1001"
                        "// line comment\n"
                        "/* this is\n"
                        "a\n"
                        "    /* nested block */\n"
                        " comment */"
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

    return 0;
}
