#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "lexer.h"
#include "lexer.c"

#include "parser.h"
#include "parser.c"

#include "parse.h"
#include "parse.c"

#if 0
int main(int argc, char **argv)
{
    uint8_t *string = u8"1243 - 0x3ff * 0b111 とあ 0b11 + 0b1001";
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
        printf("got token of type %s with value %s\n", token_kind_str[token.kind], token_value);
    }

    return 0;
}
#else
int main(int argc, char **argv)
{
    if (argc == 2) {
        parse_file((uint8_t*)argv[1]);
    } else {
        parse_string(u8"10 - (0x6 * 0b111 / 0b10 + 0b1001)");
    }
    return 0;
}
#endif
