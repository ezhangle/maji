#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "lexer.h"
#include "lexer.c"

int main(int argc, char **argv)
{
    uint8_t *buffer = u8"1243 - 0x3ff * 0b111 とあ 0b11 + 0b1001";

    struct lexer lexer = {
        .buffer = buffer,
        .at = buffer,
        .column = 1,
        .line = 1
    };

    for (struct token token = lexer_get_token(&lexer);
        token.kind != TOKEN_KIND_EOF;
        token = lexer_get_token(&lexer)) {
        uint8_t token_value[255];
        lexer_print_token(token, token_value, sizeof(token_value));
        printf("got token of type %s with value %s\n", token_kind_str[token.kind], token_value);
    }

    return 0;
}
