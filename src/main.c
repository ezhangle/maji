#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "lexer.h"
#include "lexer.c"

#include "ast.h"

#include "parser.h"
#include "parser.c"

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
    ast_parse_test();
    return 0;
}
