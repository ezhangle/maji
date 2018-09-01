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

int main(int argc, char **argv)
{
    // uint8_t *string = u8"1243 - 0x3ff * 0b111 とあ 0b11 + 0b1001";
    uint8_t *string = u8"10 - (0x6 * 0b111 / 0b10 + 0b1001)";
    parse_string(string);
    return 0;
}
