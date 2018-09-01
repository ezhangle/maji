#ifndef MAJI_PARSE_H
#define MAJI_PARSE_H

#include "parser.h"

int parse_primary(struct parser *parser);
int parse_unary(struct parser *parser);
int parse_factor(struct parser *parser);
int parse_term(struct parser *parser);
int parse_expression(struct parser *parser);

void parse_string(uint8_t *string);

#endif
