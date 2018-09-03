#ifndef MAJI_CHAR_UTIL_H
#define MAJI_CHAR_UTIL_H

#include <stdbool.h>

static inline bool
is_whitespace(char c)
{
    bool result = ((c == ' ') ||
                   (c == '\t') ||
                   (c == '\v') ||
                   (c == '\f') ||
                   (c == '\n') ||
                   (c == '\r'));
    return result;
}

static inline bool
is_decimal(char c)
{
    bool result = (c >= '0' && c <= '9');
    return result;
}

static inline bool
is_octal(char c)
{
    bool result = (c >= '0' && c <= '7');
    return result;
}

static inline bool
is_hexadecimal(char c)
{
    bool result = ((c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F'));
    return result;
}

static inline bool
is_binary(char c)
{
    bool result = (c >= '0' && c <= '1');
    return result;
}

static inline bool
is_alpha(char c)
{
    bool result = ((c >= 'a' && c <= 'z') ||
                   (c >= 'A' && c <= 'Z'));
    return result;
}

static inline bool
is_utf8(unsigned char c)
{
    bool result = (c >= 0xC0);
    return result;
}

static inline bool
is_alpha_num(char c)
{
    bool result = ((is_alpha(c)) ||
                   (is_decimal(c)));
    return result;
}

static inline bool
is_identifier(char c)
{
    bool result = (is_alpha_num(c)) ||
                  (is_utf8(c)) ||
                  (c == '_');
    return result;
}

#endif
