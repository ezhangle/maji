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
is_alpha(char c)
{
    bool result = ((c >= 'a' && c <= 'z') ||
                   (c >= 'A' && c <= 'Z'));
    return result;

}

static inline bool
is_digit(char c)
{
    bool result = (c >= '0' && c <= '9');
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

#endif
