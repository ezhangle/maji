#ifndef MAJI_UTF8_UTIL_H
#define MAJI_UTF8_UTIL_H

#include <stdint.h>
typedef uint32_t utf8_char;

static inline utf8_char
utf8_get_char_and_bytes(uint8_t *str, int *num_bytes)
{
    utf8_char uc = *str++;
    if (uc < 0x80) {
        *num_bytes = 1;
        return uc;
    }

    if (uc < 0xe0) {
        *num_bytes = 2;
        return ((uc & 0x1f) << 6) | (*str & 0x3f);
    }

    if (uc < 0xf0) {
        *num_bytes = 3;
        return ((uc & 0xf) << 12) | ((str[0] & 0x3f) << 6) | (str[1] & 0x3f);
    }

    *num_bytes = 4;
    return ((uc & 0x7) << 18) | ((str[0] & 0x3f) << 12) | ((str[1] & 0x3f) << 6) | (str[2] & 0x3f);
}

static inline utf8_char
utf8_get_char(uint8_t *str)
{
    int num_bytes;
    return utf8_get_char_and_bytes(str, &num_bytes);
}

#endif
