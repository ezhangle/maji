#ifndef MAJI_STRING_UTIL_H
#define MAJI_STRING_UTIL_H

#include "buffer.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

#define u8 (uint8_t*)

struct interned_string
{
    size_t length;
    uint8_t *string;
};

static struct interned_string *interned_strings;

static inline size_t
length_of_string(const uint8_t *a)
{
    size_t length = strlen((char*)a);
    return length;
}

static inline uint8_t *
copy_string_count(const uint8_t *a, size_t length)
{
    uint8_t *result = malloc(length + 1);
    memcpy(result, a, length);
    result[length] = '\0';
    return result;
}

static inline uint8_t *
copy_string(const uint8_t *a)
{
    size_t length = length_of_string(a);
    uint8_t *result = copy_string_count(a, length);
    return result;
}

static inline bool
same_string(const uint8_t *a, const uint8_t *b)
{
    bool result = a && b && (strcmp((char*)a, (char*)b) == 0);
    return result;
}

static uint8_t escape_char_map[] =
{
    ['"'] = '"',

    ['\\'] = '\\',

    ['0'] = '\0',
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['f'] = '\f',
};

static inline uint8_t *
resolve_string_count(const uint8_t *a, size_t length)
{
    uint8_t *result = malloc(length + 1);
    uint8_t *cursor = result;

    for (size_t i = 0; i < length; ++i) {
        if (a[i] == '\\') {
            *cursor++ = escape_char_map[a[++i]];
        } else {
            *cursor++ = a[i];
        }
    }

    *cursor = '\0';
    return result;
}

static inline uint8_t *
resolve_string(const uint8_t *a)
{
    size_t length = length_of_string(a);
    uint8_t *result = resolve_string_count(a, length);
    return result;
}

static inline uint8_t *
intern_string_count(const uint8_t *string, size_t length)
{
    for (int i = 0; i < buf_len(interned_strings); ++i) {
        if ((interned_strings[i].length == length) &&
            (strncmp((char*)interned_strings[i].string, (char*)string, length) == 0)) {
            return interned_strings[i].string;
        }
    }

    uint8_t *i_string = copy_string_count(string, length);
    buf_push(interned_strings, ((struct interned_string) {
        .length = length,
        .string = i_string
    }));
    return i_string;
}

static inline uint8_t *
intern_string(const uint8_t *a)
{
    size_t length = length_of_string(a);
    uint8_t *result = intern_string_count(a, length);
    return result;
}

#endif
