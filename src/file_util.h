#ifndef MAJI_FILE_UTIL_H
#define MAJI_FILE_UTIL_H

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/stat.h>

static inline bool
file_exists(const uint8_t *file)
{
    struct stat sb;
    bool result = stat((char*)file, &sb) == 0;
    return result;
}

static inline uint8_t *
read_file(const uint8_t *file)
{
    if (!file_exists(file)) return NULL;

    FILE *handle = fopen((char*)file, "rb");
    if (!handle) return NULL;

    fseek(handle, 0, SEEK_END);
    size_t length = ftell(handle);
    fseek(handle, 0, SEEK_SET);

    uint8_t *result = malloc(length + 1);
    fread(result, length, 1, handle);
    result[length] = '\0';

    fclose(handle);
    return result;
}

#endif
