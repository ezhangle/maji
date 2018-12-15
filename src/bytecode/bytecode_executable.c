#include "bytecode_executable.h"

#include <stdio.h>
#include <stdlib.h>

bool bytecode_write_executable(const char *absolutepath, struct bytecode_executable *exe)
{
    FILE *handle = fopen(absolutepath, "wb");
    if (!handle) return false;

    fwrite(exe->header, sizeof(struct bytecode_header), 1, handle);
    fwrite(exe->data_segment, exe->header->data_size, 1, handle);
    fwrite(exe->text_segment, exe->header->text_size, 1, handle);

    fclose(handle);
    return true;
}

static inline bool
bytecode_verify_executable(struct bytecode_header *header)
{
    bool result = ((header->magic[0] == 'b') &&
                   (header->magic[1] == 'c') &&
                   (header->magic[2] == 'r'));
    return result;
}

bool bytecode_load_executable(const char *absolutepath, struct bytecode_executable *exe)
{
    FILE *handle = fopen(absolutepath, "rb");
    if (!handle) return false;

    fseek(handle, 0, SEEK_END);
    unsigned length = ftell(handle);
    fseek(handle, 0, SEEK_SET);

    char *contents = malloc(length + 1);
    fread(contents, length, 1, handle);
    contents[length] = '\0';

    struct bytecode_header *header = (struct bytecode_header *) contents;
    if (!bytecode_verify_executable(header)) return false;

    exe->header = header;
    exe->data_segment = contents + sizeof(struct bytecode_header);
    exe->text_segment = (uint64_t *)((char *)exe->data_segment + exe->header->data_size);

    return true;
}
