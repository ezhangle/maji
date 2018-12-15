#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <getopt.h>
#include <string.h>

#include "lexer.h"
#include "parser.h"
#include "resolve.h"
#include "ast.h"

#include "lexer.c"
#include "parser.c"
#include "resolve.c"

#include "bytecode/bytecode_generator.c"

void parse_arguments(int argc, char **argv, struct compiler_options *options)
{
    int option;
    const char *short_options = "o:";
    struct option long_options[] = {
        { NULL, 0, NULL, 0 }
    };

    while ((option = getopt_long(argc, argv, short_options, long_options, NULL)) != -1) {
        switch (option) {
        case 'o': {
            options->output_file = copy_string((uint8_t*)optarg);
        } break;
        }
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    uint8_t *source_file = intern_string((uint8_t*)argv[1]);
    if (!file_exists(source_file)) {
        printf("file '%s' does not exist\n", argv[1]);
        return EXIT_FAILURE;
    }

    struct compiler_options options = {};
    parse_arguments(argc, argv, &options);

    struct resolver resolver;
    resolver_init(&resolver);
    buf_push(resolver.files, source_file);

    printf("+-----------------+\n");
    printf("| parsing files.. |\n");
    printf("+-----------------+\n");
    for (int i = 0; i < buf_len(resolver.files); ++i) {
        uint8_t *file = resolver.files[i];
        printf("| + %s\n", file);

        parser_init(&resolver.parser, file);
        while (!parser_eof(&resolver.parser)) {
            struct ast_decl *decl = parse_decl(&resolver);
            if (!decl) continue;

            buf_push(resolver.decls, decl);
            resolve_decl(&resolver, decl);
        }
        parser_destroy(&resolver.parser);
    }

    printf("\n+---------------------+\n");
    printf("| resolving symbols.. |\n");
    printf("+---------------------+\n");
    for (int i = 0; i < buf_len(resolver.symbols); ++i) {
        struct symbol *it = resolver.symbols[i];
        complete_symbol(&resolver, it);
    }

    /*
    for (int i = 0; i < buf_len(resolver.ordered_symbols); ++i) {
        struct symbol *it = resolver.ordered_symbols[i];
        if (it->decl) {
            ast_print_decl(it->decl);
        } else {
            printf("%s", it->name);
        }
        printf("\n");
    }
    */

    printf("\n+---------------------+\n");
    printf("| emitting bytecode.. |\n");
    printf("+---------------------+\n");
    bytecode_generate(&resolver, &options);

    return EXIT_SUCCESS;
}
