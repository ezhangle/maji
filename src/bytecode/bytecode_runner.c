#include "bytecode_runner.h"
#include "bytecode_opcode.h"

#include "bytecode_instruction.h"
#include "bytecode_instruction.c"

#include "bytecode_instruction_handler.h"
#include "bytecode_instruction_handler.c"

#include "bytecode_disassembler.c"

#include "bytecode_executable.h"
#include "bytecode_executable.c"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <inttypes.h>
#include <time.h>

void bytecode_runner_init(struct bytecode_runner *bcr, struct bytecode_executable *program)
{
    bcr->compare = 0;
    bcr->cycle_count = 0;

    bcr->text = program->text_segment;
    bcr->text_size = program->header->text_size;

    bcr->data = program->data_segment;
    bcr->data_size = program->header->data_size;

    bcr->type = program->type_segment;
    bcr->type_size = program->header->type_size;

    bcr->stack_size = program->header->stack_size;
    bcr->stack = malloc(bcr->stack_size);
    memset(bcr->stack, 0, bcr->stack_size);

    bcr->stack_info_size = program->header->stack_size;
    bcr->stack_info = malloc(bcr->stack_info_size);
    memset(bcr->stack_info, 0, bcr->stack_info_size);

    memset(bcr->reg, 0, sizeof(uint64_t) * BYTECODE_REGISTER_COUNT);
    memset(bcr->reg_type, BYTECODE_REGISTER_KIND_I64, sizeof(uint64_t) * BYTECODE_REGISTER_COUNT);
}

void bytecode_runner_destroy(struct bytecode_runner *bcr)
{
    free(bcr->stack);
    memset(bcr, 0, sizeof(struct bytecode_runner));
}

void bytecode_disassembler_run(struct bytecode_runner *bcr)
{
    uint64_t eof = bcr->text_size / sizeof(*bcr->text);
    while (bcr->reg[BYTECODE_REGISTER_RIP] < eof) {
        uint64_t raw_instr = fetch_instruction(bcr);
        struct bytecode_instruction instr = decode_instruction(raw_instr);
        disassemble_instruction(bcr, instr);
    }
}

struct bytecode_result bytecode_runner_run(struct bytecode_runner *bcr)
{
    clock_t timed_block_begin = clock();

    bcr->is_running = true;
    while (bcr->is_running) {
        bytecode_runner_print_registers(bcr);
        bytecode_runner_print_stack(bcr);

        uint64_t raw_instr = fetch_instruction(bcr);
        struct bytecode_instruction instr = decode_instruction(raw_instr);
        bytecode_runner_print_instruction(bcr, &instr);

#ifdef OPCODE_PROFILE
        clock_t timed_block_begin = clock();
        bytecode_instruction_execute(bcr, instr);
        clock_t timed_block_end = clock();
        double timed_block_elapsed = ((timed_block_end - timed_block_begin) / (double)CLOCKS_PER_SEC) * 1000.0f;
        printf("instruction %s took %.4fms\n", bytecode_opcode_str[instr.op], timed_block_elapsed);
        if (bcr->single_step) getchar();
#else
        bytecode_instruction_execute(bcr, instr);
        if (bcr->single_step) getchar();
#endif
    }

    clock_t timed_block_end = clock();
    double timed_block_elapsed = ((timed_block_end - timed_block_begin) / (double)CLOCKS_PER_SEC) * 1000.0f;
    printf("program exited after: %.4fms\n", timed_block_elapsed);

    struct bytecode_result result = {
        .kind = bcr->reg_type[BYTECODE_REGISTER_RAX],
        .i64 = bcr->reg[BYTECODE_REGISTER_RAX]
    };
    return result;
}

void bytecode_runner_print_registers(struct bytecode_runner *bcr)
{
    if (!bcr->verbose) return;

    static int num_col = 4;

    for (int i = 1; i <= BYTECODE_REGISTER_COUNT; ++i) {
        printf("%-3s %12s ", bytecode_register_kind_str[bcr->reg_type[i - 1]], bytecode_register_str[i - 1]);
        if ((i % num_col) == 0) {
            printf("\n");
            for (int j = 1; j <= num_col; ++j) {
                printf("%016" PRIx64 " ", bcr->reg[i - 1 - num_col + j]);
            }
            printf("\n");
        }

        if (i == BYTECODE_REGISTER_COUNT) {
            int rem = i % num_col;
            printf("\n");
            for (int j = 1; j <= rem; ++j) {
                printf("%016" PRIx64 " ", bcr->reg[i - 1 - rem + j]);
            }
            printf("\n");
        }
    }
}

void bytecode_runner_print_stack(struct bytecode_runner *bcr)
{
    if (!bcr->verbose) return;

    printf("compare: %d\n", bcr->compare);

    printf("stack [\n");
    for (int i = 0; i < bcr->reg[BYTECODE_REGISTER_RSP]; ++i) {
        printf("%.2X ", (unsigned char)bcr->stack[i]);

        if (((i+1) % 4) == 0) {
            printf("  ");
        }

        if (((i+1) % 20) == 0) {
            printf("\n");
        }
    }
    printf("]\n");
}

void bytecode_runner_print_instruction(struct bytecode_runner *bcr, struct bytecode_instruction *instr)
{
    if (!bcr->verbose) return;

    printf("cycle %3" PRIu64 ": op = %-15s r1 = %s, r2 = %s, imm = %" PRIu64 ", r1type = %s, r2type = %s\n",
            ++bcr->cycle_count,
            bytecode_opcode_str[instr->op],
            bytecode_register_str[instr->r1],
            bytecode_register_str[instr->r2],
            bcr->text[bcr->reg[BYTECODE_REGISTER_RIP]],
            bytecode_register_kind_str[bcr->reg_type[instr->r1]],
            bytecode_register_kind_str[bcr->reg_type[instr->r2]]);
}

static int bcr_sample_exe = 1;
static const char *program;
void parse_arguments(int argc, char **argv, struct bytecode_runner *bcr)
{
    const char *short_opt = "vds:p:D";
    struct option long_opt[] = {
        { "verbose", no_argument, NULL, 'v' },
        { "disasm", no_argument, NULL, 'D' },
        { "debug", no_argument, NULL, 'd' },
        { "sample", required_argument, NULL, 's' },
        { "program", required_argument, NULL, 'p' },
        { NULL, 0, NULL, 0 }
    };

    int option;
    while ((option = getopt_long(argc, argv, short_opt, long_opt, NULL)) != -1) {
        switch (option) {
        case 'v': {
            bcr->verbose = true;
        } break;
        case 'd': {
            bcr->verbose = true;
            bcr->single_step = true;
        } break;
        case 'D': {
            bcr->disassemble = true;
        } break;
        case 's': {
            sscanf(optarg, "%d", &bcr_sample_exe);
        } break;
        case 'p': {
            program = strdup(optarg);
        }
        }
    }
}

int main(int argc, char **argv)
{
    struct bytecode_runner bcr = {};
    parse_arguments(argc, argv, &bcr);

    char exe_path[255] = {};
    if (program == NULL) {
        snprintf(exe_path, sizeof(exe_path), "./samples/%d/sample.bcr", bcr_sample_exe);
    } else {
        printf("loading program '%s'..\n", program);
        memcpy(exe_path, program, strlen(program)+1);
    }

    struct bytecode_result result = {};
    struct bytecode_executable executable;
    if (bytecode_load_executable(exe_path, &executable)) {
        bytecode_runner_init(&bcr, &executable);

        if (bcr.disassemble) {
            bytecode_disassembler_run(&bcr);
        } else {
            result = bytecode_runner_run(&bcr);
        }

        bytecode_runner_destroy(&bcr);
        return result.i32;
    }

    return EXIT_FAILURE;
}
