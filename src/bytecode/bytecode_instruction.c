#include "bytecode_instruction.h"
#include "bytecode_instruction_handler.h"
#include "bytecode_runner.h"
#include "bytecode_opcode.h"

#include <stdio.h>

uint64_t fetch_instruction(struct bytecode_runner *bcr)
{
    return bcr->text[bcr->reg[BYTECODE_REGISTER_RIP]++];
}

struct bytecode_instruction decode_instruction(uint64_t raw_instr)
{
    struct bytecode_instruction instr;
    instr.op = (raw_instr & 0xff00000000000000) >> 56;
    instr.r1 = (raw_instr & 0x00ff000000000000) >> 48;
    instr.r2 = (raw_instr & 0x0000ff0000000000) >> 40;
    return instr;
}

uint64_t encode_instruction(uint8_t instr)
{
    uint64_t result = (uint64_t)instr << 56;
    return result;
}

uint64_t encode_instruction_r1(uint8_t instr, uint8_t r1)
{
    uint64_t result = ((uint64_t)instr << 56) | ((uint64_t)r1 << 48);
    return result;
}

uint64_t encode_instruction_r2(uint8_t instr, uint8_t r1, uint8_t r2)
{
    uint64_t result = ((uint64_t)instr << 56) | ((uint64_t)r1 << 48) | ((uint64_t)r2 << 40);
    return result;
}
