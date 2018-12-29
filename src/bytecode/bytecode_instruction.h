#ifndef BYTECODE_INSTRUCTION_H
#define BYTECODE_INSTRUCTION_H

#define bytecode_instruction_handler_(name) void name(struct bytecode_runner *bcr, uint8_t op, uint8_t reg1, uint8_t reg2)
typedef bytecode_instruction_handler_(bytecode_instruction_handler);

struct bytecode_runner;

struct bytecode_instruction
{
    uint8_t op;
    uint8_t r1;
    uint8_t r2;
};

uint64_t fetch_instruction(struct bytecode_runner *bcr);
struct bytecode_instruction decode_instruction(uint64_t raw_instr);
uint64_t encode_instruction(uint8_t instr);
uint64_t encode_instruction_r1(uint8_t instr, uint8_t r1);
uint64_t encode_instruction_r2(uint8_t instr, uint8_t r1, uint8_t r2);

#define mov_i8_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT8_REG_IMM, reg), val
#define mov_i16_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT16_REG_IMM, reg), val
#define mov_i32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT32_REG_IMM, reg), val
#define mov_i64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT64_REG_IMM, reg), val
#define mov_f32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MOV_FLT32_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define mov_f64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MOV_FLT64_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define mov_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MOV_REG_REG, reg1, reg2)
#define mov_i8_lcl_imm(offset, val) encode_instruction(BYTECODE_OPCODE_MOV_INT8_LCL_IMM), offset, val
#define mov_i16_lcl_imm(offset, val) encode_instruction(BYTECODE_OPCODE_MOV_INT16_LCL_IMM), offset, val
#define mov_i32_lcl_imm(offset, val) encode_instruction(BYTECODE_OPCODE_MOV_INT32_LCL_IMM), offset, val
#define mov_i64_lcl_imm(offset, val) encode_instruction(BYTECODE_OPCODE_MOV_INT64_LCL_IMM), offset, val
#define mov_f32_lcl_imm(offset, val) encode_instruction(BYTECODE_OPCODE_MOV_FLT32_LCL_IMM), offset, (uint64_t)(*(uint64_t *)&val)
#define mov_f64_lcl_imm(offset, val) encode_instruction(BYTECODE_OPCODE_MOV_FLT64_LCL_IMM), offset, (uint64_t)(*(uint64_t *)&val)
#define mov_lcl_reg(offset, reg) encode_instruction_r1(BYTECODE_OPCODE_MOV_LCL_REG, reg), offset
#define mov_i8_reg_lcl(reg, offset) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT8_REG_LCL, reg), offset
#define mov_i16_reg_lcl(reg, offset) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT16_REG_LCL, reg), offset
#define mov_i32_reg_lcl(reg, offset) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT32_REG_LCL, reg), offset
#define mov_i64_reg_lcl(reg, offset) encode_instruction_r1(BYTECODE_OPCODE_MOV_INT64_REG_LCL, reg), offset
#define mov_f32_reg_lcl(reg, offset) encode_instruction_r1(BYTECODE_OPCODE_MOV_FLT32_REG_LCL, reg), offset
#define mov_f64_reg_lcl(reg, offset) encode_instruction_r1(BYTECODE_OPCODE_MOV_FLT64_REG_LCL, reg), offset

#define push_i8_imm(val) encode_instruction(BYTECODE_OPCODE_PUSH_INT8_IMM), val
#define push_i16_imm(val) encode_instruction(BYTECODE_OPCODE_PUSH_INT16_IMM), val
#define push_i32_imm(val) encode_instruction(BYTECODE_OPCODE_PUSH_INT32_IMM), val
#define push_i64_imm(val) encode_instruction(BYTECODE_OPCODE_PUSH_INT64_IMM), val
#define push_f32_imm(val) encode_instruction(BYTECODE_OPCODE_PUSH_FLT32_IMM), (uint64_t)(*(uint64_t *)&val)
#define push_f64_imm(val) encode_instruction(BYTECODE_OPCODE_PUSH_FLT64_IMM), (uint64_t)(*(uint64_t *)&val)
#define push_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_PUSH_REG, reg)

#define pop_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_REG, reg)
#define pop_i8_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_INT8_REG, reg)
#define pop_i16_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_INT16_REG, reg)
#define pop_i32_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_INT32_REG, reg)
#define pop_i64_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_INT64_REG, reg)
#define pop_f32_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_FLT32_REG, reg)
#define pop_f64_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_POP_FLT64_REG, reg)

#define add_i8_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_ADD_INT8_REG_IMM, reg), val
#define add_i16_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_ADD_INT16_REG_IMM, reg), val
#define add_i32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_ADD_INT32_REG_IMM, reg), val
#define add_i64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_ADD_INT64_REG_IMM, reg), val
#define add_f32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_ADD_FLT32_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define add_f64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_ADD_FLT64_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define add_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_ADD_REG_REG, reg1, reg2)

#define sub_i8_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_SUB_INT8_REG_IMM, reg), val
#define sub_i16_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_SUB_INT16_REG_IMM, reg), val
#define sub_i32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_SUB_INT32_REG_IMM, reg), val
#define sub_i64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_SUB_INT64_REG_IMM, reg), val
#define sub_f32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_SUB_FLT32_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define sub_f64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_SUB_FLT64_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define sub_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_SUB_REG_REG, reg1, reg2)

#define mul_i8_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MUL_INT8_REG_IMM, reg), val
#define mul_i16_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MUL_INT16_REG_IMM, reg), val
#define mul_i32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MUL_INT32_REG_IMM, reg), val
#define mul_i64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MUL_INT64_REG_IMM, reg), val
#define mul_f32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MUL_FLT32_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define mul_f64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MUL_FLT64_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define mul_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MUL_REG_REG, reg1, reg2)

#define div_i8_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_DIV_INT8_REG_IMM, reg), val
#define div_i16_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_DIV_INT16_REG_IMM, reg), val
#define div_i32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_DIV_INT32_REG_IMM, reg), val
#define div_i64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_DIV_INT64_REG_IMM, reg), val
#define div_f32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_DIV_FLT32_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define div_f64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_DIV_FLT64_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define div_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_DIV_REG_REG, reg1, reg2)

#define log_not_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_LOG_NOT_REG, reg)
#define not_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_NOT_REG, reg)
#define neg_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_NEG_REG, reg)
#define inc_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_INC_REG, reg)
#define dec_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_DEC_REG, reg)
#define xor_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_XOR_REG_REG, reg1, reg2)

#define call_imm(val) encode_instruction(BYTECODE_OPCODE_CALL_IMM), val
#define call_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CALL_REG, reg)
#define call_foreign(reg_arg_count, ret_type) encode_instruction(BYTECODE_OPCODE_CALL_FOREIGN), reg_arg_count, ret_type

#define begin_call_frame() encode_instruction(BYTECODE_OPCODE_BEGIN_CALL_FRAME)
#define end_call_frame() encode_instruction(BYTECODE_OPCODE_END_CALL_FRAME)
#define ret() encode_instruction(BYTECODE_OPCODE_RETURN)

#define lea_lcl_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_LEA_LCL_REG_IMM, reg), val
#define lea_lcl_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_LEA_LCL_REG_REG, reg1, reg2)
#define lea_bss_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_LEA_BSS_REG_IMM, reg), val
#define lea_bss_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_LEA_BSS_REG_REG, reg1, reg2)

#define memw_i8_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MEMW_INT8_REG_IMM, reg), val
#define memw_i16_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MEMW_INT16_REG_IMM, reg), val
#define memw_i32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MEMW_INT32_REG_IMM, reg), val
#define memw_i64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MEMW_INT64_REG_IMM, reg), val
#define memw_f32_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MEMW_FLT32_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define memw_f64_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_MEMW_FLT64_REG_IMM, reg), (uint64_t)(*(uint64_t *)&val)
#define memw_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMW_REG_REG, reg1, reg2)

#define memr_i8_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMR_INT8_REG_REG, reg1, reg2)
#define memr_i16_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMR_INT16_REG_REG, reg1, reg2)
#define memr_i32_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMR_INT32_REG_REG, reg1, reg2)
#define memr_i64_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMR_INT64_REG_REG, reg1, reg2)
#define memr_f32_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMR_FLT32_REG_REG, reg1, reg2)
#define memr_f64_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_MEMR_FLT64_REG_REG, reg1, reg2)

#define test_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_TEST_REG_IMM, reg), val
#define test_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_TEST_REG_REG, reg1, reg2)

#define cmp_reg_imm(reg, val) encode_instruction_r1(BYTECODE_OPCODE_CMP_REG_IMM, reg), val
#define cmp_reg_reg(reg1, reg2) encode_instruction_r2(BYTECODE_OPCODE_CMP_REG_REG, reg1, reg2)

#define jmp_imm(val) encode_instruction(BYTECODE_OPCODE_JMP_IMM), val
#define jnz_imm(val) encode_instruction(BYTECODE_OPCODE_JNZ_IMM), val
#define jz_imm(val) encode_instruction(BYTECODE_OPCODE_JZ_IMM), val
#define jle_imm(val) encode_instruction(BYTECODE_OPCODE_JLE_IMM), val
#define jl_imm(val) encode_instruction(BYTECODE_OPCODE_JL_IMM), val
#define jge_imm(val) encode_instruction(BYTECODE_OPCODE_JGE_IMM), val
#define jg_imm(val) encode_instruction(BYTECODE_OPCODE_JG_IMM), val

#define conv_i8_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CONV_INT8_REG, reg)
#define conv_i16_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CONV_INT16_REG, reg)
#define conv_i32_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CONV_INT32_REG, reg)
#define conv_i64_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CONV_INT64_REG, reg)
#define conv_f32_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CONV_FLT32_REG, reg)
#define conv_f64_reg(reg) encode_instruction_r1(BYTECODE_OPCODE_CONV_FLT64_REG, reg)

#define nop() encode_instruction(BYTECODE_OPCODE_NOP)
#define halt() encode_instruction(BYTECODE_OPCODE_HALT)

#endif
