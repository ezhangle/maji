#include "bytecode_instruction_handler.h"
#include "bytecode_runner.h"

#include <dlfcn.h>
#include <ffi.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define as_i8_ptr(val) (int8_t*)&val
#define as_i16_ptr(val) (int16_t*)&val
#define as_i32_ptr(val) (int32_t*)&val
#define as_i64_ptr(val) (int64_t*)&val
#define as_f32_ptr(val) (float*)&val
#define as_f64_ptr(val) (double*)&val

#define as_i8(val) (int8_t)(*(int8_t*)&val)
#define as_i16(val) (int16_t)(*(int16_t*)&val)
#define as_i32(val) (int32_t)(*(int32_t*)&val)
#define as_i64(val) (int64_t)(*(int64_t*)&val)
#define as_f32(val) (float)(*(float*)&val)
#define as_f64(val) (double)(*(double*)&val)

#define bytecode_do_bit_op(op, val) do {\
    switch (bcr->reg_type[reg1]) { \
    case BYTECODE_REGISTER_KIND_I64: { \
        *as_i64_ptr(bcr->reg[reg1]) op as_i64(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_I32: { \
        *as_i32_ptr(bcr->reg[reg1]) op as_i32(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_I16: { \
        *as_i16_ptr(bcr->reg[reg1]) op as_i16(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_I8: { \
        *as_i8_ptr(bcr->reg[reg1]) op as_i8(val); \
    } break; \
    } \
    } while (0)

#define bytecode_do_op(op, val) do {\
    switch (bcr->reg_type[reg1]) { \
    case BYTECODE_REGISTER_KIND_I64: { \
        *as_i64_ptr(bcr->reg[reg1]) op as_i64(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_I32: { \
        *as_i32_ptr(bcr->reg[reg1]) op as_i32(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_I16: { \
        *as_i16_ptr(bcr->reg[reg1]) op as_i16(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_I8: { \
        *as_i8_ptr(bcr->reg[reg1]) op as_i8(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_F64: { \
        *as_f64_ptr(bcr->reg[reg1]) op as_f64(val); \
    } break; \
    case BYTECODE_REGISTER_KIND_F32: { \
        *as_f32_ptr(bcr->reg[reg1]) op as_f32(val); \
    } break; \
    } \
    } while (0)

#define bytecode_push_reg(reg1) do {\
    switch (bcr->reg_type[reg1]) { \
    case BYTECODE_REGISTER_KIND_I64: { \
        int64_t *stack = as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *stack++ = as_i64(bcr->reg[reg1]); \
        bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(int64_t); \
        *bcr->stack_info++ = BYTECODE_REGISTER_KIND_I64; \
    } break; \
    case BYTECODE_REGISTER_KIND_I32: { \
        int32_t *stack = as_i32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *stack++ = as_i32(bcr->reg[reg1]); \
        bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(int32_t); \
        *bcr->stack_info++ = BYTECODE_REGISTER_KIND_I32; \
    } break; \
    case BYTECODE_REGISTER_KIND_I16: { \
        int16_t *stack = as_i16_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *stack++ = as_i16(bcr->reg[reg1]); \
        bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(int16_t); \
        *bcr->stack_info++ = BYTECODE_REGISTER_KIND_I16; \
    } break; \
    case BYTECODE_REGISTER_KIND_I8: { \
        int8_t *stack = as_i8_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *stack++ = as_i8(bcr->reg[reg1]); \
        bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(int8_t); \
        *bcr->stack_info++ = BYTECODE_REGISTER_KIND_I8; \
    } break; \
    case BYTECODE_REGISTER_KIND_F64: { \
        double *stack = as_f64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *stack++ = as_f64(bcr->reg[reg1]); \
        bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(double); \
        *bcr->stack_info++ = BYTECODE_REGISTER_KIND_F64; \
    } break; \
    case BYTECODE_REGISTER_KIND_F32: { \
        float *stack = as_f32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *stack++ = as_f32(bcr->reg[reg1]); \
        bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(float); \
        *bcr->stack_info++ = BYTECODE_REGISTER_KIND_F32; \
    } break; \
    } \
    } while (0)

#define bytecode_pop_reg1(reg1) do {\
    enum bytecode_register_kind type = *--bcr->stack_info; \
    bcr->reg_type[reg1] = type; \
    switch (bcr->reg_type[reg1]) { \
    case BYTECODE_REGISTER_KIND_I64: { \
        int64_t *stack = as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i64_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int64_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_I32: { \
        int32_t *stack = as_i32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i32_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int32_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_I16: { \
        int16_t *stack = as_i16_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i16_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int16_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_I8: { \
        int8_t *stack = as_i8_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i8_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int8_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_F64: { \
        double *stack = as_f64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_f64_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(double); \
    } break; \
    case BYTECODE_REGISTER_KIND_F32: { \
        float *stack = as_f32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_f32_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(float); \
    } break; \
    } \
    } while (0)

#define bytecode_pop_reg(reg1, type) do {\
    bcr->reg_type[reg1] = type; \
    switch (bcr->reg_type[reg1]) { \
    case BYTECODE_REGISTER_KIND_I64: { \
        int64_t *stack = as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i64_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int64_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_I32: { \
        int32_t *stack = as_i32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i32_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int32_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_I16: { \
        int16_t *stack = as_i16_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i16_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int16_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_I8: { \
        int8_t *stack = as_i8_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_i8_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int8_t); \
    } break; \
    case BYTECODE_REGISTER_KIND_F64: { \
        double *stack = as_f64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_f64_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(double); \
    } break; \
    case BYTECODE_REGISTER_KIND_F32: { \
        float *stack = as_f32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]); \
        *as_f32_ptr(bcr->reg[reg1]) = *--stack; \
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(float); \
    } break; \
    } \
    } while (0)

static bytecode_instruction_handler *instruction_handlers[BYTECODE_OPCODE_COUNT] = {
    [BYTECODE_OPCODE_HALT]               = exec_op_halt,

    [BYTECODE_OPCODE_MOV_INT8_REG_IMM]   = exec_op_mov_int8_reg_imm,
    [BYTECODE_OPCODE_MOV_INT16_REG_IMM]  = exec_op_mov_int16_reg_imm,
    [BYTECODE_OPCODE_MOV_INT32_REG_IMM]  = exec_op_mov_int32_reg_imm,
    [BYTECODE_OPCODE_MOV_INT64_REG_IMM]  = exec_op_mov_int64_reg_imm,
    [BYTECODE_OPCODE_MOV_FLT32_REG_IMM]  = exec_op_mov_flt32_reg_imm,
    [BYTECODE_OPCODE_MOV_FLT64_REG_IMM]  = exec_op_mov_flt64_reg_imm,
    [BYTECODE_OPCODE_MOV_REG_REG]        = exec_op_mov_reg_reg,
    [BYTECODE_OPCODE_MOV_INT8_LCL_IMM]   = exec_op_mov_int8_lcl_imm,
    [BYTECODE_OPCODE_MOV_INT16_LCL_IMM]  = exec_op_mov_int16_lcl_imm,
    [BYTECODE_OPCODE_MOV_INT32_LCL_IMM]  = exec_op_mov_int32_lcl_imm,
    [BYTECODE_OPCODE_MOV_INT64_LCL_IMM]  = exec_op_mov_int64_lcl_imm,
    [BYTECODE_OPCODE_MOV_FLT32_LCL_IMM]  = exec_op_mov_flt32_lcl_imm,
    [BYTECODE_OPCODE_MOV_FLT64_LCL_IMM]  = exec_op_mov_flt64_lcl_imm,
    [BYTECODE_OPCODE_MOV_LCL_REG]        = exec_op_mov_lcl_reg,
    [BYTECODE_OPCODE_MOV_INT8_REG_LCL]   = exec_op_mov_int8_reg_lcl,
    [BYTECODE_OPCODE_MOV_INT16_REG_LCL]  = exec_op_mov_int16_reg_lcl,
    [BYTECODE_OPCODE_MOV_INT32_REG_LCL]  = exec_op_mov_int32_reg_lcl,
    [BYTECODE_OPCODE_MOV_INT64_REG_LCL]  = exec_op_mov_int64_reg_lcl,
    [BYTECODE_OPCODE_MOV_FLT32_REG_LCL]  = exec_op_mov_flt32_reg_lcl,
    [BYTECODE_OPCODE_MOV_FLT64_REG_LCL]  = exec_op_mov_flt64_reg_lcl,

    [BYTECODE_OPCODE_PUSH_INT8_IMM]      = exec_op_push_int8_imm,
    [BYTECODE_OPCODE_PUSH_INT16_IMM]     = exec_op_push_int16_imm,
    [BYTECODE_OPCODE_PUSH_INT32_IMM]     = exec_op_push_int32_imm,
    [BYTECODE_OPCODE_PUSH_INT64_IMM]     = exec_op_push_int64_imm,
    [BYTECODE_OPCODE_PUSH_FLT32_IMM]     = exec_op_push_flt32_imm,
    [BYTECODE_OPCODE_PUSH_FLT64_IMM]     = exec_op_push_flt64_imm,
    [BYTECODE_OPCODE_PUSH_REG]           = exec_op_push_reg,

    [BYTECODE_OPCODE_POP_REG]            = exec_op_pop_reg,
    [BYTECODE_OPCODE_POP_INT8_REG]       = exec_op_pop_int8_reg,
    [BYTECODE_OPCODE_POP_INT16_REG]      = exec_op_pop_int16_reg,
    [BYTECODE_OPCODE_POP_INT32_REG]      = exec_op_pop_int32_reg,
    [BYTECODE_OPCODE_POP_INT64_REG]      = exec_op_pop_int64_reg,
    [BYTECODE_OPCODE_POP_FLT32_REG]      = exec_op_pop_flt32_reg,
    [BYTECODE_OPCODE_POP_FLT64_REG]      = exec_op_pop_flt64_reg,

    [BYTECODE_OPCODE_ADD_INT8_REG_IMM]   = exec_op_add_int8_reg_imm,
    [BYTECODE_OPCODE_ADD_INT16_REG_IMM]  = exec_op_add_int16_reg_imm,
    [BYTECODE_OPCODE_ADD_INT32_REG_IMM]  = exec_op_add_int32_reg_imm,
    [BYTECODE_OPCODE_ADD_INT64_REG_IMM]  = exec_op_add_int64_reg_imm,
    [BYTECODE_OPCODE_ADD_FLT32_REG_IMM]  = exec_op_add_flt32_reg_imm,
    [BYTECODE_OPCODE_ADD_FLT64_REG_IMM]  = exec_op_add_flt64_reg_imm,
    [BYTECODE_OPCODE_ADD_REG_REG]        = exec_op_add_reg_reg,

    [BYTECODE_OPCODE_SUB_INT8_REG_IMM]   = exec_op_sub_int8_reg_imm,
    [BYTECODE_OPCODE_SUB_INT16_REG_IMM]  = exec_op_sub_int16_reg_imm,
    [BYTECODE_OPCODE_SUB_INT32_REG_IMM]  = exec_op_sub_int32_reg_imm,
    [BYTECODE_OPCODE_SUB_INT64_REG_IMM]  = exec_op_sub_int64_reg_imm,
    [BYTECODE_OPCODE_SUB_FLT32_REG_IMM]  = exec_op_sub_flt32_reg_imm,
    [BYTECODE_OPCODE_SUB_FLT64_REG_IMM]  = exec_op_sub_flt64_reg_imm,
    [BYTECODE_OPCODE_SUB_REG_REG]        = exec_op_sub_reg_reg,

    [BYTECODE_OPCODE_MUL_INT8_REG_IMM]   = exec_op_mul_int8_reg_imm,
    [BYTECODE_OPCODE_MUL_INT16_REG_IMM]  = exec_op_mul_int16_reg_imm,
    [BYTECODE_OPCODE_MUL_INT32_REG_IMM]  = exec_op_mul_int32_reg_imm,
    [BYTECODE_OPCODE_MUL_INT64_REG_IMM]  = exec_op_mul_int64_reg_imm,
    [BYTECODE_OPCODE_MUL_FLT32_REG_IMM]  = exec_op_mul_flt32_reg_imm,
    [BYTECODE_OPCODE_MUL_FLT64_REG_IMM]  = exec_op_mul_flt64_reg_imm,
    [BYTECODE_OPCODE_MUL_REG_REG]        = exec_op_mul_reg_reg,

    [BYTECODE_OPCODE_DIV_INT8_REG_IMM]   = exec_op_div_int8_reg_imm,
    [BYTECODE_OPCODE_DIV_INT16_REG_IMM]  = exec_op_div_int16_reg_imm,
    [BYTECODE_OPCODE_DIV_INT32_REG_IMM]  = exec_op_div_int32_reg_imm,
    [BYTECODE_OPCODE_DIV_INT64_REG_IMM]  = exec_op_div_int64_reg_imm,
    [BYTECODE_OPCODE_DIV_FLT32_REG_IMM]  = exec_op_div_flt32_reg_imm,
    [BYTECODE_OPCODE_DIV_FLT64_REG_IMM]  = exec_op_div_flt64_reg_imm,
    [BYTECODE_OPCODE_DIV_REG_REG]        = exec_op_div_reg_reg,

    [BYTECODE_OPCODE_LOG_NOT_REG]        = exec_op_log_not_reg,
    [BYTECODE_OPCODE_NOT_REG]            = exec_op_not_reg,
    [BYTECODE_OPCODE_NEG_REG]            = exec_op_neg_reg,
    [BYTECODE_OPCODE_INC_REG]            = exec_op_inc_reg,
    [BYTECODE_OPCODE_DEC_REG]            = exec_op_dec_reg,
    [BYTECODE_OPCODE_XOR_REG_REG]        = exec_op_xor_reg_reg,
    [BYTECODE_OPCODE_OR_REG_REG]         = exec_op_or_reg_reg,
    [BYTECODE_OPCODE_AND_REG_REG]        = exec_op_and_reg_reg,
    [BYTECODE_OPCODE_LSHIFT_REG_REG]     = exec_op_lshift_reg_reg,
    [BYTECODE_OPCODE_RSHIFT_REG_REG]     = exec_op_rshift_reg_reg,

    [BYTECODE_OPCODE_TEST_REG_IMM]       = exec_op_test_reg_imm,
    [BYTECODE_OPCODE_TEST_REG_REG]       = exec_op_test_reg_reg,
    [BYTECODE_OPCODE_CMP_REG_IMM]        = exec_op_cmp_reg_imm,
    [BYTECODE_OPCODE_CMP_REG_REG]        = exec_op_cmp_reg_reg,
    [BYTECODE_OPCODE_JMP_IMM]            = exec_op_jmp_imm,
    [BYTECODE_OPCODE_JNZ_IMM]            = exec_op_jnz_imm,
    [BYTECODE_OPCODE_JZ_IMM]             = exec_op_jz_imm,
    [BYTECODE_OPCODE_JLE_IMM]            = exec_op_jle_imm,
    [BYTECODE_OPCODE_JL_IMM]             = exec_op_jl_imm,
    [BYTECODE_OPCODE_JGE_IMM]            = exec_op_jge_imm,
    [BYTECODE_OPCODE_JG_IMM]             = exec_op_jg_imm,

    [BYTECODE_OPCODE_CALL_IMM]           = exec_op_call_imm,
    [BYTECODE_OPCODE_CALL_REG]           = exec_op_call_reg,
    [BYTECODE_OPCODE_CALL_FOREIGN]       = exec_op_call_foreign,

    [BYTECODE_OPCODE_BEGIN_CALL_FRAME]   = exec_op_begin_call_frame,
    [BYTECODE_OPCODE_END_CALL_FRAME]     = exec_op_end_call_frame,
    [BYTECODE_OPCODE_RETURN]             = exec_op_return,

    [BYTECODE_OPCODE_LEA_LCL_REG_IMM]    = exec_op_lea_lcl_reg_imm,
    [BYTECODE_OPCODE_LEA_LCL_REG_REG]    = exec_op_lea_lcl_reg_reg,
    [BYTECODE_OPCODE_LEA_BSS_REG_IMM]    = exec_op_lea_bss_reg_imm,
    [BYTECODE_OPCODE_LEA_BSS_REG_REG]    = exec_op_lea_bss_reg_reg,

    [BYTECODE_OPCODE_MEMW_INT8_REG_IMM]  = exec_op_memw_int8_reg_imm,
    [BYTECODE_OPCODE_MEMW_INT16_REG_IMM] = exec_op_memw_int16_reg_imm,
    [BYTECODE_OPCODE_MEMW_INT32_REG_IMM] = exec_op_memw_int32_reg_imm,
    [BYTECODE_OPCODE_MEMW_INT64_REG_IMM] = exec_op_memw_int64_reg_imm,
    [BYTECODE_OPCODE_MEMW_FLT32_REG_IMM] = exec_op_memw_flt32_reg_imm,
    [BYTECODE_OPCODE_MEMW_FLT64_REG_IMM] = exec_op_memw_flt64_reg_imm,
    [BYTECODE_OPCODE_MEMW_REG_REG]       = exec_op_memw_reg_reg,

    [BYTECODE_OPCODE_MEMR_INT8_REG_REG]  = exec_op_memr_int8_reg_reg,
    [BYTECODE_OPCODE_MEMR_INT16_REG_REG] = exec_op_memr_int16_reg_reg,
    [BYTECODE_OPCODE_MEMR_INT32_REG_REG] = exec_op_memr_int32_reg_reg,
    [BYTECODE_OPCODE_MEMR_INT64_REG_REG] = exec_op_memr_int64_reg_reg,
    [BYTECODE_OPCODE_MEMR_FLT32_REG_REG] = exec_op_memr_flt32_reg_reg,
    [BYTECODE_OPCODE_MEMR_FLT64_REG_REG] = exec_op_memr_flt64_reg_reg,

    [BYTECODE_OPCODE_MEMC_REG_REG_IMM]   = exec_op_memc_reg_reg_imm,

    [BYTECODE_OPCODE_CONV_INT8_REG]      = exec_op_conv_int8_reg,
    [BYTECODE_OPCODE_CONV_INT16_REG]     = exec_op_conv_int16_reg,
    [BYTECODE_OPCODE_CONV_INT32_REG]     = exec_op_conv_int32_reg,
    [BYTECODE_OPCODE_CONV_INT64_REG]     = exec_op_conv_int64_reg,
    [BYTECODE_OPCODE_CONV_FLT32_REG]     = exec_op_conv_flt32_reg,
    [BYTECODE_OPCODE_CONV_FLT64_REG]     = exec_op_conv_flt64_reg,

    [BYTECODE_OPCODE_NOP]                = exec_op_nop
};

void bytecode_instruction_execute(struct bytecode_runner *bcr, struct bytecode_instruction instr)
{
    if (instruction_handlers[instr.op]) {
        instruction_handlers[instr.op](bcr, instr.op, instr.r1, instr.r2);
    } else {
        fprintf(stderr, "%2X: missing instruction handler\n", instr.op);
    }
}

bytecode_instruction_handler_(exec_op_halt)
{
    bcr->is_running = false;
}

bytecode_instruction_handler_(exec_op_nop)
{
}

bytecode_instruction_handler_(exec_op_mov_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i8_ptr(bcr->reg[reg1]) = as_i8(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I8;
}
bytecode_instruction_handler_(exec_op_mov_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i16_ptr(bcr->reg[reg1]) = as_i16(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I16;
}
bytecode_instruction_handler_(exec_op_mov_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i32_ptr(bcr->reg[reg1]) = as_i32(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I32;
}
bytecode_instruction_handler_(exec_op_mov_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) = as_i64(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}
bytecode_instruction_handler_(exec_op_mov_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f32_ptr(bcr->reg[reg1]) = as_f32(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F32;
}
bytecode_instruction_handler_(exec_op_mov_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f64_ptr(bcr->reg[reg1]) = as_f64(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F64;
}
bytecode_instruction_handler_(exec_op_mov_reg_reg)
{
    bcr->reg[reg1] = bcr->reg[reg2];
    bcr->reg_type[reg1] = bcr->reg_type[reg2];
}

bytecode_instruction_handler_(exec_op_mov_int8_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    *as_i8_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i8(raw);
}
bytecode_instruction_handler_(exec_op_mov_int16_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    *as_i16_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i16(raw);
}
bytecode_instruction_handler_(exec_op_mov_int32_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    *as_i32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i32(raw);
}
bytecode_instruction_handler_(exec_op_mov_int64_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i64(raw);
}
bytecode_instruction_handler_(exec_op_mov_flt32_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    *as_f32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_f32(raw);
}
bytecode_instruction_handler_(exec_op_mov_flt64_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    *as_f64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_f64(raw);
}
bytecode_instruction_handler_(exec_op_mov_lcl_reg)
{
    uint64_t offset = fetch_instruction(bcr);

    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I64: {
        *as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i64(bcr->reg[reg1]);
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        *as_i32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i32(bcr->reg[reg1]);
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        *as_i16_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i16(bcr->reg[reg1]);
    } break;
    case BYTECODE_REGISTER_KIND_I8: {
        *as_i8_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_i8(bcr->reg[reg1]);
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        *as_f64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_f64(bcr->reg[reg1]);
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        *as_f32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]) = as_f32(bcr->reg[reg1]);
    } break;
    }
}
bytecode_instruction_handler_(exec_op_mov_int8_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_i8_ptr(bcr->reg[reg1]) = as_i8(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I8;
}
bytecode_instruction_handler_(exec_op_mov_int16_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_i16_ptr(bcr->reg[reg1]) = as_i16(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I16;
}
bytecode_instruction_handler_(exec_op_mov_int32_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_i32_ptr(bcr->reg[reg1]) = as_i32(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I32;
}
bytecode_instruction_handler_(exec_op_mov_int64_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) = as_i64(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}
bytecode_instruction_handler_(exec_op_mov_flt32_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_f32_ptr(bcr->reg[reg1]) = as_f32(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F32;
}
bytecode_instruction_handler_(exec_op_mov_flt64_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_f64_ptr(bcr->reg[reg1]) = as_f64(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F64;
}

bytecode_instruction_handler_(exec_op_push_int8_imm)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    int8_t *stack = as_i8_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);
    uint64_t raw = fetch_instruction(bcr);
    int8_t val = as_i8(raw);
    *stack++ = val;
    bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(val);
}
bytecode_instruction_handler_(exec_op_push_int16_imm)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    int16_t *stack = as_i16_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);
    uint64_t raw = fetch_instruction(bcr);
    int16_t val = as_i16(raw);
    *stack++ = val;
    bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(val);
}
bytecode_instruction_handler_(exec_op_push_int32_imm)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    int32_t *stack = as_i32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);
    uint64_t raw = fetch_instruction(bcr);
    int32_t val = as_i32(raw);
    *stack++ = val;
    bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(val);
}
bytecode_instruction_handler_(exec_op_push_int64_imm)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    int64_t *stack = as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);
    uint64_t raw = fetch_instruction(bcr);
    int64_t val = as_i64(raw);
    *stack++ = val;
    bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(val);
}
bytecode_instruction_handler_(exec_op_push_flt32_imm)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    float *stack = as_f32_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);
    uint64_t raw = fetch_instruction(bcr);
    float val = as_f32(raw);
    *stack++ = val;
    bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(val);
}
bytecode_instruction_handler_(exec_op_push_flt64_imm)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    double *stack = as_f64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);
    uint64_t raw = fetch_instruction(bcr);
    double val = as_f64(raw);
    *stack++ = val;
    bcr->reg[BYTECODE_REGISTER_RSP] += sizeof(val);
}
bytecode_instruction_handler_(exec_op_push_reg)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] < bcr->stack_size);
    bytecode_push_reg(reg1);
}

bytecode_instruction_handler_(exec_op_pop_reg)
{
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg1(reg1);
}
bytecode_instruction_handler_(exec_op_pop_int8_reg)
{
    assert(0);
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg(reg1, BYTECODE_REGISTER_KIND_I8);
}
bytecode_instruction_handler_(exec_op_pop_int16_reg)
{
    assert(0);
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg(reg1, BYTECODE_REGISTER_KIND_I16);
}
bytecode_instruction_handler_(exec_op_pop_int32_reg)
{
    assert(0);
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg(reg1, BYTECODE_REGISTER_KIND_I32);
}
bytecode_instruction_handler_(exec_op_pop_int64_reg)
{
    assert(0);
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg(reg1, BYTECODE_REGISTER_KIND_I64);
}
bytecode_instruction_handler_(exec_op_pop_flt32_reg)
{
    assert(0);
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg(reg1, BYTECODE_REGISTER_KIND_F32);
}
bytecode_instruction_handler_(exec_op_pop_flt64_reg)
{
    assert(0);
    assert(bcr->reg[BYTECODE_REGISTER_RSP] > 0);
    bytecode_pop_reg(reg1, BYTECODE_REGISTER_KIND_F64);
}

bytecode_instruction_handler_(exec_op_add_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i8_ptr(bcr->reg[reg1]) += as_i8(raw);
}
bytecode_instruction_handler_(exec_op_add_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i16_ptr(bcr->reg[reg1]) += as_i16(raw);
}
bytecode_instruction_handler_(exec_op_add_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i32_ptr(bcr->reg[reg1]) += as_i32(raw);
}
bytecode_instruction_handler_(exec_op_add_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) += as_i64(raw);
}
bytecode_instruction_handler_(exec_op_add_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f32_ptr(bcr->reg[reg1]) += as_f32(raw);
}
bytecode_instruction_handler_(exec_op_add_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f64_ptr(bcr->reg[reg1]) += as_f64(raw);
}
bytecode_instruction_handler_(exec_op_add_reg_reg)
{
    assert(bcr->reg_type[reg1] == bcr->reg_type[reg2]);
    bytecode_do_op(+=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_sub_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i8_ptr(bcr->reg[reg1]) -= as_i8(raw);
}
bytecode_instruction_handler_(exec_op_sub_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i16_ptr(bcr->reg[reg1]) -= as_i16(raw);
}
bytecode_instruction_handler_(exec_op_sub_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i32_ptr(bcr->reg[reg1]) -= as_i32(raw);
}
bytecode_instruction_handler_(exec_op_sub_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) -= as_i64(raw);
}
bytecode_instruction_handler_(exec_op_sub_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f32_ptr(bcr->reg[reg1]) -= as_f32(raw);
}
bytecode_instruction_handler_(exec_op_sub_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f64_ptr(bcr->reg[reg1]) -= as_f64(raw);
}
bytecode_instruction_handler_(exec_op_sub_reg_reg)
{
    assert(bcr->reg_type[reg1] == bcr->reg_type[reg2]);
    bytecode_do_op(-=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_mul_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i8_ptr(bcr->reg[reg1]) *= as_i8(raw);
}
bytecode_instruction_handler_(exec_op_mul_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i16_ptr(bcr->reg[reg1]) *= as_i16(raw);
}
bytecode_instruction_handler_(exec_op_mul_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i32_ptr(bcr->reg[reg1]) *= as_i32(raw);
}
bytecode_instruction_handler_(exec_op_mul_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) *= as_i64(raw);
}
bytecode_instruction_handler_(exec_op_mul_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f32_ptr(bcr->reg[reg1]) *= as_f32(raw);
}
bytecode_instruction_handler_(exec_op_mul_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f64_ptr(bcr->reg[reg1]) *= as_f64(raw);
}
bytecode_instruction_handler_(exec_op_mul_reg_reg)
{
    assert(bcr->reg_type[reg1] == bcr->reg_type[reg2]);
    bytecode_do_op(*=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_div_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i8_ptr(bcr->reg[reg1]) /= as_i8(raw);
}
bytecode_instruction_handler_(exec_op_div_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i16_ptr(bcr->reg[reg1]) /= as_i16(raw);
}
bytecode_instruction_handler_(exec_op_div_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i32_ptr(bcr->reg[reg1]) /= as_i32(raw);
}
bytecode_instruction_handler_(exec_op_div_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) /= as_i64(raw);
}
bytecode_instruction_handler_(exec_op_div_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f32_ptr(bcr->reg[reg1]) /= as_f32(raw);
}
bytecode_instruction_handler_(exec_op_div_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_f64_ptr(bcr->reg[reg1]) /= as_f64(raw);
}
bytecode_instruction_handler_(exec_op_div_reg_reg)
{
    assert(bcr->reg_type[reg1] == bcr->reg_type[reg2]);
    bytecode_do_op(/=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_log_not_reg)
{
    bytecode_do_op(= !, bcr->reg[reg1]);
}

bytecode_instruction_handler_(exec_op_not_reg)
{
    bytecode_do_bit_op(= ~, bcr->reg[reg1]);
}

bytecode_instruction_handler_(exec_op_neg_reg)
{
    bytecode_do_op(= -, bcr->reg[reg1]);
}

bytecode_instruction_handler_(exec_op_inc_reg)
{
    int64_t one = 1;
    bytecode_do_op(+=, one);
}

bytecode_instruction_handler_(exec_op_dec_reg)
{
    int64_t one = 1;
    bytecode_do_op(-=, one);
}

bytecode_instruction_handler_(exec_op_xor_reg_reg)
{
    bytecode_do_bit_op(^=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_or_reg_reg)
{
    bytecode_do_bit_op(|=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_and_reg_reg)
{
    bytecode_do_bit_op(&=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_lshift_reg_reg)
{
    bytecode_do_bit_op(<<=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_rshift_reg_reg)
{
    bytecode_do_bit_op(>>=, bcr->reg[reg2]);
}

bytecode_instruction_handler_(exec_op_test_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    bcr->compare = as_i64(bcr->reg[reg1]) & as_i64(raw);
}
bytecode_instruction_handler_(exec_op_test_reg_reg)
{
    bcr->compare = as_i64(bcr->reg[reg1]) & as_i64(bcr->reg[reg2]);
}
bytecode_instruction_handler_(exec_op_cmp_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    bcr->compare = as_i64(bcr->reg[reg1]) - as_i64(raw);
}
bytecode_instruction_handler_(exec_op_cmp_reg_reg)
{
    bcr->compare = as_i64(bcr->reg[reg1]) - as_i64(bcr->reg[reg2]);
}
bytecode_instruction_handler_(exec_op_jmp_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
}
bytecode_instruction_handler_(exec_op_jnz_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    if (bcr->compare != 0) {
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    }
}
bytecode_instruction_handler_(exec_op_jz_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    if (bcr->compare == 0) {
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    }
}
bytecode_instruction_handler_(exec_op_jle_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    if (bcr->compare <= 0) {
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    }
}
bytecode_instruction_handler_(exec_op_jl_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    if (bcr->compare < 0) {
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    }
}
bytecode_instruction_handler_(exec_op_jge_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    if (bcr->compare >= 0) {
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    }
}
bytecode_instruction_handler_(exec_op_jg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    if (bcr->compare > 0) {
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    }
}

bytecode_instruction_handler_(exec_op_call_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    bytecode_push_reg(BYTECODE_REGISTER_RIP);
    *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(raw);
    bcr->reg_type[BYTECODE_REGISTER_RIP] = BYTECODE_REGISTER_KIND_I64;
}

bytecode_instruction_handler_(exec_op_call_reg)
{
    bytecode_push_reg(BYTECODE_REGISTER_RIP);
    *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RIP]) = as_i64(bcr->reg[reg1]);
    bcr->reg_type[BYTECODE_REGISTER_RIP] = bcr->reg_type[reg1];
}

static inline int type_info_get_kind(struct bytecode_runner *bcr, int type_info)
{
    return *(int*)(bcr->type + type_info);
}

static inline int type_info_get_size(struct bytecode_runner *bcr, int type_info)
{
    return *(int*)(bcr->type + type_info + 4);
}

static inline int type_info_get_align(struct bytecode_runner *bcr, int type_info)
{
    return *(int*)(bcr->type + type_info + 8);
}

static inline int type_info_get_field_count(struct bytecode_runner *bcr, int type_info)
{
    return *(int*)(bcr->type + type_info + 12);
}

static inline int type_info_get_field(struct bytecode_runner *bcr, int type_info, int field_index)
{
    return *(int*)(bcr->type + type_info + 12 + (4 * (field_index + 1)));
}

struct bytecode_runner_type_info
{
    int kind;
    int size;
    int align;
    int fields;
};

static inline struct bytecode_runner_type_info create_type_info(struct bytecode_runner *bcr, int type_info_pos)
{
    struct bytecode_runner_type_info info = {
        .kind = type_info_get_kind(bcr, type_info_pos),
        .size = type_info_get_size(bcr, type_info_pos),
        .align = type_info_get_align(bcr, type_info_pos),
        .fields = type_info_get_field_count(bcr, type_info_pos)
    };
    return info;
}


static inline ffi_type *bytecode_create_libffi_type(struct bytecode_runner *bcr, int arg_type_info_pos)
{
    struct bytecode_runner_type_info arg_type_info = create_type_info(bcr, arg_type_info_pos);

    if (arg_type_info.kind == BYTECODE_TYPE_STRUCT) {
        ffi_type **struct_elements = malloc(sizeof(ffi_type) * (arg_type_info.fields+1));
        for (int field_index = 0; field_index < arg_type_info.fields; ++field_index) {
            int field_type_info_pos = type_info_get_field(bcr, arg_type_info_pos, field_index);
            struct_elements[field_index] = bytecode_create_libffi_type(bcr, field_type_info_pos);
        }
        struct_elements[arg_type_info.fields] = NULL;
        ffi_type *struct_type = malloc(sizeof(ffi_type));
        struct_type->size = 0;
        struct_type->alignment = 0;
        struct_type->type = FFI_TYPE_STRUCT;
        struct_type->elements = struct_elements;
        return struct_type;
    } else if (arg_type_info.kind == BYTECODE_TYPE_PTR) {
        return &ffi_type_pointer;
    } else if (arg_type_info.kind == BYTECODE_TYPE_I64) {
        return &ffi_type_sint64;
    } else if (arg_type_info.kind == BYTECODE_TYPE_I32) {
        return &ffi_type_sint32;
    } else if (arg_type_info.kind == BYTECODE_TYPE_I16) {
        return &ffi_type_sint16;
    } else if (arg_type_info.kind == BYTECODE_TYPE_I8) {
        return &ffi_type_sint8;
    } else if (arg_type_info.kind == BYTECODE_TYPE_F64) {
        return &ffi_type_double;
    } else if (arg_type_info.kind == BYTECODE_TYPE_F32) {
        return &ffi_type_float;
    } else if (arg_type_info.kind == BYTECODE_TYPE_VOID) {
        return &ffi_type_void;
    } else {
        printf("UNKNOWN RETURN TYPE %d SUPPLIED TO FOREIGN FUNCTION\n", arg_type_info.kind);
        assert(0);
        return 0;
    }
}

static inline void bytecode_destroy_libffi_type(ffi_type *type)
{
    if (type->type == FFI_TYPE_STRUCT) {
        for (ffi_type **elements = type->elements; *elements; ++elements) {
            bytecode_destroy_libffi_type(*elements);
        }
        free(type->elements);
        free(type);
    }
}

int bytecode_type_info_kind_to_reg_type_kind(enum bytecode_type kind)
{
    switch (kind) {
    case BYTECODE_TYPE_VOID:   return BYTECODE_REGISTER_KIND_I64;
    case BYTECODE_TYPE_I8:     return BYTECODE_REGISTER_KIND_I8;
    case BYTECODE_TYPE_I16:    return BYTECODE_REGISTER_KIND_I16;
    case BYTECODE_TYPE_I32:    return BYTECODE_REGISTER_KIND_I32;
    case BYTECODE_TYPE_I64:    return BYTECODE_REGISTER_KIND_I64;
    case BYTECODE_TYPE_F32:    return BYTECODE_REGISTER_KIND_F32;
    case BYTECODE_TYPE_F64:    return BYTECODE_REGISTER_KIND_F64;
    case BYTECODE_TYPE_PTR:    return BYTECODE_REGISTER_KIND_I64;
    case BYTECODE_TYPE_STRUCT: return BYTECODE_REGISTER_KIND_I64;
    default: assert(0); return 0;
    }
}

bytecode_instruction_handler_(exec_op_call_foreign)
{
    int64_t *stack = as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RSP]]);

    char *sym_name = (char *)((int64_t)*--stack);
    bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int64_t);
    --bcr->stack_info;

    char *lib_name = (char *)((int64_t)*--stack);
    bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int64_t);
    --bcr->stack_info;

    void *handle = dlopen(lib_name, RTLD_LAZY);
    assert(handle);

    void *func = dlsym(handle, sym_name);
    assert(func);

    uint64_t reg_arg_count = fetch_instruction(bcr);
    uint64_t ret_type_info_pos = fetch_instruction(bcr);

    struct bytecode_runner_type_info ret_type_info = create_type_info(bcr, ret_type_info_pos);
    bcr->reg_type[BYTECODE_REGISTER_RAX] = bytecode_type_info_kind_to_reg_type_kind(ret_type_info.kind);

    void *values[reg_arg_count];
    ffi_type *args[reg_arg_count];

    for (int i = 0; i < reg_arg_count; ++i) {
        int arg_type_info_pos = (int)((int64_t)*--stack);
        bcr->reg[BYTECODE_REGISTER_RSP] -= sizeof(int64_t);
        --bcr->stack_info;

        args[i]   = bytecode_create_libffi_type(bcr, arg_type_info_pos);
        values[i] = args[i]->type == FFI_TYPE_STRUCT
                      ? (void*)bcr->reg[bytecode_call_registers[i]]
                      : &bcr->reg[bytecode_call_registers[i]];
    }

    ffi_type *return_type = bytecode_create_libffi_type(bcr, ret_type_info_pos);

    ffi_cif cif;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, reg_arg_count, return_type, args) != FFI_OK) {
        fprintf(stderr, "ffi_prep_cif failed\n");
        exit(1);
    }

    if (return_type->type == FFI_TYPE_STRUCT) {
        ffi_call(&cif, FFI_FN(func), (void*)bcr->reg[BYTECODE_REGISTER_RDI], values);
        *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RAX]) = as_i64(bcr->reg[BYTECODE_REGISTER_RDI]);
    } else {
        ffi_call(&cif, FFI_FN(func), &bcr->reg[BYTECODE_REGISTER_RAX], values);
    }

    for (int i = 0; i < reg_arg_count; ++i) {
        bytecode_destroy_libffi_type(args[i]);
    }

    bytecode_destroy_libffi_type(return_type);
}

bytecode_instruction_handler_(exec_op_begin_call_frame)
{
    bytecode_push_reg(BYTECODE_REGISTER_RBP);
    *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RBP]) = as_i64(bcr->reg[BYTECODE_REGISTER_RSP]);
    bcr->reg_type[BYTECODE_REGISTER_RBP] = bcr->reg_type[BYTECODE_REGISTER_RSP];
}

bytecode_instruction_handler_(exec_op_end_call_frame)
{
    *as_i64_ptr(bcr->reg[BYTECODE_REGISTER_RSP]) = as_i64(bcr->reg[BYTECODE_REGISTER_RBP]);
    bcr->reg_type[BYTECODE_REGISTER_RSP] = bcr->reg_type[BYTECODE_REGISTER_RBP];
    bytecode_pop_reg1(BYTECODE_REGISTER_RBP);
}

bytecode_instruction_handler_(exec_op_return)
{
    bytecode_pop_reg1(BYTECODE_REGISTER_RIP);
}

bytecode_instruction_handler_(exec_op_lea_lcl_reg_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    *as_i64_ptr(bcr->reg[reg1]) = (uint64_t) as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + offset]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}

bytecode_instruction_handler_(exec_op_lea_lcl_reg_reg)
{
    *as_i64_ptr(bcr->reg[reg1]) = (uint64_t) as_i64_ptr(bcr->stack[bcr->reg[BYTECODE_REGISTER_RBP] + bcr->reg[reg2]]);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}

bytecode_instruction_handler_(exec_op_lea_bss_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    char *effective_address = bcr->data + as_i64(raw);
    assert(effective_address < bcr->data + bcr->data_size);
    *as_i64_ptr(bcr->reg[reg1]) = as_i64(effective_address);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}

bytecode_instruction_handler_(exec_op_lea_bss_reg_reg)
{
    char *effective_address = bcr->data + as_i64(bcr->reg[reg2]);
    assert(effective_address < bcr->data + bcr->data_size);
    *as_i64_ptr(bcr->reg[reg1]) = as_i64(effective_address);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}

bytecode_instruction_handler_(exec_op_memw_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    int8_t *address = (int8_t *) as_i64(bcr->reg[reg1]);
    *address = as_i8(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I8;
}
bytecode_instruction_handler_(exec_op_memw_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    int16_t *address = (int16_t *) as_i64(bcr->reg[reg1]);
    *address = as_i16(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I16;
}
bytecode_instruction_handler_(exec_op_memw_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    int32_t *address = (int32_t *) as_i64(bcr->reg[reg1]);
    *address = as_i32(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I32;
}
bytecode_instruction_handler_(exec_op_memw_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    int64_t *address = (int64_t *) as_i64(bcr->reg[reg1]);
    *address = as_i64(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}
bytecode_instruction_handler_(exec_op_memw_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    float *address = (float *) as_i64(bcr->reg[reg1]);
    *address = as_f32(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F32;
}
bytecode_instruction_handler_(exec_op_memw_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    double *address = (double *) as_i64(bcr->reg[reg1]);
    *address = as_f64(raw);
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F64;
}
bytecode_instruction_handler_(exec_op_memw_reg_reg)
{
    switch (bcr->reg_type[reg2]) {
    case BYTECODE_REGISTER_KIND_I64: {
        int64_t *address = (int64_t *) as_i64(bcr->reg[reg1]);
        *address = as_i64(bcr->reg[reg2]);
        bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        int32_t *address = (int32_t *) as_i64(bcr->reg[reg1]);
        *address = as_i32(bcr->reg[reg2]);
        bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I32;
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        int16_t *address = (int16_t *) as_i64(bcr->reg[reg1]);
        *address = as_i16(bcr->reg[reg2]);
        bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I16;
    } break;
    case BYTECODE_REGISTER_KIND_I8: {
        int8_t *address = (int8_t *) as_i64(bcr->reg[reg1]);
        *address = as_i8(bcr->reg[reg2]);
        bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I8;
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        double *address = (double *) as_i64(bcr->reg[reg1]);
        *address = as_f64(bcr->reg[reg2]);
        bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F64;
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        float *address = (float *) as_i64(bcr->reg[reg1]);
        *address = as_f32(bcr->reg[reg2]);
        bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F32;
    } break;
    }
}

bytecode_instruction_handler_(exec_op_memr_int8_reg_reg)
{
    int8_t *address = (int8_t *) as_i64(bcr->reg[reg2]);
    *as_i8_ptr(bcr->reg[reg1]) = *address;
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I8;
}
bytecode_instruction_handler_(exec_op_memr_int16_reg_reg)
{
    int16_t *address = (int16_t *) as_i64(bcr->reg[reg2]);
    *as_i16_ptr(bcr->reg[reg1]) = *address;
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I16;
}
bytecode_instruction_handler_(exec_op_memr_int32_reg_reg)
{
    int32_t *address = (int32_t *) as_i64(bcr->reg[reg2]);
    *as_i32_ptr(bcr->reg[reg1]) = *address;
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I32;
}
bytecode_instruction_handler_(exec_op_memr_int64_reg_reg)
{
    int64_t *address = (int64_t *) as_i64(bcr->reg[reg2]);
    *as_i64_ptr(bcr->reg[reg1]) = *address;
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}
bytecode_instruction_handler_(exec_op_memr_flt32_reg_reg)
{
    float *address = (float *) as_i64(bcr->reg[reg2]);
    *as_f32_ptr(bcr->reg[reg1]) = *address;
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F32;
}
bytecode_instruction_handler_(exec_op_memr_flt64_reg_reg)
{
    double *address = (double *) as_i64(bcr->reg[reg2]);
    *as_f64_ptr(bcr->reg[reg1]) = *address;
    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F64;
}

bytecode_instruction_handler_(exec_op_memc_reg_reg_imm)
{
    uint64_t size = fetch_instruction(bcr);
    void *src = (void *) as_i64(bcr->reg[reg2]);
    void *dest = (void *) as_i64(bcr->reg[reg1]);
    memcpy(dest, src, size);
}

bytecode_instruction_handler_(exec_op_conv_int8_reg)
{
    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I8: {
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        int16_t value = as_i16(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i8_ptr(bcr->reg[reg1]) = (int8_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        int32_t value = as_i32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i8_ptr(bcr->reg[reg1]) = (int8_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I64: {
        int64_t value = as_i64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i8_ptr(bcr->reg[reg1]) = (int8_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        float value = as_f32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i8_ptr(bcr->reg[reg1]) = (int8_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        double value = as_f64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i8_ptr(bcr->reg[reg1]) = (int8_t) value;
    } break;
    }

    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I8;
}
bytecode_instruction_handler_(exec_op_conv_int16_reg)
{
    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I8: {
        int8_t value = as_i8(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i16_ptr(bcr->reg[reg1]) = (int16_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        int32_t value = as_i32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i16_ptr(bcr->reg[reg1]) = (int16_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I64: {
        int64_t value = as_i64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i16_ptr(bcr->reg[reg1]) = (int16_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        float value = as_f32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i16_ptr(bcr->reg[reg1]) = (int16_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        double value = as_f64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i16_ptr(bcr->reg[reg1]) = (int16_t) value;
    } break;
    }

    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I16;
}
bytecode_instruction_handler_(exec_op_conv_int32_reg)
{
    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I8: {
        int8_t value = as_i8(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i32_ptr(bcr->reg[reg1]) = (int32_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        int16_t value = as_i16(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i32_ptr(bcr->reg[reg1]) = (int32_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
    } break;
    case BYTECODE_REGISTER_KIND_I64: {
        int64_t value = as_i64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i32_ptr(bcr->reg[reg1]) = (int32_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        float value = as_f32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i32_ptr(bcr->reg[reg1]) = (int32_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        double value = as_f64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_i32_ptr(bcr->reg[reg1]) = (int32_t) value;
    } break;
    }

    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I32;
}
bytecode_instruction_handler_(exec_op_conv_int64_reg)
{
    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I8: {
        int8_t value = as_i8(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = (int64_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        int16_t value = as_i16(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = (int64_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        int32_t value = as_i32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = (int64_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_I64: {
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        float value = as_f32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = (int64_t) value;
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        double value = as_f64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = (int64_t) value;
    } break;
    }

    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_I64;
}
bytecode_instruction_handler_(exec_op_conv_flt32_reg)
{
    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I8: {
        int8_t value = as_i8(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_f32_ptr(bcr->reg[reg1]) = (float) value;
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        int16_t value = as_i16(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_f32_ptr(bcr->reg[reg1]) = (float) value;
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        int32_t value = as_i32(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_f32_ptr(bcr->reg[reg1]) = (float) value;
    } break;
    case BYTECODE_REGISTER_KIND_I64: {
        int64_t value = as_i64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_f32_ptr(bcr->reg[reg1]) = (float) value;
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
        double value = as_f64(bcr->reg[reg1]);
        *as_i64_ptr(bcr->reg[reg1]) = 0;
        *as_f32_ptr(bcr->reg[reg1]) = (float) value;
    } break;
    }

    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F32;
}
bytecode_instruction_handler_(exec_op_conv_flt64_reg)
{
    switch (bcr->reg_type[reg1]) {
    case BYTECODE_REGISTER_KIND_I8: {
        int8_t value = as_i8(bcr->reg[reg1]);
        *as_f64_ptr(bcr->reg[reg1]) = (double) value;
    } break;
    case BYTECODE_REGISTER_KIND_I16: {
        int16_t value = as_i16(bcr->reg[reg1]);
        *as_f64_ptr(bcr->reg[reg1]) = (double) value;
    } break;
    case BYTECODE_REGISTER_KIND_I32: {
        int32_t value = as_i32(bcr->reg[reg1]);
        *as_f64_ptr(bcr->reg[reg1]) = (double) value;
    } break;
    case BYTECODE_REGISTER_KIND_I64: {
        int64_t value = as_i64(bcr->reg[reg1]);
        *as_f64_ptr(bcr->reg[reg1]) = (double) value;
    } break;
    case BYTECODE_REGISTER_KIND_F32: {
        float value = as_f32(bcr->reg[reg1]);
        *as_f64_ptr(bcr->reg[reg1]) = (double) value;
    } break;
    case BYTECODE_REGISTER_KIND_F64: {
    } break;
    }

    bcr->reg_type[reg1] = BYTECODE_REGISTER_KIND_F64;
}
