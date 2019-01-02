#include "bytecode_opcode.h"
#include "bytecode_instruction.h"

bytecode_instruction_handler_(disasm_op_halt)
{
}

bytecode_instruction_handler_(disasm_op_nop)
{
}

bytecode_instruction_handler_(disasm_op_mov_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_mov_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_mov_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_mov_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_mov_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_mov_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_mov_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_mov_int8_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], 0x%llx", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, raw);
}
bytecode_instruction_handler_(disasm_op_mov_int16_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], 0x%llx", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, raw);
}
bytecode_instruction_handler_(disasm_op_mov_int32_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], 0x%llx", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, raw);
}
bytecode_instruction_handler_(disasm_op_mov_int64_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], 0x%llx", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, raw);
}
bytecode_instruction_handler_(disasm_op_mov_flt32_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], 0x%llx", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, raw);
}
bytecode_instruction_handler_(disasm_op_mov_flt64_lcl_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], 0x%llx", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, raw);
}
bytecode_instruction_handler_(disasm_op_mov_lcl_reg)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t[%s+0x%llx], %s", bytecode_register_str[BYTECODE_REGISTER_RBP], offset, bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_mov_int8_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, [%s+0x%llx]", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}
bytecode_instruction_handler_(disasm_op_mov_int16_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, [%s+0x%llx]", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}
bytecode_instruction_handler_(disasm_op_mov_int32_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, [%s+0x%llx]", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}
bytecode_instruction_handler_(disasm_op_mov_int64_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, [%s+0x%llx]", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}
bytecode_instruction_handler_(disasm_op_mov_flt32_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, [%s+0x%llx]", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}
bytecode_instruction_handler_(disasm_op_mov_flt64_reg_lcl)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, [%s+0x%llx]", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}

bytecode_instruction_handler_(disasm_op_push_int8_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_push_int16_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_push_int32_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_push_int64_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_push_flt32_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_push_flt64_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_push_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_pop_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_pop_int8_reg)
{
    assert(0);
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_pop_int16_reg)
{
    assert(0);
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_pop_int32_reg)
{
    assert(0);
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_pop_int64_reg)
{
    assert(0);
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_pop_flt32_reg)
{
    assert(0);
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_pop_flt64_reg)
{
    assert(0);
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_add_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_add_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_add_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_add_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_add_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_add_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_add_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_sub_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_sub_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_sub_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_sub_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_sub_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_sub_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_sub_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_mul_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_mul_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_mul_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_mul_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_mul_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_mul_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_mul_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_div_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_div_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_div_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_div_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_div_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_div_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_div_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_log_not_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_not_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_neg_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_inc_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_dec_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_xor_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_or_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_and_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_lshift_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_rshift_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_test_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_test_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_cmp_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_cmp_reg_reg)
{
    printf("\t%3s, %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_jmp_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_jnz_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_jz_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_jle_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_jl_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_jge_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}
bytecode_instruction_handler_(disasm_op_jg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}

bytecode_instruction_handler_(disasm_op_call_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t0x%llx", raw);
}

bytecode_instruction_handler_(disasm_op_call_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

bytecode_instruction_handler_(disasm_op_call_foreign)
{
    uint64_t reg_arg_count = fetch_instruction(bcr);
    uint64_t ret_kind = fetch_instruction(bcr);
    printf("\t0x%llx, 0x%llx", reg_arg_count, ret_kind);
}

bytecode_instruction_handler_(disasm_op_begin_call_frame)
{
}

bytecode_instruction_handler_(disasm_op_end_call_frame)
{
}

bytecode_instruction_handler_(disasm_op_return)
{
}

bytecode_instruction_handler_(disasm_op_lea_lcl_reg_imm)
{
    uint64_t offset = fetch_instruction(bcr);
    printf("\t%3s, %s+0x%llx", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], offset);
}

bytecode_instruction_handler_(disasm_op_lea_lcl_reg_reg)
{
    printf("\t%3s, %s+%s", bytecode_register_str[reg1], bytecode_register_str[BYTECODE_REGISTER_RBP], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_lea_bss_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t%3s, bss+0x%llx", bytecode_register_str[reg1], raw);
}

bytecode_instruction_handler_(disasm_op_lea_bss_reg_reg)
{
    printf("\t%3s, bss+%s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_memw_int8_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%3s], 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_memw_int16_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%3s], 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_memw_int32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%3s], 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_memw_int64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%3s], 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_memw_flt32_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%3s], 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_memw_flt64_reg_imm)
{
    uint64_t raw = fetch_instruction(bcr);
    printf("\t[%3s], 0x%llx", bytecode_register_str[reg1], raw);
}
bytecode_instruction_handler_(disasm_op_memw_reg_reg)
{
    printf("\t[%3s], %s", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_memr_int8_reg_reg)
{
    printf("\t%3s, [%s]", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_memr_int16_reg_reg)
{
    printf("\t%3s, [%s]", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_memr_int32_reg_reg)
{
    printf("\t%3s, [%s]", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_memr_int64_reg_reg)
{
    printf("\t%3s, [%s]", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_memr_flt32_reg_reg)
{
    printf("\t%3s, [%s]", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}
bytecode_instruction_handler_(disasm_op_memr_flt64_reg_reg)
{
    printf("\t%3s, [%s]", bytecode_register_str[reg1], bytecode_register_str[reg2]);
}

bytecode_instruction_handler_(disasm_op_memc_reg_reg_imm)
{
    uint64_t size = fetch_instruction(bcr);
    printf("\t[%3s], [%s], 0x%llx", bytecode_register_str[reg1], bytecode_register_str[reg2], size);
}

bytecode_instruction_handler_(disasm_op_conv_int8_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_conv_int16_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_conv_int32_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_conv_int64_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_conv_flt32_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}
bytecode_instruction_handler_(disasm_op_conv_flt64_reg)
{
    printf("\t%3s", bytecode_register_str[reg1]);
}

static bytecode_instruction_handler *instruction_disasm_handlers[BYTECODE_OPCODE_COUNT] = {
    [BYTECODE_OPCODE_HALT]               = disasm_op_halt,

    [BYTECODE_OPCODE_MOV_INT8_REG_IMM]   = disasm_op_mov_int8_reg_imm,
    [BYTECODE_OPCODE_MOV_INT16_REG_IMM]  = disasm_op_mov_int16_reg_imm,
    [BYTECODE_OPCODE_MOV_INT32_REG_IMM]  = disasm_op_mov_int32_reg_imm,
    [BYTECODE_OPCODE_MOV_INT64_REG_IMM]  = disasm_op_mov_int64_reg_imm,
    [BYTECODE_OPCODE_MOV_FLT32_REG_IMM]  = disasm_op_mov_flt32_reg_imm,
    [BYTECODE_OPCODE_MOV_FLT64_REG_IMM]  = disasm_op_mov_flt64_reg_imm,
    [BYTECODE_OPCODE_MOV_REG_REG]        = disasm_op_mov_reg_reg,
    [BYTECODE_OPCODE_MOV_INT8_LCL_IMM]   = disasm_op_mov_int8_lcl_imm,
    [BYTECODE_OPCODE_MOV_INT16_LCL_IMM]  = disasm_op_mov_int16_lcl_imm,
    [BYTECODE_OPCODE_MOV_INT32_LCL_IMM]  = disasm_op_mov_int32_lcl_imm,
    [BYTECODE_OPCODE_MOV_INT64_LCL_IMM]  = disasm_op_mov_int64_lcl_imm,
    [BYTECODE_OPCODE_MOV_FLT32_LCL_IMM]  = disasm_op_mov_flt32_lcl_imm,
    [BYTECODE_OPCODE_MOV_FLT64_LCL_IMM]  = disasm_op_mov_flt64_lcl_imm,
    [BYTECODE_OPCODE_MOV_LCL_REG]        = disasm_op_mov_lcl_reg,
    [BYTECODE_OPCODE_MOV_INT8_REG_LCL]   = disasm_op_mov_int8_reg_lcl,
    [BYTECODE_OPCODE_MOV_INT16_REG_LCL]  = disasm_op_mov_int16_reg_lcl,
    [BYTECODE_OPCODE_MOV_INT32_REG_LCL]  = disasm_op_mov_int32_reg_lcl,
    [BYTECODE_OPCODE_MOV_INT64_REG_LCL]  = disasm_op_mov_int64_reg_lcl,
    [BYTECODE_OPCODE_MOV_FLT32_REG_LCL]  = disasm_op_mov_flt32_reg_lcl,
    [BYTECODE_OPCODE_MOV_FLT64_REG_LCL]  = disasm_op_mov_flt64_reg_lcl,

    [BYTECODE_OPCODE_PUSH_INT8_IMM]      = disasm_op_push_int8_imm,
    [BYTECODE_OPCODE_PUSH_INT16_IMM]     = disasm_op_push_int16_imm,
    [BYTECODE_OPCODE_PUSH_INT32_IMM]     = disasm_op_push_int32_imm,
    [BYTECODE_OPCODE_PUSH_INT64_IMM]     = disasm_op_push_int64_imm,
    [BYTECODE_OPCODE_PUSH_FLT32_IMM]     = disasm_op_push_flt32_imm,
    [BYTECODE_OPCODE_PUSH_FLT64_IMM]     = disasm_op_push_flt64_imm,
    [BYTECODE_OPCODE_PUSH_REG]           = disasm_op_push_reg,

    [BYTECODE_OPCODE_POP_REG]            = disasm_op_pop_reg,
    [BYTECODE_OPCODE_POP_INT8_REG]       = disasm_op_pop_int8_reg,
    [BYTECODE_OPCODE_POP_INT16_REG]      = disasm_op_pop_int16_reg,
    [BYTECODE_OPCODE_POP_INT32_REG]      = disasm_op_pop_int32_reg,
    [BYTECODE_OPCODE_POP_INT64_REG]      = disasm_op_pop_int64_reg,
    [BYTECODE_OPCODE_POP_FLT32_REG]      = disasm_op_pop_flt32_reg,
    [BYTECODE_OPCODE_POP_FLT64_REG]      = disasm_op_pop_flt64_reg,

    [BYTECODE_OPCODE_ADD_INT8_REG_IMM]   = disasm_op_add_int8_reg_imm,
    [BYTECODE_OPCODE_ADD_INT16_REG_IMM]  = disasm_op_add_int16_reg_imm,
    [BYTECODE_OPCODE_ADD_INT32_REG_IMM]  = disasm_op_add_int32_reg_imm,
    [BYTECODE_OPCODE_ADD_INT64_REG_IMM]  = disasm_op_add_int64_reg_imm,
    [BYTECODE_OPCODE_ADD_FLT32_REG_IMM]  = disasm_op_add_flt32_reg_imm,
    [BYTECODE_OPCODE_ADD_FLT64_REG_IMM]  = disasm_op_add_flt64_reg_imm,
    [BYTECODE_OPCODE_ADD_REG_REG]        = disasm_op_add_reg_reg,

    [BYTECODE_OPCODE_SUB_INT8_REG_IMM]   = disasm_op_sub_int8_reg_imm,
    [BYTECODE_OPCODE_SUB_INT16_REG_IMM]  = disasm_op_sub_int16_reg_imm,
    [BYTECODE_OPCODE_SUB_INT32_REG_IMM]  = disasm_op_sub_int32_reg_imm,
    [BYTECODE_OPCODE_SUB_INT64_REG_IMM]  = disasm_op_sub_int64_reg_imm,
    [BYTECODE_OPCODE_SUB_FLT32_REG_IMM]  = disasm_op_sub_flt32_reg_imm,
    [BYTECODE_OPCODE_SUB_FLT64_REG_IMM]  = disasm_op_sub_flt64_reg_imm,
    [BYTECODE_OPCODE_SUB_REG_REG]        = disasm_op_sub_reg_reg,

    [BYTECODE_OPCODE_MUL_INT8_REG_IMM]   = disasm_op_mul_int8_reg_imm,
    [BYTECODE_OPCODE_MUL_INT16_REG_IMM]  = disasm_op_mul_int16_reg_imm,
    [BYTECODE_OPCODE_MUL_INT32_REG_IMM]  = disasm_op_mul_int32_reg_imm,
    [BYTECODE_OPCODE_MUL_INT64_REG_IMM]  = disasm_op_mul_int64_reg_imm,
    [BYTECODE_OPCODE_MUL_FLT32_REG_IMM]  = disasm_op_mul_flt32_reg_imm,
    [BYTECODE_OPCODE_MUL_FLT64_REG_IMM]  = disasm_op_mul_flt64_reg_imm,
    [BYTECODE_OPCODE_MUL_REG_REG]        = disasm_op_mul_reg_reg,

    [BYTECODE_OPCODE_DIV_INT8_REG_IMM]   = disasm_op_div_int8_reg_imm,
    [BYTECODE_OPCODE_DIV_INT16_REG_IMM]  = disasm_op_div_int16_reg_imm,
    [BYTECODE_OPCODE_DIV_INT32_REG_IMM]  = disasm_op_div_int32_reg_imm,
    [BYTECODE_OPCODE_DIV_INT64_REG_IMM]  = disasm_op_div_int64_reg_imm,
    [BYTECODE_OPCODE_DIV_FLT32_REG_IMM]  = disasm_op_div_flt32_reg_imm,
    [BYTECODE_OPCODE_DIV_FLT64_REG_IMM]  = disasm_op_div_flt64_reg_imm,
    [BYTECODE_OPCODE_DIV_REG_REG]        = disasm_op_div_reg_reg,

    [BYTECODE_OPCODE_LOG_NOT_REG]        = disasm_op_log_not_reg,
    [BYTECODE_OPCODE_NOT_REG]            = disasm_op_not_reg,
    [BYTECODE_OPCODE_NEG_REG]            = disasm_op_neg_reg,
    [BYTECODE_OPCODE_INC_REG]            = disasm_op_inc_reg,
    [BYTECODE_OPCODE_DEC_REG]            = disasm_op_dec_reg,
    [BYTECODE_OPCODE_XOR_REG_REG]        = disasm_op_xor_reg_reg,
    [BYTECODE_OPCODE_OR_REG_REG]         = disasm_op_or_reg_reg,
    [BYTECODE_OPCODE_AND_REG_REG]        = disasm_op_and_reg_reg,
    [BYTECODE_OPCODE_LSHIFT_REG_REG]     = disasm_op_lshift_reg_reg,
    [BYTECODE_OPCODE_RSHIFT_REG_REG]     = disasm_op_rshift_reg_reg,

    [BYTECODE_OPCODE_TEST_REG_IMM]       = disasm_op_test_reg_imm,
    [BYTECODE_OPCODE_TEST_REG_REG]       = disasm_op_test_reg_reg,
    [BYTECODE_OPCODE_CMP_REG_IMM]        = disasm_op_cmp_reg_imm,
    [BYTECODE_OPCODE_CMP_REG_REG]        = disasm_op_cmp_reg_reg,
    [BYTECODE_OPCODE_JMP_IMM]            = disasm_op_jmp_imm,
    [BYTECODE_OPCODE_JNZ_IMM]            = disasm_op_jnz_imm,
    [BYTECODE_OPCODE_JZ_IMM]             = disasm_op_jz_imm,
    [BYTECODE_OPCODE_JLE_IMM]            = disasm_op_jle_imm,
    [BYTECODE_OPCODE_JL_IMM]             = disasm_op_jl_imm,
    [BYTECODE_OPCODE_JGE_IMM]            = disasm_op_jge_imm,
    [BYTECODE_OPCODE_JG_IMM]             = disasm_op_jg_imm,

    [BYTECODE_OPCODE_CALL_IMM]           = disasm_op_call_imm,
    [BYTECODE_OPCODE_CALL_REG]           = disasm_op_call_reg,
    [BYTECODE_OPCODE_CALL_FOREIGN]       = disasm_op_call_foreign,

    [BYTECODE_OPCODE_BEGIN_CALL_FRAME]   = disasm_op_begin_call_frame,
    [BYTECODE_OPCODE_END_CALL_FRAME]     = disasm_op_end_call_frame,
    [BYTECODE_OPCODE_RETURN]             = disasm_op_return,

    [BYTECODE_OPCODE_LEA_LCL_REG_IMM]    = disasm_op_lea_lcl_reg_imm,
    [BYTECODE_OPCODE_LEA_LCL_REG_REG]    = disasm_op_lea_lcl_reg_reg,
    [BYTECODE_OPCODE_LEA_BSS_REG_IMM]    = disasm_op_lea_bss_reg_imm,
    [BYTECODE_OPCODE_LEA_BSS_REG_REG]    = disasm_op_lea_bss_reg_reg,

    [BYTECODE_OPCODE_MEMW_INT8_REG_IMM]  = disasm_op_memw_int8_reg_imm,
    [BYTECODE_OPCODE_MEMW_INT16_REG_IMM] = disasm_op_memw_int16_reg_imm,
    [BYTECODE_OPCODE_MEMW_INT32_REG_IMM] = disasm_op_memw_int32_reg_imm,
    [BYTECODE_OPCODE_MEMW_INT64_REG_IMM] = disasm_op_memw_int64_reg_imm,
    [BYTECODE_OPCODE_MEMW_FLT32_REG_IMM] = disasm_op_memw_flt32_reg_imm,
    [BYTECODE_OPCODE_MEMW_FLT64_REG_IMM] = disasm_op_memw_flt64_reg_imm,
    [BYTECODE_OPCODE_MEMW_REG_REG]       = disasm_op_memw_reg_reg,

    [BYTECODE_OPCODE_MEMR_INT8_REG_REG]  = disasm_op_memr_int8_reg_reg,
    [BYTECODE_OPCODE_MEMR_INT16_REG_REG] = disasm_op_memr_int16_reg_reg,
    [BYTECODE_OPCODE_MEMR_INT32_REG_REG] = disasm_op_memr_int32_reg_reg,
    [BYTECODE_OPCODE_MEMR_INT64_REG_REG] = disasm_op_memr_int64_reg_reg,
    [BYTECODE_OPCODE_MEMR_FLT32_REG_REG] = disasm_op_memr_flt32_reg_reg,
    [BYTECODE_OPCODE_MEMR_FLT64_REG_REG] = disasm_op_memr_flt64_reg_reg,

    [BYTECODE_OPCODE_MEMC_REG_REG_IMM]   = disasm_op_memc_reg_reg_imm,

    [BYTECODE_OPCODE_CONV_INT8_REG]      = disasm_op_conv_int8_reg,
    [BYTECODE_OPCODE_CONV_INT16_REG]     = disasm_op_conv_int16_reg,
    [BYTECODE_OPCODE_CONV_INT32_REG]     = disasm_op_conv_int32_reg,
    [BYTECODE_OPCODE_CONV_INT64_REG]     = disasm_op_conv_int64_reg,
    [BYTECODE_OPCODE_CONV_FLT32_REG]     = disasm_op_conv_flt32_reg,
    [BYTECODE_OPCODE_CONV_FLT64_REG]     = disasm_op_conv_flt64_reg,

    [BYTECODE_OPCODE_NOP]                = disasm_op_nop
};

void disassemble_instruction(struct bytecode_runner *bcr, struct bytecode_instruction instr)
{
    if (instruction_disasm_handlers[instr.op]) {
        printf("0x%08llx: %18s", bcr->reg[BYTECODE_REGISTER_RIP]-1, bytecode_opcode_str[instr.op]);
        instruction_disasm_handlers[instr.op](bcr, instr.op, instr.r1, instr.r2);
        printf("\n");
    } else {
        printf("<unknown>\n");
    }
}
