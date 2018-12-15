#ifndef BYTECODE_INSTRUCTION_HANDLERS
#define BYTECODE_INSTRUCTION_HANDLERS

#include "bytecode_instruction.h"

struct bytecode_runner;
void bytecode_instruction_execute(struct bytecode_runner *bcr, struct bytecode_instruction instr);

bytecode_instruction_handler_(exec_op_halt);
bytecode_instruction_handler_(exec_op_nop);

bytecode_instruction_handler_(exec_op_mov_int8_reg_imm);
bytecode_instruction_handler_(exec_op_mov_int16_reg_imm);
bytecode_instruction_handler_(exec_op_mov_int32_reg_imm);
bytecode_instruction_handler_(exec_op_mov_int64_reg_imm);
bytecode_instruction_handler_(exec_op_mov_flt32_reg_imm);
bytecode_instruction_handler_(exec_op_mov_flt64_reg_imm);
bytecode_instruction_handler_(exec_op_mov_reg_reg);

bytecode_instruction_handler_(exec_op_mov_int8_lcl_imm);
bytecode_instruction_handler_(exec_op_mov_int16_lcl_imm);
bytecode_instruction_handler_(exec_op_mov_int32_lcl_imm);
bytecode_instruction_handler_(exec_op_mov_int64_lcl_imm);
bytecode_instruction_handler_(exec_op_mov_flt32_lcl_imm);
bytecode_instruction_handler_(exec_op_mov_flt64_lcl_imm);
bytecode_instruction_handler_(exec_op_mov_lcl_reg);
bytecode_instruction_handler_(exec_op_mov_int8_reg_lcl);
bytecode_instruction_handler_(exec_op_mov_int16_reg_lcl);
bytecode_instruction_handler_(exec_op_mov_int32_reg_lcl);
bytecode_instruction_handler_(exec_op_mov_int64_reg_lcl);
bytecode_instruction_handler_(exec_op_mov_flt32_reg_lcl);
bytecode_instruction_handler_(exec_op_mov_flt64_reg_lcl);

bytecode_instruction_handler_(exec_op_push_int8_imm);
bytecode_instruction_handler_(exec_op_push_int16_imm);
bytecode_instruction_handler_(exec_op_push_int32_imm);
bytecode_instruction_handler_(exec_op_push_int64_imm);
bytecode_instruction_handler_(exec_op_push_flt32_imm);
bytecode_instruction_handler_(exec_op_push_flt64_imm);
bytecode_instruction_handler_(exec_op_push_reg);

bytecode_instruction_handler_(exec_op_pop_int8_reg);
bytecode_instruction_handler_(exec_op_pop_int16_reg);
bytecode_instruction_handler_(exec_op_pop_int32_reg);
bytecode_instruction_handler_(exec_op_pop_int64_reg);
bytecode_instruction_handler_(exec_op_pop_flt32_reg);
bytecode_instruction_handler_(exec_op_pop_flt64_reg);

bytecode_instruction_handler_(exec_op_add_int8_reg_imm);
bytecode_instruction_handler_(exec_op_add_int16_reg_imm);
bytecode_instruction_handler_(exec_op_add_int32_reg_imm);
bytecode_instruction_handler_(exec_op_add_int64_reg_imm);
bytecode_instruction_handler_(exec_op_add_flt32_reg_imm);
bytecode_instruction_handler_(exec_op_add_flt64_reg_imm);
bytecode_instruction_handler_(exec_op_add_reg_reg);

bytecode_instruction_handler_(exec_op_sub_int8_reg_imm);
bytecode_instruction_handler_(exec_op_sub_int16_reg_imm);
bytecode_instruction_handler_(exec_op_sub_int32_reg_imm);
bytecode_instruction_handler_(exec_op_sub_int64_reg_imm);
bytecode_instruction_handler_(exec_op_sub_flt32_reg_imm);
bytecode_instruction_handler_(exec_op_sub_flt64_reg_imm);
bytecode_instruction_handler_(exec_op_sub_reg_reg);

bytecode_instruction_handler_(exec_op_mul_int8_reg_imm);
bytecode_instruction_handler_(exec_op_mul_int16_reg_imm);
bytecode_instruction_handler_(exec_op_mul_int32_reg_imm);
bytecode_instruction_handler_(exec_op_mul_int64_reg_imm);
bytecode_instruction_handler_(exec_op_mul_flt32_reg_imm);
bytecode_instruction_handler_(exec_op_mul_flt64_reg_imm);
bytecode_instruction_handler_(exec_op_mul_reg_reg);

bytecode_instruction_handler_(exec_op_div_int8_reg_imm);
bytecode_instruction_handler_(exec_op_div_int16_reg_imm);
bytecode_instruction_handler_(exec_op_div_int32_reg_imm);
bytecode_instruction_handler_(exec_op_div_int64_reg_imm);
bytecode_instruction_handler_(exec_op_div_flt32_reg_imm);
bytecode_instruction_handler_(exec_op_div_flt64_reg_imm);
bytecode_instruction_handler_(exec_op_div_reg_reg);

bytecode_instruction_handler_(exec_op_not_reg);
bytecode_instruction_handler_(exec_op_neg_reg);
bytecode_instruction_handler_(exec_op_inc_reg);
bytecode_instruction_handler_(exec_op_dec_reg);
bytecode_instruction_handler_(exec_op_xor_reg_reg);

bytecode_instruction_handler_(exec_op_test_reg_imm);
bytecode_instruction_handler_(exec_op_test_reg_reg);
bytecode_instruction_handler_(exec_op_cmp_reg_imm);
bytecode_instruction_handler_(exec_op_cmp_reg_reg);
bytecode_instruction_handler_(exec_op_jmp_imm);
bytecode_instruction_handler_(exec_op_jnz_imm);
bytecode_instruction_handler_(exec_op_jz_imm);
bytecode_instruction_handler_(exec_op_jle_imm);
bytecode_instruction_handler_(exec_op_jl_imm);
bytecode_instruction_handler_(exec_op_jge_imm);
bytecode_instruction_handler_(exec_op_jg_imm);

bytecode_instruction_handler_(exec_op_call_imm);
bytecode_instruction_handler_(exec_op_call_reg);
bytecode_instruction_handler_(exec_op_call_foreign);

bytecode_instruction_handler_(exec_op_begin_call_frame);
bytecode_instruction_handler_(exec_op_end_call_frame);
bytecode_instruction_handler_(exec_op_return);

bytecode_instruction_handler_(exec_op_lea_lcl_reg_imm);
bytecode_instruction_handler_(exec_op_lea_lcl_reg_reg);
bytecode_instruction_handler_(exec_op_lea_bss_reg_imm);
bytecode_instruction_handler_(exec_op_lea_bss_reg_reg);

bytecode_instruction_handler_(exec_op_memw_int8_reg_imm);
bytecode_instruction_handler_(exec_op_memw_int16_reg_imm);
bytecode_instruction_handler_(exec_op_memw_int32_reg_imm);
bytecode_instruction_handler_(exec_op_memw_int64_reg_imm);
bytecode_instruction_handler_(exec_op_memw_flt32_reg_imm);
bytecode_instruction_handler_(exec_op_memw_flt64_reg_imm);
bytecode_instruction_handler_(exec_op_memw_reg_reg);

bytecode_instruction_handler_(exec_op_memr_int8_reg_reg);
bytecode_instruction_handler_(exec_op_memr_int16_reg_reg);
bytecode_instruction_handler_(exec_op_memr_int32_reg_reg);
bytecode_instruction_handler_(exec_op_memr_int64_reg_reg);
bytecode_instruction_handler_(exec_op_memr_flt32_reg_reg);
bytecode_instruction_handler_(exec_op_memr_flt64_reg_reg);

#endif
