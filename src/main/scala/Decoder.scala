package decoder

import chisel3._
import chisel3.util._

import statecode.CoreState
import statecode.AluOpCode
import statecode.RegInputOp
import statecode.DecoderState

class Decoder extends Module {
    val io = IO(new Bundle {
        val core_state = Input(CoreState())
        val instruction = Input(UInt(16.W))

        // Instruction signals
        val decoded_reg_address = new Bundle {
            val rd = Output(UInt(4.W))
            val rs = Output(UInt(4.W))
            val rt = Output(UInt(4.W))
        }
        val decoded_nzp = Output(UInt(3.W))
        val decoded_immediate = Output(UInt(8.W))

        // Control signals
        val mem_rw_enable = new Bundle {
            val read_enable = Output(Bool())
            val write_enable = Output(Bool())
        }
        val decoded_reg_write_enable = Output(Bool())
        val decoded_nzp_write_enable = Output(Bool())
        val decoded_reg_input_mux = Output(RegInputOp())

        val decoded_alu_op = new Bundle {
            val arithmetic_mux = Output(AluOpCode())
            val output_mux = Output(Bool())
        }
        val decoded_pc_mux = Output(Bool())

        // Return (finished executing thread)
        val decoded_ret = Output(Bool())
    })

    val decoded_rd_address = RegInit(0.U(4.W))
    val decoded_rs_address = RegInit(0.U(4.W))
    val decoded_rt_address = RegInit(0.U(4.W))
    val decoded_nzp = RegInit(0.U(3.W))
    val decoded_immediate = RegInit(0.U(8.W))
    val decoded_reg_write_enable = RegInit(false.B)
    val decoded_mem_read_enable = RegInit(false.B)
    val decoded_mem_write_enable = RegInit(false.B)
    val decoded_nzp_write_enable = RegInit(false.B)
    val decoded_reg_input_mux = RegInit(RegInputOp.ARITHMETIC)
    val decoded_alu_arithmetic_mux = RegInit(AluOpCode.ADD)
    val decoded_alu_output_mux = RegInit(false.B)
    val decoded_pc_mux = RegInit(false.B)
    val decoded_ret = RegInit(false.B)

    when(!reset.asBool) {
        when(io.core_state === CoreState.DECODE) {
            decoded_rd_address := io.instruction(11, 8)
            decoded_rs_address := io.instruction(7, 4)
            decoded_rt_address := io.instruction(3, 0)
            decoded_immediate := io.instruction(7, 0)
            decoded_nzp := io.instruction(11, 9)

            // Control signals reset on every decode and set conditionally by instruction
            decoded_reg_write_enable := false.B
            decoded_mem_read_enable := false.B
            decoded_mem_write_enable := false.B
            decoded_nzp_write_enable := false.B
            decoded_reg_input_mux := RegInputOp.ARITHMETIC
            decoded_alu_arithmetic_mux := AluOpCode.ADD
            decoded_alu_output_mux := false.B
            decoded_pc_mux := false.B
            decoded_ret := false.B

            val (instruction_opcode, _) =
                DecoderState.safe(io.instruction(15, 12))
            switch(instruction_opcode) {
                is(DecoderState.NOP) {
                    // no-op
                }
                is(DecoderState.BRnzp) {
                    decoded_pc_mux := true.B
                }
                is(DecoderState.CMP) {
                    decoded_alu_output_mux := true.B
                    decoded_nzp_write_enable := true.B
                }
                is(DecoderState.ADD) {
                    decoded_reg_write_enable := true.B
                    decoded_reg_input_mux := RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux := AluOpCode.ADD
                }
                is(DecoderState.SUB) {
                    decoded_reg_write_enable := true.B
                    decoded_reg_input_mux := RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux := AluOpCode.SUB
                }
                is(DecoderState.MUL) {
                    decoded_reg_write_enable := true.B
                    decoded_reg_input_mux := RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux := AluOpCode.MUL
                }
                is(DecoderState.DIV) {
                    decoded_reg_write_enable := true.B
                    decoded_reg_input_mux := RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux := AluOpCode.DIV
                }
                is(DecoderState.LDR) {
                    decoded_reg_write_enable := true.B
                    decoded_reg_input_mux := RegInputOp.MEMORY
                    decoded_mem_read_enable := true.B
                }
                is(DecoderState.STR) {
                    decoded_mem_write_enable := true.B
                }
                is(DecoderState.CONST) {
                    decoded_reg_write_enable := true.B
                    decoded_reg_input_mux := RegInputOp.CONSTANT
                }
                is(DecoderState.RET) {
                    decoded_ret := true.B
                }
            }
        }
    }

    io.decoded_reg_address.rd := decoded_rd_address
    io.decoded_reg_address.rs := decoded_rs_address
    io.decoded_reg_address.rt := decoded_rt_address
    io.decoded_nzp := decoded_nzp
    io.decoded_immediate := decoded_immediate
    io.decoded_reg_write_enable := decoded_reg_write_enable
    io.mem_rw_enable.read_enable := decoded_mem_read_enable
    io.mem_rw_enable.write_enable := decoded_mem_write_enable
    io.decoded_nzp_write_enable := decoded_nzp_write_enable
    io.decoded_reg_input_mux := decoded_reg_input_mux
    io.decoded_alu_op.arithmetic_mux := decoded_alu_arithmetic_mux
    io.decoded_alu_op.output_mux := decoded_alu_output_mux
    io.decoded_pc_mux := decoded_pc_mux
    io.decoded_ret := decoded_ret
}
