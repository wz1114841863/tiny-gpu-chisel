package decoder

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import statecode.AluOpCode
import statecode.RegInputOp

class DecoderModel {
    var decoded_rd_address = 0
    var decoded_rs_address = 0
    var decoded_rt_address = 0
    var decoded_nzp = 0
    var decoded_immediate = 0
    var decoded_reg_write_enable = false
    var decoded_mem_read_enable = false
    var decoded_mem_write_enable = false
    var decoded_nzp_write_enable = false
    var decoded_reg_input_mux = RegInputOp.ARITHMETIC
    var decoded_alu_arithmetic_mux = AluOpCode.ADD
    var decoded_alu_output_mux = false
    var decoded_pc_mux = false
    var decoded_ret = false

    def update(
        core_state: CoreState.Type,
        instruction: Int
    ): Unit = {
        if (core_state == CoreState.DECODE) {
            decoded_rd_address = (instruction & 0xf00) >> 8
            decoded_rs_address = (instruction & 0xf0) >> 4
            decoded_rt_address = instruction & 0x000f
            decoded_immediate = instruction & 0x00ff
            decoded_nzp = (instruction & 0xe00) >> 9

            // reset
            decoded_reg_write_enable = false
            decoded_mem_read_enable = false
            decoded_mem_write_enable = false
            decoded_nzp_write_enable = false
            decoded_reg_input_mux = RegInputOp.ARITHMETIC
            decoded_alu_arithmetic_mux = AluOpCode.ADD
            decoded_alu_output_mux = false
            decoded_pc_mux = false
            decoded_ret = false

            val instruction_opcode = (instruction & 0xf000) >> 12
            instruction_opcode match {
                case 0x0 => {
                    // DecoderState.NOP
                }
                case 0x1 => {
                    // DecoderState.BRnzp
                    decoded_pc_mux = true
                }
                case 0x2 => {
                    // DecoderState.CMP
                    decoded_alu_output_mux = true
                    decoded_nzp_write_enable = true
                }
                case 0x3 => {
                    // DecoderState.ADD
                    decoded_reg_write_enable = true
                    decoded_reg_input_mux = RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux = AluOpCode.ADD
                }
                case 0x4 => {
                    // DecoderState.SUB
                    decoded_reg_write_enable = true
                    decoded_reg_input_mux = RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux = AluOpCode.SUB
                }
                case 0x5 => {
                    // DecoderState.MUL
                    decoded_reg_write_enable = true
                    decoded_reg_input_mux = RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux = AluOpCode.MUL
                }
                case 0x6 => {
                    // DecoderState.DIV
                    decoded_reg_write_enable = true
                    decoded_reg_input_mux = RegInputOp.ARITHMETIC
                    decoded_alu_arithmetic_mux = AluOpCode.DIV
                }
                case 0x7 => {
                    // DecoderState.LDR
                    decoded_reg_write_enable = true
                    decoded_reg_input_mux = RegInputOp.MEMORY
                    decoded_mem_read_enable = true
                }
                case 0x8 => {
                    // DecoderState.STR
                    decoded_mem_write_enable = true
                }
                case 0x9 => {
                    // DecoderState.CONST
                    decoded_reg_write_enable = true
                    decoded_reg_input_mux = RegInputOp.CONSTANT
                }
                case 0xf => {
                    // DecoderState.RET
                    decoded_ret = true
                }
                case _ => {}
            }
        }
    }
}

class DecoderSpec extends AnyFreeSpec with Matchers {
    "Test decoder" in {
        val ProgramAddrBits: Int = 8
        val ProgramDataBits: Int = 16
        simulate(new Decoder) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            var cnt = 0
            val rng = new scala.util.Random(42)
            val decoderModel = new DecoderModel()
            while (cnt < 10000) {
                val core_state = CoreState.DECODE
                val instruction = rng.nextInt(65536) // 2^16 for 16-bit data

                dut.io.core_state.poke(core_state)
                dut.io.instruction.poke(instruction.U)

                dut.clock.step()

                decoderModel.update(core_state, instruction)
                dut.io.decoded_reg_address.rd.expect(decoderModel.decoded_rd_address.U)
                dut.io.decoded_reg_address.rs.expect(decoderModel.decoded_rs_address.U)
                dut.io.decoded_reg_address.rt.expect(decoderModel.decoded_rt_address.U)
                dut.io.decoded_immediate.expect(decoderModel.decoded_immediate.U)

                dut.io.mem_rw_enable.read_enable.expect(decoderModel.decoded_mem_read_enable.B)
                dut.io.mem_rw_enable.write_enable.expect(decoderModel.decoded_mem_write_enable.B)

                dut.io.decoded_reg_write_enable.expect(decoderModel.decoded_reg_write_enable.B)
                dut.io.decoded_nzp_write_enable.expect(decoderModel.decoded_nzp_write_enable.B)
                assert(dut.io.decoded_reg_input_mux.peekValue().asBigInt == decoderModel.decoded_reg_input_mux.litValue)

                assert(
                    dut.io.decoded_alu_op.arithmetic_mux
                        .peekValue()
                        .asBigInt == decoderModel.decoded_alu_arithmetic_mux.litValue
                )
                dut.io.decoded_alu_op.output_mux.expect(decoderModel.decoded_alu_output_mux.B)

                dut.io.decoded_pc_mux.expect(decoderModel.decoded_pc_mux.B)
                dut.io.decoded_ret.expect(decoderModel.decoded_ret.B)

                cnt += 1
            }
        }
    }
}
