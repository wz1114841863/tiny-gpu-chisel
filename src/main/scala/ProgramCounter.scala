package pc

import chisel3._
import chisel3.util._
import statecode.CoreState
import scala.reflect.internal.Reporter.INFO

class ProgramCounter(DataMemWidth: Int = 8, MemAddrWidth: Int = 8)
    extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val core_state = Input(CoreState())

        val decoded_nzp = Input(UInt(3.W))
        val decoded_immediate = Input(UInt(DataMemWidth.W))
        val decoded_nzp_write_enable = Input(Bool())
        val decoded_pc_mux = Input(Bool())

        val alu_out = Input(UInt(DataMemWidth.W))

        val current_pc = Input(UInt(MemAddrWidth.W))
        val next_pc = Output(UInt(MemAddrWidth.W))
    })
    val nzp = RegInit(0.U(3.W))
    val next_pc = RegInit(0.U(MemAddrWidth.W))

    when(io.enable && io.core_state === CoreState.EXECUTE) {
        when(io.decoded_pc_mux) {
            when((nzp & io.decoded_nzp) =/= 0.U) {
                next_pc := io.decoded_immediate
            }.otherwise {
                next_pc := io.current_pc + 1.U
            }
        }.otherwise {
            next_pc := io.current_pc + 1.U
        }
    }

    when(io.enable && io.core_state === CoreState.UPDATE) {
        when(io.decoded_nzp_write_enable) {
            nzp := io.alu_out(2, 0)
        }
    }

    when(reset.asBool) {
        io.next_pc := 0.U
    }.otherwise {
        io.next_pc := next_pc
    }
}
