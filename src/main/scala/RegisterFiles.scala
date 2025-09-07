package registers

import chisel3._
import chisel3.util._
import statecode.CoreState
import statecode.RegInputOp
import javax.swing.InputMap

class RegisterFiles(ThreadsPerBlk: Int = 4, ThreadId: Int = 0, DataBits: Int = 8) extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        // Kernel Execution
        val block_id = Input(UInt(8.W))
        // State
        val core_state = Input(CoreState())
        // Instruction Signals
        val decoded_reg_address = new Bundle (
            val rs = Input(UInt(4.W)),
            val rt = Input(UInt(4.W)),
            val rd = Input(UInt(4.W))
        )
        // Control Signals
        val decoded_reg_write_enable = Input(Bool())
        val decoded_reg_input_op = Input(RegInputOp())
    })
}
