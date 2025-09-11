package dispatch

import chisel3._
import chisel3.util._
import org.apache.commons.lang3.ThreadUtils.ThreadPredicate

class Dispatch(NumCores: Int = 2, ThreadsPerCore: Int = 4) extends Module {
    val ThreadCountWidth = log2Ceil(ThreadsPerCore) + 1
    val io = IO(new Bundle {
        val start = Input(Bool())
        // Kernel Metadata
        val thread_count = Input(UInt(8.W))
        // core states
        val core_done = Input(Vec(NumCores, Bool()))
        val core_start = Output(Vec(NumCores, Bool()))
        val core_reset = Output(Vec(NumCores, Bool()))
        val core_block_id = Output(Vec(NumCores, UInt(8.W)))
        val core_thread_count = Output(Vec(NumCores, UInt(8.W)))
        // kernel execution
        val done = Output(Bool())
    })
    val total_blocks = (io.thread_count + ThreadsPerCore.U - 1.U) / ThreadsPerCore.U

    val done = RegInit(false.B)
    val blocks_dispatched = RegInit(0.U(8.W))
    val blocks_done = RegInit(0.U(8.W))
    val start_execution = RegInit(false.B)
    val core_start = RegInit(VecInit(Seq.fill(NumCores)(false.B)))
    val core_reset = RegInit(VecInit(Seq.fill(NumCores)(true.B)))
    val core_block_id = RegInit(VecInit(Seq.fill(NumCores)(0.U(8.W))))
    val core_thread_count = RegInit(VecInit(Seq.fill(NumCores)(ThreadsPerCore.U(ThreadCountWidth.W))))

    // keep track of which cores are busy
    when(!reset.asBool && io.start) {
        when(!start_execution) {
            start_execution := true.B
            core_reset.foreach(_ := true.B)
        }
        // if the last block has finished, mark this kernel as done executing
        when(blocks_done === total_blocks) {
            done := true.B
        }

        for (i <- 0 until NumCores) {
            when(core_reset(i)) {
                core_reset(i) := false.B

                // if this core was just reset, check if there are more blocks to be dispatched
                when(blocks_dispatched < total_blocks) {
                    core_start(i) := true.B
                    core_block_id(i) := blocks_dispatched
                    core_thread_count(i) := Mux(
                        blocks_dispatched === (total_blocks - 1.U),
                        io.thread_count - (blocks_dispatched * ThreadsPerCore.U),
                        ThreadsPerCore.U
                    )
                    blocks_dispatched := blocks_dispatched + 1.U
                }
            }

            when(core_start(i) && io.core_done(i)) {
                core_reset(i) := true.B
                core_start(i) := false.B
                blocks_done := blocks_done + 1.U
            }
        }
    }
    io.core_start := core_start
    io.core_reset := core_reset
    io.core_block_id := core_block_id
    io.core_thread_count := core_thread_count
    io.done := done
}
