package dispatch

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class DispatchModel(NumCores: Int = 2, ThreadsPerCore: Int = 4) {
    val ThreadCountWidth = log2Ceil(ThreadsPerCore) + 1

    val core_start = Array.fill(NumCores)(false)
    var core_reset = Array.fill(NumCores)(true)
    var core_block_id = Array.fill(NumCores)(0)
    var core_thread_count = Array.fill(NumCores)(ThreadsPerCore)
    var done = false

    // interal states
    var start_execution = false
    var blocks_done = 0
    var blocks_dispatched = 0

    def update(
        start: Boolean,
        thread_count: Int,
        core_done: Array[Boolean]
    ): Unit = {
        if (!start) {
            return
        }

        if (!start_execution) {
            start_execution = true
            core_reset.map(_ => true)
        }

        val total_blocks = (thread_count + ThreadsPerCore - 1) / ThreadsPerCore
        if (blocks_done == total_blocks) {
            done = true
        }

        var run_cnt = 0
        var prev_core_start = core_start.clone()
        for (i <- 0 until NumCores) {
            if (core_reset(i)) {
                core_reset(i) = false

                if (blocks_dispatched < total_blocks) {
                    core_start(i) = true
                    core_block_id(i) = blocks_dispatched
                    core_thread_count(i) = if (blocks_dispatched == total_blocks - 1) {
                        thread_count - (blocks_dispatched * ThreadsPerCore)
                    } else { ThreadsPerCore }
                    run_cnt += 1
                }
            }

            if (prev_core_start(i) && core_done(i)) {
                core_reset(i) = true
                core_start(i) = false
                blocks_done += 1
            }
        }
        blocks_dispatched += run_cnt
    }
}

class DispatchSpec extends AnyFreeSpec with Matchers {
    "Test dispatch" in {
        val NumCores = 2
        val ThreadsPerCore = 4

        simulate(new Dispatch(NumCores, ThreadsPerCore)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            var cnt = 0
            val rng = new scala.util.Random(42)
            val dispatch_model = new DispatchModel()

            while (cnt < 1000) {
                val core_done = Array.fill(NumCores)(rng.nextBoolean())
                val thread_count = rng.nextInt(ThreadsPerCore)

                dut.io.start.poke(true.B)
                dut.io.thread_count.poke(thread_count.U)
                for (i <- 0 until NumCores) {
                    dut.io.core_done(i).poke(core_done(i).B)
                }
                dut.clock.step()

                dispatch_model.update(true, thread_count, core_done)

                dut.io.done.expect(dispatch_model.done.B)
                for (i <- 0 until NumCores) {
                    dut.io.core_start(i).expect(dispatch_model.core_start(i).B)
                    dut.io.core_reset(i).expect(dispatch_model.core_reset(i).B)
                    dut.io.core_block_id(i).expect(dispatch_model.core_block_id(i).U)
                    dut.io.core_thread_count(i).expect(dispatch_model.core_thread_count(i).U)
                }

                cnt += 1
            }
        }
    }
}
