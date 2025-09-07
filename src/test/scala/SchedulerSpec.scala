package scheduler

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import statecode.LSUState
import statecode.FetcherState

class SchedulerModel(ThreadsPerBlock: Int = 4) {
    var current_pc = 0
    var core_state = CoreState.IDLE
    var done = false
    def update(
        start: Boolean,
        mem_read_enable: Boolean,
        mem_write_enable: Boolean,
        decoded_ret: Boolean,
        fetcher_state: FetcherState.Type,
        lsu_state: Array[LSUState.Type],
        next_pc: Array[Int]
    ): Unit = {
        core_state match {
            case CoreState.IDLE =>
                if (start) {
                    core_state = CoreState.FETCH
                }

            case CoreState.FETCH =>
                if (fetcher_state == FetcherState.FETCHED) {
                    core_state = CoreState.DECODE
                }

            case CoreState.DECODE =>
                core_state = CoreState.REQUEST

            case CoreState.REQUEST =>
                core_state = CoreState.WAIT

            case CoreState.WAIT =>
                // Check if any LSU is waiting or requesting
                val any_lsu_waiting =
                    lsu_state.exists(state => state == LSUState.WAITING || state == LSUState.REQUESTING)
                if (!any_lsu_waiting) {
                    core_state = CoreState.EXECUTE
                }

            case CoreState.EXECUTE =>
                core_state = CoreState.UPDATE

            case CoreState.UPDATE =>
                if (decoded_ret) {
                    done = true
                    core_state = CoreState.DONE
                } else {
                    // Use the last thread's next_pc (same as in module)
                    current_pc = next_pc(ThreadsPerBlock - 1)
                    core_state = CoreState.FETCH
                }

            case CoreState.DONE =>
            // Stay in DONE state
        }
    }
}

class SchedulerSpec extends AnyFreeSpec with Matchers {
    "Test scheduler shoule match behavior with random inputs" in {
        val ThreadsPerBlock: Int = 4
        simulate(new Scheduler(ThreadsPerBlock)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            var cnt = 0
            val rng = new scala.util.Random(42)
            val scheduler_model = new SchedulerModel(ThreadsPerBlock)

            def random_lsu_state(rng: scala.util.Random): LSUState.Type = {
                val v = rng.nextInt(4)
                v match {
                    case 0 => LSUState.IDLE
                    case 1 => LSUState.REQUESTING
                    case 2 => LSUState.WAITING
                    case 3 => LSUState.DONE
                }
            }

            while (cnt < 10000) {
                val start = rng.nextBoolean()
                val mem_read_enable = rng.nextBoolean()
                val mem_write_enable = if (mem_read_enable) {
                    false
                } else {
                    rng.nextBoolean()
                }
                val decoded_ret = rng.nextBoolean()
                val (fetcher_state, _) = FetcherState.safe(rng.nextInt(3).U)
                val lsu_state = Array.fill(ThreadsPerBlock)(random_lsu_state(rng))
                val next_pc = Array.fill(ThreadsPerBlock)(rng.nextInt(256))

                dut.io.start.poke(start.B)
                dut.io.mem_rw_enable.read_enable.poke(mem_read_enable.B)
                dut.io.mem_rw_enable.write_enable.poke(mem_write_enable.B)
                dut.io.decoded_ret.poke(decoded_ret.B)
                dut.io.fetcher_state.poke(fetcher_state)
                for (i <- 0 until ThreadsPerBlock) {
                    dut.io.lsu_state(i).poke(lsu_state(i))
                    dut.io.next_pc(i).poke(next_pc(i).U)
                }

                dut.clock.step()

                // Update model
                scheduler_model.update(
                    start = start,
                    mem_read_enable = mem_read_enable,
                    mem_write_enable = mem_write_enable,
                    decoded_ret = decoded_ret,
                    fetcher_state = fetcher_state,
                    lsu_state = lsu_state,
                    next_pc = next_pc
                )

                // Compare model with DUT
                dut.io.current_pc.expect(scheduler_model.current_pc.U)
                assert(dut.io.core_state.peekValue().asBigInt == scheduler_model.core_state.litValue)
                dut.io.done.expect(scheduler_model.done.B)

                cnt += 1
            }
        }
    }
}
