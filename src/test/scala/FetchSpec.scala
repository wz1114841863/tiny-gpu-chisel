package fetcher

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import statecode.FetcherState

class FetcherModel {
    var fetcher_state: FetcherState.Type = FetcherState.IDLE
    var read_valid = false
    var read_address = 0
    var instruction = 0

    def update(
        core_state: CoreState.Type,
        current_pc: Int,
        mem_read_ready: Boolean,
        mem_read_data: Int
    ): Unit = {
        fetcher_state match {
            case FetcherState.IDLE =>
                if (core_state == CoreState.FETCH) {
                    fetcher_state = FetcherState.FETCHING
                    read_valid = true
                    read_address = current_pc
                }

            case FetcherState.FETCHING =>
                if (mem_read_ready) {
                    fetcher_state = FetcherState.FETCHED
                    instruction = mem_read_data
                    read_valid = false
                }

            case FetcherState.FETCHED =>
                if (core_state == CoreState.DECODE) {
                    fetcher_state = FetcherState.IDLE
                }
        }
    }

    def reset(): Unit = {
        fetcher_state = FetcherState.IDLE
        read_valid = false
        read_address = 0
        instruction = 0
    }
}

class FetcherSpec extends AnyFreeSpec with Matchers {
    "should match model behavior with random inputs" in {
        val ProgramMemAddrBits: Int = 8
        val ProgramMemDataBits: Int = 16
        simulate(new Fetcher(ProgramMemAddrBits, ProgramMemDataBits)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            def randomCoreState(rng: scala.util.Random): CoreState.Type = {
                val v = rng.nextInt(3)
                v match {
                    case 0 => CoreState.FETCH
                    case 1 => CoreState.DECODE
                    case 2 => CoreState.EXECUTE
                }
            }

            var cnt = 0
            val rng = new scala.util.Random(42)
            val fetcherModel = new FetcherModel()

            while (cnt < 10000) {
                val core_state = randomCoreState(rng)
                val current_pc = rng.nextInt(256)
                val mem_read_ready = rng.nextBoolean()
                val mem_read_data = rng.nextInt(65536) // 2^16 for 16-bit data

                dut.io.core_state.poke(core_state)
                dut.io.current_pc.poke(current_pc.U)
                dut.io.mem_read_address_sender.ready.poke(mem_read_ready.B)
                dut.io.mem_read_data.poke(mem_read_data.U)

                dut.clock.step()

                fetcherModel.update(core_state, current_pc, mem_read_ready, mem_read_data)

                assert(dut.io.fetcher_state.peekValue().asBigInt == fetcherModel.fetcher_state.litValue)
                dut.io.mem_read_address_sender.valid.expect(fetcherModel.read_valid.B)
                dut.io.mem_read_address_sender.bits.expect(fetcherModel.read_address.U)
                dut.io.instruction.expect(fetcherModel.instruction.U)

                cnt += 1
            }
        }
    }
}
