package pc

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import chisel3.simulator.PeekPokeAPI
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class PCModel(DataMemWidth: Int = 8, MemAddrWidth: Int = 8) {
    var nzp = 0
    var next_pc = 0
    val max_pc = (1 << MemAddrWidth)

    def update(
        enable: Boolean,
        core_state: CoreState.Type,
        decoded_nzp: Int,
        decoded_immediate: Int,
        decoded_nzp_write_enable: Boolean,
        decoded_pc_mux: Boolean,
        alu_out: Int,
        current_pc: Int
    ) = {
        if (enable) {
            if (core_state == CoreState.EXECUTE) {
                if (decoded_pc_mux) {
                    if ((nzp & decoded_nzp) != 0) {
                        next_pc = decoded_immediate
                    } else {
                        next_pc = (current_pc + 1) % max_pc
                    }
                } else {
                    next_pc = (current_pc + 1) % max_pc
                }
            }

            if (core_state == CoreState.UPDATE) {
                if (decoded_nzp_write_enable) {
                    nzp = alu_out & 0x7 // Take only lowest 3 bits
                }
            }
        }
    }

    def reset(): Unit = {
        next_pc = 0
        nzp = 0
    }

    def getNzp: Int = nzp
}

class PCSpec extends AnyFreeSpec with Matchers {
    "Test Program Counter for all supported operations" in {
        val DataMemWidth = 8
        val MemAddrWidth = 8
        simulate(new ProgramCounter(DataMemWidth, MemAddrWidth)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            def randomCoreState(rng: scala.util.Random): CoreState.Type = {
                val v = rng.nextInt(2)
                v match {
                    case 0 => CoreState.FETCH
                    case 1 => CoreState.EXECUTE
                }
            }

            dut.io.enable.poke(true.B)

            var cnt = 0
            var rng = new scala.util.Random(42)
            val pc_model = new PCModel(DataMemWidth, MemAddrWidth)

            while (cnt < 1000) {
                val core_state = randomCoreState(rng)
                val decoded_nzp = rng.nextInt(8)
                val decoded_immediate = rng.nextInt(1 << DataMemWidth)
                val decoded_nzp_write_enable = rng.nextInt(2) == 1
                val decoded_pc_mux = rng.nextInt(2) == 1
                val alu_out = rng.nextInt(1 << DataMemWidth)
                val current_pc = rng.nextInt(1 << MemAddrWidth)

                dut.io.core_state.poke(core_state)
                dut.io.decoded_nzp.poke(decoded_nzp.U)
                dut.io.decoded_immediate.poke(decoded_immediate.U)
                dut.io.decoded_nzp_write_enable.poke(decoded_nzp_write_enable.B)
                dut.io.decoded_pc_mux.poke(decoded_pc_mux.B)
                dut.io.alu_out.poke(alu_out.U)
                dut.io.current_pc.poke(current_pc.U)

                dut.clock.step()

                pc_model.update(
                    enable = true,
                    core_state,
                    decoded_nzp,
                    decoded_immediate,
                    decoded_nzp_write_enable,
                    decoded_pc_mux,
                    alu_out,
                    current_pc
                )

                val ref_next = pc_model.next_pc // 你的参考模型
                val hw_next = dut.io.next_pc.peek().litValue

                if (hw_next != ref_next) {
                    println(
                        f"*** FAIL ***" +
                            f"  ref_next=$ref_next%04x  hw_next=$hw_next%04x" +
                            f"  pc_mux=${dut.io.decoded_pc_mux.peek().litValue}" +
                            f"  nzp_we=${dut.io.decoded_nzp_write_enable.peek().litValue}" +
                            f"  nzp=${dut.io.decoded_nzp.peek().litValue}%01x" +
                            f"  current_pc=${dut.io.current_pc.peek().litValue}%04x" +
                            f"  immediate=${dut.io.decoded_immediate.peek().litValue}%04x"
                    )
                }

                dut.io.next_pc.expect(
                    pc_model.next_pc.U
                )

                cnt += 1
            }
        }
    }
}
