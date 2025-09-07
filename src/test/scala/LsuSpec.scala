package lsu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import statecode.LSUState

class lsu_model {
    var lsu_state: LSUState.Type = LSUState.IDLE
    var output_data = 0
    var read_valid = false
    var read_address = 0
    var write_valid = false
    var write_data = 0
    var write_address = 0

    def update(
        enable: Boolean,
        core_state: CoreState.Type,
        read_enable: Boolean,
        write_enable: Boolean,
        rs: Int,
        rt: Int,
        mem_read_data: Int,
        mem_read_ready: Boolean,
        mem_write_ready: Boolean
    ): Unit = {
        if (enable) {
            if (read_enable) {
                lsu_state match {
                    case LSUState.IDLE =>
                        if (core_state == CoreState.REQUEST) {
                            lsu_state = LSUState.REQUESTING
                        }
                    case LSUState.REQUESTING =>
                        read_valid = true
                        read_address = rs
                        lsu_state = LSUState.WAITING
                    case LSUState.WAITING =>
                        if (mem_read_ready) {
                            read_valid = false
                            output_data = mem_read_data
                            lsu_state = LSUState.DONE
                        }
                    case LSUState.DONE => {
                        if (core_state == CoreState.UPDATE) {
                            lsu_state = LSUState.IDLE
                        }
                    }
                }
            }

            if (write_enable) {
                lsu_state match {
                    case LSUState.IDLE =>
                        if (core_state == CoreState.REQUEST) {
                            lsu_state = LSUState.REQUESTING
                        }
                    case LSUState.REQUESTING =>
                        write_valid = true
                        write_address = rs
                        write_data = rt
                        lsu_state = LSUState.WAITING
                    case LSUState.WAITING =>
                        if (mem_write_ready) {
                            write_valid = false
                            lsu_state = LSUState.DONE
                        }
                    case LSUState.DONE =>
                        if (core_state == CoreState.UPDATE) {
                            lsu_state = LSUState.IDLE
                        }
                }
            }
        }
    }
}

class LsuSpec extends AnyFreeSpec with Matchers {
    "Test Load/Store Unit" in {
        simulate(new MemLoadStoreUnit) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step(5)
            dut.reset.poke(false.B)
            dut.clock.step(5)

            def randomCoreState(rng: scala.util.Random): CoreState.Type = {
                val v = rng.nextInt(2)
                v match {
                    case 0 => CoreState.REQUEST
                    case 1 => CoreState.UPDATE
                }
            }

            var cnt = 0
            val rng = new scala.util.Random(42)
            val lsu_model = new lsu_model()

            while (cnt < 1000) {
                val enable = true
                val core_state = randomCoreState(rng)
                val read_enable = rng.nextBoolean()
                val write_enable = if (read_enable) {
                    false
                } else {
                    rng.nextBoolean()
                }
                val rs = rng.nextInt(256)
                val rt = rng.nextInt(256)
                val mem_read_data = rng.nextInt(256)
                val mem_read_ready = rng.nextBoolean()
                val mem_write_ready = rng.nextBoolean()

                dut.io.enable.poke(enable.B)
                dut.io.core_state.poke(core_state)
                dut.io.mem_rw_enable.mem_read_enable.poke(read_enable.B)
                dut.io.mem_rw_enable.mem_write_enable.poke(write_enable.B)
                dut.io.reg_in.rs.poke(rs.U)
                dut.io.reg_in.rt.poke(rt.U)
                dut.io.mem_read_data.poke(mem_read_data.U)
                dut.io.mem_read_address_sender.ready
                    .poke(mem_read_ready.B)
                dut.io.mem_write_sender.ready.poke(mem_write_ready.B)

                lsu_model.update(
                    enable,
                    core_state,
                    read_enable,
                    write_enable,
                    rs,
                    rt,
                    mem_read_data,
                    mem_read_ready,
                    mem_write_ready
                )

                dut.clock.step()

                if (
                    dut.io.lsu_state
                        .peekValue()
                        .asBigInt != lsu_model.lsu_state.litValue
                ) {
                    println(
                        f"[$cnt%3d] enable=$enable%5s, core_state=$core_state, read_enable=$read_enable%5s, write_enable=$write_enable%5s"
                    )
                    println(
                        f"        rs=$rs%3d, rt=$rt%3d, mem_read_data=$mem_read_data%3d, mem_read_ready=$mem_read_ready%5s, mem_write_ready=$mem_write_ready%5s"
                    )
                    println(
                        f"        model.lsu_state=${lsu_model.lsu_state}, dut.lsu_state=${dut.io.lsu_state.peekValue().asBigInt}"
                    )
                    println(
                        f"        model.read_valid=${lsu_model.read_valid}, dut.read_valid=${dut.io.mem_read_address_sender.valid.peekValue().asBigInt}"
                    )
                    println(
                        f"        model.write_valid=${lsu_model.write_valid}, dut.write_valid=${dut.io.mem_write_sender.valid.peekValue().asBigInt}"
                    )
                }
                // check mem read channel
                assert(
                    dut.io.lsu_state
                        .peekValue()
                        .asBigInt == lsu_model.lsu_state.litValue
                )
                dut.io.lsu_out.expect(lsu_model.output_data.U)
                dut.io.mem_read_address_sender.valid
                    .expect(lsu_model.read_valid.B)
                if (lsu_model.read_valid) {
                    dut.io.mem_read_address_sender.bits
                        .expect(lsu_model.read_address.U)
                }
                dut.io.mem_write_sender.valid.expect(lsu_model.write_valid.B)
                if (lsu_model.write_valid) {
                    dut.io.mem_write_sender.bits.address
                        .expect(lsu_model.write_address.U)
                    dut.io.mem_write_sender.bits.data
                        .expect(lsu_model.write_data.U)
                }

                cnt += 1
            }

        }
    }
}
