package core

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import statecode.FetcherState
import statecode.LSUState
import statecode.RegInputOp
import statecode.AluOpCode

class CoreModel(
    DataMemAddrBits: Int = 8,
    DataMemDataBits: Int = 8,
    ProgramMemAddrBits: Int = 8,
    ProgramMemDataBits: Int = 16,
    ThreadsPerBlock: Int = 4
) {
    var fetcher_model = new fetcher.FetcherModel()
    var decoder_model = new decoder.DecoderModel()
    var scheduler_model = new scheduler.SchedulerModel(ThreadsPerBlock)

    var alu_models = Array.fill(ThreadsPerBlock)(new alu.AluModel())
    var lsu_models = Array.fill(ThreadsPerBlock)(new lsu.LsuModel())
    var regfils_models = Array.fill(ThreadsPerBlock)(new registers.RegModel())
    var pc_models = Array.fill(ThreadsPerBlock)(new pc.PCModel())

    class PrevCycleData {
        // Fetcher Module Outputs
        var fetcher_state = FetcherState.IDLE
        var read_valid = false
        var read_address = 0
        var instruction = 0

        // Decoder Module Outputs
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

        // Scheduler Module Outputs
        var current_pc = 0
        var core_state = CoreState.IDLE
        var done = false

        // ALU Module Outputs
        var alu_out = Array.fill(ThreadsPerBlock)(0)

        // LSU Module Outputs
        var lsu_out = Array.fill(ThreadsPerBlock)(0)
        var lsu_state = Array.fill(ThreadsPerBlock)(LSUState.IDLE)

        // PC Module Outputs
        var next_pc = Array.fill(ThreadsPerBlock)(0)

        // Register File Module Outputs
        var regout_rs = Array.fill(ThreadsPerBlock)(0)
        var regout_rt = Array.fill(ThreadsPerBlock)(0)

        def save(model: fetcher.FetcherModel): Unit = {
            fetcher_state = model.fetcher_state
            read_valid = model.read_valid
            read_address = model.read_address
            instruction = model.instruction
        }

        def save(model: decoder.DecoderModel) = {
            decoded_rd_address = model.decoded_rd_address
            decoded_rs_address = model.decoded_rs_address
            decoded_rt_address = model.decoded_rt_address
            decoded_nzp = model.decoded_nzp
            decoded_immediate = model.decoded_immediate
            decoded_reg_write_enable = model.decoded_reg_write_enable
            decoded_mem_read_enable = model.decoded_mem_read_enable
            decoded_mem_write_enable = model.decoded_mem_write_enable
            decoded_nzp_write_enable = model.decoded_nzp_write_enable
            decoded_reg_input_mux = model.decoded_reg_input_mux
            decoded_alu_arithmetic_mux = model.decoded_alu_arithmetic_mux
            decoded_alu_output_mux = model.decoded_alu_output_mux
            decoded_pc_mux = model.decoded_pc_mux
            decoded_ret = model.decoded_ret
        }

        def save(model: scheduler.SchedulerModel) = {
            core_state = model.core_state
            current_pc = model.current_pc
            done = model.done
        }

        def save(model: alu.AluModel, idx: Int) = {
            alu_out(idx) = model.alu_out
        }

        def save(model: pc.PCModel, idx: Int) = {
            next_pc(idx) = model.next_pc
        }

        def save(model: lsu.LsuModel, idx: Int) = {
            lsu_state(idx) = model.lsu_state
            lsu_out(idx) = model.output_data
        }

        def save(model: registers.RegModel, idx: Int) = {
            regout_rs(idx) = model.rs_out()
            regout_rt(idx) = model.rt_out()
        }
    }
    var prev_cycle_data = new PrevCycleData

    var done = false

    var program_mem_read_address_valid = false
    var program_mem_read_address_bits = 0

    var data_mem_read_address_valid = Array.fill(ThreadsPerBlock)(false)
    var data_mem_read_address_bits = Array.fill(ThreadsPerBlock)(0)

    var data_mem_write_address_valid = Array.fill(ThreadsPerBlock)(false)
    var data_mem_write_address_bits = Array.fill(ThreadsPerBlock)(0)
    var data_mem_write_data_bits = Array.fill(ThreadsPerBlock)(0)

    var cycle = 0

    def update(
        // Kernel execution inputs
        start: Boolean,
        block_id: Int,
        thread_count: Int,

        // Memory inputs
        program_mem_read_ready: Boolean,
        program_mem_read_data: Int,

        // Data Memory signals
        data_mem_read_data: Array[Int],
        data_mem_read_ready: Array[Boolean],
        data_mem_write_ready: Array[Boolean]
    ): Unit = {
        // Update fetcher
        fetcher_model.update(
            core_state = prev_cycle_data.core_state,
            current_pc = prev_cycle_data.current_pc,
            mem_read_ready = program_mem_read_ready,
            mem_read_data = program_mem_read_data
        )

        // Update decoder
        decoder_model.update(
            core_state = prev_cycle_data.core_state,
            instruction = prev_cycle_data.instruction
        )

        // Update scheduler
        scheduler_model.update(
            start = start,
            mem_read_enable = prev_cycle_data.decoded_mem_read_enable,
            mem_write_enable = prev_cycle_data.decoded_mem_write_enable,
            decoded_ret = prev_cycle_data.decoded_ret,
            fetcher_state = prev_cycle_data.fetcher_state,
            lsu_state = prev_cycle_data.lsu_state,
            next_pc = prev_cycle_data.next_pc
        )

        // Update per-thread components
        for (i <- 0 until ThreadsPerBlock) {
            val enable = i < thread_count

            val rd_in = prev_cycle_data.decoded_rd_address
            val rs_in = prev_cycle_data.decoded_rs_address
            val rt_in = prev_cycle_data.decoded_rt_address

            // Update ALU
            alu_models(i).update(
                enable = enable,
                core_state = prev_cycle_data.core_state,
                rs = prev_cycle_data.regout_rs(i),
                rt = prev_cycle_data.regout_rt(i),
                arithmetic_mux = prev_cycle_data.decoded_alu_arithmetic_mux,
                output_mux = prev_cycle_data.decoded_alu_output_mux
            )

            // Update LSU
            lsu_models(i).update(
                enable = enable,
                core_state = prev_cycle_data.core_state,
                read_enable = prev_cycle_data.decoded_mem_read_enable,
                write_enable = prev_cycle_data.decoded_mem_write_enable,
                rs = prev_cycle_data.regout_rs(i),
                rt = prev_cycle_data.regout_rt(i),
                mem_read_data = data_mem_read_data(i),
                mem_read_ready = data_mem_read_ready(i),
                mem_write_ready = data_mem_write_ready(i)
            )

            // Update Register Files
            regfils_models(i).update(
                enable = enable,
                block_id = block_id,
                core_state = prev_cycle_data.core_state,
                rs = rs_in,
                rd = rd_in,
                rt = rt_in,
                decoded_reg_write_enable = prev_cycle_data.decoded_reg_write_enable,
                decoded_reg_input_op = prev_cycle_data.decoded_reg_input_mux,
                decoded_immediate = prev_cycle_data.decoded_immediate,
                alu_out = prev_cycle_data.alu_out(i),
                lsu_out = prev_cycle_data.lsu_out(i)
            )

            // Update PC
            pc_models(i).update(
                enable = enable,
                core_state = prev_cycle_data.core_state,
                decoded_nzp = prev_cycle_data.decoded_nzp,
                decoded_immediate = prev_cycle_data.decoded_immediate,
                decoded_nzp_write_enable = prev_cycle_data.decoded_nzp_write_enable,
                decoded_pc_mux = prev_cycle_data.decoded_pc_mux,
                alu_out = prev_cycle_data.alu_out(i),
                current_pc = prev_cycle_data.current_pc
            )

            prev_cycle_data.save(alu_models(i), i)
            prev_cycle_data.save(lsu_models(i), i)
            prev_cycle_data.save(regfils_models(i), i)
            prev_cycle_data.save(pc_models(i), i)
        }

        // update outputs
        done = scheduler_model.done
        program_mem_read_address_valid = fetcher_model.read_valid
        program_mem_read_address_bits = fetcher_model.read_address

        for (i <- 0 until ThreadsPerBlock) {
            data_mem_read_address_valid(i) = lsu_models(i).read_valid
            data_mem_read_address_bits(i) = lsu_models(i).read_address
            data_mem_write_address_valid(i) = lsu_models(i).write_valid
            data_mem_write_address_bits(i) = lsu_models(i).write_address
            data_mem_write_data_bits(i) = lsu_models(i).write_data
        }

        prev_cycle_data.save(fetcher_model)
        prev_cycle_data.save(decoder_model)
        prev_cycle_data.save(scheduler_model)

        cycle += 1
    }
}

class CoreSpec extends AnyFreeSpec with Matchers {
    "Test Core" in {
        val DataMemAddrBits: Int = 8
        val DataMemDataBits: Int = 8
        val ProgramMemAddrBits: Int = 8
        val ProgramMemDataBits: Int = 16
        val ThreadsPerBlock: Int = 4

        simulate(new Core(DataMemAddrBits, DataMemDataBits, ProgramMemAddrBits, ProgramMemDataBits, ThreadsPerBlock)) {
            dut =>
                dut.reset.poke(true.B)
                dut.clock.step()
                dut.reset.poke(false.B)
                dut.clock.step()

                var cnt = 0
                val rng = new scala.util.Random(42)
                val coreModel = new CoreModel()

                while (cnt < 10000) {
                    val block_id = rng.nextInt(13)
                    val thread_count = rng.nextInt(ThreadsPerBlock)
                    val mem_read_ready = rng.nextBoolean()
                    val mem_read_data = rng.nextInt(256)
                    val data_mem_read_data = Array.fill(ThreadsPerBlock)(rng.nextInt(256))
                    val data_mem_read_ready = Array.fill(ThreadsPerBlock)(rng.nextBoolean())
                    val data_mem_write_ready = Array.fill(ThreadsPerBlock)(rng.nextBoolean())

                    dut.io.start.poke(true.B)
                    dut.io.block_id.poke(block_id.U)
                    dut.io.thread_count.poke(thread_count.U)

                    dut.io.program_mem_read_address_sender.ready.poke(mem_read_ready.B)
                    dut.io.program_mem_read_data.poke(mem_read_data.U)

                    for (i <- 0 until ThreadsPerBlock) {
                        dut.io.data_mem_read_data(i).poke(data_mem_read_data(i).U)
                        dut.io.data_mem_read_address_sender(i).ready.poke(data_mem_read_ready(i).B)
                        dut.io.data_mem_write_sender(i).ready.poke(data_mem_write_ready(i).B)
                    }

                    dut.clock.step()

                    // println(s"\n=== Random Values for Cycle $cnt ===")
                    // println(s"Block ID: $block_id")
                    // println(s"Thread Count: $thread_count")
                    // println(s"Program Memory Read: [ready=$mem_read_ready, data=$mem_read_data]")
                    // println(s"Read Data:    [${data_mem_read_data.mkString(", ")}]")
                    // println(s"Read Ready:   [${data_mem_read_ready.mkString(", ")}]")
                    // println(s"Write Ready:  [${data_mem_write_ready.mkString(", ")}]")
                    // println()
                    // println("--Aft Core State: " + dut.io.core_state.peekValue() + ", current_pc: " + dut.io.current_pc.peek())

                    coreModel.update(
                        start = true,
                        block_id = block_id,
                        thread_count = thread_count,
                        // Memory inputs
                        program_mem_read_ready = mem_read_ready,
                        program_mem_read_data = mem_read_data,
                        // Data Memory signals
                        data_mem_read_data = data_mem_read_data,
                        data_mem_read_ready = data_mem_read_ready,
                        data_mem_write_ready = data_mem_write_ready
                    )

                    // verify all 8 outputs
                    dut.io.done.expect(coreModel.done.B)
                    dut.io.program_mem_read_address_sender.valid.expect(coreModel.program_mem_read_address_valid.B)
                    dut.io.program_mem_read_address_sender.bits.expect(coreModel.program_mem_read_address_bits.U)

                    for (i <- 0 until ThreadsPerBlock) {
                        dut.io.data_mem_read_address_sender(i).valid.expect(coreModel.data_mem_read_address_valid(i).B)
                        dut.io.data_mem_read_address_sender(i).bits.expect(coreModel.data_mem_read_address_bits(i).U)
                        dut.io.data_mem_write_sender(i).valid.expect(coreModel.data_mem_write_address_valid(i).B)
                        dut.io.data_mem_write_sender(i).bits.address.expect(coreModel.data_mem_write_address_bits(i).U)
                        dut.io.data_mem_write_sender(i).bits.data.expect(coreModel.data_mem_write_data_bits(i).U)
                    }

                    cnt += 1
                }
        }
    }
}
