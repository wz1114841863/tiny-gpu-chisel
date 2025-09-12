package gpu

import chisel3._
import chisel3.util._

import dcr.DeviceControlRegister
import controller.Controller
import dispatch.Dispatch
import core.Core

class Gpu(
    DataMemAddrBits: Int = 8,
    DataMemDataBits: Int = 8,
    DataMemNumChannels: Int = 4,
    ProgramMemAddrBits: Int = 8,
    ProgramMemDataBits: Int = 16,
    ProgramMemNumChannels: Int = 1,
    NumCores: Int = 2,
    ThreadsPerBlock: Int = 4
) extends Module {
    val io = IO(new Bundle {
        val start = Input(Bool())
        val done = Output(Bool())

        val device_control_write_enable = Input(Bool())
        val device_control_data = Input(UInt(8.W))

        val program_mem_read_sender = Vec(
            ProgramMemNumChannels,
            new DecoupledIO(UInt(ProgramMemAddrBits.W))
        )
        val program_mem_read_data = Input(Vec(ProgramMemNumChannels, UInt(ProgramMemDataBits.W)))

        val data_mem_read_sender = Vec(
            DataMemNumChannels,
            new DecoupledIO(UInt(DataMemAddrBits.W))
        )
        val data_mem_read_data = Input(Vec(DataMemNumChannels, UInt(DataMemDataBits.W)))
        val data_mem_write_sender = Vec(
            DataMemNumChannels,
            new DecoupledIO(new Bundle {
                val address = UInt(DataMemAddrBits.W)
                val data = UInt(DataMemDataBits.W)
            })
        )
    })

    val core_outputs = Wire(
        Vec(
            NumCores,
            new Bundle {
                val done = Bool()
                val program_mem_read_receiver = Flipped(new DecoupledIO(UInt(ProgramMemAddrBits.W)))
                val data_mem_read_receiver = Vec(ThreadsPerBlock, Flipped(new DecoupledIO(UInt(DataMemAddrBits.W))))
                val data_mem_write_receiver = Vec(
                    ThreadsPerBlock,
                    Flipped(new DecoupledIO(new Bundle {
                        val address = UInt(DataMemAddrBits.W)
                        val data = UInt(DataMemDataBits.W)
                    }))
                )
            }
        )
    )

    // Control
    val thread_count = RegInit(0.U(8.W))
    val num_lsus = NumCores * ThreadsPerBlock
    val num_fetchers = NumCores

    // Device Control Register
    val dcr = Module(new DeviceControlRegister())
    dcr.io.device_control_write_enable := io.device_control_write_enable
    dcr.io.device_control_data := io.device_control_data
    thread_count := dcr.io.thread_count

    // Data Memory Controller
    val data_memory_controller = Module(new Controller(DataMemAddrBits, DataMemDataBits, num_lsus, DataMemNumChannels))
    data_memory_controller.io.mem_read_data := io.data_mem_read_data
    data_memory_controller.io.mem_read_sender <> io.data_mem_read_sender
    data_memory_controller.io.mem_write_sender.map(_ <> io.data_mem_write_sender)
    for (i <- 0 until NumCores) {
        for (j <- 0 until ThreadsPerBlock) {
            val mem_idx = i * ThreadsPerBlock + j
            data_memory_controller.io.consumer_read_addr_receiver(mem_idx) <> core_outputs(i).data_mem_read_receiver(j)
            data_memory_controller.io.consumer_write_receiver.map(
                _.apply(mem_idx) <> core_outputs(i).data_mem_write_receiver(j)
            )
        }
    }

    // Program Memory Controller (read only)
    val program_memory_controller = Module(
        new Controller(ProgramMemAddrBits, ProgramMemDataBits, num_fetchers, ProgramMemNumChannels, false)
    )
    program_memory_controller.io.mem_read_data := io.program_mem_read_data
    program_memory_controller.io.mem_read_sender <> io.program_mem_read_sender
    for (i <- 0 until NumCores) {
        core_outputs(i).program_mem_read_receiver <> program_memory_controller.io.consumer_read_addr_receiver(i)
    }

    // Dispatcher inputs
    val dispatcher = Module(new Dispatch(NumCores, ThreadsPerBlock))
    val start = RegInit(false.B)
    start := io.start
    dispatcher.io.start := start
    dispatcher.io.thread_count := dcr.io.thread_count
    for (i <- 0 until NumCores) {
        dispatcher.io.core_done(i) := core_outputs(i).done
    }

    // Compute Cores
    for (i <- 0 until NumCores) {
        val core = Module(
            new Core(DataMemAddrBits, DataMemDataBits, ProgramMemAddrBits, ProgramMemDataBits, ThreadsPerBlock)
        )
        core.io.start := dispatcher.io.core_start(i)
        core.io.block_id := dispatcher.io.core_block_id(i)
        core.io.thread_count := dispatcher.io.core_thread_count(i)

        core.io.program_mem_read_data := program_memory_controller.io.consumer_read_data(i)
        core_outputs(i).program_mem_read_receiver <> core.io.program_mem_read_address_sender

        core_outputs(i).data_mem_read_receiver <> core.io.data_mem_read_address_sender
        core_outputs(i).data_mem_write_receiver <> core.io.data_mem_write_sender

        for (j <- 0 until ThreadsPerBlock) {
            val lsu_index = i * ThreadsPerBlock + j
            core.io.data_mem_read_data(j) := data_memory_controller.io.consumer_read_data(lsu_index)
            core.io.data_mem_read_address_sender(j) <> data_memory_controller.io.consumer_read_addr_receiver(lsu_index)
            data_memory_controller.io.consumer_write_receiver.map(
                core.io.data_mem_write_sender(j) <> _.apply(lsu_index)
            )
        }
        core_outputs(i).done := core.io.done
    }

    io.done := dispatcher.io.done
}
