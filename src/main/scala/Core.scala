package core

import chisel3._
import chisel3.util._
import statecode.CoreState

import fetcher.Fetcher
import decoder.Decoder
import scheduler.Scheduler
import registers.RegisterFiles
import alu.Alu
import lsu.MemLoadStoreUnit
import pc.ProgramCounter

class ComputeUnit(
    Idx: Int,
    DataBits: Int,
    AddrBits: Int
)(implicit threads: Int)
    extends Module {}

class Core(
    DataMemAddrBits: Int = 8,
    DataMemDataBits: Int = 8,
    ProgramMemAddrBits: Int = 8,
    ProgramMemDataBits: Int = 16,
    ThreadsPerBlock: Int = 4
) extends Module {
    val io = IO(new Bundle {
        // kernel Execution
        val start = Input(Bool())
        val done = Output(Bool())

        // Block Metatdata
        val block_id = Input(UInt(8.W))
        val thread_count = Input(UInt(log2Ceil(ThreadsPerBlock).W))

        // Program Memory
        val program_mem_read_address_sender = new DecoupledIO(UInt(ProgramMemAddrBits.W))
        val program_mem_read_data = Input(UInt(ProgramMemDataBits.W))

        // Data Memory
        val data_mem_read_data = Input(Vec(ThreadsPerBlock, UInt(DataMemDataBits.W)))
        val data_mem_read_address_sender = Vec(ThreadsPerBlock, new DecoupledIO(UInt(DataMemAddrBits.W)))
        val data_mem_write_sender = Vec(
            ThreadsPerBlock,
            new DecoupledIO(new Bundle {
                val address = UInt(DataMemAddrBits.W)
                val data = UInt(DataMemDataBits.W)
            })
        )

        // debug outputs
        val core_state = Output(CoreState())
        val current_pc = Output(UInt(8.W))
    })

    val fetcher = Module(new Fetcher(ProgramMemAddrBits, ProgramMemDataBits))
    val decoder = Module(new Decoder())
    val scheduler = Module(new Scheduler(ThreadsPerBlock))

    val core_state = Wire(CoreState())
    val current_pc = Wire(UInt(8.W))

    core_state := scheduler.io.core_state
    current_pc := fetcher.io.current_pc

    // Fetcher inputs connections
    fetcher.io.core_state := core_state
    fetcher.io.current_pc := current_pc
    fetcher.io.mem_read_data := io.program_mem_read_data

    // Decoder inputs connections
    decoder.io.core_state := scheduler.io.core_state

}
