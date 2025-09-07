package fetcher

import chisel3._
import chisel3.util._

import statecode.CoreState
import statecode.FetcherState

class Fetcher(ProgramMemAddrBits: Int = 8, ProgramMemDataBits: Int = 16) extends Module {
    val io = IO(new Bundle {
        val core_state = Input(CoreState())
        val current_pc = Input(UInt(8.W))

        // Program Memory
        val mem_read_address_sender = new DecoupledIO(UInt(ProgramMemAddrBits.W))
        val mem_read_data = Input(UInt(ProgramMemDataBits.W))

        val fetcher_state = Output(FetcherState())
        val instruction = Output(UInt(ProgramMemDataBits.W))
    })

    val fetcher_state = RegInit(FetcherState.IDLE)
    val mem_read_valid = RegInit(false.B)
    val mem_read_address = RegInit(0.U(ProgramMemAddrBits.W))
    val instruction = RegInit(0.U(ProgramMemDataBits.W))

    when(!reset.asBool) {
        switch(fetcher_state) {
            is(FetcherState.IDLE) {
                when(io.core_state === CoreState.FETCH) {
                    fetcher_state := FetcherState.FETCHING
                    mem_read_valid := true.B
                    mem_read_address := io.current_pc
                }
            }
            is(FetcherState.FETCHING) {
                when(io.mem_read_address_sender.ready) {
                    fetcher_state := FetcherState.FETCHED
                    instruction := io.mem_read_data
                    mem_read_valid := false.B
                }
            }
            is(FetcherState.FETCHED) {
                when(io.core_state === CoreState.DECODE) {
                    fetcher_state := FetcherState.IDLE
                }
            }
        }
    }

    io.fetcher_state := fetcher_state
    io.mem_read_address_sender.valid := mem_read_valid
    io.mem_read_address_sender.bits := mem_read_address
    io.instruction := instruction
}
