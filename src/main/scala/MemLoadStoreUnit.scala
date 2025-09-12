package lsu

import chisel3._
import chisel3.util._
import statecode.CoreState
import statecode.LSUState

class MemLoadStoreUnit extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val core_state = Input(CoreState())

        // Decoder outputs
        val mem_rw_enable = new Bundle {
            val read_enable = Input(Bool())
            val write_enable = Input(Bool())
        }

        val reg_in = new Bundle {
            val rs = Input(UInt(8.W))
            val rt = Input(UInt(8.W))
        }

        val mem_read_data = Input(UInt(8.W))
        val mem_read_address_sender = new DecoupledIO(UInt(8.W))

        val mem_write_sender = DecoupledIO(new Bundle {
            val address = UInt(8.W)
            val data = UInt(8.W)
        })

        val lsu_state = Output(LSUState())
        val lsu_out = Output(UInt(8.W))
    })

    val lsu_state = RegInit(LSUState.IDLE)
    val lsu_out = RegInit(0.U(8.W))
    val mem_read_valid = RegInit(false.B)
    val mem_read_address = RegInit(0.U(8.W))
    val mem_write_valid = RegInit(false.B)
    val mem_write_data = RegInit(0.U(8.W))
    val mem_write_address = RegInit(0.U(8.W))

    when(io.enable) {
        // if memory read enable is triggered(LDR instruction)
        when(io.mem_rw_enable.read_enable) {
            switch(lsu_state) {
                is(LSUState.IDLE) {
                    when(io.core_state === CoreState.REQUEST) {
                        lsu_state := LSUState.REQUESTING
                    }
                }
                is(LSUState.REQUESTING) {
                    mem_read_valid := true.B
                    mem_read_address := io.reg_in.rs
                    lsu_state := LSUState.WAITING
                }
                is(LSUState.WAITING) {
                    when(io.mem_read_address_sender.ready) {
                        mem_read_valid := false.B
                        lsu_out := io.mem_read_data
                        lsu_state := LSUState.DONE
                    }
                }
                is(LSUState.DONE) {
                    when(io.core_state === CoreState.UPDATE) {
                        lsu_state := LSUState.IDLE
                    }
                }
            }
        }

        // if memory write enable is triggered(STR instruction)
        when(io.mem_rw_enable.write_enable) {
            switch(lsu_state) {
                is(LSUState.IDLE) {
                    when(io.core_state === CoreState.REQUEST) {
                        lsu_state := LSUState.REQUESTING
                    }
                }
                is(LSUState.REQUESTING) {
                    mem_write_valid := true.B
                    mem_write_address := io.reg_in.rs
                    mem_write_data := io.reg_in.rt
                    lsu_state := LSUState.WAITING
                }
                is(LSUState.WAITING) {
                    when(io.mem_write_sender.ready) {
                        mem_write_valid := false.B
                        lsu_state := LSUState.DONE
                    }
                }
                is(LSUState.DONE) {
                    when(io.core_state === CoreState.UPDATE) {
                        lsu_state := LSUState.IDLE
                    }
                }
            }
        }
    }

    io.lsu_state := lsu_state
    io.lsu_out := lsu_out
    io.mem_read_address_sender.valid := mem_read_valid
    io.mem_read_address_sender.bits := mem_read_address
    io.mem_write_sender.valid := mem_write_valid
    io.mem_write_sender.bits.address := mem_write_address
    io.mem_write_sender.bits.data := mem_write_data
}
