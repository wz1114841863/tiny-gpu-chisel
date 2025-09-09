package controller

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.ControlState

class ControllerModel(
    AddrBits: Int = 8,
    DataBits: Int = 16,
    NumConsumers: Int = 4,
    NumChannels: Int = 1
) {
    var controller_state = Array.fill(NumChannels)(ControlState.IDLE)
    var mem_read_valid = Array.fill(NumChannels)(false)
    var mem_read_address = Array.fill(NumChannels)(0)
    var mem_write_valid = Array.fill(NumChannels)(false)
    var mem_write_address = Array.fill(NumChannels)(0)
    var mem_write_data = Array.fill(NumChannels)(0)

    var consumer_read_ready = Array.fill(NumConsumers)(false)
    var consumer_read_data = Array.fill(NumConsumers)(0)
    var consumer_write_ready = Array.fill(NumConsumers)(false)
    var channel_serving_consumer = Array.fill(NumConsumers)(false)
    var current_consumer = Array.fill(NumChannels)(0)

    def update(
        consumer_read_valid: Array[Boolean],
        consumer_read_address: Array[Int],
        consumer_write_valid: Array[Boolean],
        consumer_write_address: Array[Int],
        consumer_write_data: Array[Int],
        mem_read_ready: Array[Boolean],
        mem_read_data: Array[Int],
        mem_write_ready: Array[Boolean]
    ): Unit = {}
}
