package controller

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.ControlState

class controller_model(
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
    ): Unit = {
        // updata state machine for each channel
        for (i <- 0 until NumChannels) {
            controller_state(i) match {
                case ControlState.IDLE =>
                    // check if any consumer has read request
                    var break = false
                    var j = 0
                    while (j < NumConsumers && !break) {
                        if (consumer_read_valid(j) && !channel_serving_consumer(j)) {
                            channel_serving_consumer(j) = true
                            current_consumer(i) = j
                            mem_read_valid(i) = true
                            mem_read_address(i) = consumer_read_address(j)
                            controller_state(i) = ControlState.READ_WAITING
                            break = true
                        } else if (consumer_write_valid(j) && !channel_serving_consumer(j)) {
                            channel_serving_consumer(j) = true
                            current_consumer(i) = j
                            mem_write_valid(i) = true
                            mem_write_address(i) = consumer_write_address(j)
                            mem_write_data(i) = consumer_write_data(j)
                            controller_state(i) = ControlState.WRITE_WAITING
                            break = true
                        }
                        j += 1
                    }

                case ControlState.READ_WAITING =>
                    if (mem_read_ready(i)) {
                        mem_read_valid(i) = false
                        consumer_read_ready(current_consumer(i)) = true
                        consumer_read_data(current_consumer(i)) = mem_read_data(i)
                        controller_state(i) = ControlState.READ_RELAYING
                    }

                case ControlState.WRITE_WAITING =>
                    if (mem_write_ready(i)) {
                        mem_write_valid(i) = false
                        consumer_write_ready(current_consumer(i)) = true
                        controller_state(i) = ControlState.WRITE_RELAYING
                    }

                case ControlState.READ_RELAYING =>
                    if (!consumer_read_valid(current_consumer(i))) {
                        channel_serving_consumer(current_consumer(i)) = false
                        consumer_read_ready(current_consumer(i)) = false
                        controller_state(i) = ControlState.IDLE
                    }

                case ControlState.WRITE_RELAYING =>
                    if (!consumer_write_valid(current_consumer(i))) {
                        channel_serving_consumer(current_consumer(i)) = false
                        consumer_write_ready(current_consumer(i)) = false
                        controller_state(i) = ControlState.IDLE
                    }
            }
        }
    }
}

class ControllerSpec extends AnyFreeSpec with Matchers {
    "Test controller" in {
        val AddrBits = 8
        val DataBits = 16
        val NumConsumers = 4
        val NumChannels = 1

        simulate(new Controller(AddrBits, DataBits, NumConsumers, NumChannels)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step(2)
            dut.reset.poke(false.B)
            dut.clock.step(2)

            var cnt = 0
            val rng = new scala.util.Random(0)
            val controller_model = new controller_model(AddrBits, DataBits, NumConsumers, NumChannels)
            while (cnt < 1000) {
                val consumer_read_valid = Array.fill(NumConsumers)(rng.nextBoolean())
                val consumer_read_addr = Array.fill(NumConsumers)(rng.nextInt(AddrBits))
                val consumer_write_valid = Array.fill(NumConsumers)(rng.nextBoolean())
                val consumer_write_addr = Array.fill(NumConsumers)(rng.nextInt(AddrBits))
                val consumer_write_data = Array.fill(NumConsumers)(rng.nextInt(DataBits))
                val mem_read_ready = Array.fill(NumChannels)(rng.nextBoolean())
                val mem_read_data = Array.fill(NumChannels)(rng.nextInt(DataBits))
                val mem_write_ready = Array.fill(NumChannels)(rng.nextBoolean())

                for (i <- 0 until NumConsumers) {
                    dut.io.consumer_read_addr_receiver(i).valid.poke(consumer_read_valid(i).B)
                    dut.io.consumer_read_addr_receiver(i).bits.poke(consumer_read_addr(i).U)
                    dut.io.consumer_write_receiver.map(receiver => {
                        receiver(i).valid.poke(consumer_write_valid(i).B)
                        receiver(i).bits.address.poke(consumer_write_addr(i).U)
                        receiver(i).bits.data.poke(consumer_write_data(i).U)
                    })
                }

                for (i <- 0 until NumChannels) {
                    dut.io.mem_read_sender(i).ready.poke(mem_read_ready(i).B)
                    dut.io.mem_read_data(i).poke(mem_read_data(i).U)
                    dut.io.mem_write_sender.map(_.apply(i).ready.poke(mem_write_ready(i).B))
                }

                dut.clock.step()

                controller_model.update(
                    consumer_read_valid,
                    consumer_read_addr,
                    consumer_write_valid,
                    consumer_write_addr,
                    consumer_write_data,
                    mem_read_ready,
                    mem_read_data,
                    mem_write_ready
                )

                for (i <- 0 until NumConsumers) {
                    dut.io.consumer_read_addr_receiver(i).ready.expect(controller_model.consumer_read_ready(i).B)
                    dut.io.consumer_read_data(i).expect(controller_model.consumer_read_data(i).U)
                    dut.io.consumer_write_receiver.map(
                        _.apply(i).ready.expect(controller_model.consumer_write_ready(i).B)
                    )
                }

                for (i <- 0 until NumChannels) {
                    dut.io.mem_read_sender(i).valid.expect(controller_model.mem_read_valid(i).B)
                    dut.io.mem_read_sender(i).bits.expect(controller_model.mem_read_address(i).U)
                    dut.io.mem_write_sender.map(sender => {
                        sender(i).valid.expect(controller_model.mem_write_valid(i).B)
                        if (controller_model.mem_write_valid(i)) {
                            sender(i).bits.address.expect(controller_model.mem_write_address(i).U)
                            sender(i).bits.data.expect(controller_model.mem_write_data(i).U)
                        }
                    })
                }
                cnt += 1
            }

        }
    }
}
