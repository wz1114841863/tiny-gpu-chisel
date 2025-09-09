package controller

import chisel3._
import chisel3.util._

import statecode.ControlState

class Controller(
    AddrBits: Int = 8,
    DataBits: Int = 16,
    NumConsumers: Int = 4,
    NumChannels: Int = 4,
    WriteEnable: Boolean = true
) extends Module {
    val io = IO(new Bundle {
        // Consumer Interfaces (Fectchers/LSUs)
        val consumer_read_addr_receiver = Vec(NumConsumers, Flipped(new DecoupledIO(UInt(AddrBits.W))))
        val consumer_read_data = Output(Vec(NumConsumers, UInt(DataBits.W)))

        val consumer_write_receiver =
            if (WriteEnable)
                Some(
                    Vec(
                        NumConsumers,
                        Flipped(new DecoupledIO(new Bundle {
                            val address = UInt(AddrBits.W)
                            val data = UInt(DataBits.W)
                        }))
                    )
                )
            else None

        val mem_read_sender = Vec(NumChannels, new DecoupledIO(UInt(AddrBits.W)))
        val mem_read_data = Input(Vec(NumChannels, UInt(DataBits.W)))

        val mem_write_sender =
            if (WriteEnable)
                Some(
                    Vec(
                        NumChannels,
                        new DecoupledIO(new Bundle {
                            val address = UInt(AddrBits.W)
                            val data = UInt(DataBits.W)
                        })
                    )
                )
            else None
    })
    val mem_read_valid = RegInit(VecInit(Seq.fill(NumChannels)(false.B)))
    val mem_read_address = RegInit(VecInit(Seq.fill(NumChannels)(0.U(AddrBits.W))))

    val mem_write_valid = RegInit(VecInit(Seq.fill(NumChannels)(false.B)))
    val mem_write_address = RegInit(VecInit(Seq.fill(NumChannels)(0.U(AddrBits.W))))
    val mem_write_data = RegInit(VecInit(Seq.fill(NumChannels)(0.U(DataBits.W))))

    val consumer_read_ready = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))
    val consumer_read_data = RegInit(VecInit(Seq.fill(NumConsumers)(0.U(DataBits.W))))
    val consumer_write_ready = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))

    // 每个通道当前正在服务的消费者编号
    val current_consumer = RegInit(VecInit(Seq.fill(NumChannels)(0.U(log2Ceil(NumConsumers).W))))
    // 每个通道的状态机状态
    val controller_state = RegInit(VecInit(Seq.fill(NumChannels)(ControlState.IDLE)))
    // 记录每个消费者当前是否被某个通道服务
    val channel_serving_consumer = Wire(Vec(Seq.fill(NumConsumers)(false.B)))

    while (!reset.asBool) {
        for (i <- 0 until NumChannels) {
            switch(controller_state(i)) {
                is(ControlState.IDLE) {
                    // 创建一个临时的布尔向量, 长度位NumConsumers
                    // 用于表示该消费者是否发起了读请求, 且当前没有被其他通道服务
                    val read_signals = Wire(Vec(NumConsumers, Bool()))
                    // 创建一个临时的布尔向量, 长度位NumConsumers
                    // 用于表示该消费者是否发起了写请求, 且当前没有被其他通道服务
                    // 用VecInit是因为 write_signals 是可选接口(Option)
                    val write_signals = VecInit(Seq.fill(NumConsumers)(false.B))
                    for (j <- 0 until NumConsumers) {
                        read_signals(j) = io.consumer_read_addr_receiver(j).valid && !channel_serving_consumer(j)
                        io.consumer_write_receiver.map(write_receiver =>
                            write_signals(j) := write_receiver(j).valid && !channel_serving_consumer(j)
                        )
                    }

                    // 在空闲状态下,从所有消费者中挑一个优先级最高的请求
                    // (读优先于写,序号小的优先),
                    // 发起对应的内存访问,并切换到等待状态.

                    // PriorityEncoder 把独热码(one-hot)变成二进制索引,并且从低位开始优先.
                    val first_read_idx = PriorityEncoder(read_signals)
                    val first_write_idx = PriorityEncoder(write_signals)

                    when(read_signals.asUint > 0.U && first_read_idx <= first_write_idx) {
                        // 只要有读请求,并且读请求的序号 ≤ 写请求的序号,
                        // 就先处理读. 序号相同时, 读优先
                        val read_address = io.consumer_read_addr_receiver(first_read_idx).bits
                        channel_serving_consumer(first_read_idx) := true.current_consumer(i) := first_read_idx.U

                        controller_state(i) := ControlState.READ_WAITING
                    }.elsewhen(write_signals.asUint > 0.U) {
                        val write_address =
                            // 因为写接口是 Option[Vec[...]],必须 .map(...).getOrElse(...) 安全取值.
                            io.consumer_write_receiver.map(_.apply(first_write_idx).bits.address).getOrElse(0.U)
                        val write_data =
                            io.consumer_write_receiver.map(_.apply(first_write_idx).bits.data).getOrElse(0.U)

                        channel_serving_consumer(first_write_idx) := true.B
                        current_consumer(i) := first_write_idx

                        mem_write_valid(i) := true.B
                        mem_write_address(i) := write_address
                        mem_write_data(i) := write_data

                        controller_state(i) := ControlState.WRITE_WAITING
                    }
                }
                is(ControlState.READ_WAITING) {
                    // 等待内存读数据返回
                    when(io.mem_read_sender(i).ready) {
                        mem_read_valid(i) := false.B
                        consumer_read_ready(current_consumer(i)) := true.B
                        consumer_read_data(current_consumer(i)) := io.mem_read_data(i)
                        controller_state(i) := ControlState.READ_RELAYING
                    }
                }
                is(ControlState.WRITE_WAITING) {
                    // 等待内存写请求被接受
                    val write_ready = io.mem_write_sender.map(_.apply(i).ready).getOrElse(true.B)
                    when(write_ready) {
                        mem_write_valid(i) := false.B
                        consumer_write_ready(current_consumer(i)) := true.B
                        controller_state(i) := ControlState.WRITE_RELAYING
                    }
                }
                is(ControlState.READ_RELAYING) {
                    when(!io.consumer_read_addr_receiver(current_consumer(i)).valid) {
                        channel_serving_consumer(current_consumer(i)) := false.B
                        consumer_read_ready(current_consumer(i)) := false.B
                        controller_state(i) := ControlState.IDLE
                    }
                }
                is(ControlState.WRITE_RELAYING) {
                    val write_valid =
                        io.consumer_write_receiver.map(!_.apply(current_consumer(i)).valid).getOrElse(false.B)
                    when(write_valid) {
                        channel_serving_consumer(current_consumer(i)) := false.B
                        consumer_write_ready(current_consumer(i)) := false.B
                        controller_state(i) := ControlState.IDLE
                    }
                }
            }
        }
    }

    // 输出连接
    for (i <- 0 until NumConsumers) {
        io.consumer_read_addr_receiver(i).ready := consumer_read_ready(i)
        io.consumer_read_data(i) := consumer_read_data(i)
        io.consumer_write_receiver.map(_.apply(i).ready := consumer_write_ready(i))
    }

    for (i <- 0 until NumChannels) {
        io.mem_read_sender(i).valid := mem_read_valid(i)
        io.mem_read_sender(i).bits := mem_read_address(i)
        io.mem_write_sender.map(sender => {
            sender(i).valid := mem_write_valid(i)
            sender(i).bits.address := mem_write_address(i)
            sender(i).bits.data := mem_write_data(i)
        })
    }
}
