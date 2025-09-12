package registers

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

import statecode.CoreState
import statecode.RegInputOp

class RegModel {
    var registers: ArrayBuffer[Int] = ArrayBuffer.fill(16)(0)
    var rs_in = 0
    var rt_in = 0
    private var lastSnap: Snap = Snap(false, 0, 0, 0, 0, 0, 0, 0, 0)

    def rs_out(): Int = {
        registers(rs_in)
    }

    def rt_out(): Int = {
        registers(rt_in)
    }

    def dump(): ArrayBuffer[Int] = registers.clone()
    case class Snap(
        we: Boolean,
        rd: Int,
        rs: Int,
        rt: Int,
        block: Int,
        op: Int,
        imm: Int,
        alu: Int,
        lsu: Int
    )

    def update(
        enable: Boolean,
        core_state: CoreState.Type,
        block_id: Int,
        decoded_reg_write_enable: Boolean,
        decoded_reg_input_op: RegInputOp.Type,
        decoded_immediate: Int,
        alu_out: Int,
        lsu_out: Int,
        rs: Int,
        rt: Int,
        rd: Int
    ): Unit = {
        lastSnap = Snap(
            decoded_reg_write_enable,
            rd,
            rs,
            rt,
            block_id,
            decoded_reg_input_op.litValue.toInt,
            decoded_immediate,
            alu_out,
            lsu_out
        )
        if (enable) {
            registers(13) = block_id
            if (core_state == CoreState.REQUEST) {
                rs_in = rs
                rt_in = rt
            } else if (core_state == CoreState.UPDATE) {
                if (decoded_reg_write_enable && rd < 13) {
                    val write_data = decoded_reg_input_op match {
                        case RegInputOp.CONSTANT   => decoded_immediate
                        case RegInputOp.ARITHMETIC => alu_out
                        case RegInputOp.MEMORY     => lsu_out
                    }
                    registers(rd) = write_data
                }
            } // else {
            //     // 测试Core 模块时, CoreState可能会有其他值
            //     println("RegModel: Unknown core_state")
            // }
        }
    }

    def lastInputSnap: Snap = lastSnap
}

class RegFileSpec extends AnyFreeSpec with Matchers {
    "Test load/store for RegisterFiles" in {
        val threadsPerBlk = 4
        val threadId = 0
        val dataBits = 8
        simulate(new RegisterFiles(threadsPerBlk, threadId, dataBits)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step(5)
            dut.reset.poke(false.B)
            dut.clock.step(5)

            dut.io.enable.poke(true.B)
            // test reg14 and reg15 init value
            dut.io.core_state.poke(CoreState.REQUEST)
            dut.io.decoded_reg_address.rs.poke(14.U)
            dut.io.decoded_reg_address.rt.poke(15.U)
            dut.clock.step()
            dut.io.reg_out.rs.expect(threadsPerBlk.U)
            dut.io.reg_out.rt.expect(threadId.U)

            // helpfor function
            def randomRegOp(rng: scala.util.Random): RegInputOp.Type = {
                val v = rng.nextInt(3)
                v match {
                    case 0 => RegInputOp.CONSTANT
                    case 1 => RegInputOp.ARITHMETIC
                    case 2 => RegInputOp.MEMORY
                }
            }

            def randomCoreState(rng: scala.util.Random): CoreState.Type = {
                val v = rng.nextInt(2)
                v match {
                    case 0 => CoreState.REQUEST
                    case 1 => CoreState.UPDATE
                }
            }

            val reg_model = new RegModel()

            def dumpRegFile(): Unit = {
                val we = dut.io.decoded_reg_write_enable.peek().litValue
                // ERROR: op无法导出
                // val op = dut.io.decoded_reg_input_op.toBigInt
                // val opName = op.toInt match {
                //     case 0 => "ARITH"
                //     case 1 => "MEM"
                //     case 2 => "CONST"
                //     case _ => "?"
                // }
                val blk = dut.io.block_id.peek().litValue
                val rd = dut.io.decoded_reg_address.rd.peek().litValue
                val rs = dut.io.decoded_reg_address.rs.peek().litValue
                val rt = dut.io.decoded_reg_address.rt.peek().litValue
                val imm = dut.io.decoded_immediate.peek().litValue
                val alu = dut.io.alu_out.peek().litValue
                val lsu = dut.io.lsu_out.peek().litValue
                val hwRs = dut.io.reg_out.rs.peek().litValue
                val hwRt = dut.io.reg_out.rt.peek().litValue

                // 把整个寄存器堆从模型里拉出来(你需要在 RegModel 里加一句导出方法,见下方)
                val modelSnap = reg_model.dump() // Array[Int](16),每行一个寄存器

                println(
                    f"@[we=$we%5s blk=$blk%2x]" +
                        f"  in: rd=$rd%2d rs=$rs%2d rt=$rt%2d imm=$imm%02x alu=$alu%02x lsu=$lsu%02x" +
                        f"  out: rs=$hwRs%02x rt=$hwRt%02x"
                )
                println(
                    "  model regs: " + modelSnap
                        .map(v => f"$v%02x")
                        .mkString(" ")
                )
            }

            val rng = new scala.util.Random(42)
            for (i <- 0 until 100) {
                val block_id = rng.nextInt(256)
                val rd_in = rng.nextInt(12)
                val rs_in = rng.nextInt(12)
                val rt_in = rng.nextInt(12)
                val op = randomRegOp(rng)
                val core_state = randomCoreState(rng)
                val alu_out = rng.nextInt(256)
                val lsu_out = rng.nextInt(256)
                val immediate = rng.nextInt(256)

                dut.io.core_state.poke(core_state)
                val is_write = core_state == CoreState.UPDATE
                dut.io.decoded_reg_write_enable.poke(is_write.B)
                dut.io.block_id.poke(block_id.U)
                dut.io.decoded_reg_address.rd.poke(rd_in.U)
                dut.io.decoded_reg_address.rs.poke(rs_in.U)
                dut.io.decoded_reg_address.rt.poke(rt_in.U)

                dut.io.decoded_reg_input_op.poke(op)
                dut.io.decoded_immediate.poke(immediate.U)
                dut.io.alu_out.poke(alu_out.U)
                dut.io.lsu_out.poke(lsu_out.U)

                // Update model
                reg_model.update(
                    enable = true,
                    block_id = block_id,
                    core_state = core_state,
                    rd = rd_in,
                    rs = rs_in,
                    rt = rt_in,
                    decoded_reg_write_enable = is_write,
                    decoded_reg_input_op = op,
                    decoded_immediate = immediate,
                    alu_out = alu_out,
                    lsu_out = lsu_out
                )

                dut.clock.step()

                val hw_rs = dut.io.reg_out.rs.peek().litValue
                val hw_rt = dut.io.reg_out.rt.peek().litValue
                val ref_rs = reg_model.rs_out()
                val ref_rt = reg_model.rt_out()

                // if (hw_rs != ref_rs || hw_rt != ref_rt) {
                // println(
                //     f"*** FAIL ***" +
                //         f"  rs:(ref=$ref_rs%02x,hw=$hw_rs%02x)" +
                //         f"  rt:(ref=$ref_rt%02x,hw=$hw_rt%02x)" +
                //         f"  we=${dut.io.decoded_reg_write_enable.peek().litValue}" +
                //         f"  block=${dut.io.block_id.peek().litValue}" +
                //         f"  rd=${dut.io.decoded_reg_address.rd.peek().litValue}"
                // )
                //     dumpRegFile()
                // }

                if ((core_state == CoreState.REQUEST) && (hw_rs != ref_rs || hw_rt != ref_rt)) {
                    val s = reg_model.lastInputSnap // 参考模型上一拍输入
                    println("-------- FAIL --------")
                    println(
                        f"HW  : we=${dut.io.decoded_reg_write_enable.peek().litValue}" +
                            f" blk=${dut.io.block_id.peek().litValue}" +
                            f" rd=${dut.io.decoded_reg_address.rd.peek().litValue}" +
                            f" rs=${dut.io.decoded_reg_address.rs.peek().litValue}" +
                            f" rt=${dut.io.decoded_reg_address.rt.peek().litValue}" +
                            f" imm=${dut.io.decoded_immediate.peek().litValue}%02x" +
                            f" alu=${dut.io.alu_out.peek().litValue}%02x" +
                            f" lsu=${dut.io.lsu_out.peek().litValue}%02x"
                    )
                    println(
                        f"MODEL:we=${s.we} blk=${s.block}%2x" +
                            f" rd=${s.rd}%2d rs=${s.rs}%2d rt=${s.rt}%2d" +
                            f" imm=${s.imm}%02x alu=${s.alu}%02x lsu=${s.lsu}%02x" +
                            f" op=${s.op}%d"
                    )
                    println(
                        f"OUT  : rs(ref=$ref_rs%02x,hw=$hw_rs%02x)  rt(ref=$ref_rt%02x,hw=$hw_rt%02x)"
                    )
                    println(
                        "  model regs: " + reg_model
                            .dump()
                            .map(v => f"$v%02x")
                            .mkString(" ")
                    )
                }

                if (core_state == CoreState.REQUEST) {
                    dut.io.reg_out.rs.expect(reg_model.rs_out().U)
                    dut.io.reg_out.rt.expect(reg_model.rt_out().U)
                }
            }
        }
    }
}
