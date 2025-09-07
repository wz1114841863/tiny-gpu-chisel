package statecode

import chisel3._

object CoreState extends ChiselEnum {
    // 对应于scheduler的状态机
    val IDLE = Value("b000".U)
    val FETCH = Value("b001".U)
    val DECODE = Value("b010".U)
    val REQUEST = Value("b011".U)
    val WAIT = Value("b100".U)
    val EXECUTE = Value("b101".U)
    val UPDATE = Value("b110".U)
    val DONE = Value("b111".U)
}

object AluOpCode extends ChiselEnum {
    // 对应于alu的操作码
    val ADD, SUB, MUL, DIV = Value
}

object RegInputOp extends ChiselEnum {
    // 对应于registerfiles模块写入数据的选择
    val ARITHMETIC = Value("b00".U) // 来自算术逻辑单元的结果
    val MEMORY = Value("b01".U) // 来自内存的数据
    val CONSTANT = Value("b10".U) // 来自指令中的立即数
}

object DecoderState extends ChiselEnum {
    // 对应于decoder模块对指令的解码
    val NOP = Value("b0000".U)
    val BRnzp = Value("b0001".U)
    val CMP = Value("b0010".U)
    val ADD = Value("b0011".U)
    val SUB = Value("b0100".U)
    val MUL = Value("b0101".U)
    val DIV = Value("b0110".U)
    val LDR = Value("b0111".U)
    val STR = Value("b1000".U)
    val CONST = Value("b1001".U)
    val RET = Value("b1111".U)
}

object ControlState extends ChiselEnum {
    // 对应于controller模块的状态机
    val IDLE = Value("b000".U)
    val READ_WAITING = Value("b010".U)
    val WRITE_WAITING = Value("b011".U)
    val READ_RELAYING = Value("b100".U)
    val WRITE_RELAYING = Value("b101".U)
}

object FetcherState extends ChiselEnum {
    // 对应于fetcher模块的状态机
    val IDLE = Value("b00".U)
    val FETCHING = Value("b01".U)
    val FETCHED = Value("b10".U)
}

object LSUState extends ChiselEnum {
    // 对应于LSU模块的状态机
    val IDLE, REQUESTING, WAITING, DONE = Value
}
