object Elaborate extends App {
    val firetool_options = Array(
        "--lowering-options=" + List(
            // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
            "disallowLocalVariables",
            "disallowPackedArrays",
            "locationInfoStyle=wrapInAtSquareBracket"
        ).reduce(_ + "," + _)
    )
    circt.stage.ChiselStage.emitSystemVerilogFile(new alu.Alu(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new pc.ProgramCounter, args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new lsu.MemLoadStoreUnit(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new registers.RegisterFiles(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new scheduler.Scheduler(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new fetcher.Fetcher(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new decoder.Decoder(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new core.Core(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new dispatch.Dispatch(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new dcr.DeviceControlRegister(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new controller.Controller(), args, firetool_options)
    // circt.stage.ChiselStage.emitSystemVerilogFile(new gpu.Gpu(), args, firetool_options)
}
