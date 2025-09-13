Tiny-gpu-chisel Project
=======================

[源项目地址](https://github.com/Max-astro/tiny-gpu-chisel)

=======================
```
构建流程
1. 从chisel-template项目生成代码模板
2. 环境测试, 已经使用cs安装了scala相关环境, 对示例代码进行测试
3. 使用'sbt test'和'./mill tiny-gpu-chisel.test'均测试通过
4. 删除示例文件, 准备添加对应实现
```
```
测试流程
# 测试一个文件
sbt "testOnly alu.AluSpec"

# 测试所有文件
sbt test  # 内存占据太多，导致执行失败
# 顶层模块测试失败，逐个测试子模块
sbt "testOnly alu.AluSpec"
sbt "testOnly pc.PCSpec"
sbt "testOnly registers.RegFileSpec"
sbt "testOnly lsu.LsuSpec"
sbt "testOnly decoder.DecoderSpec"
sbt "testOnly fetcher.FetcherSpec"
sbt "testOnly scheduler.SchedulerSpec"
sbt "testOnly core.CoreSpec"
sbt "testOnly controller.ControllerSpec"
sbt "testOnly dispatch.DispatchSpec"
# 子模块均通过测试
sbt "testOnly gpu.GpuSpec"
sbt "testOnly gpu.GpuBehaviorSpec"
# 顶层通过测试

# 对asm文件进行解析
# test:runMain：显式指定test路径下的main
sbt "Test / runMain gpu.LexerTest"
sbt "Test / runMain gpu.AsmParserTest1"
sbt "Test / runMain gpu.AsmParserTest2"
sbt "Test / runMain gpu.GpuVMAddTest"
sbt "Test / runMain gpu.GpuVMMulTest"
sbt "Test / runMain gpu.MachineCodeEmitterTest"

# 使用脚本
 ./assembler.sh ./asm_files/matadd.asm

# 清理SBT缓存
sbt clean

# 清理系统缓存
sudo sync && echo 3 | sudo tee /proc/sys/vm/drop_caches
```
