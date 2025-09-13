#!/bin/bash
# Usage: Tiny-GPU MachineCodeEmitter <asm_file> [--idx: Print instructions with index]

sbt "Test / runMain gpu.MachineCodeEmitter $1 ${@:2}"
