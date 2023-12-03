hsim:
    ./suite/nand2tetris/tools/HardwareSimulator.sh
cpu-emu:
    ./suite/nand2tetris/tools/CPUEmulator.sh
asm:
    ./suite/nand2tetris/tools/Assembler.sh
vm:
    ./suite/nand2tetris/tools/VMEmulator.sh
jackc:
    ./suite/nand2tetris/tools/JackCompiler.sh

hackasm:
    scala-cli run ./suite/nand2tetris/projects/06/hasm/Main.scala