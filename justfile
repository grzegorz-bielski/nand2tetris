# nand2tetris suite programs

hsim:
    ./suite/nand2tetris/tools/HardwareSimulator.sh
cpu-emu:
    ./suite/nand2tetris/tools/CPUEmulator.sh
asm:
    ./suite/nand2tetris/tools/Assembler.sh
vm:
    ./suite/nand2tetris/tools/VMEmulator.sh
jackc path:
    ./suite/nand2tetris/tools/JackCompiler.sh {{path}}

# executable projects

hasm:
    scala-cli run ./suite/nand2tetris/projects/06/hasm/

hvm:
    scala-cli run ./suite/nand2tetris/projects/07/hvm/