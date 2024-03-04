# nand2tetris

My complete solutions to the [nand2tetris](https://www.nand2tetris.org/) course exercises.

It includes three Scala projects available as scala-cli [VS Code workspace](./nand2tetris.code-workspace); Hack assembler, Hack VM and Jack compiler.

The rest is in native Hack platform code; HDL, ASM, VM bytecode and Jack language.

## Setup

1. Download tools from [here](https://www.nand2tetris.org/software) and put them in `suite/nand2tetris/tools`. 
2. Install [scala-cli](https://scala-cli.virtuslab.org/)
3. Optionally you can also install [Just](https://github.com/casey/just) and maybe [Nand2Tetris IDE](https://marketplace.visualstudio.com/items?itemName=AvivYaish.nand-ide) VS Code extension.


## Solutions

- [Not, And, Or, Xor, Mux, DMux, Not16, And16, Or16, Mux16, Or8Way, Mux4Way16, Mux8Way16, DMux4Way, DMux8Way](./suite/nand2tetris/projects/01/) logic gates in HDL
- [HalfAdder, FullAdder, Add16, Inc16, ALU](./suite/nand2tetris/projects/02/) chips
- [Bit, Register, RAM8, RAM64, RAM512, RAM4K, RAM16K, PC](./suite/nand2tetris/projects/03) chips
- [Mult.asm and Fill.asm](./suite/nand2tetris/projects/04/) programs for the Hack computer
- [Memory, CPU, Computer](./suite/nand2tetris/projects/05/) chips
- [Hack Assembler in Scala 3](./suite/nand2tetris/projects/06/hasm/)
- [Hack VM translator in Scala 3](./suite/nand2tetris/projects/07/hvm/)
- [HitNRun Jack program](./suite/nand2tetris/projects/09/HitNRun) - the most crude of Jack games If you crash into black square - you lose
- [Jack lang compiler for the Hack VM in Scala 3](./suite/nand2tetris/projects/10/jackc/)
- [Jack OS std lib](./suite/nand2tetris/projects/12/)