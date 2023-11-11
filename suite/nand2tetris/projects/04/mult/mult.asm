// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// Assumes that R0 >= 0, R1 >= 0, and R0 * R1 < 32768.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// R2 = R0 * R1

// LOOP:
//  if (R1 == 0 || R0 == 0) goto ASSIGN
//  else 
//     R1 = R1 - 1
//     RESULT = RESULT + R0
//     goto LOOP
// ASSIGN: 
//  R2 = RESULT
//  goto END
// END:

@RESULT
M=0

(LOOP)
    // if (R0 == 0) goto ASSIGN
    @R0
    D=M
    @ASSIGN
    D;JEQ
    // if (R1 == 0) goto ASSIGN
    @R1
    D=M
    @ASSIGN
    D;JEQ
    // R1 = R1 - 1
    @R1
    M=M-1
    // RESULT = RESULT + R0
    @R0
    D=M
    @RESULT
    M=M+D
    // goto LOOP
    @LOOP
    0;JMP
(ASSIGN)
    // R2 = RESULT
    @RESULT
    D=M
    @R2
    M=D
(END)
    // end loop
    @END
    0;JMP
