// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen
// by writing 'black' in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen by writing
// 'white' in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(APP_LOOP)
    @KBD
    D=M
    @FILL_BLACK
    D;JNE
    @FILL_WHITE
    D;JEQ

(FILL_BLACK)
    @FILL_VALUE
    M=-1
    @FILL_SCREEN
    0;JMP

(FILL_WHITE)
    @FILL_VALUE
    M=0
    @FILL_SCREEN
    0;JMP

(FILL_SCREEN)
    // Screen is 32 16-bit words starting from @SCREEN
    // 0 - white pixel
    // 1 - black pixel

    // R0 = array start address
    @SCREEN
    D=A
    @R0
    M=D
    // R1 = array size
    // 256 rows * 32 pixels/row = 8192 pixels
    @8192
    D=A
    @R1
    M=D
    // generic array fill
    @n
    M=0
(FILL_LOOP)
    // if (n == R1) goto FILL_LOOP_END
    @n
    D=M
    @R1
    D=D-M
    @FILL_LOOP_END
    D;JEQ
    // *(R0 + n) = @FILL_VALUE
    @R0
    D=M
    @n
    D=D+M
    @FILL_PTR
    M=D
    @FILL_VALUE
    D=M
    @FILL_PTR
    A=M
    M=D
    // n = n + 1
    // @16
    // D=A
    @n
    M=M+1
    // goto FILL_LOOP
    @FILL_LOOP
    0;JMP
(FILL_LOOP_END)
    @APP_LOOP
    0;JMP
