@17
D=A
@SP
AM=M+1
A=A-1
M=D
@17
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_9
D;JEQ
@SP
A=M-1
M=0
@CHECK_END_StackTest_9
0;JMP
(ON_TRUE_StackTest_9)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_9)
@17
D=A
@SP
AM=M+1
A=A-1
M=D
@16
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_12
D;JEQ
@SP
A=M-1
M=0
@CHECK_END_StackTest_12
0;JMP
(ON_TRUE_StackTest_12)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_12)
@16
D=A
@SP
AM=M+1
A=A-1
M=D
@17
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_15
D;JEQ
@SP
A=M-1
M=0
@CHECK_END_StackTest_15
0;JMP
(ON_TRUE_StackTest_15)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_15)
@892
D=A
@SP
AM=M+1
A=A-1
M=D
@891
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_18
D;JLT
@SP
A=M-1
M=0
@CHECK_END_StackTest_18
0;JMP
(ON_TRUE_StackTest_18)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_18)
@891
D=A
@SP
AM=M+1
A=A-1
M=D
@892
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_21
D;JLT
@SP
A=M-1
M=0
@CHECK_END_StackTest_21
0;JMP
(ON_TRUE_StackTest_21)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_21)
@891
D=A
@SP
AM=M+1
A=A-1
M=D
@891
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_24
D;JLT
@SP
A=M-1
M=0
@CHECK_END_StackTest_24
0;JMP
(ON_TRUE_StackTest_24)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_24)
@32767
D=A
@SP
AM=M+1
A=A-1
M=D
@32766
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_27
D;JGT
@SP
A=M-1
M=0
@CHECK_END_StackTest_27
0;JMP
(ON_TRUE_StackTest_27)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_27)
@32766
D=A
@SP
AM=M+1
A=A-1
M=D
@32767
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_30
D;JGT
@SP
A=M-1
M=0
@CHECK_END_StackTest_30
0;JMP
(ON_TRUE_StackTest_30)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_30)
@32766
D=A
@SP
AM=M+1
A=A-1
M=D
@32766
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
D=M-D
@ON_TRUE_StackTest_33
D;JGT
@SP
A=M-1
M=0
@CHECK_END_StackTest_33
0;JMP
(ON_TRUE_StackTest_33)
@SP
A=M-1
M=-1
(CHECK_END_StackTest_33)
@57
D=A
@SP
AM=M+1
A=A-1
M=D
@31
D=A
@SP
AM=M+1
A=A-1
M=D
@53
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
M=D+M
@112
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
M=M-D
@SP
A=M-1
M=-M
@SP
AM=M-1
D=M
A=A-1
M=D&M
@82
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
M=D|M
@SP
A=M-1
M=!M