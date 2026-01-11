/*
	CodeDesigner 3 ~ Created by: Gtlcpimp
*/

//------------------------- ELF Configuration
address $00100000
define _ELF_Stack_Address $01F00000
define _ELF_Stack_Size	$00008000
define _ELF_Global_Position $00000000
define _ELF_Heap_Address $00800000
define _ELF_Heap_Size $FFFFFFFF

elf.entry :_ELF_Init

_ELF_Init:

//------------------------- Clear BSS
// We don't have it so do nothing

//------------------------- Setup Thread
setreg a0, :_ELF_Global_Position
setreg a1, :_ELF_Stack_Address
setreg a2, :_ELF_Stack_Size
daddu a3, zero, zero
setreg t0, :_ELF_Exit_Thread
daddu gp, a0, zero
v1 = $003c
syscall (00000) //--------- RFU060
sp = v0

//------------------------- Setup Heap
setreg a0, :_ELF_Heap_Address
setreg a1, :_ELF_Heap_Size
v1 = $003d
syscall (00000) //--------- RFU061

//------------------------- Flush Cache
a0 = 0
v1 = $0064
syscall (00000) //--------- FlushCache

//------------------------- Launch main()
ei
call main()

//------------------------- Exit Thread
_ELF_Exit_Thread:
v1 = $23
syscall (00000)
jr ra
nop
