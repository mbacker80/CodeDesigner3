/*
	Virtual Cheat Server for PCSX2
	Created by: Gtlcpimp
*/

//------------------------ Kernel Hook
address 0x800002fc
jal :_init


//------------------------ Main Engine
address 0x00070000

/*
	Flag Options:
		0 = Idle
		1 = Ready to Write
		2 = Writing / Busy
*/
define writeFlag 0x8005fffc
define codeSpace 0x80060000


_init:
addiu sp, sp, $fff0
sq ra, $0000(sp)
jalr k0
nop
lq ra, $0000(sp)
j :main
addiu sp, sp, $0010


fnc main()\at,v0,v1,a0,a1,a2,a3,s0,s1,s2,s3,s4,s5,s6,s7,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,k0,k1,fp,gp
{
	setreg s0, :writeFlag
	lw v0, $0000(s0)
	if (v0 == 0)
	{
		return 0
	}
	
	//------------------- Set write flag to busy
	v1 = 2
	sw v1, $0000(s0)
	
	setreg a0, :codeSpace
	call ProcessCodeChunk(a0)
	
	//------------------- Set write flag to idle
	sw zero, $0000(s0)
}
