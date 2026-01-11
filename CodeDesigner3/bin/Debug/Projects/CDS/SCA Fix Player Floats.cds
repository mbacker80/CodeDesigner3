/*
	CodeDesigner v3 ~ Created by: Gtlcpimp
*/



//========================================

// Any whatever hook here, i just went off main loop for testing


address $001f7af8
j :mainLoopHook

address $000c0000

mainLoopHook:

call fixPlayerFloats()

j $001f7aa4
//========================================





//========================================

fnc fixPlayerFloats()\s0,s1,s2
{
	setreg s0, $00709680
	lw s1, $0000(s0)
	lw s2, $0004(s0)
	while (s1 < s2)
	{
		lw s0, $0000(s1)
		setreg v0, $006e6d60
		lw v1, $0000(s0)
		if (v0 == v1)
		{
			lw s0, $0030(s0)
			call floatFixLoop(s0)
		}
		s1 += 4
	}
}

fnc floatFixLoop(EE a0)\s0,s1,s2
{
	s0 = a0
	lw s1, $0050(s0)
	lw s2, $0054(s0)
	
	call fixFloats(s0)
	
	while (s1 < s2)
	{
		lw v0, $0000(s1)
		call floatFixLoop(v0)
		s1 += 4
	}
}

fnc fixFloats(EE a0)\s0,s1,s2
{
	s0 = a0
	s0 += 0x10
	
	for (s1 = 0; s1 < 9; s1 += 1)
	{
		lwc1 f0, $0000(s0)
		cvt.w.s f0, f0
		mfc1 v0, f0
		if (v0 > 1)
			sw zero, $0000(s0)
		if (v0 < -1)
			sw zero, $0000(s0)
		if (v0 == 0)
			sw zero, $0000(s0)
		s0 += 4
	}
}