/*
	CodeDesigner v3 ~ Created by: Gtlcpimp
*/

address $0028cf90
j :test
nop

address $000f0000

fnc test()\s0,s1,s2,s3,s4,s5,s6,s7
{
	lui v0, $000e
	sw a0, $0000(v0)
	sw a1, $0004(v0)
	sw a2, $0008(v0)
	sw a3, $000c(v0)
	sw t0, $0010(v0)
	sw ra, $0014(v0)
	
	s0 = a0
	s1 = a1
	s2 = a2
	s3 = a3
	s4 = t0
	/*
	for (s5 = 0; s5 < 5; s5 ++)
	{
		a0 = s0
		a1 = s1
		a2 = s2
		a3 = s3
		t0 = s4
		
		jal :spawnVeh
		nop
	}
	*/
	jal :spawnVeh
	nop
}

spawnVeh:
addiu sp, sp, $ff90
j $0028cf98
lui v0, $0002
