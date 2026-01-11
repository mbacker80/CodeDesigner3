/*
	CodeDesigner v3 ~ Created by: Gtlcpimp
*/

address $000c0000

setreg sp $01ff0000

fnc test()\s0,s1,s2
{
	daddu a0, zero, zero
	for (a1 = 0; a1 < 100; a1 += 1)
	{
		a0 += 5
	}
	return 1
}