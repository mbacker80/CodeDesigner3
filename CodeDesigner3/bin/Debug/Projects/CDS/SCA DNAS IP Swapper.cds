/*
	CodeDesigner v3 ~ Created by: Gtlcpimp
*/

address $0029c244
jal :dnas_swapper_install

address $000f8000

fnc dnas_swapper_install(void) \s0,s1
{
	setreg s0, :dnas_swapper
	srl s0, s0, 2
	lui s1, $0800
	or s0, s0, s1
	setreg s1, $01367348
	sw s0, $0000(s1)
	
	jal $001f65a0
	nop
}


pc_ip:
print "192.168.0.19"

dnas_swapper:
setreg a0, $0136738c
setreg a1, $013673cc
setreg a2, :pc_ip
lbu v0, $0000(a2)
while (v0)
{
	sb v0, $0000(a0)
	sb v0, $0000(a1)
	a0++
	a1++
	a2++
	lbu v0, $0000(a2)
}
sb zero, $0000(a0)
j $013673b4
sb zero, $0000(a1)
