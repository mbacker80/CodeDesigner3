address $000c0000

prochook $0027abc8 -j

define pad_address $007157dc
define cAppCamera $00711268
define camCZSeal $dc
define xhair_X $03fc
define xhair_Y $0400
define xhair_Z $0404

thread main

fnc main(void)
{
	call Controller_KeyPress()
	switch (v0)
	{
		case $FFFD // L3
		{
			call spawnTest()
		}
		case $FFFB // R3
		{
			call spawnHumveeTest()
		}
	}
	
}
fnc Controller_KeyPress(void) \s0,s1
{
	beq zero, zero, :Controller_KeyPress_SaveData_Branch
	nop
	Controller_KeyPress_SaveData:
	nop
	Controller_KeyPress_SaveData_Branch:
	
	setreg s0, :pad_address
	setreg s1, :Controller_KeyPress_SaveData
	
	lhu a0, $0000(s0)
	xori a0, a0, $ffff
	
	lhu a1, $0000(s1)
	nor a2, a1, a1
	and v0, a0, a2
	sw a0, $0000(s1)
	
	addiu v1, zero, -1
	xor v0, v0, v1
	
	// v0 = new button press
}


fnc spawnHumveeTest()\s0,s1,s2,s3,s4,s5,s6,s7
{
	// Pull crosshair coordinates
	setreg s0, :cAppCamera
	lw s0, $0000(s0)
	lw s0, $00dc(s0)
	
	lw t0, :xhair_X(s0)
	lw t1, :xhair_Y(s0)
	lw t2, :xhair_Z(s0)
	
	
	// Write coordinates to argument 3
	setreg v0, :__arg_a3_DATA
	sw t0, $0000(v0)
	sw t1, $0004(v0)
	sw t2, $0008(v0)
	
	setreg v0, :__arg_a1_DATA
	sw t0, $006c(v0)
	sw t1, $0070(v0)
	sw t2, $0074(v0)
	
	setreg a0, :__arg_a1_DATA
	call appendItem(a0)
	s1 = v1
	s2 = v0
	
	setreg a0, $00709890
	//setreg a1, :__arg_a1_DATA
	a1 = v0
	
	setreg a1, $011de490
	
	setreg a2, :__arg_a2_DATA
	setreg a3, :__arg_a3_DATA
	setreg t0, :__arg_t0_DATA
	jal $0028cf90
	nop
	
	sw s1, $000c(v0)
	s3 = v0
	
	call initVeh(s1, s2, s3)
	
	v0 = s3
}

fnc initVeh(EE a0, EE a1, EE a2)\s0,s1,s2,s3
{
	s0 = a1
	s1 = a0
	s3 = a2
	
	addiu sp, sp, $ff00
	daddu a0, sp, zero
	jal $00521220
	sw zero, $0000(sp)
	
	setreg a0, $00709680
	jal $0027a2f0
	daddu a1, sp, zero
	
	s2 = v0
	addiu sp, sp, $0100
	
	lw t9, $0000(s2)
	lw t9, $0090(t9)
	jalr t9
	daddu a0, s2, zero
	
	lw t9, $0000(s2)
	lw t9, $008c(t9)
	jalr t9
	daddu a0, s2, zero
	
	daddu a0, s2, zero
	addiu a1, zero, -1
	jal $00348550
	ori a2, zero, 1
	
	
	
	jal $0027d010
	ori a0, zero, 1
	
	setreg a0, $00709890
	jal $0028c160
	daddu a1, zero, zero
}

fnc spawnTest()\s0,s1,s2,s3
{
	setreg s0, $00709680
	lw s1, $0004(s0)
	lw s0, $0000(s0)
	
	while (s0 < s1)
	{
		lw a0, $0000(s0)
		lw a0, $0030(a0)
		lw a0, $0008(a0)
		addiu v0, zero, 1
		sb v0, $00a7(a0)
		
		s0 += 4
	}
	
	
/*

		lw s1, $0000(a0)
		lw s1, $0030(s1)
		lw s1, $0008(s1)
		
		setreg v0, :cAppCamera
		lw v0, $0000(v0)
		lw v0, $00dc(v0)
		lw v0, $0030(v0)
		lw v0, $0008(v0)
		if (s1 <> v0)
		{
			lw v0, $0128(s1)
			if (v0 <= 0)
			{
				addiu v0, zero, 1
				sb v0, $00A7(s1)
			}
		}
*/
}

fnc appendItem(EE a0)\s0,s1,s2,s3,s4
{
	setreg s0, $00709c80
	lw s4, $0000(s0)
	lw s0, $0000(s4)
	lw s1, $0004(s4)
	lw s2, $0008(s4)
	s3 = a0
	
	// s0 = List Start
	// s1 = List End / Write Address for next Item
	// s2 = End of Allocated List Space
	
	// s3 = Custom Object to Add
	v1 = s1
	for (a0 = 0; a0 < 0x1ec; a0 += 4)
	{
		lw v0, $0000(s3)
		sw v0, $0000(v1)
		s3 += 4
		v1 += 4
	}
	sw v1, $0004(s4)
	
	subu v0, s1, s0
	ori v1, zero, $01ec
	divu v0, v1
	mflo v0
	
	sh v0, $01E6(s1)
	v1 = v0
	v0 = s1
}

//-------------------- C Mission
memalign quad
__arg_a0:
hexcode $00709890

//-------------------- Point to Data Block
memalign quad
__arg_a1:
hexcode :__arg_a1_DATA

//-------------------- Point to C Load State?
memalign quad
__arg_a2:
hexcode :__arg_a2_DATA

//-------------------- Point to Coordinates
memalign quad
__arg_a3:
hexcode :__arg_a3_DATA

//-------------------- Point to Orientation?
memalign quad
__arg_t0:
hexcode :__arg_t0_DATA






//========================================================= Data Block
memalign quad
__arg_a1_DATA:

print "multi_turret_humvee2"
nop
nop
print "_16696_M1025_UI_MSG"
nop
nop
nop
hexcode $b68bc4e6
nop
nop
nop
nop
hexcode $00000002
hexcode $00000001
nop
nop
nop
nop
hexcode $beefbeef // x
hexcode $beefbeef // y
hexcode $beefbeef // z

padding $e8

hexcode $ffffffff
nop
hexcode $bf800000
hexcode $3f19999a
hexcode $3f000000
hexcode $3f800000
hexcode $43fa0000
hexcode $43340000
hexcode $3f000000
nop
nop
hexcode $3f000000
padding $50
hexcode $00010001
nop
nop







//========================================================= C Load State ?
memalign quad
__arg_a2_DATA:
hexcode $ffffffff
hexcode $050020c7
hexcode $006ebed8
nop
nop
nop
hexcode $00000001
nop
nop
nop
hexcode $006ebed8
nop
nop
nop
hexcode $006ebea0
nop
nop
nop
hexcode $0062caf8
nop
nop
nop
nop
nop
hexcode $25f8856f
hexcode $9dbe0f56
hexcode $f34f1da9
hexcode $05f531db
hexcode $5fdacba0
hexcode $215c113d
hexcode $0062c8b8
nop
hexcode $bb77fce8
nop
hexcode $3d088889
nop
nop
nop
hexcode $006ebed8 // CLoadState
nop
nop
nop
hexcode $00000001
nop
nop
nop
hexcode $0062cbdc
nop
nop
nop
hexcode $01ffed2f
nop
nop
nop
hexcode $3d088889
nop
nop
nop
hexcode $00000001
nop
nop
nop
hexcode $002e4e58
nop
hexcode $014a2cd0 // -> "veh_multi_turret_humvee.rdr"
hexcode $00030001
nop
nop
nop
nop
hexcode $0080a184
nop
nop
nop
hexcode $00000001
nop
nop
nop
hexcode $00000001
nop
nop
nop
hexcode $0080a184
nop
nop
nop
hexcode $001f7ae4
nop
hexcode $002ed4b0
nop
hexcode $0080a184
nop
nop
nop
hexcode $00000001
nop
nop
nop
hexcode $001f6f78
nop
nop
nop

//========================================================= Coordinates
memalign quad
__arg_a3_DATA:
nop
nop
nop
hexcode $3f800000



//========================================================= Orientation
memalign quad
__arg_t0_DATA:
nop
hexcode $b3bbbe2f
nop
hexcode $bf800000
nop
hexcode $3f800000
nop
nop
hexcode $bf34fdf4
nop
hexcode $3f34fdf4
nop
