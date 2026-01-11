/*

	CheatersLounge CheatsEngine v1.4
	Created by: Gtlcpimp
	
*/

/*
// Original Engine Address
address $80078910


*/

//============================================================
/*
	Chunk Style
	
	00000000
	
	TTWWAAAA
	
	T: Type
		00 Normal Code
		01 Embeded Data -------------> Not Implemented Yet
		02 Embeded Routine ----------> Not Implemented Yet
	W: Write
		00 Normal Code Write
		01 Embeded Data Write -------> Not Implemented Yet
		02 Embeded Routine No Write -> Not Implemented Yet
	A: Address on stack
*/
fnc Engine_1_4_Main()\s0,s1
{
	setreg s0, $80078250 // Chunk Table
	setreg s1, $80060000 // Code Chunks
	lw a0, $0000(s0)
	while (a0 <> 0)
	{
		lb a0, $0003(s0)
		if (a0 == $00) //---------------- Normal Code Chunk
		{
			lhu a0, $0000(s0)
			sll a0, a0, 2
			
			addu a0, s1, a0
			call ProcessCodeChunk(a0)
		}
		else if (a0 == $01) //----------- Embedded Routine
		{
			// Not impelemented yet
		}
		else if (a0 == $02) //----------- Embedded Routine
		{
			// Not impelemented yet
		}
		
		
		s0 += 4
		lw a0, $0000(s0)
	}
}


//============================================================
/*
	ProcessCodeChunk(Int32 ChunkAddr);
	Input:
		a0 = Code chunk memory address
	Output:
		n/a
*/

fnc ProcessCodeChunk(EE a0)\s0,s1,s2,s3
{
	s0 = a0
	
	lw s1, $0000(s0)
	while (s1 <> 0)
	{
		//------------------- Extract operation
		srl s2, s1, 24
		andi s2, s2, $00fc
		
		//------------------- Extract memory address -> s3
		lui v0, $01ff
		ori v0, v0, $ffff
		and s3, s1, v0
		
		switch (s2)
		{
			case $00 //--------------------------------- 8 Bit Write
			{
				lb v0, $0004(s0)
				sb v0, $0000(s3)
				s0 += 8
			}
			case $10 //--------------------------------- 16 Bit Write
			{
				lh v0, $0004(s0)
				sh v0, $0000(s3)
				s0 += 8
			}
			case $20 //--------------------------------- 32 Bit Write
			{
				lw v0, $0004(s0)
				sw v0, $0000(s3)
				s0 += 8
			}
			case $30 //--------------------------------- Increment / Decrement
			{
				/*
					-- Increment / Decrement --
					
					8 Bit Increment
					300000nn aaaaaaaa
					
					8 Bit Decrement
					301000nn aaaaaaaa
					
					16 Bit Increment
					3020nnnn aaaaaaaa
					
					16 Bit Decrement
					3030nnnn aaaaaaaa
					
					32 Bit Increment
					30400000 aaaaaaaa
					nnnnnnnn 00000000
					
					32 Bit Decrement
					30500000 aaaaaaaa
					nnnnnnnn 00000000
					
					a = Address
					n = Amount To Increase / Decrease
				*/
				lb a0, $0002(s0)
				if (a0 == $00) //-------------- 8 Bit Increment
				{
					lbu v0, $0000(s0)
					lbu v1, $0000(s3)
					addu v0, v0, v1
					sb v0, $0000(s3)
					s0 += 8
				}
				else if (a0 == $10) //--------- 8 Bit Decrement
				{
					lbu v0, $0000(s0)
					lbu v1, $0000(s3)
					subu v0, v0, v1
					sb v0, $0000(s3)
					s0 += 8
				}
				else if (a0 == $20) //--------- 16 Bit Increment
				{
					lhu v0, $0000(s0)
					lhu v1, $0000(s3)
					addu v0, v0, v1
					sh v0, $0000(s3)
					s0 += 8
				}
				else if (a0 == $30) //--------- 16 Bit Decrement
				{
					lhu v0, $0000(s0)
					lhu v1, $0000(s3)
					subu v0, v0, v1
					sh v0, $0000(s3)
					s0 += 8
				}
				else if (a0 == $40) //--------- 32 Bit Increment
				{
					lwu v0, $0008(s0)
					lwu v1, $0000(s3)
					addu v0, v0, v1
					sw v0, $0000(s3)
					s0 += 16
				}
				else if (a0 == $50) //--------- 32 Bit Decrement
				{
					lwu v0, $0008(s0)
					lwu v1, $0000(s3)
					subu v0, v0, v1
					sw v0, $0000(s3)
					s0 += 16
				}
			}
			case $40 //--------------------------------- 32-bit Multiple Address Write
			{
				/*
					-- 32 Bit Multiple Address Write --
					
					4aaaaaaa wwwwssss
					dddddddd nnnnnnnn
					
					a = Starting Address
					w = Number Of Lines To Write
					s = Number Of Lines To Skip
					d = Data To Write
					n = Ammount To Increase Given Data By
				*/
				lh a0, $0004(s0)
				lh a1, $0006(s0)
				sll a0, a0, 2
				
				lw v0, $0008(s0)
				lw v1, $000c(s0)
				
				while (a1 > 0)
				{
					sw v0, $0000(s3)
					
					v0 += v1
					s3 += 4
					s3 += a0
					a1--
				}
				
				s0 += 16
			}
			case $50 //--------------------------------- Copy Bytes
			{
				/*
					-- Copy Bytes --
					
					5fffffff nnnnnnnn
					tttttttt 00000000
					
					f = Address To Copy From
					t = Address To Copy To
					n = Number Of Bytes To Copy
				*/
				lw a0, $0004(s0)
				lw a1, $0008(s0)
				
				
				while (a0 > 0)
				{
					lb v0, $0000(s3)
					s3++
					sb v0, $0000(a1)
					a1++
					
					a0--
				}
				
				s0 += 16
			}
			case $60 //--------------------------------- Pointer Write
			{
				
			}
			case $70 //--------------------------------- 32 Bit Boolean
			{
				/*
					-- 32 Bit Boolean --
					
					7aaaaaaa dddddddd
					
					a = Address to read
					d = Data to compare
				*/
				
				lw v0, $0004(s0)
				lw v1, $0000(s3)
				if (v0 <> v1)
					return 0
					
				s0 += 8
			}
			case $80 //--------------------------------- Find and Replace
			{
				/*
					-- Find and Replace --
					
					8 Bit Search
					8aaaaaaa 000000ff
					000000rr 0000iiii
					
					16 Bit Search
					8aaaaaaa 0000ffff
					0000rrrr 0010iiii
					
					32 Bit Search
					8aaaaaaa ffffffff
					rrrrrrrr 0020iiii
					
					a = Address to start
					f = Data to find
					r = Data to replace with
					i = Search count
				*/
				lb a0, $000e(s0)
				lh a1, $000c(s0)
				if (a0 == $00) //-------------- 8 Bit
				{
					lb v0, $0004(s0)
					lb v1, $0008(s0)
					for (a2 = 0; a2 < a1; a2 += 1)
					{
						lb a3, $0000(s3)
						if (a3 == v0)
						{
							sb v1, $0000(s3)
						}
						s3++
					}
				}
				else if (a0 == $10) //--------- 16 Bit
				{
					lh v0, $0004(s0)
					lh v1, $0008(s0)
					for (a2 = 0; a2 < a1; a2 += 1)
					{
						lh a3, $0000(s3)
						if (a3 == v0)
						{
							sh v1, $0000(s3)
						}
						s3 += 2
					}
				}
				else if (a0 == $20) //--------- 32 Bit
				{
					lw v0, $0004(s0)
					lw v1, $0008(s0)
					for (a2 = 0; a2 < a1; a2 += 1)
					{
						lw a3, $0000(s3)
						if (a3 == v0)
						{
							sw v1, $0000(s3)
						}
						s3 += 4
					}
				}
				
				
				s0 += 16
			}
			case $90 //--------------------------------- Execute Data
			{
				/*
					-- Execute Data --
					
					9fffffff aaaaaaaa
					
					f = Address of function (also called a sub routine)
					a = Argument sent to function (sent in register a0)
				*/
				
				jalr s3
				lw a0, $0004(s0)
				
				s0 += 8
			}
			case $a0 //--------------------------------- (not implemented)
			{
				
			}
			case $b0 //--------------------------------- (not implemented)
			{
				
			}
			case $c0 //--------------------------------- (not implemented)
			{
				
			}
			case $d0 //--------------------------------- Boolean: Half (Aka: Joker)
			{
				/*
					-- 16 Bit Boolean --
					
					Daaaaaaa 00f0dddd
					
					a = Address to read
					d = Data to compare
					f = Flag
							0: Equal to
							1: Not equal to
				*/
				
				lh v0, $0004(s0)
				lh v1, $0000(s3)
				lb a0, $0006(s0)
				if (a0 == $00) //---------- Compare if equal
				{
					if (v0 <> v1)
						return 0
				}
				else if (a0 == $10) // ---- Compare if NOT equal
				{
					if (v0 == v1)
						return 0
				}
				
				s0 += 8
			}
			case $e0 //--------------------------------- Line Specific Boolean: Half (Aka: "Multi-Line" Joker)
			{
				/*
					-- Line Specific Boolean Half --
					
					Eaaaaaaa nnnfdddd
					
					a = Address to read
					n = Number of lines assigned to boolean
					f = Flag
							0: Equal to
							1: Not equal to
					d = Data to check
				*/
				
				lh v0, $0004(s0)
				lb a0, $0006(s0)
				andi a0, a0, $000f
				lhu a1, $0006(s0)
				srl a1, a1, 4
				sll a1, a1, 3
				
				if (a0 == $00) //---------- Compare if equal
				{
					if (v0 <> v1)
					{
						s0 += a1
					}
				}
				else if (a0 == $10) // ---- Compare if NOT equal
				{
					if (v0 == v1)
					{
						s0 += a1
					}
				}
				
				s0 += 8
			}
			case $f0 //--------------------------------- Extended Operations
			{
				srl s2, s1, 24
				if (s2 == $f0) //--------------------------------- IOP 8 Bit Constant Write
				{
					lui v0, $bc00
					daddu a0, s3, v0
					
					lb v0, $0004(s0)
					sb v0, $0000(a0)
					s0 += 8
				}
				else if (s2 == $f1) //---------------------------- IOP 16 Bit Constant Write
				{
					lui v0, $bc00
					daddu a0, s3, v0
					
					lh v0, $0004(s0)
					sh v0, $0000(a0)
					s0 += 8
				}
				else if (s2 == $f2) //---------------------------- IOP 32 Bit Constant Write
				{
					lui v0, $bc00
					daddu a0, s3, v0
					
					lw v0, $0004(s0)
					sw v0, $0000(a0)
					s0 += 8
				}
				// F3 - FF not implemented yet
			}
		}
		
		
		lw s1, $0000(s0)
	}
}

