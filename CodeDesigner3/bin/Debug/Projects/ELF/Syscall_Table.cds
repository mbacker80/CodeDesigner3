/*
	CodeDesigner 3 ~ Created by: Gtlcpimp
*/

_SYS_ResetEE:
addiu v1, zero, $0001
syscall (00000)
jr ra
nop


_SYS_SetGsCrt:
addiu v1, zero, $0002
syscall (00000)
jr ra
nop

_SYS_LoadExecPS2:
addiu v1, zero, $0006
syscall (00000)
jr ra
nop

_SYS_ExecPS2:
addiu v1, zero, $0007
syscall (00000)
jr ra
nop


_SYS_AddDmacHandler:
addiu v1, zero, $0012
syscall (00000)
jr ra
nop


_SYS_RemoveDmacHandler:
addiu v1, zero, $0013
syscall (00000)
jr ra
nop


_SYS_EndOfHeap:
addiu v1, zero, $003e
syscall (00000)
jr ra
nop


_SYS_CreateSema:
addiu v1, zero, $0040
syscall (00000)
jr ra
nop


_SYS_DeleteSema:
addiu v1, zero, $0041
syscall (00000)
jr ra
nop


_SYS_SignalSema:
addiu v1, zero, $0042
syscall (00000)
jr ra
nop


_SYS_WaitSema:
addiu v1, zero, $0044
syscall (00000)
jr ra
nop


_SYS_FlushCache:
addiu v1, zero, $0064
syscall (00000)
jr ra
nop


_SYS_sceSifDmaStat:
addiu v1, zero, $0076
syscall (00000)
jr ra
nop


_SYS_sceSifSetDma:
addiu v1, zero, $0077
syscall (00000)
jr ra
nop


_SYS_sceSifSetDchain:
addiu v1, zero, $0078
syscall (00000)
jr ra
nop


