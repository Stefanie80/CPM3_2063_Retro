	title 'bank & move module for CP/M3 linked BIOS'

	cseg

	public ?move,?xmove,?bank
	extrn @cbnk,setbank,dumpregs,iputs,@BNKBF

	;maclib z80
	;maclib ports

debug	equ 0

?xmove:		; ALTOS can't perform interbank moves
	ret

?move:
if debug
	push bc
	push de
	push hl
	call iputs
	db 'MOVE: ',0
	call dumpregs
	pop hl
	pop de
	pop bc
endif
	ex de,hl		; we are passed source in DE and dest in HL
	ldir		; use Z80 block move instruction
	ex de,hl		; need next addresses in same regs
	ret


?bank:
	push bc				; save register b for temp
	call setbank
	pop bc				; restore register b
	ret

				;	128 bytes at a time

	end
