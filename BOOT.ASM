	title	'Boot loader module for CP/M 3.0'

true equ -1
false equ not true

banked	equ true
debug 	equ false

	public	?init,?ldccp,?rlccp,?time
	extrn	?pmsg,?conin
	extrn	@civec,@covec,@aivec,@aovec,@lovec
	extrn 	@cbnk,?bnksl,dumpivars,spamon,spamoff
	extrn 	iputs,dumpregs,ictc3,ictc0,usrfunc

	maclib ports
	maclib z80

bdos	equ 5

	if banked
tpa$bank	equ 1
ccp$bank	equ 0Eh
	else
tpa$bank	equ 0
ccp$bank	equ 0Eh
	endif

	dseg	; init done from banked memory

?init:
if debug
	call iputs
	db '?INIT ',0
endif
	lxi h,08000h ! shld @civec 
	lxi h,09000h ! shld @covec	; assign console to CRT:
	lxi h,02000h ! shld @lovec 					; assign printer to LPT:
	lxi h,04000h ! shld @aivec ! shld @aovec	; assign AUX to CRT1:
	;call ictc0
	call ictc3	
	lxi h,signon$msg ! call ?pmsg		; print signon message
	ret	

;out$blocks:
	;mov a,m ! ora a ! rz ! mov b,a
	;inx h ! mov c,m ! inx h
	;outir
	;jmp out$blocks


	cseg	; boot loading most be done from resident memory
	
    ;	This version of the boot loader loads the CCP from a file
    ;	called CCP.COM on the system drive (A:).


?ldccp:
if debug
	call iputs
	db '?LDCCP ',0
endif	
	; First time, load the A:CCP.COM file into TPA
	xra a ! sta ccp$fcb+15		; zero extent
	lxi h,0 ! shld fcb$nr		; start at beginning of file
	lxi d,ccp$fcb ! call open	; open file containing CCP
	inr a ! jz no$CCP			; error if no file...
	lxi d,0100h ! call setdma	; start of TPA
	lxi d,128 ! call setmulti	; allow up to 16k bytes
	mvi a,tpa$bank ! call ?bnksl	; select TPA
	lxi d,ccp$fcb ! call read	; load the thing
					; now, copy CCP to bank 0 for reloading
	lda @cbnk ! push psw		; save current bank
	
	lxi b,0003h					; B=0=sys,C=3=save CCP
	call usrfunc
	
	lxi b,0001h					; B=0=sys,C=1=Init
	lxi d,08000h				; window base=common memory
	call usrfunc

	lxi b,0103h					; B=1=vdp,C=3=read font
	lxi h,08000h				; save to common memory
	call usrfunc

	lxi b,0000h					; B=0=sys,C=0=RW block
	lxi d,01E2h					; D=1=write, block number E2 (E0+E1 used for CCP)
	call usrfunc

	lxi b,0104h					; B=1=vdp,C=4=convert font
	lxi h,08000h				; save to common memory
	call usrfunc

	lxi b,0000h					; B=0=sys,C=0=RW block
	lxi d,01E3h					; D=1=write, block number E3 (E0+E1 used for CCP, E2=ANSI font)
	call usrfunc

	pop psw ! call ?bnksl		; restore original bank
	ret

no$CCP:			; here if we couldn't find the file
	lxi h,ccp$msg ! call ?pmsg	; report this...
	call ?conin			; get a response
	jmp ?ldccp			; and try again

?rlccp:
	lxi b,0004h					; B=0=sys,C=4=load CCP
	call usrfunc

	lxi b,0001h					; B=0=sys,C=1=Init
	lxi d,08000h				; window base=common memory
	call usrfunc

	lxi b,0101h					; B=1=vdp,C=1=Init
	call usrfunc
	
	lxi b,0000h					; B=0=sys,C=0=RW block
	lxi d,00E3h					; D=0=read, block number E3 (font with inverted upper chars)
	call usrfunc

	lxi b,0102h					; B=1=vdp,C=2=load font
	lxi h,08000h				; read from common memory
	call usrfunc

	ret

    ; No external clock.
?time:
	ret

	; CP/M BDOS Function Interfaces

open:
	mvi c,15 ! jmp bdos		; open file control block

setdma:
	mvi c,26 ! jmp bdos		; set data transfer address

setmulti:
	mvi c,44 ! jmp bdos		; set record count

read:
	mvi c,20 ! jmp bdos		; read records

opnmsg		db 	13,10,'BDOS OPEN: ',0

signon$msg	db	13,10,'CP/M Plus for Z80 Retro!',13,10,10,0
			
ccp$msg		db	13,10,'BIOS Err on A: No CCP.COM file',0

ccp$fcb		db	1,'CCP     ','COM',0,0,0,0
		ds	16
fcb$nr		db	0,0,0

; init$table	db 0
		; db 	0
		; db	3,p$zpio$3a,0CFh,0FFh,07h	; set up config port
		; db	3,p$zpio$3b,0CFh,000h,07h	; set up bank port
		; db	1,p$bank$select,0	; select bank 0
		; db	0			; end of init$table

	end

;ldccp$msg: 	db 'LDCCP ',0
;fccp$msg: 	db 'CCP ',0
;opn$msg: 	db 'OPEN: ',0
;crlf: 		db 0Dh,0Ah,0

	; push b ! push d ! push h
	; lxi h,opn$msg ! call ?pmsg		
	; lxi h,ccp$fcb+1 ! call ?pmsg	
	; lxi h,crlf ! call ?pmsg			
	; pop h ! pop d ! pop b
