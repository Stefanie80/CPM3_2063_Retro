;##########################################################################
; CP/M 3 BIOS for 2063 z80 retro board
;
; Sources originally written by John Winans with contributions from Trevor Jacobs
; Refactored for CP/M 3 by SolderGirl
;
;##########################################################################

.debug:			equ	0
.debug_vdp:		equ 0
.debugger:		equ 0	; have some options disabled for running inside DDTZ


EXTRN @MXTPA

	cr			equ	0dh	; carriage return
	lf			equ	0ah	; line feed

.low_bank		equ 00h    ; The RAM BANK to use for the bottom 32K

;the ROM code will load us at C000h, so lets set our stack below ourselves
;the real BIOS will fix it later
;			org 0C000h

; This is the jump table for CPMLDR. It needs to have all the jump points to 
; maintain the structure as specified in the CP/M 3 System Guide
cseg

BOOT:   JP      .bios_boot      ;0 JMP BOOT Perform cold start initialization
WBOOT:  JP      .bios_wboot     ;1 JMP WBOOT Perform warm start initialization
CONST:  JP      .dummy     		;2 JMP CONST Check for console input character ready
CONIN:  JP      .dummy     		;3 JMP CONIN Read Console Character in
CONOUT: JP      .bios_conout    ;4 JMP CONOUT Write Console Character out
LIST:   JP      .dummy      	;5 JMP LIST Write List Character out
PUNCH:  JP      .dummy     		;6 JMP AUXOUT Write Auxiliary Output Character
READER: JP      .dummy    		;7 JMP AUXIN Read Auxiliary Input Character
HOME:   JP      .bios_home      ;8 JMP HOME Move to Track 00 on Selected Disk
SELDSK: JP      .bios_seldsk    ;9 JMP SELDSK Select Disk Drive
SETTRK: JP      .bios_settrk    ;10 JMP SETTRK Set Track Number
SETSEC: JP      .bios_setsec    ;11 JMP SETSEC Set Sector Number
SETDMA: JP      .bios_setdma    ;12 JMP SETDMA Set DMA Address
READ:   JP      nc_read       ;13 JMP READ Read Specified Sector
WRITE:  JP      .dummy      	;14 JMP WRITE Write Specified Sector
PRSTAT: JP      .dummy    		;15 JMP LISTST Return List Status
SECTRN: JP      .bios_sectrn    ;16 JMP SECTRN Translate Logical to Physical Sector
CONOST: JP		.dummy			;17 JMP CONOST Return Output Status of Console
AUXIST:	JP		.dummy			;18 JMP AUXIST Return Input Status of Aux. Port
AUXOST:	JP		.dummy			;19 JMP AUXOST Return Output Status of Aux. Port
DEVTBL:	JP		.dummy			;20 JMP DEVTBL Return Address of Char. I/O Table
DEVINI:	JP		.dummy			;21 JMP DEVINI Initialize Char. I/O Devices
DRVTBL:	JP		.dummy			;22 JMP DRVTBL Return Address of Disk Drive Table
MULTIO:	JP		.dummy			;23 JMP MULTIO Set Number of Logically Consecutive sectors to be read or written
FLUSH:	JP		.dummy			;24 JMP FLUSH Force Physical Buffer Flushing for user-supported deblocking
MOVE:	JP		.dummy			;25 JMP MOVE Memory to Memory Move
TIME:	JP		.dummy			;26 JMP TIME Time Set/Get signal
SELMEM:	JP		.dummy			;27 JMP SELMEM Select Bank of memory
SETBNK:	JP		.dummy			;28 JMP SETBNK Specify Bank for DMA Operation
XMOVE:	JP		.dummy			;29 JMP XMOVE Set Bank When a Buffer is in a Bank other than 0 or 1
USERF:	JP		.dummy			
RES1:	JP		.dummy
RES2:	JP		.dummy


gpio_in				equ 00h		; GP input port
gpio_out			equ	10h		; GP output port
sio_ad				equ	30h		; SIO port A, data
sio_ac				equ	32h		; SIO port A, control
ctc_1				equ	41h		; CTC port 1
gpio_out_sd_mosi	equ	01h
gpio_out_sd_clk		equ	02h
gpio_out_sd_ssel	equ	04h
gpio_in_sd_det		equ	40h
gpio_in_sd_miso		equ	80h

;
;##########################################################################
; Libraries
;##########################################################################
include sdcard.asm
include spi.asm
include nocache.asm


;##########################################################################
;
; CP/M 3 System Manual Page 37:
; BIOS Function 0: BOOT
; Get Control from Cold Start Loader and Initialize System
; Entry Parameters: None
; Returned Values: None
;
; The BOOT entry point gets control from the Cold Start Loader in Bank 0
; and is responsible for basic system initialization. Any remaining hardware
; initialization that is not done by the boot ROMs, the Cold Boot Loader,
; or the LDRBIOS should be performed by the BOOT routine.
; The BOOT routine must perform the system initialization outlined in
; Section 2.3, "System Initialization." This includes initializing
; Page Zero jumps and loading the CCP.
; BOOT usually prints a sign-on message, but this can be omitted.
; Control is then transferred to the CCP in the TPA at 0100H.
;
; To initialize Page Zero, the BOOT routine must place a jump at
; location 0000H to BIOS base + 3, the BIOS warm start entry point.
; The BOOT routine must also place a jump instruction at location 0005H to
; the address contained in the System Control Block variable, @MXTPA.
; The BOOT routine must establish its own stack area if it calls any BDOS
; or BIOS routines. In a banked system, the stack is in Bank 0 when the
; Cold BOOT routine is entered.
; The stack must be placed in common mem
;
; NOTE: Since we are in the bootloader, we don't need to do all of this.
; Instead of loading the CCP ourself, we just RET to the CPMLDR code supplied by DRI
; We can count on the fact that our own full initialisation code will do the rest later.
; Also, since we are zeroing out the zero page here, we don't need that in the full BIOS
;##########################################################################
.bios_boot:
	; This will select low-bank 0, idle the SD card, and idle the printer
	ld	a,0fh + (.low_bank<<4)
	ld	(gpio_out_cache),a

;if .debugger = 0		; do not access the bank select bits from within DDTZ

	out	(gpio_out),a
;endif

	; make sure we have a viable stack
	;ld	sp,bios_stack		; use the private BIOS stack to get started

	call	.init_console		; Note: console should still be initialized from the ROM boot loader
	call 	vdp_init			; Init VDP

if .debug > 0
	call	iputs
	db	cr,lf,'LDRBIOS: BOOT',cr, lf, 0
	db	cr,lf,'         Debug level 0x'0
	ld	a,.debug		; A = the current debug level
	call	hexdump_a		; print the current level number
	call	puts_crlf		; and a newline
endif

	; Display a hello world message.
	ld	hl,.boot_msg
	call	puts

if .debugger = 0	; lets not clobber the zero page while inside DDTZ
	; Initialize the zero page
	ld	hl,0
	ld	de,1
	ld	bc,0ffh
	ld	(hl),0
	ldir

	ld	a,0c3h		; opcode for JP
	ld	(0),a
	ld	hl,WBOOT
	ld	(1),hl		; address 0 now = JP WBOOT
	ld	(5),a		; opcode for JP
	ld	hl,@MXTPA	; this will probably be garbage at this point
					; but tested working 
	ld	(6),hl		; address 6 now = JP FBASE
endif
	; Either ensure the stack is in high RAM or disable IRQs to call rw_init!
	; Assume no IRQs. Technically, we don't need write functionality.
	; Possible space savings
	call	nc_init	; initialize anything needed for disk read/write

if .debug > 0
	call	iputs
	db	cr,lf,'LDRBIOS: Done.',cr,lf,0
	call	iputs
	db	cr,lf,'RET to CPMLDR',cr,lf,0
endif

if .debug >= 3
	; dump the zero-page for reference
	ld	hl,0		; start address
	ld	bc,100h	; number of bytes
	ld	e,1		; fancy format
	call	hexdump
endif
	ret

;##########################################################################
; BIOS Function 1: WBOOT
; Get Control When a Warm Start Occurs
; Entry Parameters: None
; Returned Values: None
;##########################################################################
; We are inside the bootloader, all of that is done elswhere
.bios_wboot:

if .debug > 0
	call	iputs
	db	cr,lf,'LDRBIOS: WBOOT',cr,lf,0
endif

if .debug >= 3
	; dump the zero-page for reference
	ld	hl,0		; start address
	ld	bc,100h	; number of bytes
	ld	e,1		; fancy format
	call	hexdump
endif
	ret

;##########################################################################
;CP/M 3 System Manual Page 40
;BIOS Function 4: CONOUT
;Output Character to Console
;Entry Parameters:
;C=Console Character
;Returned Values: None
;Send the character in register C to the console output device. The character is in ASCII with no parity.
;##########################################################################
.bios_conout:
con_tx_char:
sioa_tx_char:
	call	sioa_tx_ready
	jr	z,sioa_tx_char
	ld	a,c
	out	(sio_ad),a	; send the character
	ret

sioa_tx_ready:
	in	a,(sio_ac)	; read sio control status byte
	and	4		; check the xmtr empty bit
	ret			; a = 0 = not ready
	
;##########################################################################
;CP/M 3 System Manual Page 42
;BIOS Function 8: HOME
;Select Track 00 of the Specified Drive
;Entry Parameters: None
;Returned Values: None
;Return the disk head of the currently selected disk to the track 00 position. Usually, you can translate the
;HOME call into a call on SETTRK with a parameter of 0.
;##########################################################################
.bios_home:
if .debug >= 2
	call	iputs
	db	'.bios_home entered',cr,lf,0
endif

	ld	bc,0		; BC = 0 = track number passed into .bios_settrk
    call .bios_settrk
    ret

	; Fall into .bios_settrk <--------------- NOTICE!!
    ; changed to explicit call

;##########################################################################
;
; CP/M 2.2 Alteration Guide p19:
; Register BC contains the track number for subsequent disk
; accesses on the currently selected drive.  BC can take on
; values from 0-65535.
;
;##########################################################################
.bios_settrk:
	ld	(bios_disk_track),bc

if .debug >= 2
	call	iputs
	db	'.bios_settrk entered: ',0
	call	bios_debug_disk
endif
	ret

;##########################################################################
;CP/M 3 System Manual Page 42/43
;BIOS Function 9: SELDSK
;Select the Specified Disk Drive
;Entry Parameters:
;C=Disk Drive (0-15)
;E=Initial Select Flag
;Returned Values:
;HL=Address of Disk Parameter
;Header (DPH) if drive exists
;HL=0000H if drive does not exist
;##########################################################################
.bios_seldsk:
	ld	a,c
	ld	(bios_disk_disk),a
; XXX should we save this now or defer till after we validate the value?
if .debug >= 2
	call	iputs
	db	'.bios_seldsk entered: ',0
	call	bios_debug_disk
endif

	ld	hl,0			; HL = 0 = invalid disk
	ld	a,(bios_disk_disk)
	or	a			; did drive A get selected?
	ret	nz			; no -> error

	ld	hl,.bios_dph		; the DPH for disk A
	ret

;##########################################################################
;CP/M 3 System Manual Page 43
;BIOS Function 11: SETSEC
;Set Specified Sector Number
;Entry Parameters:
;BC=Sector Number
;Returned Values: None
;Register BC contains the sector number for the subsequent disk access on
;the currently selected drive. This number is the value returned by SECTRN.
;Usually, you delay actual sector selection until a READ or WRITE
;operation occurs.
;##########################################################################
.bios_setsec:
	ld	(bios_disk_sector),bc

if .debug >= 2
	call	iputs
	db	'.bios_setsec entered: ',0
	call	bios_debug_disk
endif

	ret

;##########################################################################
;CP/M 3 System Manual Page 44
;BIOS Function 12: SETDMA
;Set Address for Subsequent Disk I/O
;Entry Parameters:
;BC=Direct Memory Access Address
;Returned Values: None
;Register BC contains the DMA (Direct Memory Access) address for the
;subsequent READ or WRITE operation.
;For example, if B = 00H and C = 80H when the BDOS calls SETDMA,
;then the subsequent read operation reads its data starting at 80H, or
;the subsequent write operation gets its data from 80H, until the next
;call to SETDMA occurs.
;##########################################################################
.bios_setdma:
	ld	(bios_disk_buf),bc

if .debug >= 2
	call	iputs
	db	'.bios_setdma entered: ',0
	call	bios_debug_disk
endif

	ret

;##########################################################################
;CP/M 3 System Manual Page 45
;BIOS Function 16: SECTRN
;Translate Sector Number Given Translate Table
;Entry Parameters:
;BC=Logical Sector Number
;DE=Translate Table Address
;Returned Values:
;HL=Physical Sector Number
;##########################################################################
.bios_sectrn:
	; 1:1 translation  (no skew factor)
	ld	h,b
	ld	l,c
	ret

.boot_msg:
	defb	cr,lf
	;defb	'................................'
	defb	':        Z80 Retro BIOS        :',cr,lf
	defb	':     (C) 2021 John Winans     :',cr,lf
	defb	':..............................:',cr,lf
	defb	cr,lf,0

;##########################################################################
; Initialize the console port.  Note that this includes CTC port 1.
;##########################################################################
.init_console:
	; Init CTC first
	;ld	c,6			; C = 6 = 19200 bps
	
	ld	c,12			; C = 12 = 9600 bpst
    ld      a,047h      ; TC follows, Counter, Control, Reset
    out     (ctc_1),a
    ld      a,c
    out     (ctc_1),a

	;call	init_ctc_1		; start CTC1 in case J11-A selects it!
	; just initialize SIO A.
	
	ld	c,sio_ac	; port to write into (port A control)
	ld	hl,.sio_init_wr	; point to init string
	ld	b,.sio_init_len_wr ; number of bytes to send
	otir			; write B bytes from (HL) into port in the C reg
	ret

; this should never get called.
.dummy:
	call iputs
	db 'Not implemented!',cr,lf,0
	ld a,0ffh
	ret

;##############################################################
; Write the null-terminated string starting after the call
; instruction invoking this subroutine to the console.
; Clobbers AF, C
;##############################################################
iputs:
        ex      (sp),hl                 ; hl = @ of string to print
	call	.puts_loop
        inc     hl                      ; point past the end of the string
        ex      (sp),hl
        ret

;##############################################################
; Write the null-terminated staring starting at the address in
; HL to the console.
; Clobbers: AF, C
;##############################################################
puts:
	push	hl
	call	.puts_loop
	pop	hl
	ret

.puts_loop:
        ld      a,(hl)                  ; get the next byte to send
        or      a
        jr      z,.puts_done             ; if A is zero, return
        ld      c,a
        call    .bios_conout
        inc     hl                      ; point to next byte to write
        jp      .puts_loop
.puts_done:
        ret

		
;##########################################################################
; General storage space
;##########################################################################

dseg

gpio_out_cache:		db	0fh + (.low_bank<<4)
bios_disk_track: 	dw 0
bios_disk_sector: 	dw 0
bios_disk_buf:		dw 0
bios_disk_disk: 	db 0
;bios_stk:			ds 128
;.bios_wboot_stack:
	
;##############################################################
; Initialization string for the Z80 SIO
;##############################################################
.sio_init_wr:
	db	00011000b	; wr0 = reset everything
	db	00000100b	; wr0 = select reg 4
	db	01000100b	; wr4 = /16 N1 (115200 from 1.8432 MHZ clk)
	db	00000011b	; wr0 = select reg 3
	db	11000001b	; wr3 = RX enable, 8 bits/char
	db	00000101b	; wr0 = select reg 5
	db	01101000b	; wr5 = DTR=0, TX enable, 8 bits/char
.sio_init_len_wr:   equ $-.sio_init_wr

;##########################################################################
; Disk stuff below here
;##########################################################################

; NOTE: since we are in the bootloader, we do not need a full XDPH
; The DPH we provide however must meet the specifications in the CP/M 3 System Guide

.bios_dph:
	dw	0			; XLT sector translation table (no xlation done)
	dw	0,0,0,0		; scratchpad
	db  0			; scratch
	db 	0			; media flag
	dw	.bios_dpb_a	; DPB pointer
	dw	0			; CSV pointer (optional, not implemented)
	dw	.bios_alv_a	; ALV pointer
	dw	.bios_dirbuf	; DIRBUF pointer
	dw	.bios_datbuf	; dtabcb
	dw	0FFFFh		; HASH disable hashing for now
	dw	0			; HBANK
	
.bios_dirbuf:
	ds	128		; scratch directory buffer
.bios_datbuf:
	ds	128		; scratch directory buffer

.bios_dpb_a:
	dw	4		; SPT
	db	6		; BSH
	db	63		; BLM
	db	3		; EXM
	dw	1021	; DSM (max allocation block number)
	dw	511		; DRM
	db	0c0h	; AL0
	db	0		; AL1
	dw	8000h 	; CKS
	dw	32		; OFF
	db	0		; PSH
	db	0		; PHM


.bios_alv_a:
	ds	(4087/8)+1,0aah	; scratchpad used by BDOS for disk allocation info
.bios_alv_a_end:

;##############################################################
; Print a CRLF
; Clobbers AF, C
;##############################################################
puts_crlf:
        call    iputs
        db    cr,lf,0 	;'wtf \r\n\0'
        ret

.font_6x8:
db 000h,000h,000h,000h,000h,000h,000h,000h ; 000
db 038h,044h,06Ch,044h,054h,044h,038h,000h ; 001
db 038h,07Ch,054h,07Ch,044h,07Ch,038h,000h ; 002
db 000h,028h,07Ch,07Ch,07Ch,038h,010h,000h ; 003
db 000h,010h,038h,07Ch,07Ch,038h,010h,000h ; 004
db 010h,038h,038h,010h,07Ch,07Ch,010h,000h ; 005
db 000h,010h,038h,07Ch,07Ch,010h,038h,000h ; 006
db 000h,000h,000h,000h,000h,000h,000h,000h ; 007
db 000h,000h,000h,000h,000h,000h,000h,000h ; 008
db 000h,000h,000h,000h,000h,000h,000h,000h ; 009
db 000h,000h,000h,000h,000h,000h,000h,000h ; 00A
db 000h,01Ch,00Ch,034h,048h,048h,030h,000h ; 00B
db 038h,044h,044h,038h,010h,038h,010h,000h ; 00C
db 000h,000h,000h,000h,000h,000h,000h,000h ; 00D
db 00Ch,034h,02Ch,034h,02Ch,06Ch,060h,000h ; 00E
db 000h,054h,038h,06Ch,038h,054h,000h,000h ; 00F
db 020h,030h,038h,03Ch,038h,030h,020h,000h ; 010
db 008h,018h,038h,078h,038h,018h,008h,000h ; 011
db 010h,038h,07Ch,010h,07Ch,038h,010h,000h ; 012
db 028h,028h,028h,028h,028h,000h,028h,000h ; 013
db 03Ch,054h,054h,034h,014h,014h,014h,000h ; 014
db 038h,044h,030h,028h,018h,044h,038h,000h ; 015
db 000h,000h,000h,000h,000h,078h,078h,000h ; 016
db 010h,038h,07Ch,010h,07Ch,038h,010h,038h ; 017
db 010h,038h,07Ch,010h,010h,010h,010h,000h ; 018
db 010h,010h,010h,010h,07Ch,038h,010h,000h ; 019
db 000h,010h,018h,07Ch,018h,010h,000h,000h ; 01A
db 000h,010h,030h,07Ch,030h,010h,000h,000h ; 01B
db 000h,000h,000h,040h,040h,040h,07Ch,000h ; 01C
db 000h,028h,028h,07Ch,028h,028h,000h,000h ; 01D
db 010h,010h,038h,038h,07Ch,07Ch,000h,000h ; 01E
db 07Ch,07Ch,038h,038h,010h,010h,000h,000h ; 01F
db 000h,000h,000h,000h,000h,000h,000h,000h ; 020
db 010h,038h,038h,010h,010h,000h,010h,000h ; 021
db 06Ch,06Ch,048h,000h,000h,000h,000h,000h ; 022
db 000h,028h,07Ch,028h,028h,07Ch,028h,000h ; 023
db 020h,038h,040h,030h,008h,070h,010h,000h ; 024
db 064h,064h,008h,010h,020h,04Ch,04Ch,000h ; 025
db 020h,050h,050h,020h,054h,048h,034h,000h ; 026
db 030h,030h,020h,000h,000h,000h,000h,000h ; 027
db 010h,020h,020h,020h,020h,020h,010h,000h ; 028
db 020h,010h,010h,010h,010h,010h,020h,000h ; 029
db 000h,028h,038h,07Ch,038h,028h,000h,000h ; 02A
db 000h,010h,010h,07Ch,010h,010h,000h,000h ; 02B
db 000h,000h,000h,000h,000h,030h,030h,020h ; 02C
db 000h,000h,000h,07Ch,000h,000h,000h,000h ; 02D
db 000h,000h,000h,000h,000h,030h,030h,000h ; 02E
db 000h,004h,008h,010h,020h,040h,000h,000h ; 02F
db 038h,044h,04Ch,054h,064h,044h,038h,000h ; 030
db 010h,030h,010h,010h,010h,010h,038h,000h ; 031
db 038h,044h,004h,018h,020h,040h,07Ch,000h ; 032
db 038h,044h,004h,038h,004h,044h,038h,000h ; 033
db 008h,018h,028h,048h,07Ch,008h,008h,000h ; 034
db 07Ch,040h,040h,078h,004h,044h,038h,000h ; 035
db 018h,020h,040h,078h,044h,044h,038h,000h ; 036
db 07Ch,004h,008h,010h,020h,020h,020h,000h ; 037
db 038h,044h,044h,038h,044h,044h,038h,000h ; 038
db 038h,044h,044h,03Ch,004h,008h,030h,000h ; 039
db 000h,000h,030h,030h,000h,030h,030h,000h ; 03A
db 000h,000h,030h,030h,000h,030h,030h,020h ; 03B
db 008h,010h,020h,040h,020h,010h,008h,000h ; 03C
db 000h,000h,07Ch,000h,000h,07Ch,000h,000h ; 03D
db 020h,010h,008h,004h,008h,010h,020h,000h ; 03E
db 038h,044h,004h,018h,010h,000h,010h,000h ; 03F
db 038h,044h,05Ch,054h,05Ch,040h,038h,000h ; 040
db 038h,044h,044h,044h,07Ch,044h,044h,000h ; 041
db 078h,044h,044h,078h,044h,044h,078h,000h ; 042
db 038h,044h,040h,040h,040h,044h,038h,000h ; 043
db 078h,044h,044h,044h,044h,044h,078h,000h ; 044
db 07Ch,040h,040h,078h,040h,040h,07Ch,000h ; 045
db 07Ch,040h,040h,078h,040h,040h,040h,000h ; 046
db 038h,044h,040h,05Ch,044h,044h,03Ch,000h ; 047
db 044h,044h,044h,07Ch,044h,044h,044h,000h ; 048
db 038h,010h,010h,010h,010h,010h,038h,000h ; 049
db 004h,004h,004h,004h,044h,044h,038h,000h ; 04A
db 044h,048h,050h,060h,050h,048h,044h,000h ; 04B
db 040h,040h,040h,040h,040h,040h,07Ch,000h ; 04C
db 044h,06Ch,054h,044h,044h,044h,044h,000h ; 04D
db 044h,064h,054h,04Ch,044h,044h,044h,000h ; 04E
db 038h,044h,044h,044h,044h,044h,038h,000h ; 04F
db 078h,044h,044h,078h,040h,040h,040h,000h ; 050
db 038h,044h,044h,044h,054h,048h,034h,000h ; 051
db 078h,044h,044h,078h,048h,044h,044h,000h ; 052
db 038h,044h,040h,038h,004h,044h,038h,000h ; 053
db 07Ch,010h,010h,010h,010h,010h,010h,000h ; 054
db 044h,044h,044h,044h,044h,044h,038h,000h ; 055
db 044h,044h,044h,044h,044h,028h,010h,000h ; 056
db 044h,044h,054h,054h,054h,054h,028h,000h ; 057
db 044h,044h,028h,010h,028h,044h,044h,000h ; 058
db 044h,044h,044h,028h,010h,010h,010h,000h ; 059
db 078h,008h,010h,020h,040h,040h,078h,000h ; 05A
db 038h,020h,020h,020h,020h,020h,038h,000h ; 05B
db 000h,040h,020h,010h,008h,004h,000h,000h ; 05C
db 038h,008h,008h,008h,008h,008h,038h,000h ; 05D
db 010h,028h,044h,000h,000h,000h,000h,000h ; 05E
db 000h,000h,000h,000h,000h,000h,000h,0FCh ; 05F
db 030h,030h,010h,000h,000h,000h,000h,000h ; 060
db 000h,000h,038h,004h,03Ch,044h,03Ch,000h ; 061
db 040h,040h,078h,044h,044h,044h,078h,000h ; 062
db 000h,000h,038h,044h,040h,044h,038h,000h ; 063
db 004h,004h,03Ch,044h,044h,044h,03Ch,000h ; 064
db 000h,000h,038h,044h,078h,040h,038h,000h ; 065
db 018h,020h,020h,078h,020h,020h,020h,000h ; 066
db 000h,000h,03Ch,044h,044h,03Ch,004h,038h ; 067
db 040h,040h,070h,048h,048h,048h,048h,000h ; 068
db 010h,000h,010h,010h,010h,010h,018h,000h ; 069
db 008h,000h,018h,008h,008h,008h,048h,030h ; 06A
db 040h,040h,048h,050h,060h,050h,048h,000h ; 06B
db 010h,010h,010h,010h,010h,010h,018h,000h ; 06C
db 000h,000h,068h,054h,054h,044h,044h,000h ; 06D
db 000h,000h,070h,048h,048h,048h,048h,000h ; 06E
db 000h,000h,038h,044h,044h,044h,038h,000h ; 06F
db 000h,000h,078h,044h,044h,044h,078h,040h ; 070
db 000h,000h,03Ch,044h,044h,044h,03Ch,004h ; 071
db 000h,000h,058h,024h,020h,020h,070h,000h ; 072
db 000h,000h,038h,040h,038h,004h,038h,000h ; 073
db 000h,020h,078h,020h,020h,028h,010h,000h ; 074
db 000h,000h,048h,048h,048h,058h,028h,000h ; 075
db 000h,000h,044h,044h,044h,028h,010h,000h ; 076
db 000h,000h,044h,044h,054h,07Ch,028h,000h ; 077
db 000h,000h,048h,048h,030h,048h,048h,000h ; 078
db 000h,000h,048h,048h,048h,038h,010h,060h ; 079
db 000h,000h,078h,008h,030h,040h,078h,000h ; 07A
db 018h,020h,020h,060h,020h,020h,018h,000h ; 07B
db 010h,010h,010h,000h,010h,010h,010h,000h ; 07C
db 030h,008h,008h,00Ch,008h,008h,030h,000h ; 07D
db 028h,050h,000h,000h,000h,000h,000h,000h ; 07E
db 010h,038h,06Ch,044h,044h,07Ch,000h,000h ; 07F
db 038h,044h,040h,040h,044h,038h,010h,030h ; 080
db 048h,000h,048h,048h,048h,058h,028h,000h ; 081
db 00Ch,000h,038h,044h,078h,040h,038h,000h ; 082
db 038h,000h,038h,004h,03Ch,044h,03Ch,000h ; 083
db 028h,000h,038h,004h,03Ch,044h,03Ch,000h ; 084
db 030h,000h,038h,004h,03Ch,044h,03Ch,000h ; 085
db 038h,028h,038h,004h,03Ch,044h,03Ch,000h ; 086
db 000h,038h,044h,040h,044h,038h,010h,030h ; 087
db 038h,000h,038h,044h,078h,040h,038h,000h ; 088
db 028h,000h,038h,044h,078h,040h,038h,000h ; 089
db 030h,000h,038h,044h,078h,040h,038h,000h ; 08A
db 028h,000h,010h,010h,010h,010h,018h,000h ; 08B
db 038h,000h,010h,010h,010h,010h,018h,000h ; 08C
db 020h,000h,010h,010h,010h,010h,018h,000h ; 08D
db 028h,000h,010h,028h,044h,07Ch,044h,000h ; 08E
db 038h,028h,038h,06Ch,044h,07Ch,044h,000h ; 08F
db 00Ch,000h,07Ch,040h,078h,040h,07Ch,000h ; 090
db 000h,000h,078h,014h,07Ch,050h,03Ch,000h ; 091
db 03Ch,050h,050h,07Ch,050h,050h,05Ch,000h ; 092
db 038h,000h,030h,048h,048h,048h,030h,000h ; 093
db 028h,000h,030h,048h,048h,048h,030h,000h ; 094
db 060h,000h,030h,048h,048h,048h,030h,000h ; 095
db 038h,000h,048h,048h,048h,058h,028h,000h ; 096
db 060h,000h,048h,048h,048h,058h,028h,000h ; 097
db 028h,000h,048h,048h,048h,038h,010h,060h ; 098
db 048h,030h,048h,048h,048h,048h,030h,000h ; 099
db 028h,000h,048h,048h,048h,048h,030h,000h ; 09A
db 000h,000h,004h,038h,058h,068h,070h,080h ; 09B
db 018h,024h,020h,078h,020h,024h,05Ch,000h ; 09C
db 03Ch,04Ch,054h,054h,054h,064h,078h,000h ; 09D
db 000h,044h,028h,010h,028h,044h,000h,000h ; 09E
db 008h,014h,010h,038h,010h,010h,050h,020h ; 09F
db 018h,000h,038h,004h,03Ch,044h,03Ch,000h ; 0A0
db 018h,000h,010h,010h,010h,010h,018h,000h ; 0A1
db 018h,000h,030h,048h,048h,048h,030h,000h ; 0A2
db 018h,000h,048h,048h,048h,058h,028h,000h ; 0A3
db 028h,050h,000h,070h,048h,048h,048h,000h ; 0A4
db 028h,050h,000h,048h,068h,058h,048h,000h ; 0A5
db 038h,004h,03Ch,044h,03Ch,000h,03Ch,000h ; 0A6
db 030h,048h,048h,048h,030h,000h,078h,000h ; 0A7
db 010h,000h,010h,030h,040h,044h,038h,000h ; 0A8
db 078h,094h,0ACh,0B4h,0ACh,084h,078h,000h ; 0A9
db 000h,000h,0FCh,004h,004h,000h,000h,000h ; 0AA
db 040h,048h,050h,038h,044h,008h,01Ch,000h ; 0AB
db 040h,048h,050h,02Ch,054h,01Ch,004h,000h ; 0AC
db 010h,000h,010h,010h,038h,038h,010h,000h ; 0AD
db 000h,000h,024h,048h,024h,000h,000h,000h ; 0AE
db 000h,000h,048h,024h,048h,000h,000h,000h ; 0AF
db 054h,000h,0A8h,000h,054h,000h,0A8h,000h ; 0B0
db 054h,0A8h,054h,0A8h,054h,0A8h,054h,0A8h ; 0B1
db 0A8h,0FCh,054h,0FCh,0A8h,0FCh,054h,0FCh ; 0B2
db 010h,010h,010h,010h,010h,010h,010h,010h ; 0B3
db 010h,010h,010h,0F0h,010h,010h,010h,010h ; 0B4
db 018h,000h,010h,028h,044h,07Ch,044h,000h ; 0B5
db 038h,000h,010h,028h,044h,07Ch,044h,000h ; 0B6
db 030h,000h,010h,028h,044h,07Ch,044h,000h ; 0B7
db 078h,084h,0B4h,0A4h,0B4h,084h,078h,000h ; 0B8
db 050h,0D0h,010h,0D0h,050h,050h,050h,050h ; 0B9
db 050h,050h,050h,050h,050h,050h,050h,050h ; 0BA
db 000h,0F0h,010h,0D0h,050h,050h,050h,050h ; 0BB
db 050h,0D0h,010h,0F0h,000h,000h,000h,000h ; 0BC
db 000h,010h,038h,040h,040h,038h,010h,000h ; 0BD
db 044h,028h,010h,07Ch,010h,07Ch,010h,000h ; 0BE
db 000h,000h,000h,0F0h,010h,010h,010h,010h ; 0BF
db 010h,010h,010h,01Ch,000h,000h,000h,000h ; 0C0
db 010h,010h,010h,0FCh,000h,000h,000h,000h ; 0C1
db 000h,000h,000h,0FCh,010h,010h,010h,010h ; 0C2
db 010h,010h,010h,01Ch,010h,010h,010h,010h ; 0C3
db 000h,000h,000h,0FCh,000h,000h,000h,000h ; 0C4
db 010h,010h,010h,0FCh,010h,010h,010h,010h ; 0C5
db 014h,028h,038h,004h,03Ch,044h,03Ch,000h ; 0C6
db 014h,028h,010h,028h,044h,07Ch,044h,000h ; 0C7
db 050h,05Ch,040h,07Ch,000h,000h,000h,000h ; 0C8
db 000h,07Ch,040h,05Ch,050h,050h,050h,050h ; 0C9
db 050h,0DCh,000h,0FCh,000h,000h,000h,000h ; 0CA
db 000h,0FCh,000h,0DCh,050h,050h,050h,050h ; 0CB
db 050h,05Ch,040h,05Ch,050h,050h,050h,050h ; 0CC
db 000h,0FCh,000h,0FCh,000h,000h,000h,000h ; 0CD
db 050h,0DCh,000h,0DCh,050h,050h,050h,050h ; 0CE
db 044h,038h,044h,044h,044h,038h,044h,000h ; 0CF
db 030h,040h,020h,010h,038h,048h,030h,000h ; 0D0
db 038h,024h,024h,074h,024h,024h,038h,000h ; 0D1
db 038h,000h,07Ch,040h,078h,040h,07Ch,000h ; 0D2
db 028h,000h,07Ch,040h,078h,040h,07Ch,000h ; 0D3
db 030h,000h,07Ch,040h,078h,040h,07Ch,000h ; 0D4
db 010h,010h,010h,000h,000h,000h,000h,000h ; 0D5
db 018h,000h,038h,010h,010h,010h,038h,000h ; 0D6
db 038h,000h,038h,010h,010h,010h,038h,000h ; 0D7
db 028h,000h,038h,010h,010h,010h,038h,000h ; 0D8
db 010h,010h,010h,0F0h,000h,000h,000h,000h ; 0D9
db 000h,000h,000h,01Ch,010h,010h,010h,010h ; 0DA
db 0FCh,0FCh,0FCh,0FCh,0FCh,0FCh,0FCh,0FCh ; 0DB
db 000h,000h,000h,000h,0FCh,0FCh,0FCh,0FCh ; 0DC
db 010h,010h,010h,000h,010h,010h,010h,000h ; 0DD
db 030h,000h,038h,010h,010h,010h,038h,000h ; 0DE
db 0FCh,0FCh,0FCh,0FCh,000h,000h,000h,000h ; 0DF
db 018h,030h,048h,048h,048h,048h,030h,000h ; 0E0
db 000h,070h,048h,070h,048h,048h,070h,040h ; 0E1
db 038h,030h,048h,048h,048h,048h,030h,000h ; 0E2
db 060h,030h,048h,048h,048h,048h,030h,000h ; 0E3
db 028h,050h,000h,030h,048h,048h,030h,000h ; 0E4
db 028h,050h,030h,048h,048h,048h,030h,000h ; 0E5
db 000h,000h,048h,048h,048h,070h,040h,040h ; 0E6
db 000h,060h,040h,070h,048h,070h,040h,060h ; 0E7
db 060h,040h,070h,048h,048h,070h,040h,060h ; 0E8
db 018h,000h,048h,048h,048h,048h,030h,000h ; 0E9
db 038h,000h,048h,048h,048h,048h,030h,000h ; 0EA
db 060h,000h,048h,048h,048h,048h,030h,000h ; 0EB
db 018h,000h,048h,048h,048h,038h,010h,060h ; 0EC
db 018h,000h,044h,028h,010h,010h,010h,000h ; 0ED
db 000h,038h,000h,000h,000h,000h,000h,000h ; 0EE
db 030h,030h,020h,000h,000h,000h,000h,000h ; 0EF
db 000h,000h,000h,038h,000h,000h,000h,000h ; 0F0
db 000h,010h,038h,010h,000h,038h,000h,000h ; 0F1
db 000h,000h,07Ch,000h,000h,07Ch,000h,000h ; 0F2
db 0C0h,068h,0D0h,02Ch,054h,01Ch,004h,000h ; 0F3
db 03Ch,054h,054h,034h,014h,014h,014h,000h ; 0F4
db 038h,044h,030h,028h,018h,044h,038h,000h ; 0F5
db 000h,010h,000h,07Ch,000h,010h,000h,000h ; 0F6
db 000h,000h,000h,038h,018h,000h,000h,000h ; 0F7
db 030h,048h,048h,030h,000h,000h,000h,000h ; 0F8
db 000h,000h,000h,028h,000h,000h,000h,000h ; 0F9
db 000h,000h,000h,020h,000h,000h,000h,000h ; 0FA
db 020h,060h,020h,020h,000h,000h,000h,000h ; 0FB
db 070h,020h,030h,060h,000h,000h,000h,000h ; 0FC
db 060h,010h,020h,070h,000h,000h,000h,000h ; 0FD
db 000h,000h,078h,078h,078h,078h,000h,000h ; 0FE
db 000h,000h,000h,000h,000h,000h,000h,000h 	; 0FF
.font_len equ	$-.font_6x8				   ; number of bytes to write

vdp_init:
if .debug_vdp > 0
	call iputs
	db 	'VDP INIT ',cr,lf,0
endif
	; Initialize the VDP for Text Mode
	ld	hl,.vdpinit
	ld	b,.vdpinit_len
	ld	c,vdp_reg
	otir				; write the config bytes
	; Now, load font data
	; Set the VRAM write address to 0
	ld	a,0		; LSB
	out	(vdp_reg),a
	push	hl	; waste some time
	pop	hl
	ld	a,040h		; MSB
	out	(vdp_reg),a

	ld	hl,.font_6x8		; buffer-o-bytes to send
	ld	c,vdp_vram		; the I/O port number
	ld	de,.font_len	; number of bytes to send

.vram_init_loop:
	outi				; note: this clobbers B

	; waste time between transfers
	push	hl			; waste much time between transfers
	pop	hl
	push	hl
	pop	hl
	dec	de
	ld	a,d
	or	e
	jp	nz,.vram_init_loop
	;Write a test byte
	ld	a,0		; LSB
	out	(vdp_reg),a
	push	hl	; waste some time
	pop	hl
	ld	a,048h		; MSB
	out	(vdp_reg),a
	ret

.vdpinit:
	db	00000000b,080h	; R0 = Text mode, no EXT video
	db	11010000b,081h	; R1 = 16K RAM, enable display, disable INT, 8x8 sprites, mag off
	db	00000010b,082h	; R2 = name table = 0x0800
	db	00000000b,084h	; R4 = pattern table = 0x0000
	db	11110101b,087h	; R7 = white on light blue
.vdpinit_len: equ	$-.vdpinit	; number of bytes to write

vdp_vram			equ	080h	; VDP port for accessing the VRAM
vdp_reg				equ	081h	; VDP port for accessing the registers

