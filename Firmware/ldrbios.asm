;##########################################################################
; CP/M 3 BIOS for 2063 z80 retro board
;
; Sources originally written by John Winans with contributions from Trevor Jacobs
; Refactored for CP/M 3 by SolderGirl
;
;##########################################################################
org 0
.debug:			equ	1
.debug_vdp:		equ 1
.debugger:		equ 0	; have some options disabled for running inside DDTZ


;EXTRN @MXTPA

	cr			equ	0dh	; carriage return
	lf			equ	0ah	; line feed

.low_bank		equ 00h    ; The RAM BANK to use for the bottom 32K

;the ROM code will load us at C000h, so lets set our stack below ourselves
;the real BIOS will fix it later
;			org 0C000h

; This is the jump table for CPMLDR. It needs to have all the jump points to 
; maintain the structure as specified in the CP/M 3 System Guide
cseg

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

; IO Adresses
vdp_vram	equ	080h	; VDP port for accessing the VRAM
vdp_reg		equ	081h	; VDP port for accessing the registers

; VRAM Adresses for Graphics Mode 2
patterntbl	equ 00000h	; Pattern Table
spritetbl	equ 01800h	; Sprite Patterns
colortbl	equ 02000h	; Color Table
nametbl		equ 03800h	; Name Table
spriteatr	equ 03B00h	; Sprite Attributes


;
;##########################################################################
; Libraries
;##########################################################################

	extrn 	fnt8x8
	extrn 	chrW,chrH,z80bmp,bmpH
	
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
; NOTE: This is the ROM firmware!
;  Since this is the first piece of code to be run, we have to initialize everything!
;  And cleanly!
;
;##########################################################################

;extrn bdos	
; only following functions are supported:
;	13	reset disk system
;	09	print string
;	15	open function
;	20	read sequential
;	26	set dma address

	; The very first step must be:
	;  LDIR 64k
	ld	hl,0
	ld	de,0
	ld	bc,0;4000h
	ldir				; Copy all the code in the FLASH into RAM at same address.
	; Disable the FLASH and run from SRAM only from this point on.
	in	a,(070h)	; Dummy-read this port to disable the FLASH.

.bios_boot:
	; This will select low-bank 0, idle the SD card, and idle the printer
	ld	a,0fh + (.low_bank<<4)
	ld	(gpio_out_cache),a
	out	(gpio_out),a
	; make sure we have a viable stack
	ld	sp,bios_stack		; use the private BIOS stack to get started
	
	call .init_console		; Init CTC & SIO
if .debug > 0
	call	iputs
	db	cr,lf,'BOOT',cr, lf, 0
	call	iputs
	db	cr,lf,'Debug level 0x'0
	ld	a,.debug		; A = the current debug level
	call	hexdump_a		; print the current level number
	call	puts_crlf		; and a newline
endif
	call vdp_init			; Init VDP
if .debug > 0
	call	iputs
	db	 'V ', 0
endif
	call vdp_ldfnt			; Load ANSI 8x8 Font
if .debug > 0
	call	iputs
	db	 'F ', 0
endif
	call vdp_ldcol			; Init Color Table
if .debug > 0
	call	iputs
	db	 'C ', 0
endif
	call loadlogo
if .debug > 0
	call	iputs
	db	 'P ',0
endif
	call clear_screen		; Clear Nametable
	
	ld de,0020Bh
	ld hl,strng
	
	call vdp_string			
	call .colorize
	
	call	iputs
	db 'done',cr,lf,0

	jp $ ; spin in place for debug

strng: 
	db 'Z80 Retro!',0

vdp_string:
	ld a,(hl)
	or a
	ret z
	ld a,(hl)
	ld c,a
	push de
	push hl
	call vdp_char	; print char in C at coordinates in D:E
	pop hl
	pop de
	inc e
	inc hl
	jr vdp_string

	; At this point, the VDP should be in Mode 2 with ANSI Font loaded
	; Lets display a logo in the upper 3rd of the screen
	
	; Somehow read MBR and check for partitions
	; Then, use BDOS to check for files
	; CPM3.SYS is obvious
	; CP/M 2.2 won't have that, but live at partition base
	; The official Firmware has the String "Z80 Retro Board 2063"
	; Plus version info within the first 0x100 bytes
	; At least 2 versions have it at exactly 0x9F
	; From all that, build a list of bootable devices
	; And display it in a fancy manner


	; Display a hello world message.


if .debug >= 3
	; dump the zero-page for reference
	ld	hl,0		; start address
	ld	bc,100h	; number of bytes
	ld	e,1		; fancy format
	call	hexdump
endif
	ret

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
	
;##############################################################
; Return NZ (with A=1) if sio A rx is ready and Z (with A=0) if not ready.
; Clobbers: AF
;##############################################################
con_rx_ready:
sioa_rx_ready:
	in	a,(sio_ac)	; read sio control status byte
	and	1		; check the rcvr ready bit
	ret			; 0 = not ready

con_rx_char:
sioa_rx_char:
	call	sioa_rx_ready
	jr	z,sioa_rx_char
	in	a,(sio_ad)
	ret
	
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

;##########################################################################
; Initialize the console port.  Note that this includes CTC port 1.
;##########################################################################
.init_console:
	; Init CTC first
	ld	c,12			; C = 12 = 9600 bpst
    ld      a,047h      ; TC follows, Counter, Control, Reset
    out     (ctc_1),a
    ld      a,c
    out     (ctc_1),a
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
        call    con_tx_char
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
bios_stk:			ds 128
bios_stack:			db 0

	
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

	CSEG
;##############################################################
; Print a CRLF
; Clobbers AF, C
;##############################################################
puts_crlf:
        call    iputs
        db    cr,lf,0 	;'wtf \r\n\0'
        ret

vdp_init:
if .debug_vdp > 0
	call iputs
	db 	'VDP INIT ',cr,lf,0
endif
	; Initialize the VDP
	ld	hl,.vdpinit
	ld	b,.vdpinit_len
	ld	c,vdp_reg
	otir				; write the config bytes
	; Now, load font data
	; Set the VRAM write address to 0
	ld	a,0		; LSB
	out	(vdp_reg),a
	call .vdp_del
	ld	a,040h		; MSB
	out	(vdp_reg),a
	ret
	
vdp_ldfnt:
	ld hl,patterntbl
	ld	a,l				; LSB
	out	(vdp_reg),a
	call .vdp_del
	ld a,h
	or 040h				; VRAM Write
	out	(vdp_reg),a
	ld	c,vdp_vram		; the I/O port number
	ld d,3
.ldfnt_l:
	push de
	ld	de,2048			; number of bytes to send
	ld hl,fnt8x8
.ldfnt_loop:
	call .vdp_del
	outi				; note: this clobbers B
	dec	de
	ld	a,d
	or	e
	jr	nz,.ldfnt_loop
	pop de
	dec d
	jr nz,.ldfnt_l
	call .savechrs		; chain
	ret

;##################################
; Load bitmap pattern
;##################################
.vram_adr:
	ld	a,l				; LSB
	out	(vdp_reg),a
	call .vdp_del
	ld a,h
	or 040h				; VRAM Write
	out	(vdp_reg),a
	call .vdp_del
	ret
	

loadlogo:
	; dest=VRAM(patterntbl+(8 * (.clobber+128)))
	ld hl,(.clobber)
	ld de,128
	add hl,de			
	ex de,hl			; DE = (.clobber) + 128

	ld hl,patterntbl
	ld b,8				; 8 Bytes / Pattern
.ladd:
	add hl,de
	dec b
	jr nz,.ladd
	call .vram_adr
	ld	c,vdp_vram		; the I/O port number
	ld hl,z80bmp
	ld a,9;(chrW)
	ld d,a			; 9 chrs / line
	ld a,64;(bmpH)
	ld e,a			; 8 lines * 8 bytes/char
	; src=z80bmp, len=chrW x bmpH
.lbyte:
	outi ;C=port,B=count,HL=src
	call .vdp_del
	dec d
	jr nz,.lbyte
	ld a,9;(chrW)
	ld d,a			; 9 chrs / line
	dec e
	jr nz,.lbyte

	; Pattern Data loaded,
	; Now load the Name table (iscreen)
	ld hl,28
	ld de,128
	add hl,de			
	ld a,e
	push af			; first pattern no.
	; 9 per line x 8 lines
	ld hl,iscreen	; destination
	ld a,9 ;(chrW)
	ld d,a			; 9 chrs / line
	ld a,8;(chrH)
	ld e,a			; 8 lines 
	pop af
	ld a,156
.namloop:
	ld (hl),a
	inc hl
	inc a
	dec d
	jr nz,.namloop	; one line done
	push af
	ld a,9;(chrW)
	ld d,a			; 9 chrs / line
	pop af
	push de
	ld de,23		; skip 23 chrs
	add hl,de
	pop de
	dec e
	jr nz,.namloop	; next line
	;jp .colorize
	ret
	
	; nametable loaded, now colors
.colorize:
	; Cols A,1 but A,4 for '80'
	ld hl,02000h	;colortbl
	call .vram_adr
	;ld d,9			; 9 chrs / line
	
	ld e,8			; 8 lines 
	ld a,0A3h
	ld d,72			; 8 bytes/char
.cllop:
	ld a,0A3h
	out (vdp_vram),a
	call .vdp_del
	dec d
	jr nz,.cllop	; one line done
	inc h
	call .vram_adr
	ld d,72			; 9 chrs / line
	dec e
	jr nz,.cllop	; next line
	
ret

;##################################
; Save a set of characters 
; Before loading different patterns
;##################################
	DSEG
.keepchrs:	; Save some graphic characters
; 128 = 0x80
db 0B9h,0BAh,0BBh,0BCh,0C8h,0C9h,0CAh,0CBh
db 0CCh,0CDh,0CEh,0B0h,0B1h,0B2h,0AEh,0AFh
db 0B3h,0B4h,0C0h,0C1h,0C2h,0C3h,0C4h,0C5h
db 0D9h,0DAh,0BFh,0 
.clobber:	dw $-.keepchrs	; First char to clobber
	CSEG

.savechrs:
	; dest=VRAM(patterntbl+128)
	ld hl,patterntbl
	ld de,128
	ld b,8
.patadd:
	add hl,de
	dec b
	jr nz,.patadd
	
	ld	a,l				; LSB
	out	(vdp_reg),a
	call .vdp_del
	ld a,h
	or 040h				; VRAM Write
	out	(vdp_reg),a
	call .vdp_del
	ld	c,vdp_vram		; the I/O port number
	; source=fnt8x8+(keepchr*8)
	ld hl,.keepchrs
	
.perchar:
	ld a,(hl)
	cp 0
	ret z	; all done
	push hl
	; A=chr
	ld hl,fnt8x8
	ld de,0
	ld e,a
	ld b,8
	add hl,de
	dec b
.chradd:
	adc hl,de
	dec b
	jr nz,.chradd
	; HL->source
	ld b,8
.perbyte:
	outi ;C=port,B=count,HL=src
	call .vdp_del
	jr nz,.perbyte
	; next character
	pop hl
	inc hl
	jr .perchar
	ret

vdp_ldcol:
	ld hl,colortbl
	call .vram_adr
	ld d,8
	ld e,3				; 3x8 loops
	ld b,0	
	ld a,0F1h			; White on Black (?)Backdrop(?)
.color_loop:
	out (vdp_vram),a
	call .vdp_del
	inc b
	jr nz,.color_loop
	dec d
	jr nz,.color_loop
	ld d,8
	dec e
	jr nz,.color_loop
	ret

clear_screen:
	ld hl,nametbl
	call .vram_adr
	;ld de,(screenlen)	
	ld de,768			; bytes to write
	ld hl,iscreen		; screenbuffer
	ld c,vdp_vram
.fill_loop:
	outi
	call .vdp_del
	dec	de
	ld	a,d
	or	e
	jr	nz,.fill_loop
	ret

.vdp_del:
	push hl
	pop hl
	push hl
	pop hl
	ret

;#############################################################################
; Print the value in A in hex
; Clobbers C
;#############################################################################
hexdump_a:
	push	af
	srl	a
	srl	a
	srl	a
	srl	a
	call	.hexdump_nib
	pop	af
	push	af
	and	00fh
	call	.hexdump_nib
	pop	af
	ret

.hexdump_nib:
	add	'0'
	cp	'9'+1
	jp	m,.hexdump_num
	add	'A'-'9'-1
.hexdump_num:
	ld	c,a
	jp	con_tx_char	   ; tail

vdp_char:			; print char in C at coordinates in D:E
	push bc			; save character
	ld hl,0			; clear HL
	ld l,e			
	ld c,d
	ld de,0			; clear DE
	ld a,32
	ld e,a
.addloop0:
	add hl,de
	dec c
	jp m,.noadd
	jr nz,.addloop0
.noadd:	
	ld de,nametbl ; 0800h		; offset for VRAM
	
	add hl,de

	ld a,l
	out (vdp_reg),a
	ld a,040h
	or h
	out (vdp_reg),a	; VDP address loaded
	pop bc
	ld a,c
	out (vdp_vram),a	; Write to the VDP
	ret

DSEG

.vdpinit:
			db	00000010b,080h	; R0 = Graphics II, no EXT video
			db	11000000b,081h	; R1 = 16K RAM, enable display, disable INT, 8x8 sprites, mag off
			db	00001110b,082h	; R2 = name table = 0x3800
			db	11111111b,083h	; R3 = name table = 0x2000
			db	00000011b,084h	; R4 = pattern table = 0x0000
			db	01110110b,085h	; R5 = Sprite Attribute table = 0x3B00
			db	00000011b,086h	; R6 = sprite pattern table = 0x1800
.vdpcol:	db	00000100b,087h	; R7 = Dark Blue Backdrop
.vdpinit_len: equ	$-.vdpinit	; number of bytes to write

iscreen:	; 3 blocks x 8 lines x 32 chars
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,085h,089h,089h,089h,089h,089h,089h
db 089h,089h,089h,089h,089h,089h,089h,089h,089h,089h,089h,089h,089h,089h,089h,082h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h
db 000h,001h,002h,003h,004h,005h,006h,007h,008h,081h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,081h

db 0C9h,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CAh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh
db 0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0B9h
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh

db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0BAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h
db 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,0BAh
db 0C8h,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh
db 0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0CDh,0BCh
screenlen: dw	$-iscreen	; number of bytes to write

CSEG 
include Lsdcard.asm
include Lspi.asm
include Lnocache.asm

end
