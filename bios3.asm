;##########################################################################
; CP/M 3 BIOS for 2063 z80 retro board
;
; Sources originally written by John Winans with contributions from Trevor Jacobs
; Refactored for CP/M 3 by SolderGirl
;
;##########################################################################

true	equ -1
false	equ not true
banked	equ true
.debug:		equ	0
m_debug		equ 0
VDP			equ 1
VDP_font 	equ 0
RTC_debug	equ 0

;##########################################################################
; 'System Control Block Definition for CP/M3 BIOS'
; From the CP/M 3 System Manual over at
; http://www.s100computers.com/Software%20Folder/CPM3%20BIOS%20Installation/CPM3%20System%20Guide.pdf
;
; These are all the public variables defined by CPM3
;##########################################################################

	extrn @civec, @covec, @aivec, @aovec, @lovec, @bnkbf
	extrn @crdma, @crdsk, @vinfo, @resel, @fx, @usrcd
	extrn @mltio, @ermde, @erdsk, @media, @bflgs
	extrn @date, @hour, @min, @sec, ?erjmp, @mxtpa, @dtbl

; These are all the public variables defined by BIOSKRNL
;##########################################################################

	extrn @adrv,@rdrv,@trk,@sect	; parameters for disk I/O
	extrn @dma,@dbnk,@cnt,@cbnk

	extrn disk_dump, dumpdpb, dumpdtbl, hexdmp
	extrn hexdump_a, dumpdma, dump_regs, dumpivars


cr			equ	0dh		; carriage return
lf			equ	0ah		; line feed


;****************************************************************************
;
;    Copyright (C) 2021 John Winans
;
;    This library is free software; you can redistribute it and/or
;    modify it under the terms of the GNU Lesser General Public
;    License as published by the Free Software Foundation; either
;    version 2.1 of the License, or (at your option) any later version.
;
;    This library is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public
;    License along with this library; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
;    USA
;
; https://github.com/johnwinans/2063-Z80-cpm
;
;****************************************************************************




CSEG

	;public console routines
	public	?cinit,?ci,?co,?cist,?cost
	public con_tx_char,con_rx_ready

	;public values for SPI
	public	mosi,sdclk,sdsel,miso
	mosi	equ	gpio_out_sd_mosi
	miso	equ	gpio_in_sd_miso
	sdclk	equ	gpio_out_sd_clk
	sdsel	equ	gpio_out_sd_ssel

	public 	gpio_in,gpio_out,gpiocache

	;publics for SIO
	public	satxrdy,sbtxrdy,sarxrdy,sbrxrdy
		satxrdy	equ	sioa_tx_ready
		sbtxrdy	equ siob_tx_ready
		sarxrdy	equ sioa_rx_ready
		sbrxrdy	equ	siob_rx_ready

	public	satxch,sbtxch,sarxch,sbrxch
		satxch	equ	sioa_tx_char
		sbtxch	equ	siob_tx_char
		sarxch	equ	sioa_rx_char
		sbrxch	equ	siob_rx_char

	public	sainit,sbinit
		sainit	equ	sioa_init
		sbinit	equ	siob_init

	;publics for CTC
	public 	ictc1,ictc2,ictc3
		ictc1 equ init_ctc_1
		ictc2 equ init_ctc_2
		ictc3 equ init_ctc_3

	;publics for debug
	public puts_crlf,cr,lf,iputs,puts
	public spamon,spamoff

	extrn disk_dump, dumpdpb, dumpdtbl, hexdmp
	extrn hexdump_a, dumpdma, dump_regs, dumpivars

	public setbank
	
; Z80 Retro Rev 3 IO port definitions

gpio_in:		equ 000h		; GP input port
gpio_out:		equ	010h		; GP output port
prn_dat:		equ	020h		; printer data out

sio_ad:			equ	030h		; SIO port A, data
sio_bd:			equ	031h		; SIO port B, data
sio_ac:			equ	032h		; SIO port A, control
sio_bc:			equ	033h		; SIO port B, control

ctc_0			equ	040h		; CTC port 0
ctc_1			equ	041h		; CTC port 1
ctc_2			equ	042h		; CTC port 2
ctc_3			equ	043h		; CTC port 3

flash_disable:	equ	070h		; dummy-read from this port to disable the FLASH


; bit-assignments for General Purpose output port
gpio_out_sd_mosi	equ	001h
gpio_out_sd_clk		equ	002h
gpio_out_sd_ssel	equ	004h
gpio_out_prn_stb	equ	008h
gpio_out_a15		equ	010h
gpio_out_a16		equ	020h
gpio_out_a17		equ	040h
gpio_out_a18		equ	080h

; a bitmask representing all of the lobank address bits
gpio_out_lobank:	equ	0|(gpio_out_a15|gpio_out_a16|gpio_out_a17|gpio_out_a18)

.low_bank	equ 0h

gpiocache: 				; GPIO output latch cache
			db	gpio_out_sd_mosi|gpio_out_sd_clk|gpio_out_sd_ssel|gpio_out_prn_stb|(.low_bank<<4)

; bit-assignments for General Purpose input port
gpio_in_prn_err		equ	001h
gpio_in_prn_stat	equ	002h
gpio_in_prn_papr	equ	004h
gpio_in_prn_bsy		equ	008h
gpio_in_prn_ack		equ	010h
gpio_in_user1		equ	020h
gpio_in_sd_det		equ	040h
gpio_in_sd_miso		equ	080h

; Software RTC / Interrupts
IVTbase				equ 0FF00h
rtc_counter:		db 0
rtc_limit:			db 153		; IRQ will happen at a freq of (CLK/256/256)
								; 1 sec = 152.587890625 ticks @ 10 MHz
					
	public	@ctbl
@ctbl:
	db 	'SIO-A '	; device 0, CRT port 0
	db	0fh
	db 	14
	db 	'PRN   '
	db 	2			; 2 -> output only
	db 	0			; 0 -> no baudrate
	db 	'SIO-B '	; device 1, LPT port 0
	db 	0fh
	db 	14
if VDP
	db 	'CRT   '
	db 	2			; 2 -> output only
	db 	0			; 0 -> no baudrate
endif	
	db	'NONE  '
	db	3			; In+Out
	db	0
	db 	0			; table terminator
					
					

rtc_irq_handler:
	ex af,af'
	exx
if RTC_debug > 0	
	ld a,'.'
	ld c,a
	call con_tx_char
endif	
	ld a,(rtc_limit)
	ld b,a	
	ld a,(rtc_counter)
	inc a
	cp b
	call z,.advance_RTC
	ld (rtc_counter),a

	ld a,(rtc_limit)
	xor 01h
	ld (rtc_limit),a
	
	exx
	ex af,af'
	ei
	reti

.advance_RTC:
if RTC_debug > 0
	ld a,'-'
	ld c,a
	call con_tx_char
endif	
	ld a,(@sec)		;@sec byte / BCD
	inc a
	daa	
	cp 060h
	jr z,.incminute
	ld (@sec),a
	xor a
	ret
.incminute:
	xor a
	ld (@sec),a
	ld a,(@min)		;@min byte / BCD
	inc a
	daa	
	cp 060h
	jr z,.inchour
	ld (@min),a
	xor a
	ret
.inchour:
	xor a
	ld (@min),a
	ld a,(@hour)	;@hour byte / BCD
	inc a
	daa	
	cp 024h
	jr z,.incdate
	ld (@hour),a
	xor a
	ret
.incdate:	
	ld hl,(@date)		;@date word
	inc hl
	ld (@date),hl			
	xor a
	ret

setbank:
if m_debug
	push af
	call iputs
	db 'SETBNK: ',0
	ld a,(@cbnk)
	call hexdump_a
    call    iputs
    db ',',0
    ld a,(@dbnk)
    call hexdump_a
	call    iputs
    db ' -> ',0
	pop af
	push af
	call dump_regs
	pop af
endif	
	add a,a
	add a,a
	add a,a
	add a,a
	and 0f0h
	ld c,a
	ld	a,(gpiocache)
	and 0fh	; mask out the upper nibble
	or	c		; then or in the new bank
	ld	(gpiocache),a
	out	(gpio_out),a
	ret
	
?cinit:
	; C = 0
	ld a,c				; C= device number
	or a
	jr nz,.notsioa
	ld	c,12			; C = 12 = 9600 bpst
	call	init_ctc_1		; start CTC1 in case J11-A selects it!
	call	sioa_init		; 115200 or 19200/9600 depending on J11-A
	ret
.notsioa:
	; C = 1
	dec a
	jr nz,.notsiob
	ld	c,12			; C = 12 = 9600 bpst
	call	init_ctc_2		; start CTC2 in case J11-A selects it!
	call	siob_init		; 115200 or 19200/9600 depending on J11-A
	ret
.notsiob:
	; C = 2
	dec a
	jr nz,.notprn
	call prn_init
	ret
.notprn:
	; C = 3
	dec a
	jr nz,.notvdp
if VDP
	;call vdp_init
	; VDP init is done in the bootloader
endif	
	ret
.notvdp:
	; C >= 4
	cp 0fh
	jr nz,.nodev
	;call init_ctc_3
.nodev:	
	; invalid device
	ret

?ci:
	ld a,b
	or a
	jp z,sioa_rx_char
	dec a
	jp z,siob_rx_char
	; other devices have no input
	ret

?co:
	ld a,b				; B = device number
	or a
	jp z,sioa_tx_char	; B >= 1
	dec a
	jp z,siob_tx_char	; B >= 2
	dec a
	jp z,prn_out		; B >= 3
	dec a
if VDP
	jp z,vdp_out
endif
	ret

?cist:
	ld a,b
	or a
	jr nz,.nota
	call	sioa_rx_ready
	ret	z		; A = 0 = not ready
	ld	a,0ffh
	ret		; A = 0xff = ready
.nota:
	call	siob_rx_ready
	ret	z		; A = 0 = not ready
	ld	a,0ffh
	ret		; A = 0xff = ready


?cost:
	ld a,b
	or a
	jr nz,.ost1
	call	sioa_tx_ready
	ret	z		; A = 0 = not ready
	ld	a,0ffh
	ret		; A = 0xff = ready
.ost1:
	dec a
	jr nz,.ost2
	call	siob_tx_ready
	ret	z		; A = 0 = not ready
	ld	a,0ffh
	ret		; A = 0xff = ready
.ost2:
	dec a
	jr nz,.ost3
	call prn_stat
	ret	z		; A = 0 = not ready
	ld	a,0ffh
	ret		; A = 0xff = ready
.ost3:
	;check VDP ready status
	ret

; Drivers for the SIO

;##############################################################
; Return NZ if sio A tx is ready
; Clobbers: AF
;##############################################################
sioa_tx_ready:
	in	a,(sio_ac)	; read sio control status byte
	and	4		; check the xmtr empty bit
	ret			; a = 0 = not ready

;##############################################################
; Return NZ if sio B tx is ready
; Clobbers: AF
;##############################################################
siob_tx_ready:
	in	a,(sio_bc)	; read sio control status byte
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

;##############################################################
; Return NZ (with A=1) if sio B rx is ready and Z (with A=0) if not ready.
; Clobbers: AF
;##############################################################
siob_rx_ready:
	in	a,(sio_bc)	; read sio control status byte
	and	1		; check the rcvr ready bit
	ret			; 0 = not ready



;##############################################################
; init SIO port A/B
; Clobbers HL, BC, AF
;##############################################################
siob_init:
	ld	c,sio_bc	; port to write into (port B control)
	jp	.sio_init

sioa_init:
	ld	c,sio_ac	; port to write into (port A control)

.sio_init:
	ld	hl,.sio_init_wr	; point to init string
	ld	b,.sio_init_len_wr ; number of bytes to send
	otir			; write B bytes from (HL) into port in the C reg
	ret

;##############################################################
; Initialization string for the Z80 SIO
;##############################################################
DSEG
.sio_init_wr:
	db	00011000b	; wr0 = reset everything
	db	00000100b	; wr0 = select reg 4
	db	01000100b	; wr4 = /16 N1 (115200 from 1.8432 MHZ clk)
	db	00000011b	; wr0 = select reg 3
	db	11000001b	; wr3 = RX enable, 8 bits/char
	db	00000101b	; wr0 = select reg 5
	db	01101000b	; wr5 = DTR=0, TX enable, 8 bits/char
.sio_init_len_wr:   equ $-.sio_init_wr
CSEG


;##############################################################
; Wait for the transmitter to become ready and then
; print the character in the C register.
; Clobbers: AF
;##############################################################
siob_tx_char:
	call	siob_tx_ready
	jr	z,siob_tx_char
	ld	a,c
	out	(sio_bd),a	; send the character
	ret

con_tx_char:
sioa_tx_char:
	call	sioa_tx_ready
	jr	z,sioa_tx_char
	ld	a,c
	out	(sio_ad),a	; send the character
	ret

;##############################################################
; Wait for the receiver to become ready and then return the
; character in the A register.
; Clobbers: AF
;##############################################################
siob_rx_char:
	call	siob_rx_ready
	jr	z,siob_rx_char
	ld	a,(sio_bd)
	ret

con_rx_char:
sioa_rx_char:
	call	sioa_rx_ready
	jr	z,sioa_rx_char
	in	a,(sio_ad)
	ret

; Drivers for CTC port 1

;#############################################################################
; Init the bit-rate generator for SIO A.
; C = clock divisor
;#############################################################################


init_ctc_1:
;   ld      a,007h      ; TC follows, Timer, Control, Reset

    ld      a,047h      ; TC follows, Counter, Control, Reset
    out     (ctc_1),a
    ld      a,c
    out     (ctc_1),a
    ret

; Driver for CTC port 2

;#############################################################################
; Init the bit-rate generator for SIO B.
; C = clock divisor
;#############################################################################


init_ctc_2:
;   ld      a,0x07      ; TC follows, Timer, Control, Reset

    ld      a,047h      ; TC follows, Counter, Control, Reset
    out     (ctc_2),a
    ld      a,c
    out     (ctc_2),a
    ret

; Drivers for CTC port 3

;#############################################################################
; Set CTC3 to free-run and generate IRQs at system_clock_hz/65536.
; If system_clock_hz == 10 MHZ then the IRQ rate will be approx. 152 HZ.
;
; Note: It is OK to EI before we send the time-constant because the
; timer will not yet have been started.
;#############################################################################
init_ctc_3:
if RTC_debug > 0
	;call spamon
	ld a,'#'
	ld c,a
	call sioa_tx_char
endif	
	xor a
	out (ctc_0),a		; IRQ Vector = 0

	im 2
	ld a,0ffh
	ld i,a				; IVT starts at 0xFF00
	
	ld hl,rtc_irq_handler
	ld (IVTbase+6),hl	; load the ISR into the IVT
	
	ld	a,0b7h			; EI, timer mode, /256 prescale, TC follows, reset, ctl
	out	(ctc_3),a

	xor a				; 0=256 (as slow as it can go = system_clock_hz/256/256)
	out	(ctc_3),a

	ei
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

;##############################################################
; Print a CRLF
; Clobbers AF, C
;##############################################################
puts_crlf:
        call    iputs
        defb    cr,lf,0
        ret

spamon:
	ld a,16
	out (0FDh),a
	ret
	
spamoff:
	xor a,a
	out (0FDh),a
	ret

;############################################################################
; An library suitable for tallking to an Epson RX-80 printer.
;############################################################################


;##########################################################################
; Just set the print strobe & line-feed signals high.
;
; NOTE: The line-feed signal is ignored here and is assumed to be
;	left set high by the init code in the SIO driver!
;
; Clobbers AF
;##########################################################################
prn_init:
	ld	a,(gpiocache)
	or	gpio_out_prn_stb	; make PRN_STB high (false)
	ld	(gpiocache),a	; save in the cached output value
	out	(gpio_out),a		; make it so in the GPIO register too
	ret

;##########################################################################
; Return A=0 if printer is not ready.
; Return A=0xff if printer is ready.
; Clobbers AF
;##########################################################################
prn_stat:
	in	a,(gpio_in)
	and	gpio_in_prn_bsy		; if this bit is low then it is ready
	jr	z,.prn_stat_ready
	xor	a			; A=0 = not ready
	ret
.prn_stat_ready:
	dec	a			; A=0xff = ready
	ret


;##########################################################################
; Print the character in the C register.
;##########################################################################
prn_out:

	; Sanity check to prevent seizing the entire OS.
	; If EVERY printer status input is high, then there is probably no
	; printer attached!  This is reasonable since paper-empty high should
	; also force gpio_in_prn_err low at same time.

	in	a,(gpio_in)
	and	gpio_in_prn_err|gpio_in_prn_stat|gpio_in_prn_papr|gpio_in_prn_bsy|gpio_in_prn_ack
	cp	gpio_in_prn_err|gpio_in_prn_stat|gpio_in_prn_papr|gpio_in_prn_bsy|gpio_in_prn_ack
	ret	z			; If all signals high then just discard the data.

	; wait until the printer is ready for data
	; XXX this can seize the system if the printer is offline!  :-(
.list_wait:
	call	prn_stat
	or	a
	jr	z,.list_wait		; if A=0 then is not ready

	; proceed to print the character
	ld	a,c
	out	(prn_dat),a		; put the character code into the output latch

	; assert the strobe signal
	ld	a,(gpiocache)
	and	~gpio_out_prn_stb	; set the strobe signal low
	out	(gpio_out),a		; write to port but not update cache!

	; A brief delay so that strobe signal can be seen by the printer.
	ld	a,010h			; loop 16 times
.list_stb_wait:
	dec	a
	jr	nz,.list_stb_wait

	; raise the strobe signal
	ld	a,(gpiocache)	; we never updated the cache, so this is right
	out	(gpio_out),a
	ret

if VDP
.debug_vdp:		equ 1
	
.vdp_cr:		; Return to start of line
	xor a
	ld (vdp_x),a
	ret
	
.vdp_lf:		; Advance one line
	ld a,(vdp_y)
	cp vdp_lines
	jr nz,.nextline
	call .vdp_scroll
	jr .clearline
.nextline:
	ld a,(vdp_y)
	inc a
	ld (vdp_y),a
.clearline:
	; clear the line, too!
	ld a,(vdp_y)
	ld d,a		; D = Line number
	xor a
	ld e,a		; E = column = 0 
	ld a,' '	
	ld c,a		; C = space character
	ld a, vdp_cols
	ld b,a		; our loop counter
.clr_chr:
	push bc
	push de
	call vdp_char
	pop de
	pop bc
	inc e
	dec b
	jr nz,.clr_chr
	ret

.vdp_delay:
	ret
	
.linebuf:
	ds 40
	
.vdp_scroll:
	ld c,23				; 23 lines to scroll
	ld hl,0828h			; starting address

.scroll2:
	push bc				; save counter
	push hl				; save address
	; read line 1
	ld c,40
	ld hl,.linebuf		; Buffer for one line
	pop de				; VRAM address of line 1
	push de				; leave on stack
	ld a,e
	out (vdp_reg),a		; address LSB
	call .vdp_delay
	ld a,d
	out (vdp_reg),a		; VDP address loaded
.scroll0:
	call .vdp_delay
	in a,(vdp_vram)	; read VRAM
	ld (hl),a			; store byte
	inc hl				; advance pointer
	dec c
	jr nz,.scroll0

	; write to line 0
	ld c,40
	pop hl
	ld de,028h
	sbc hl,de
	ex de,hl
	push de
	ld hl,.linebuf		; Buffer for one line
	ld a,e
	out (vdp_reg),a		; address LSB
	call .vdp_delay
	ld a,040h
	or d
	out (vdp_reg),a		; VDP address loaded
.scroll1:
	ld a,(hl)
	call .vdp_delay
	out (vdp_vram),a
	inc hl
	dec c
	jr nz,.scroll1
						; one line scrolled, next?
	pop hl
	ld de,050h
	add hl,de
	pop bc
	dec c
	jr nz,.scroll2
	ret
	
.vdp_crlf:		; Start a new line
	call .vdp_cr
	call .vdp_lf
	ret

.vdp_bs:
	ld a,(vdp_x)
	dec a
	ld (vdp_x),a
	ld a,'_'
	ld c,a
	call vdp_char
	;out (vdp_vram),a	; Write to the VDP
	ret

vdp_put:

	ret

vdp_char:			; print char in C at coordinates in D:E
	push bc			; save character
	ld hl,0			; clear HL
	ld l,e			
	ld c,d
	ld de,0			; clear DE
	ld a,vdp_cols
	ld e,a
.addloop0:
	add hl,de
	dec c
	jr nz,.addloop0
	ld de,0800h		; offset for VRAM
	add hl,de
	ld a,l
	out (vdp_reg),a
	ld a,040h
	or h
	out (vdp_reg),a	; VDP address loaded
	pop bc
	ld a,c
	out (vdp_vram),a	; Write to the VDP
	;print a cursor
	; ld a,'_'
	; ld c,a
	; out (vdp_vram),a	; Write to the VDP
	ret

vdp_out:
	;call spamon
	ld a,c
	cp a,lf			; linefeed
	jp z,.vdp_lf
	cp a,cr			; carriage return
	jp z,.vdp_cr
	cp a,08h		; Backspace
	jp z,.vdp_bs
					; if we get here, we have a printable character
	ld a,(vdp_x)
	ld e,a
	ld a,(vdp_y)
	ld d,a
	call vdp_char
					; advance cursor position
	;ld a,(vdp_x)
	ld hl,vdp_x
	inc (hl)
	;print a cursor
	;ld a,'_'
	;ld c,a
	;call vdp_char
	ret

.calc_pos:
	ld hl,0
	ld a,(vdp_y)
	or a
	jr z,.addcol
	; HL += (vdp_y) * (vdp_cols)
	ld bc,0
	ld de,0
	ld c,a
	ld a,vdp_cols
	ld e,a
.addloop:
	add hl,de
	dec c
	jr nz,.addloop
.addcol:
	ld a,(vdp_x)
	ld de,0
	ld e,a
	add hl,de
	ret

;screenbuf:
;	ds 960	; 40x24 characters

vdp_vram			equ	080h	; VDP port for accessing the VRAM
vdp_reg				equ	081h	; VDP port for accessing the registers
vdp_lines			equ 23
vdp_cols			equ 40
vdp_x:				db 0
vdp_y:				db 0
;vdp_addr:			dw 0

if VDP_font > 0
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
if .debug_vdp
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
endif
endif

	end
