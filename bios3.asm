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

joyport0		equ	0a8h		; I/O port for joystick 0
joyport1		equ	0a9h		; I/O port for joystick 1

					
	public	@ctbl
@ctbl:
	db 	'SIO-A '	; device 0, CRT port 0
	db	0fh
	db 	14
	db 	'SIO-B '	; device 1, LPT port 0
	db 	0fh
	db 	14
	db 	'PRN   '
	db 	2			; 2 -> output only
	db 	0			; 0 -> no baudrate
if VDP
	db 	'CRT   '
	db 	2			; 2 -> output only
	db 	0			; 0 -> no baudrate
endif	
	db	'NONE  '
	db	3			; In+Out
	db	0
	db 	0			; table terminator
					
					
extrn tickr0,tickr1
extrn joy0,joy1

public isrflags
isrflags:
	db 00h

rtc_irq_handler:
	ex af,af'	;'
	exx
if RTC_debug > 0	
	ld a,'.'
	ld c,a
	call con_tx_char
endif
	; get counter and timebase
	ld a,(rtc_limit)
	ld b,a	
	ld a,(rtc_counter)
	; increase counter
	inc a
	; compare to time base
	cp b
	; count a second
	call z,.advance_RTC
	; store counter
	ld (rtc_counter),a
	
	; flip LSB of time base.
	ld a,(rtc_limit)
	xor 01h
	ld (rtc_limit),a
	; use the last bit to branch into two subfunctions
	or 01h
	jr nz,.tick1
	
.tick0:
	ld a,(isrflags)
	and 001h
	jr z,.nojoy0
	; read joy0 here
	in a,(joyport0)
	ld (joy0),a
.nojoy0:
	ld a,(isrflags)
	and 004h
	jr z,.isr_tail
	; call ticker0 here
	ld hl,(tickr0)
	call .tailcall
	jr .isr_tail

.tick1:
	ld a,(isrflags)
	and 002h
	jr z,.nojoy1
	; read joy1 here
	in a,(joyport1)
	ld (joy1),a
.nojoy1:
	ld a,(isrflags)
	and 008h
	jr z,.isr_tail
	; call ticker1 here
	ld hl,(tickr1)
	call .tailcall
		
.isr_tail:		
	exx
	ex af,af'	;'
	ei
	reti

.tailcall:
	jp (hl)

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
	;ld a,(vdp_x)
	;push af
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
	;pop af
	;xor a
	;ld (vdp_x),a
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
	ld a,' '
	ld c,a
	call vdp_char
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
	jp m,.noadd
	jr nz,.addloop0
.noadd:	
	ld de,0800h		; offset for VRAM
	add hl,de
	ld a,l
	out (vdp_reg),a
	ld a,040h
	or h
	out (vdp_reg),a	; VDP address loaded
	pop bc
	ld a,(vdp_rev)
	or a
	jr z,.notrev
	ld a,c
	or 080h
.notrev:	
	ld a,c
	out (vdp_vram),a	; Write to the VDP
	;print a cursor
	; ld a,'_'
	; ld c,a
	; out (vdp_vram),a	; Write to the VDP
	ret

.vdp_ul:
	ld hl,vdp_y
	dec (hl)
	ret

.vdp_fs:
	ld hl,vdp_x
	inc (hl)
	ret
	
.vdp_cls:
	ld hl,0800h		; offset for VRAM
	ld a,l
	out (vdp_reg),a
	ld a,040h
	or h
	out (vdp_reg),a	; VDP address loaded
	ld b,80
	ld c,24
	ld a,' '
.cls:	
	out (vdp_vram),a	; Write to the VDP
	call .vdp_delay
	dec b
	jr nz,.cls
	dec c
	jr nz,.cls
	; fall through into Home
		
.vdp_home:
	xor a
	ld (vdp_x),a
	ld (vdp_y),a
	ret

.vdp_escape:
	ld a,1
	ld (vdp_esc),a
	ret

.term_cmd:
	ld a,(vdp_state)
	or a
	jp z,.state0
	dec a
	jp z,.term_atr
	jp .term_xy
.state0:	
	ld a,c
	cp '='
	jp z,.term_xy
	cp 'G'
	jp z,.term_atr
	ret
	
.term_xy:
	ld a,(vdp_state)
	or a
	jp z,.xy_state0
	dec a
	jp z,.xy_end
	dec a
	jp z,.xy_row
	dec a
	jp z,.xy_col
	ret
.xy_row:
	ld a,c
	sub 32
	ld (vdp_y),a
	ld hl,vdp_state
	inc (hl)
	ret
.xy_col:
	ld a,c
	sub 32
	ld (vdp_x),a
	jp .xy_end
.xy_state0:
	ld a,2
	ld (vdp_state),a
	ret
.xy_end:
	xor a
	ld (vdp_state),a
	ld (vdp_esc),a
	ret

.term_atr:
	ld a,(vdp_state)
	or a
	jp z,.atr_state0
	dec a
	jp nz,.atr_end
	ld a,c
	cp '0'
	jp z,.vdp_mode_def
	cp '4'
	jp z,.vdp_mode_rev
	cp 'P'
	jp z,.vdp_mode_def
	cp 'T'
	jp z,.vdp_mode_rev
	jp .atr_end
.atr_state0:
	ld a,1
	ld (vdp_state),a
	ret
.atr_end:
	xor a
	ld (vdp_state),a
	ld (vdp_esc),a
	ret
.vdp_mode_def:
	xor a
	ld (vdp_rev),a
	ret
.vdp_mode_rev:
	ld a,1
	ld (vdp_rev),a
	ret

	
vdp_out:
	ld a,(vdp_esc)
	or a
	jp nz,.term_cmd
	ld a,c
	cp a,01Bh
	jp z,.vdp_escape
	cp a,00Ah		; linefeed
	jp z,.vdp_lf
	cp a,00Dh		; carriage return
	jp z,.vdp_cr
	cp a,008h		; Backspace
	jp z,.vdp_bs
	cp a,00Bh		; Upline
	jp z,.vdp_ul
	cp a,00Ch		; Forespace
	jp z,.vdp_fs
	cp a,01Ah		; CLS
	jp z,.vdp_cls
	cp a,01Eh		; Home
	jp z,.vdp_home
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

; .calc_pos:
	; ld hl,0
	; ld a,(vdp_y)
	; or a
	; jr z,.addcol
	; ; HL += (vdp_y) * (vdp_cols)
	; ld bc,0
	; ld de,0
	; ld c,a
	; ld a,vdp_cols
	; ld e,a
; .addloop:
	; add hl,de
	; dec c
	; jr nz,.addloop
; .addcol:
	; ld a,(vdp_x)
	; ld de,0
	; ld e,a
	; add hl,de
	; ret


vdp_vram			equ	080h	; VDP port for accessing the VRAM
vdp_reg				equ	081h	; VDP port for accessing the registers
vdp_lines			equ 23
vdp_cols			equ 40
vdp_x:				db 0
vdp_y:				db 0
vdp_esc:			db 0
vdp_state:			db 0
vdp_rev:			db 0

endif

	end
