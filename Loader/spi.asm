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

;############################################################################
; An SPI library suitable for tallking to SD cards.
;
; This library implements SPI mode 0 (SD cards operate on SPI mode 0.)
; Data changes on falling CLK edge & sampled on rising CLK edge:
;        __                                             ___
; /SSEL    \______________________ ... ________________/      Host --> Device
;                 __    __    __   ... _    __    __
; CLK    ________/  \__/  \__/  \__     \__/  \__/  \______   Host --> Device
;        _____ _____ _____ _____ _     _ _____ _____ ______
; MOSI        \_____X_____X_____X_ ... _X_____X_____/         Host --> Device
;        _____ _____ _____ _____ _     _ _____ _____ ______
; MISO        \_____X_____X_____X_ ... _X_____X_____/         Host <-- Device
;
;############################################################################

;############################################################################
; Write 8 bits in C to the SPI port and discard the received data.
; It is assumed that the gpio_out_cache value matches the current state
; of the GP Output port and that SSEL is low.
; This will leave: CLK=1, MOSI=(the LSB of the byte written)
; Clobbers: A, D
;############################################################################

; extern declarations
 extrn puts_crlf
; extrn gpio_out_sd_mosi, gpio_out, gpio_out_sd_clk, 
extrn gpio_out_cache
; extrn gpio_out_sd_clk, gpio_in, gpio_in_sd_miso, gpio_out_sd_ssel

spi_write1:	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	0,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

spi_write8:
	ld	a,(gpio_out_cache)	; get current gpio_out value
	and	0FCh 	;0 + ~(gpio_out_sd_mosi|gpio_out_sd_clk)	; MOSI & CLK = 0
	ld	d,a			; save in D for reuse

	;--------- bit 7
	; special case for the first bit (a already has the gpio_out value)
	bit	7,c			; check the value of the bit to send
	jr	z,.spi_lo7		; if sending 0, then A is already prepared
	or	gpio_out_sd_mosi	; else set the bit to send to 1
.spi_lo7:
	out	(gpio_out),a		; set data value & CLK falling edge together
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	; send the other 7 bits
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	6,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit1		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit1: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	5,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit2		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit2: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	4,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit3		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit3: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	3,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit4		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit4: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	2,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit5		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit5: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	1,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit6		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit6: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	0,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit7		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit7: ;ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge
	

	ret

;############################################################################
; Read 8 bits from the SPI & return it in A.
; MOSI will be set to 1 during all bit transfers.
; This will leave: CLK=1, MOSI=1
; Clobbers A, D and E
; Returns the byte read in the A (and a copy of it also in E)
;############################################################################
spi_read1:
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E

;XXX consider moving this per-byte overhead into the caller????    XXX

spi_read8:
	ld	e,0			; prepare to accumulate the bits into E

	ld	a,(gpio_out_cache)	; get current gpio_out value
	and	~gpio_out_sd_clk	; CLK = 0
	or	gpio_out_sd_mosi	; MOSI = 1
	ld	d,a			; save in D for reuse

	; read the 8 bits
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;7
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;6
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;5
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;4
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;3
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;2
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;1
	ld	a,d
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; set the CLK bit
	out	(gpio_out),a		; CLK rising edge

	in	a,(gpio_in)		; read MISO
	and	gpio_in_sd_miso		; strip all but the MISO bit
	or	e			; accumulate the current MISO value
	rlca				; The LSB is read last, rotate into proper place
					; NOTE: note this only works because gpio_in_sd_miso = 0x80
	ld	e,a			; save a copy of the running value in A and E
		;0

	; The final value will be in both the E and A registers

	ret

;##############################################################
; Assert the select line (set it low)
; This will leave: SSEL=0, CLK=0, MOSI=1
; Clobbers A
;##############################################################
spi_ssel_true:
	push	de

	; read and discard a byte to generate 8 clk cycles
	call	spi_read8

	ld	a,(gpio_out_cache)

	; make sure the clock is low before we enable the card
	and	~gpio_out_sd_clk		; CLK = 0
	or	gpio_out_sd_mosi		; MOSI = 1
	out	(gpio_out),a

	; enable the card
	and	~gpio_out_sd_ssel		; SSEL = 0
	ld	(gpio_out_cache),a		; save current state in the cache
	out	(gpio_out),a

	; generate another 8 clk cycles
	call	spi_read8

	pop	de
	ret

;##############################################################
; de-assert the select line (set it high)
; This will leave: SSEL=1, CLK=0, MOSI=1
; Clobbers A
;
; See section 4 of
;	Physical Layer Simplified Specification Version 8.00
;##############################################################
spi_ssel_false:
	push	de		; save DE because read8 alters it

	; read and discard a byte to generate 8 clk cycles
	call	spi_read8

	ld	a,(gpio_out_cache)

	; make sure the clock is low before we disable the card
	and	~gpio_out_sd_clk			; CLK = 0
	out	(gpio_out),a

	or	gpio_out_sd_ssel|gpio_out_sd_mosi	; SSEL=1, MOSI=1
	ld	(gpio_out_cache),a
	out	(gpio_out),a

	; generate another 16 clk cycles
	call	spi_read8
	call	spi_read8

	pop	de
	ret


if 0
;##############################################################
; Write the message from address in HL register for length in BC to the SPI port.
; Save the returned data into the address in the DE register
;##############################################################
spi_write:
	call	spi_ssel_true

spi_write_loop:
	ld	a,b
	or	c
	jp	z,spi_write_done
	push	bc
	ld	c,(hl)
	call	spi_write8
	inc	hl
	pop	bc
	dec	bc
	jp	spi_write_loop

spi_write_done:
	call	spi_ssel_false

	ret
endif


;##############################################################
; HL = @ of bytes to write
; B = byte count
; clobbers: A, BC, D, HL
;##############################################################
spi_write_str:
	ld	c,(hl)		; get next byte to send
	call	spi_write8	; send it
	inc	hl		; point to the next byte
	djnz	spi_write_str	; count the byte & continue of not done
	ret

