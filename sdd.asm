; #######################################
; #										#
; #		SD Card Driver for CP/M 3 		#
; #			for Z80 Retro!				#
; #			by SolderGirl				#
; #		based on SPI & SD code  		#
; #			by John Winans				#
; #										#
; #######################################

; Config Equates
true	equ -1
false	equ not true
banked	equ true

.sd_debug 	equ 0
.sd_debug_cmd17 equ	0
.sd_debug_cmd24 equ	0

cr	equ 13
lf	equ 10

	CSEG ; Goes into Common Memory
	
; Equates for ports & bits
gpio_in:			equ 000h		; GP input port
gpio_out:			equ	010h	; GP output port
gpio_out_sd_mosi	equ	001h
gpio_out_sd_clk		equ	002h
gpio_out_sd_ssel	equ	004h
gpio_in_sd_det		equ	040h
gpio_in_sd_miso		equ	080h

; External variables/routines
	extrn ?co,hexdmp,dump_regs,dumpdma,dumpivars, ?bank
	extrn @dtbl,@dma,@trk,@sect,@adrv,@rdrv,@cbnk,@dbnk
	extrn gpiocache,spamon,spamoff,puts,iputs,puts_crlf
	
	extrn disk_dump,dumpdpb,dumpdtbl,hexdump_a


; Public declarations	
	
	public 	SDP0, SDP1, SDP2, SDP3 	; Partition 0..3 in the MBR
	; Following are imaged on partition 0
	; If you use any of these, do NOT use SDP0
	public 	SD0, SD1, SD2, SD3, SD4, SD5, SD6, SD7, SD8, SD9, SD10, SD11
	
	
; Local declarations	
;.dpbvec: dw	.dpb0,.dpb1,.dpb2,.dpb3,0

;	Banking Considerations:
;
; According to the CP/M 3 System Manual Page 48ff
; Disk I/O Code can reside in banked memory page 0
; if and *only* if the system supports direct memory moves accross banks
; we don't have that feature on the Retro!
; But: we may be able to do it anyways. 
; First, we need to have one physical sector worth of buffer in common memory
; Supposedly GENCPM allocates one for us, 
; and we could "blindly" point our routines to @dma,
; like we will do for the non-banked version
; But, we are breaking the rules here. BDOS might order us 
; to write the sector to a different ram bank.
; by having this buffer, we can have all SPI & SD code,
; (and probably the init code, too) in bank 0
; and very little code in common.
; In the non-banked version, this is redundant and would waste 1/2k of precious RAM

.sector_buffer equ 0100h
;	ds 512
;############################################################################
; A buffer for exchanging messages with the SD card.
;############################################################################
.sd_scratch:
	ds	6

; However: We want banking in this driver to be a separate feature.
; The non banked version of the driver should work in either
; banked or non-banked CP/M variants alike, residing fully in common memory
; Then, the code portions that can reside in bank 0 need to be declared "DSEG" *only*
; if banking = true. And all switching back and forth needs to be done very carefully 	
	
;	DPB Considerations:
;
; I'll stick with 8k blocks for compatibility, 
; since linux cpmtools seem to not like the 16k ones
; That leaves us with fixed values 
; (Table 3-4, CP/M 3 System Manual Page 30) for 
; BLS = 8192
; BSH = 6
; BLM = 63
; SPT = number of 128 byte records per track
;		SPT needs to be >4 because one SD block is 4x128=512 bytes
;		if we set SPT = 64, each track will be one 8192k logical block
;		TRK(65536) * 8k = 512 MB max. but we are limited to 256MB anyways
; DSM = number of blocks on the partition -1 
;		(in other words: the offset of the last block from the start of the partition)
;		DSM must be <=7FFFh (according to CP/M 3 System Manual Page 31)
;		That means, with DSM(8000h) and BLS(8192)
;		our maximum partition size will be 256 MB
; CKS 	will always be set to 8000h to disable checksumming
;		this declares the disk as not removable
; EXM 	would be 3 for DSM values below 256.
;		But: DSM(255) * BLS(8192) = 2040kB
;		We will just not support partitions smaller than 2MB and thus: 
;		EXM will be fixed to 7 (according to Table 3-5, CP/M 3 System Manual Page 30)
; DRM = Number of possible directory entries
;		since we will be dealing with relatively large partitions, 
;		we want this to be as big as possible therefore, 
; AL0 & AL1 will both be 0FFh
;		That reserves 16 * 8k Blocks for directory entries of 32 bytes each, resulting in
;		4095 max files per drive (have fun with that DIR on a TTY :-)
; PSH = 2	PSH & PHM are determined by the physical sector size of the SD card, 512 bytes per sector
; PHM = 3 	(Table 3-7, CP/M 3 System Manual Page 32)
; OFF = reserved tracks for bootloader. We want 16k here, and since 1 track = 8k, we use 2 tracks
;
; after careful consideration, it turns out the only parameter to be determined by the init function is:
; DSM = 8k Blocks on drive
; LBA = 512 B Blocks on drive
; DSM = (LBA/16)-1
;
; BEWARE !!!
; Enabling hashtables requires 16k (!) RAM per drive!
; For obvious reasons, this option should only be used
; in a banked system. With 32k banks, we will need 1 bank per 2 drives.


    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  0           ; Unit. This will be passed back to us as @rdrv
    db  0           ; Type. can be used for arbitrary information
SDP0:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb0     	; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  1           ; Unit. This will be passed back to us as @rdrv
    db  0           ; Type. can be used for arbitrary information
SDP1:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb1     	; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  2           ; Unit. This will be passed back to us as @rdrv
    db  0           ; Type. can be used for arbitrary information
SDP2:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb2     	; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  3           ; Unit. This will be passed back to us as @rdrv
    db  0           ; Type. can be used for arbitrary information
SDP3:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb3     	; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

; DPB Block for SDP0
; Each XDPH needs its own separate DPB
; So we can construct that during the init/login routine
; NOTE:
; we set DSM to the max possible value here, 
; as a worst case scenario for GENCPM to use.
; The actual value will be determined by init 

				; add 2 words in front of the DPB
				; this is where init will store
	dw 0,0		; the 32 bit partition start LBA
.dpb0:		
    dw  64      ; fixed	SPT (8k per Track)
    db  6       ; fixed	BSH
    db  63      ; fixed	BLM
    db  7       ; fixed	EXM
    dw  7FFFh   ; 		DSM (max allocation block number)
    dw  1023    ; fixed	DRM (max directory entries)
    db  0F0h    ; fixed AL0 (blocks for directory)
    db  000h    ; fixed	AL1 (16*8k/32=4096)
    dw  8000h   ; fixed	CKS (non removable drive)
    dw  2      	; fixed	OFF (16k reserved for CPMLDR)
    db  2       ; fixed	PSH
    db  3       ; fixed	PHM
	
				; add 2 words in front of the DPB
				; this is where init will store
	dw 0,0		; the 32 bit partition start LBA
.dpb1:		
    dw  64      ; fixed	SPT (8k per Track)
    db  6       ; fixed	BSH
    db  63      ; fixed	BLM
    db  7       ; fixed	EXM
    dw  7FFFh   ; 		DSM (max allocation block number)
    dw  1023    ; fixed	DRM (max directory entries)
    db  0F0h    ; fixed AL0 (blocks for directory)
    db  000h    ; fixed	AL1 (16*8k/32=4096)
    dw  8000h   ; fixed	CKS (non removable drive)
    dw  2      	; fixed	OFF (16k reserved for CPMLDR)
    db  2       ; fixed	PSH
    db  3       ; fixed	PHM

				; add 2 words in front of the DPB
				; this is where init will store
	dw 0,0		; the 32 bit partition start LBA
.dpb2:		
    dw  64      ; fixed	SPT (8k per Track)
    db  6       ; fixed	BSH
    db  63      ; fixed	BLM
    db  7       ; fixed	EXM
    dw  7FFFh   ; 		DSM (max allocation block number)
    dw  1023    ; fixed	DRM (max directory entries)
    db  0F0h    ; fixed AL0 (blocks for directory)
    db  000h    ; fixed	AL1 (16*8k/32=4096)
    dw  8000h   ; fixed	CKS (non removable drive)
    dw  2      	; fixed	OFF (16k reserved for CPMLDR)
    db  2       ; fixed	PSH
    db  3       ; fixed	PHM

				; add 2 words in front of the DPB
				; this is where init will store
	dw 0,0		; the 32 bit partition start LBA
.dpb3:		
    dw  64      ; fixed	SPT (8k per Track)
    db  6       ; fixed	BSH
    db  63      ; fixed	BLM
    db  7       ; fixed	EXM
    dw  7FFFh   ; 		DSM (max allocation block number)
    dw  1023    ; fixed	DRM (max directory entries)
    db  0F0h    ; fixed AL0 (blocks for directory)
    db  000h    ; fixed	AL1 (16*8k/32=4096)
    dw  8000h   ; fixed	CKS (non removable drive)
    dw  2      	; fixed	OFF (16k reserved for CPMLDR)
    db  2       ; fixed	PSH
    db  3       ; fixed	PHM

	;ds 256
.priv_stack equ 0FFFDh
	;dw 0
.old_stk:	; Beware: Stack overflow will overwrite old stack pointer
	dw 0
	
.initialized:	db 0	

; Fixed offset values for the 15 adressable portions of the first partition
; Warning: Never use these drives concurrently with SDP0 !!

; init must add the partition offset 
.drv_offset:
    dw  0000h, 00000h   ; SD Partition 0
    dw  0000h, 00000h   ; SD Partition 1
    dw  0000h, 00000h   ; SD Partition 2
    dw  0000h, 00000h   ; SD Partition 3
    dw  0000h, 00000h   ; SD logical drive 0  A: (0 .. 3)	drv_offset +8
    dw  0000h, 04000h   ; SD logical drive 1  B: (4 .. 7)
    dw  0000h, 08000h   ; SD logical drive 2  C: (8 ..11)
    dw  0000h, 0c000h   ; SD logical drive 3  D: (12..15)
    dw  0001h, 00000h   ; SD logical drive 4  E: (16..19)
    dw  0001h, 04000h   ; SD logical drive 5  F: (20..23)
    dw  0001h, 08000h   ; SD logical drive 6  G: (24..27)
    dw  0001h, 0c000h   ; SD logical drive 7  H: (28..31)
    dw  0002h, 00000h   ; SD logical drive 8  I: (32..35)
    dw  0002h, 04000h   ; SD logical drive 9  J: (36..39)
    dw  0002h, 08000h   ; SD logical drive 10  K: (40..43)
    dw  0002h, 0c000h   ; SD logical drive 11  L: (44..47)

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  4           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD0:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  5           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD1:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  6           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD2:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  7           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD3:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  8           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD4:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  9           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD5:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  10          ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD6:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  11           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD7:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  12          ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD8:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  13           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD9:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  14           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD10:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

    dw  .sdcard_write
    dw  .sdcard_read
    dw  .sdcard_login
    dw  .sdcard_init
    db  15           ; unit. This will determine the image offset
    db  1           ; Type. We will use this to distinguish partition from image adressing
SD11:
    dw  0           ; XLT sector translation table (no xlation done)
    dw  0, 0, 0, 0  ; scratchpad
    db  0           ; scratch
    db  0           ; media flag
    dw  .dpb_8k     ; DPB pointer
    dw  0FFFEh      ; CSV pointer will be created by GENCPM
    dw  0FFFEh      ; ALV pointer will be created by GENCPM
    dw  0FFFEh      ; DIRBCB pointer will be created by GENCPM
    dw  0FFFEh      ; DTABCB pointer will be created by GENCPM
    dw  0FFFEh      ; HASH determined by GENCPM
    dw  0FFFEh      ; HBANK determined by GENCPM

; These drives all share a common DPB

.dpb_8k:		
    dw  64      ; fixed	SPT (8k per Track)
    db  6       ; fixed	BSH
    db  63      ; fixed	BLM
    db  7       ; fixed	EXM
    dw  1021   ; 		DSM (max allocation block number)
    dw  511    ; fixed	DRM (max directory entries)
    db  0C0h    ; fixed AL0 (blocks for directory)
    db  000h    ; fixed	AL1 (16*8k/32=4096)
    dw  8000h   ; fixed	CKS (non removable drive)
    dw  2      	; fixed	OFF (16k reserved for CPMLDR)
    db  2       ; fixed	PSH
    db  3       ; fixed	PHM
	
.sdcard_write:
	; Switch to our internal Stack
	ld (.old_stk),sp
	ld sp,.priv_stack
	
if .sd_debug 
	call iputs
	db 'WRITE',cr,lf,0
endif
	; Prepare all parameters
	call .calc_sd_block
	; Pass them through the Stack
    push    de          ; 32-bit SD block number (big end)
    push    hl          ; 32-bit SD block number (little end)
    ld  de,(@dma)       ; DE = target buffer to read the 512-byte block
	; And call the Write routine
if banked
	ld a,(@dbnk)
	call ?bank
endif
	;di
    call    sd_cmd24        ; write the SD block
	;ei
if banked
	push af
	ld a,(@cbnk)
	call ?bank
	pop af
endif

    pop hl          ; clean the SD block number from the stack
    pop de

    or  a
    jr  z,.write_ret

    call    iputs
    db  'BIOS_WRITE SD CARD WRITE FAILED!',cr,lf,0
    ld  a,1         ; tell CP/M the read failed

.write_ret:
	; Restore old stack
	ld sp,(.old_stk)
	ret

.sdcard_read:
	; Switch to our internal Stack
	ld (.old_stk),sp
	ld sp,.priv_stack

if .sd_debug >= 1
	call iputs
	db 'READ',cr,lf,0
endif

	; Prepare all parameters
	call .calc_sd_block		; DE:HL now LBA block address
	; Pass them through the Stack
    push    de          ; 32-bit SD block number (big end)
    push    hl          ; 32-bit SD block number (little end)
    ld  de,(@dma)       ; DE = target buffer to read the 512-byte block
	; And call the Read routine

if banked
	ld a,(@dbnk)
	call ?bank
endif
	;di
    call    sd_cmd17        ; read the SD block
	;ei
if banked
	push af
	ld a,(@cbnk)
	call ?bank
	pop af
endif
    pop hl          ; clean the SD block number from the stack
    pop de
	
	or  a           ; was the SD driver read OK?
    jr  z,.read_ret

    call    iputs
    db  'BIOS_READ FAILED!',cr,lf,0
    ld  a,0FFh         ; tell CP/M the read failed

.read_ret:
if .sd_debug >= 1
	push af
	call dumpdma
	call iputs
	db '/READ',cr,lf,0
	;call dumpivars
	;call puts_crlf
	pop af
endif
	; Restore old stack
	ld sp,(.old_stk)
	ret

.sdcard_login:
if .sd_debug >= 1
	call dumpivars
	call puts_crlf
endif
	; Nothing to be done for login
	ret


	
; We need to read the partition table at least once
; And properly decode the entries.
; We can probably get away with doing all that during init(0) 
; For each drive, we need to find the size and set the DPB
; 
; MBR = Sector 0 from the SD Card
; PTBL = Offset 0x01BE 		The first entry in the Table

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
;
; An SD card library suitable for talking to SD cards in SPI mode 0.
;
; XXX As of 2022-02-14, this code has been tested on house-brand (Inland)
; MicroCenter SD version 2.0 SDHC cards:
;	https://www.microcenter.com/product/486146/micro-center-16gb-microsdhc-card-class-10-flash-memory-card-with-adapter
;
; WARNING: SD cards are 3.3v ONLY!
; Must provide a pull up on MISO to 3.3V.
; SD cards operate on SPI mode 0.
;
; References:
; - SD Simplified Specifications, Physical Layer Simplified Specification, 
;   Version 8.00:    https://www.sdcard.org/downloads/pls/
;
; The details on operating an SD card in SPI mode can be found in 
; Section 7 of the SD specification, p242-264.
;
; To initialize an SDHC/SDXC card:
; - send at least 74 CLKs
; - send CMD0 & expect reply message = 0x01 (enter SPI mode)
; - send CMD8 (establish that the host uses Version 2.0 SD SPI protocol)
; - send ACMD41 (finish bringing the SD card on line)
; - send CMD58 to verify the card is SDHC/SDXC mode (512-byte block size)
;
; At this point the card is on line and ready to read and write 
; memory blocks.
;
; - use CMD17 to read one 512-byte block
; - use CMD24 to write one 512-byte block
;
;############################################################################


;############################################################################
; CMD17 (READ_SINGLE_BLOCK)
;
; Read one block given by the 32-bit (little endian) number at 
; the top of the stack into the buffer given by address in DE.
;
; - set SSEL = true
; - send command
; - read for CMD ACK
; - wait for 'data token'
; - read data block
; - read data CRC
; - set SSEL = false
;
; A = 0 if the read operation was successful. Else A=1
; Clobbers A, IX
;############################################################################


sd_cmd17:
					; +10 = &block_number
					; +8 = return @
	push	bc			; +6
	push	hl			; +4
	push	iy			; +2
	push	de			; +0 target buffer address

	ld	iy,.sd_scratch	; iy = buffer to format command
	ld	ix,10			; 10 is the offset from sp to the location of the block number
	add	ix,sp			; ix = address of uint32_t sd_lba_block number

	ld	(iy+0),17|040h		; the command byte
	ld	a,(ix+3)		; stack = little endian
	ld	(iy+1),a		; cmd_buffer = big endian
	ld	a,(ix+2)
	ld	(iy+2),a
	ld	a,(ix+1)
	ld	(iy+3),a
	ld	a,(ix+0)
	ld	(iy+4),a
	ld	(iy+5),000h|001h	; the CRC byte

if .sd_debug_cmd17 >= 1
	; print the comand buffer
	call	iputs
	db	'CMD17: ',0
	push	iy
	pop	hl			; HL = IY = cmd_buffer address
	ld	bc,6			; B = command buffer length
	ld	e,0
	call	hexdmp

	; print the target address
	call	iputs
	db	'  Target: ',0

	pop	de			; restore DE = target buffer address
	push	de			; and keep it on the stack too

	ld	a,d
	call	hexdump_a
	ld	a,e
	call	hexdump_a
	call	puts_crlf
endif

	; assert the SSEL line
	call    spi_ssel_true

	; send the command 
	push	iy
	pop	hl			; HL = IY = cmd_buffer address
	ld	b,6			; B = command buffer length
	call    spi_write_str		; clobbers A, BC, D, HL

	; read the R1 response message
	call    .sd_read_r1		; clobbers A, B, DE

if .sd_debug_cmd17 >= 1
	push	af
	call	iputs
	db	'  R1: ',0
	pop	af
	push	af
	call	hexdump_a
	call	puts_crlf
	pop	af
endif

	; If R1 status != SD_READY (0x00) then error (SD spec p265, Section 7.2.3)
	or	a			; if (a == 0x00) then is OK
	jr	z,.sd_cmd17_r1ok

	; print the R1 status byte
	push	af
	call	iputs
	db	cr,lf,'SD CMD17 R1 error = 0x',0
	pop	af
	push af
	call	hexdump_a
	call	puts_crlf
	pop af
	jp	.sd_cmd17_err


.sd_cmd17_r1ok:

	; read and toss bytes while waiting for the data token
	ld      bc,01000h		; expect to wait a while for a reply
.sd_cmd17_loop:
	call    spi_read8		; (clobbers A, DE)
	cp	0ffh			; if a=0xff then command is not yet completed
	jr      nz,.sd_cmd17_token
	dec	bc
	ld	a,b
	or	c
	jr	nz,.sd_cmd17_loop

	call	iputs
	db	'SD CMD17 data timeout',cr,lf,0
	jp	.sd_cmd17_err		; no flag ever arrived

.sd_cmd17_token:
	cp	0feh			; A = data block token? (else is junk from the SD)
	jr	z,.sd_cmd17_tokok

	push	af
	call	iputs
	db	'SD CMD17 invalid response token: 0x',0
	pop	af
	push af
	call	hexdump_a
	call	iputs
	db	cr,lf,0
	pop af	; leave with the error code still in AF
	jp	.sd_cmd17_err

.sd_cmd17_tokok:
	pop	hl			; HL = target buffer address
	push	hl			; and keep the stack level the same
	ld	bc,0200h		; 512 bytes to read
.sd_cmd17_blk:
	call	spi_read8		; Clobbers A, DE
	ld	(hl),a
	inc	hl			; increment the buffer pointer
	dec	bc			; decrement the byte counter

if .sd_debug_cmd17 >= 2
	; A VERY verbose dump of every byte as they are read in from the card
	push	bc
	call	hexdump_a
	ld	c,' '
	call	con_tx_char
	pop	bc
	push	bc
	ld	a,c			; if %16 then 
	and	00fh
	jr	nz,.sd_cmd17_dsp
	ld	c,cr;
	call	con_tx_char
	ld	c,lf
	call	con_tx_char
.sd_cmd17_dsp:
	pop	bc
endif

	ld	a,b			; did BC reach zero?
	or	c
	jr	nz,.sd_cmd17_blk	; if not, go back & read another byte

	call	spi_read8		; read the CRC value (XXX should check this)
	call	spi_read8		; read the CRC value (XXX should check this)

	call    spi_ssel_false
	xor	a			; A = 0 = success!

.sd_cmd17_done:
	pop	de
	pop	iy
	pop	hl
	pop	bc
	ret

.sd_cmd17_err:
	push af	; save the error code
	call	spi_ssel_false
	pop af
	push af
	bit 3,a
	jr z,.not_oor
	call iputs
	db 'SD CMD17: Out of Range',cr,lf,0
	pop af
	push af

.not_oor:	
	bit 2,a
	jr z,.not_ecc
	call iputs
	db 'SD CMD17: ECC Failed',cr,lf,0
	pop af
	push af

.not_ecc:	
	bit 1,a
	jr z,.not_cc
	call iputs
	db 'SD CMD17: CC Error',cr,lf,0
	pop af
	
.not_cc:
	ld	a,001h		; return an error flag
	jr	.sd_cmd17_done


;############################################################################
; CMD24 (WRITE_BLOCK)
;
; Write one block given by the 32-bit (little endian) number at 
; the top of the stack from the buffer given by address in DE.
;
; - set SSEL = true
; - send command
; - read for CMD ACK
; - send 'data token'
; - write data block
; - wait while busy 
; - read 'data response token' (must be 0bxxx00101 else errors) (see SD spec: 7.3.3.1, p281)
; - set SSEL = false
;
; - set SSEL = true		
; - wait while busy		Wait for the write operation to complete.
; - set SSEL = false
;
; XXX This /should/ check to see if the block address was valid 
; and that there was no write protect error by sending a CMD13
; after the long busy wait has completed.
;
; A = 0 if the write operation was successful. Else A = 1.
; Clobbers A, IX
;############################################################################

sd_cmd24:
					; +10 = &block_number
					; +8 = return @
	push	bc			; +6
	push	de			; +4 target buffer address
	push	hl			; +2
	push	iy			; +0

	ld	iy,.sd_scratch		; iy = buffer to format command
	ld	ix,10			; 10 is the offset from sp to the location of the block number
	add	ix,sp			; ix = address of uint32_t sd_lba_block number

.sd_cmd24_len: equ	6

	ld	(iy+0),24|040h		; the command byte
	ld	a,(ix+3)		; stack = little endian
	ld	(iy+1),a		; cmd_buffer = big endian
	ld	a,(ix+2)
	ld	(iy+2),a
	ld	a,(ix+1)
	ld	(iy+3),a
	ld	a,(ix+0)
	ld	(iy+4),a
	ld	(iy+5),000h|001h	; the CRC byte

if .sd_debug_cmd24
	push	de
	; print the command buffer
	call	iputs
	db	'  CMD24: ',0
	push	iy
	pop	hl			; hl = &cmd_buffer
	ld	bc,.sd_cmd24_len
	ld	e,0
	call	hexdmp

	; print the target address
	call	iputs
	db	'  CMD24: source: ',0
	ld	a,(ix-5)
	call	hexdump_a
	ld	a,(ix-6)
	call	hexdump_a
	call	puts_crlf
	pop	de
endif

	; assert the SSEL line
	call    spi_ssel_true

	; send the command 
	push	iy
	pop	hl			; hl = iy = &cmd_buffer
	ld	b,.sd_cmd24_len
	call	spi_write_str		; clobbers A, BC, D, HL

	; read the R1 response message
	call    .sd_read_r1		; clobbers A, B, DE

	; If R1 status != SD_READY (0x00) then error
	or	a			; if (a == 0x00) 
	jr	z,.sd_cmd24_r1ok	; then OK
					; else error...

	push	af
	call	iputs
	db	'SD CMD24 status = ',0
	pop	af
	call	hexdump_a
	call	iputs
	db	' != SD_READY',cr,lf,0
	jp	.sd_cmd24_err		; then error


.sd_cmd24_r1ok:
	; give the SD card an extra 8 clocks before we send the start token
	call	spi_read8

	; send the start token: 0xfe
	ld	c,0feh
	call	spi_write8		; clobbers A and D

	; send 512 bytes

	ld	l,(ix-6)		; HL = source buffer address
	ld	h,(ix-5)
	ld	bc,0200h		; BC = 512 bytes to write
.sd_cmd24_blk:
	push	bc			; XXX speed this up
	ld	c,(hl)
	call	spi_write8		; Clobbers A and D
	inc	hl
	pop	bc			; XXX speed this up
	dec	bc
	ld	a,b
	or	c
	jr	nz,.sd_cmd24_blk

	; read for up to 250msec waiting on a completion status

	ld	bc,0f000h		; wait a potentially /long/ time for the write to complete
.sd_cmd24_wdr:				; wait for data response message
	call	spi_read8		; clobber A, DE
	cp	0ffh
	jr	nz,.sd_cmd24_drc
	dec	bc
	ld	a,b
	or	c
	jr	nz,.sd_cmd24_wdr

	call    iputs
	db	'SD CMD24 completion status timeout!',cr,lf,0
	jp	.sd_cmd24_err	; timed out


.sd_cmd24_drc:
	; Make sure the response is 0bxxx00101 else is an error
	and	01fh
	cp	005h
	jr	z,.sd_cmd24_ok


	push	bc
	call	iputs
	db	'ERROR: SD CMD24 completion status != 0x05 (count=',0
	pop	bc
	push	bc
	ld	a,b
	call	hexdump_a
	pop	bc
	ld	a,c
	call	hexdump_a
	call	iputs
	db	')',cr,lf,0

	jp	.sd_cmd24_err

.sd_cmd24_ok:
	call	spi_ssel_false


if 1
	; Wait until the card reports that it is not busy
	call	spi_ssel_true

.sd_cmd24_busy:
	call	spi_read8		; clobber A, DE
	cp	0ffh
	jr	nz,.sd_cmd24_busy

	call	spi_ssel_false
endif
	xor	a			; A = 0 = success!

.sd_cmd24_done:
	pop	iy
	pop	hl
	pop	de
	pop	bc
	ret

.sd_cmd24_err:
    call    spi_ssel_false

if .sd_debug_cmd24
	call	iputs
	db	'SD CMD24 write failed!',cr,lf,0
endif

	ld	a,001h		; return an error flag
	jr	.sd_cmd24_done


;############################################################################
; NOTE: Response message formats in SPI mode are different than in SD mode.
;
; Read bytes until we find one with MSB = 0 or bail out retrying.
; Return last read byte in A (and a copy also in E)
; Calls spi_read8 (see for clobbers)
; Clobbers A, B, DE
;############################################################################
.sd_read_r1:
	ld	b,0f0h		; B = number of retries
.sd_r1_loop:
	call	spi_read8	; read a byte into A (and a copy in E as well)
	and	080h		; Is the MSB set to 1?
	jr	z,.sd_r1_done	; If MSB=0 then we are done
	djnz	.sd_r1_loop	; else try again until the retry count runs out
.sd_r1_done:
	ld	a,e		; copy the final value into A
	ret


;############################################################################
; NOTE: Response message formats in SPI mode are different than in SD mode.
;
; Read an R7 message into the 5-byte buffer pointed to by HL.
; Clobbers A, B, DE, HL
;############################################################################
.sd_read_r7:
	call	.sd_read_r1	; A = byte #1
	ld	(hl),a		; save it
	inc	hl		; advance receive buffer pointer
	call	spi_read8	; A = byte #2
	ld	(hl),a		; save it
	inc	hl		; advance receive buffer pointer
	call	spi_read8	; A = byte #3
	ld	(hl),a		; save it
	inc	hl		; advance receive buffer pointer
	call	spi_read8	; A = byte #4
	ld	(hl),a		; save it
	inc	hl		; advance receive buffer pointer
	call	spi_read8	; A = byte #5
	ld	(hl),a		; save it
	ret


;############################################################################
; SSEL = HI (deassert)
; wait at least 1 msec after power up
; send at least 74 (80) SCLK rising edges
; Clobbers A, DE, B
;############################################################################
sd_boot:
	ld	b,10		; 10*8 = 80 bits to read
.sd_boot1:
	call	spi_read8	; read 8 bits (causes 8 CLK xitions)
	djnz	.sd_boot1	; if not yet done, do another byte
	ret


;############################################################################
; Send a command and read an R1 response message.
; HL = command buffer address
; B = command byte length
; Clobbers A, BC, DE, HL
; Returns A = reply message byte
;
; Modus operandi
; SSEL = LO (assert)
; send CMD
; send arg 0
; send arg 1
; send arg 2
; send arg 3
; send CRC 
; wait for reply (MSB=0)
; read reply
; SSEL = HI
;############################################################################
.sd_cmd_r1:
	; assert the SSEL line
	call    spi_ssel_true

	; write a sequence of bytes represending the CMD message
	call    spi_write_str		; write B bytes from HL buffer @

	; read the R1 response message
	call    .sd_read_r1		; A = E = message response byte

	; de-assert the SSEL line
	call    spi_ssel_false

	ld	a,e
	ret


;############################################################################
; Send a command and read an R7 response message.
; Note that an R3 response is the same size, so can use the same code.
; HL = command buffer address
; B = command byte length
; DE = 5-byte response buffer address
; Clobbers A, BC, DE, HL
;############################################################################
.sd_cmd_r3:
.sd_cmd_r7:
	call    spi_ssel_true

	push	de			; save the response buffer @
	call    spi_write_str		; write cmd buffer from HL, length=B

	; read the response message into buffer @ in HL
	pop	hl			; pop the response buffer @ HL
	call    .sd_read_r7

	; de-assert the SSEL line
	call    spi_ssel_false

	ret


;############################################################################
; Send a CMD55 (APP_CMD) message and read an R1 response.
; CMD55 is used to notify the card that the following message is an ACMD 
; (as opposed to a regular CMD.)
; Clobbers A, BC, DE, HL
; Return the 1-byte response in A
;############################################################################
sd_cmd55:
	ld	hl,.sd_cmd55_buf	; HL = buffer to write
	ld	b,.sd_cmd55_len	; B = buffer byte count
	call	.sd_cmd_r1	; write buffer, A = R1 response byte

if .sd_debug > 1
	push	af
	call	iputs
	db	'CMD55: ',0
	ld	hl,.sd_cmd55_buf
	ld	bc,.sd_cmd55_len
	ld	e,0
	call	hexdmp
	call	iputs
	db	'  R1: ',0
	pop	af
	push	af
	call	hexdump_a	; dump the response byte
	call    puts_crlf
	pop	af
endif

	ret

.sd_cmd55_buf:	db	55|040h,0,0,0,0,000h|001h
.sd_cmd55_len:	equ	$-.sd_cmd55_buf


;############################################################################
; Send a ACMD41 (SD_SEND_OP_COND) message and return an R1 response byte in A.
;
; The main purpose of ACMD41 to set the SD card state to READY so
; that data blocks may be read and written.  It can fail if the card
; is not happy with the operating voltage.
;
; Clobbers A, BC, DE, HL
; Note that A-commands are prefixed with a CMD55.
;############################################################################
sd_acmd41:
	call	sd_cmd55		; send the A-command prefix

	ld	hl,.sd_acmd41_buf	; HL = command buffer
	ld	b,.sd_acmd41_len	; B = buffer byte count
	call	.sd_cmd_r1

if .sd_debug > 1
	push	af
	call	iputs
	db	'ACMD41: ',0
	ld	hl,.sd_acmd41_buf
	ld	bc,.sd_acmd41_len
	ld	e,0
	call	hexdmp
	call	iputs
	db	'   R1: ',0
	pop	af
	push	af
	call	hexdump_a	; dump the status byte
	call    puts_crlf
	pop	af
endif

	ret


; SD spec p263 Fig 7.1 footnote 1 says we want to set the HCS bit here for HC/XC cards.
; Notes on Internet about setting the supply voltage in ACMD41. But not in SPI mode?
; The folowing works on my MicroCenter SDHC cards:

.sd_acmd41_buf:	db	41|040h,040h,0,0,0,000h|001h	; Note the HCS flag is set here
.sd_acmd41_len:	equ	$-.sd_acmd41_buf




;############################################################################
; A hack to just supply clock for a while.
;############################################################################
.sd_clk_dly:
	push	de
	push	hl
	ld	hl,080h
.sd_clk_dly1:
	call	spi_read8
	dec	hl
	ld	a,h
	or	l
	jr	nz,.sd_clk_dly1
	pop	hl
	pop	de
	ret
	

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


spi_write8:
	ld	a,(gpiocache)	; get current gpio_out value
	and	0fch	; MOSI & CLK = 0
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
	;spi_write1	6
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	6,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit6		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit6: ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	;spi_write1	5
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	5,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit5		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit5: ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	;spi_write1	4
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	4,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit4		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit4: ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	;spi_write1	3
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	3,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit3		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit3: ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	;spi_write1	2
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	2,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit2		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit2: ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	;spi_write1	1
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	1,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit1		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit1: ds 0				; for some reason, these labels disappear if there is no neumonic
	out	(gpio_out),a		; set data value & CLK falling edge
	or	gpio_out_sd_clk		; ready the CLK to send a 1
	out	(gpio_out),a		; set the CLK's rising edge

	;spi_write1	0
	ld	a,d			; a = gpio_out value w/CLK & MOSI = 0
	bit	0,c		; [8] is the bit of C a 1?
	jr	z,.lo_bit0		; [7/12] (transmit a 0)
	or	gpio_out_sd_mosi	; [7] prepare to transmit a 1 (only [4] if mask is in a reg)
.lo_bit0: ds 0				; for some reason, these labels disappear if there is no neumonic
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

spi_read8:
	ld	e,0			; prepare to accumulate the bits into E

	ld	a,(gpiocache)	; get current gpio_out value
	and	~gpio_out_sd_clk	; CLK = 0
	or	gpio_out_sd_mosi	; MOSI = 1
	ld	d,a			; save in D for reuse

	; read the 8 bits
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

	ld	a,(gpiocache)

	; make sure the clock is low before we enable the card
	and	 NOT gpio_out_sd_clk		; CLK = 0
	or	gpio_out_sd_mosi		; MOSI = 1
	out	(gpio_out),a

	; enable the card
	and	NOT gpio_out_sd_ssel		; SSEL = 0
	ld	(gpiocache),a		; save current state in the cache
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

	ld	a,(gpiocache)

	; make sure the clock is low before we disable the card
	and	 NOT gpio_out_sd_clk			; CLK = 0
	out	(gpio_out),a

	or	gpio_out_sd_ssel OR gpio_out_sd_mosi	; SSEL=1, MOSI=1
	ld	(gpiocache),a
	out	(gpio_out),a

	; generate another 16 clk cycles
	call	spi_read8
	call	spi_read8

	pop	de
	ret


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

if banked
	DSEG
endif
	  ;This was made by Runer112
;Tested by jacobly
mul16:
;BC*DE --> DEHL
; ~544.887cc as calculated in jacobly's test
;min: 214cc  (DE = 1)
;max: 667cc
;avg: 544.4507883cc   however, deferring to jacobly's result as mine may have math issues ?
    ld    a,d
    ld    d,0
    ld    h,b
    ld    l,c
    add    a,a
    jr    c,mul_14
    add    a,a
    jr    c,mul_13
    add    a,a
    jr    c,mul_12
    add    a,a
    jr    c,mul_11
    add    a,a
    jr    c,mul_10
    add    a,a
    jr    c,mul_9
    add    a,a
    jr    c,mul_8
    add    a,a
    jr    c,mul_7
    ld    a,e
     and    11111110b
    add    a,a
    jr    c,mul_6
    add    a,a
    jr    c,mul_5
    add    a,a
    jr    c,mul_4
    add    a,a
    jr    c,mul_3
    add    a,a
    jr    c,mul_2
    add    a,a
    jr    c,mul_1
    add    a,a
    jr    c,mul_0
    rr    e
    ret    c
    ld    h,d
    ld    l,e
    ret
mul_14:
    add    hl,hl
    adc    a,a
    jr    nc,mul_13
    add    hl,bc
    adc    a,d
mul_13:
    add    hl,hl
    adc    a,a
    jr    nc,mul_12
    add    hl,bc
    adc    a,d
mul_12:
    add    hl,hl
    adc    a,a
    jr    nc,mul_11
    add    hl,bc
    adc    a,d
mul_11:
    add    hl,hl
    adc    a,a
    jr    nc,mul_10
    add    hl,bc
    adc    a,d
mul_10:
    add    hl,hl
    adc    a,a
    jr    nc,mul_9
    add    hl,bc
    adc    a,d
mul_9:
    add    hl,hl
    adc    a,a
    jr    nc,mul_8
    add    hl,bc
    adc    a,d
mul_8:
    add    hl,hl
    adc    a,a
    jr    nc,mul_7
    add    hl,bc
    adc    a,d
mul_7:
    ld    d,a
    ld    a,e
    and    11111110b
    add    hl,hl
    adc    a,a
    jr    nc,mul_6
    add    hl,bc
    adc    a,0
mul_6:
    add    hl,hl
    adc    a,a
    jr    nc,mul_5
    add    hl,bc
    adc    a,0
mul_5:
    add    hl,hl
    adc    a,a
    jr    nc,mul_4
    add    hl,bc
    adc    a,0
mul_4:
    add    hl,hl
    adc    a,a
    jr    nc,mul_3
    add    hl,bc
    adc    a,0
mul_3:
    add    hl,hl
    adc    a,a
    jr    nc,mul_2
    add    hl,bc
    adc    a,0
mul_2:
    add    hl,hl
    adc    a,a
    jr    nc,mul_1
    add    hl,bc
    adc    a,0
mul_1:
    add    hl,hl
    adc    a,a
    jr    nc,mul_0
    add    hl,bc
    adc    a,0
mul_0:
    add    hl,hl
    adc    a,a
    jr    c,mul_fc
    rr    e
    ld    e,a
    ret    nc
    add    hl,bc
    ret    nc
    inc    e
    ret    nz
    inc    d
    ret
mul_fc:
    inc    d
    rr    e
    ld    e,a
    ret    nc
    add    hl,bc
    ret    nc
    inc    e
    ret

.sdcard_init:
if .sd_debug >= 2
	call iputs
	db 'INIT: ',0
endif

	; Switch to our internal Stack
	ld (.old_stk),sp
	ld sp,.priv_stack

	; Check our flag to see if the main init has already been done
	ld a,(.initialized)
	or a
	jp nz,.init0		; main init done already
	
	call sd_reset		; Just to make sure everything is in a clean state
if .sd_debug >= 2
	push af
	call dump_regs
	pop af
endif
	call .load_ptbl		; read the partition table once
if .sd_debug >= 2
	push af
	call dump_regs
	pop af
endif
	or a
	jp nz,.init_err		; fail horribly

	ld a,1
	ld (.initialized),a ; and not do this again

if .sd_debug >= 2
	call iputs
	db 'FIRST ',0
	call dump_regs
endif

	; Unify: One Pstart vector table
	; One routine to init them all
	ld hl,.sector_buffer	; start of the partition table
	ld bc,01BEh
	add hl,bc			; HL -> partition 1 table entry
	ld bc,8
	add hl,bc			; HL -> Pstart 0
	push hl
	push hl 			; save once more for later
	ld ix,.drv_offset	; IX -> Offset table
	pop iy				; IY -> Ptable entry
	ld bc,16			; Length of 1 entry in PTBL
	ld de,4				; Length of 1 entry in Offset table
	
	; Beware: Partition table is ordered diffrently!
	ld a,(iy)
	ld (ix+3),a
	ld a,(iy+1)
	ld (ix+2),a
	ld a,(iy+2)
	ld (ix+1),a
	ld a,(iy+3)
	ld (ix),a			; P0 offset populated
	add ix,de
	add iy,bc
	
	ld a,(iy)
	ld (ix+3),a
	ld a,(iy+1)
	ld (ix+2),a
	ld a,(iy+2)
	ld (ix+1),a
	ld a,(iy+3)
	ld (ix),a			; P1 offset populated
	add ix,de
	add iy,bc
	
	ld a,(iy)
	ld (ix+3),a
	ld a,(iy+1)
	ld (ix+2),a
	ld a,(iy+2)
	ld (ix+1),a
	ld a,(iy+3)
	ld (ix),a			; P2 offset populated
	add ix,de
	add iy,bc

	ld a,(iy)
	ld (ix+3),a
	ld a,(iy+1)
	ld (ix+2),a
	ld a,(iy+2)
	ld (ix+1),a
	ld a,(iy+3)
	ld (ix),a			; P3 offset populated
	add ix,de
	add iy,bc

	pop iy				; P0 start again ...
	ld a,(iy)
	ld (ix+3),a
	ld a,(iy+1)
	ld (ix+2),a
	ld a,(iy+2)
	ld (ix+1),a
	ld a,(iy+3)
	ld (ix),a			; IMG0 offset populated


if .sd_debug >= 2
	ld bc,64
	ld hl,.drv_offset
	call hexdmp
	call iputs
	db '0..4 ',0
	call dump_regs
	;call spamon
endif

	ld iy,.drv_offset
	ld ix,.drv_offset	; IX -> P0 offset
	ld bc,16			; 5*4 byte already done
	add iy,bc			; IY -> IMG 1 offset
	
	ld bc,11				; 1 done, 11 to go
.imgloop:

	push bc				; save count
	ld bc,4				; 4 bytes per entry
	add iy,bc			; advance pointer
	
	ld h,(ix+2)
	ld l,(ix+3)
	
	ld d,(iy+3)
	ld e,(iy+2)
if .sd_debug >= 1
	push bc
	push de
	push hl
	push iy
	call iputs
	db '->IMGLOOP: ',0
	call dump_regs
	call iputs
	db '(IX):',0
	push ix
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	call iputs
	db '(IY):',0
	push iy
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	pop iy
	pop hl
	pop de
	pop bc
endif

	add hl,de			; add the 2 LSW
	push hl				; save for later
	
	ld h,(ix)
	ld l,(ix+1)

	ld d,(iy+1)
	ld e,(iy)	
	adc hl,de			; add with carry MSW
	
	ld (iy),h
	ld (iy+1),l
	pop hl
	ld (iy+2),h
	ld (iy+3),l
	
	
if .sd_debug >= 1
	push bc
	push de
	push hl
	push iy
	call iputs
	db 'IMGLOOP->: ',0
	call dump_regs
	call iputs
	db ' DE:HL + (IY):',0
	push iy
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	pop iy
	pop hl
	pop de
	pop bc
endif
	;call .add32			; (IY) = DE:HL + (IX)
	
	pop bc
	dec c
	jp nz,.imgloop
	
	; at this point, the whole offset table should be populated, even for inactive drives
if .sd_debug >= 1
	;call spamoff
	call iputs
	db 'OFFTBL: ',0
	ld e,1
	ld bc,64
	ld hl,.drv_offset
	call hexdmp
	call iputs
	db '5..11 ',cr,lf,0
	call dump_regs
endif

.init0:
	; per drive stuff here
	ld hl,@dtbl
	ld a,(@adrv)
	add a,a			; double
	ld d,0
	ld e,a			; 
	add hl,de		; HL -> (XDPH)
	ld e,(hl)
	inc hl
	ld d,(hl)		; DE -> XDPH
	ld hl,0
	add hl,de	
	push hl			; save a pointer for later
	dec hl			; XDPH -1 = Type field
	ld a,(hl)
	or a
	jp z,.notimage
	
	ld a,(hl)
	dec a
	jp nz,.init_err ; if type >1 probably garbage
	
if .sd_debug >= 2
	push hl
	call iputs
	db 	'IMG ',0
	pop hl
	ld a,(hl)
	call hexdump_a
endif

	pop hl				; this should be a pointer to our XDPH
	ld bc,13
	add hl,bc			; HL -> (DPB)
	ld e,(hl)
	inc hl
	ld d,(hl)		; DE -> DPB
	push de
	jp .img_report
	
	
.notimage:	
if .sd_debug >= 2
	call iputs
	db 	'MBR ',0
	call dump_regs
endif
	; for MBR partitions, read Plen from the table in .sd_scratch
	ld ix,.sector_buffer	; start of the partition table
	ld iy,.sd_scratch		; (ab)use the SD command buffer for scratch
	ld bc,01BEh
	add ix,bc				; IX -> P0 entry
	ld bc,12				
	add ix,bc				; IX -> P0 Plen
	
	ld bc,0
	
	ld a,(@rdrv)	; we will use @rdrv to index the partition table
	add a,a			; *2
	add a,a 		; *4
	add a,a 		; *8
	add a,a 		; *16
	ld c,a			; BC -> partition table entry
	add ix,bc		; IX -> Current Plen
	
	
if .sd_debug >= 2
	ld a,(ix+4)
	call hexdump_a
	call puts_crlf
endif
	; The 32 bit value for Plen needs to be divided by 16
	; because we have 16 sectors * 512 bytes per block
	
	; Byte 0
	ld a,(ix)
	rrca
	rrca
	rrca
	rrca			; 	shift right * 4 = div/16
	and 0Fh 	; mask away the 4 garbage bits
	ld (iy),a
	
if .sd_debug >= 2
	call hexdump_a
endif	
	; Byte 1
	ld a,(ix+1)
	rrca
	rrca
	rrca
	rrca			; 	shift right * 4 = div/16
	ld b,a
	and 0Fh 	; mask away the 4 high order bits
	ld (iy+1),a
	ld a,b		; this time, we need to copy the bits to the lower byte
	and 0F0h	; take only the high order 4 bits
	or (iy)
	ld (iy),a
	
if .sd_debug > 1
	call hexdump_a
endif		
	; Byte 2
	ld a,(ix+2)
	rrca
	rrca
	rrca
	rrca			; 	shift right * 4 = div/16
	ld b,a
	and 0Fh 	; mask away the 4 high order bits
	ld (iy+2),a
	ld a,b		; this time, we need to copy the bits to the lower byte
	and 0F0h	; take only the high order 4 bits
	or (iy+1)
	ld (iy+1),a
	
if .sd_debug > 1
	call hexdump_a
endif		
	; Byte 3
	ld a,(ix+3)
	rrca
	rrca
	rrca
	rrca			; 	shift right * 4 = div/16
	ld b,a
	and 0Fh 	; mask away the 4 high order bits
	ld (iy+3),a
	ld a,b		; this time, we need to copy the bits to the lower byte
	and 0F0h	; take only the high order 4 bits
	or (iy+2)
	ld (iy+2),a
	
if .sd_debug > 1
	ld a,(iy+2)
	call hexdump_a
	ld a,(iy+3)
	call hexdump_a
endif	
	; the 32 bit value pointed to by IY now holds the number of 8k blocks on this partition
	; lets evaluate it
	ld a,(iy+3)		; the MSB
	or a			; must be zero
	jp nz,.toobig

	ld a,(iy+2)		; the next lower byte
	or a			; must also be zero
	jp nz,.toobig
	
	ld a,(iy+1)		; the next byte
	bit 7,a			; must be <80h
	jp nz,.toobig
	
	or a
	jp nz,.report	; if this is not zero, we are >0 and <8000h
					; and don't care about the lowest byte
	; if the former byte was zero
	ld a,(iy)		; the lowest byte
	or a			; must *not* be zero
	jp z,.zerosize
	jp .report

.toobig:
	; if >8000h ERROR unsupported partition (too big)
	call iputs
	db	'Partition too big. Max 256 MB',cr,lf,0	
	ld a,0FFh
	pop bc
	jp .init_done
	
.zerosize:	
	; if 0 ERROR too small or invalid
	call iputs
	db 'Invalid partiton',cr,lf,0
	ld a,0FFh
	pop bc
	jp .init_done
	
.init_err:	
	; if 0 ERROR too small or invalid
	call iputs
	db 'SD Card Error!',cr,lf,0
	call dump_regs
	ld a,0FFh
	pop bc
if .sd_debug
	halt	; just stop
endif
	jp .init_done
	
.report:
	; here, we have to copy the word at IY+2..3 to the DSM field of the DPB
	pop hl			; with any luck, our XDPH pointer is still there
	ld bc,12
	add hl,bc		; HL -> (DPB)
	ld e,(hl)
	inc hl
	ld d,(hl)		; DE -> DPB
	ex de,hl
	ld bc,5
	add hl,bc		; HL -> DSM value in DPB
					; IY -> DSM value from disk

	ld e,(iy)
	ld d,(iy+1)		; DE = size in 8k blocks
	dec de			; we need to save size-1
	ld (hl),e
	inc hl
	ld (hl),d		; at this point, the private DPB 
					; for this drive should have the correct DSM for this partition
	push de			; lets save that

	; if (fancy) print message to the user like "(@adrv): SD(@rdrv) (size)"
	ld c,'A'
	ld a,(@adrv)
	add a,c		; add @adrv
	ld c,a
	call ?co
	
	call iputs
	db ': SDP ',0	
	ld a,(@rdrv)
	call hexdump_a
	call iputs
	db ' 0x',0	

	pop de
	ld a,d	
	call hexdump_a
	ld a,e	
	call hexdump_a

	call iputs
	db ' Blocks * 8k @ LBA 0x',0
	ld a,(@rdrv)
	add a,a
	add a,a 
	ld b,0
	ld c,a
	ld ix,.drv_offset
	add ix,bc		; IX -> drive start
	
	ld a,(ix)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+1)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+2)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+3)		; assume we haven't clobbered ix
	call hexdump_a
	call iputs
	db cr,lf,0
	ld a,1

.init_done:
	; Restore old stack
	ld sp,(.old_stk)
	ret

.img_report:	
	pop ix		; our DPB pointer now in IX

	; if (fancy) print message to the user like "(@adrv): SD(@rdrv) (size)"
	ld c,'A'
	ld a,(@adrv)
	add a,c		; add @adrv
	ld c,a
	call ?co
	
	call iputs
	db ': SD0-',0	
	ld a,(@rdrv)
	sub 4		; 0..3 are SDP 
	call hexdump_a
	call iputs
	db ' 0x',0
	;Lets just get a new DPB pointer
	ld hl,@dtbl
	ld a,(@adrv)
	add a,a
	ld bc,0
	ld c,a
	add hl,bc		; HL -> (XDPH)
	ld e,(hl)
	inc hl
	ld d,(hl)		; DE -> XDPH
	ex de,hl
	ld de,12		
	add hl,de		; HL -> (DPB)
	ld e,(hl)
	inc hl
	ld d,(hl)		; DE -> DPB
	push de
	pop ix			; IX -> DPB
	
	ld a,(ix+6)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+5)		; assume we haven't clobbered ix
	call hexdump_a

	call iputs
	db ' Blocks * 8k @ LBA 0x',0
	ld hl,.drv_offset	
	ld a,(@rdrv)
	add a,a
	add a,a
	ld b,0
	ld c,a
	add hl,bc
	push hl
	pop ix
	ld a,(ix)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+1)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+2)		; assume we haven't clobbered ix
	call hexdump_a
	ld a,(ix+3)		; assume we haven't clobbered ix
	call hexdump_a
	call iputs
	db cr,lf,0
	ld a,1
	jp .init_done


.load_ptbl:
	; read sector 0
	ld de,0
	push de
	push de			; load 0x00000000 for an address
	ld de,.sector_buffer
	call sd_cmd17
	pop hl
	pop hl	; fix stack
	
	ld a,(.sector_buffer + 01BEh + 40h)
	
if .sd_debug > 1
	call hexdump_a
endif	
	cp 055h
	jp nz,.err0
	ld a,(.sector_buffer + 01BEh + 41h)
	
if .sd_debug > 1
	call hexdump_a
	push af
	call puts_crlf
	pop af
endif	

	cp 0AAh
	jp nz,.err0
	
	ld a,00h
	ret

.err0:
	ld a,0FFh
	ret
	
;##########################################################################
; Calculate the address of the SD block, given the CP/M track number
; in HL and the fact that the currently selected drive's DPH is in 
; disk_dph.
; HL = CP/M track number
; Return: the 32-bit block number in DE,HL
; Based on proposal from Trevor Jacobs - 02-15-2023
;##########################################################################
	
.calc_sd_block:	
if .sd_debug >= 1
	call iputs
	db 'CALC: ',0
	call disk_dump
endif
	
	ld ix,.drv_offset
	ld iy,.sd_scratch
	ld a,(@rdrv)
	add a,a
	add a,a
	ld b,0
	ld c,a
	add ix,bc			; IX -> Pstart

	;ld	hl,0
	ld	bc,(@trk)		; BC = 16 (sectors/track)
	ld 	de,16			; DE = requested track	
						; note: flipped might be faster
if .sd_debug >= 2
	call dump_regs
endif
						
	call mul16			; DE:HL = BC * DE

if .sd_debug >= 2
	call dump_regs
endif

	ld bc,(@sect)		; BC = the requested sector
	add hl,bc			; DE:HL now the block offset

if .sd_debug >= 2
	call dump_regs
endif

	call .add32			; (IY) = DE:HL + (IX)

if .sd_debug >= 2
	call dump_regs
	push iy
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	push ix
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	call puts_crlf
endif

    ld  d,(iy)            ; DE = low-word of the SD starting block
    ld  e,(iy+1)        ; DE = low-word of the SD starting block
    ld  h,(iy+2)      ; HL = high word of SD start block
    ld  l,(iy+3)        ; HL = high word of SD start block	

if .sd_debug >= 1
	push hl
	push de
	call dump_regs
	call iputs
	db '/CALC',cr,lf,0
	pop de
	pop hl
endif
    ret

; (IY) = DE:HL + (IX)
.add32:
if .sd_debug >= 2
	push de
	push hl
	push iy
	call iputs
	db 'ADD32: ',0
	call dump_regs
	call iputs
	db ' (IX): ',0
	push ix
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	pop iy
	pop hl
	pop de
endif

	push de		; save the MSW
	
	ld d,(ix+2)	; LSW first
	ld e,(ix+3)
	add hl,de
	ld (iy+3),l
	ld (iy+2),h

if .sd_debug >= 2
	push af
	push hl
	call dump_regs
	call iputs
	db ' (IY): ',0
	push iy
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	pop hl
	pop af
endif	
	
	pop hl
	ld d,(ix)	; MSW 
	ld e,(ix+1)

if .sd_debug >= 2
	push af	; need to save that carry flag
	call dump_regs
	pop af
endif

	adc hl,de
	jr nc,.add32done
	inc hl
.add32done:
	ld (iy+1),l
	ld (iy),h

if .sd_debug >= 2
	push iy
	push hl
	call dump_regs
	call iputs
	db ' (IY): ',0
	push iy
	pop hl
	ld bc,4
	ld e,0
	call hexdmp
	call iputs
	db '/ADD32',cr,lf,0
	pop hl
	pop iy
endif
	ret


;############################################################################
; Get the SD card to wake up ready for block transfers.
;
; XXX This is a hack added to let the BIOS reset everything. XXX
;
;############################################################################
sd_reset:
	;call	sd_boot
	call	.sd_clk_dly

	call    sd_cmd0

	ld      de,.sd_scratch
	call    sd_cmd8

	;ld      de,.sd_scratch
	;call    sd_cmd58

	ld      b,020h          ; limit the number of retries here
.sd_reset_ac41:
	push    bc
	ld      de,.sd_scratch
	call    sd_acmd41
	pop     bc
	or      a
	jr      z,.sd_reset_done
	djnz    .sd_reset_ac41

	call	iputs
	db	'SD_RESET FAILED!',cr,lf,0
	ld	a,001h
	ret

.sd_reset_done:
	ld      de,.sd_scratch
	call    sd_cmd58
	xor	a
	ret
	
;############################################################################
; Send a CMD58 message and read an R3 response.
; CMD58 is used to ask the card what voltages it supports and
; if it is an SDHC/SDXC card or not.
; Clobbers A, BC, DE, HL
; Return the 5-byte response in the buffer pointed to by DE.
;############################################################################
sd_cmd58:
if .sd_debug > 1
	push	de			; PUSH buffer address
endif

	ld	hl,.sd_cmd58_buf
	ld	b,.sd_cmd58_len
	call	.sd_cmd_r3

if .sd_debug > 1
	call	iputs
	db	'CMD58: ',0
	ld	hl,.sd_cmd58_buf
	ld	bc,.sd_cmd58_len
	ld	e,0
	call	hexdmp
	call	iputs
	db	'  R3: ',0
	pop	hl			; POP buffer address
	ld	bc,5
	ld	e,0
	call	hexdmp			; dump the reply message
endif

	ret

.sd_cmd58_buf:	db	58|040h,0,0,0,0,000h|001h
.sd_cmd58_len:	equ	$-.sd_cmd58_buf

;############################################################################
; Send a CMD0 (GO_IDLE) message and read an R1 response.
;
; CMD0 will 
; 1) Establish the card protocol as SPI (if has just powered up.)
; 2) Tell the card the voltage at which we are running it.
; 3) Enter the IDLE state.
;
; Return the response byte in A.
; Clobbers A, BC, DE, HL
;############################################################################
sd_cmd0:
	ld	hl,.sd_cmd0_buf		; HL = command buffer
	ld	b,.sd_cmd0_len		; B = command buffer length
	call	.sd_cmd_r1		; send CMD0, A=response byte

if .sd_debug > 1
	push	af
	call	iputs
	db	'CMD0: ',0
	ld	hl,.sd_cmd0_buf
	ld	bc,.sd_cmd0_len
	ld	e,0
	call	hexdmp
	call	iputs
	db	'  R1: ',0
	pop	af
	push	af
	call	hexdump_a		; dump the reply message
	call    puts_crlf
	pop	af
endif

	ret

.sd_cmd0_buf:	db	0|040h,0,0,0,0,094h|001h
.sd_cmd0_len:	equ	$-.sd_cmd0_buf


;############################################################################
; Send a CMD8 (SEND_IF_COND) message and read an R7 response.
;
; Establish that we are squawking V2.0 of spec & tell the SD 
; card the operating voltage is 3.3V.  The reply to CMD8 should 
; be to confirm that 3.3V is OK and must echo the 0xAA back as 
; an extra confirm that the command has been processed properly. 
; The 0x01 in the byte before the 0xAA in the command buffer 
; below is the flag for 2.7-3.6V operation.
;
; Establishing V2.0 of the SD spec enables the HCS bit in 
; ACMD41 and CCS bit in CMD58.
;
; Clobbers A, BC, DE, HL
; Return the 5-byte response in the buffer pointed to by DE.
; The response should be: 0x01 0x00 0x00 0x01 0xAA.
;############################################################################
sd_cmd8:
if .sd_debug > 1
	push	de			; PUSH response buffer address
endif

	ld	hl,.sd_cmd8_buf
	ld	b,.sd_cmd8_len
	call	.sd_cmd_r7

if .sd_debug > 1
	call	iputs
	db	'CMD8: ',0
	ld	hl,.sd_cmd8_buf
	ld	bc,.sd_cmd8_len
	ld	e,0
	call	hexdmp
	call	iputs
	db	'  R7: ',0
	pop	hl			; POP buffer address
	ld	bc,5
	ld	e,0
	call	hexdmp			; dump the reply message
endif

	ret

.sd_cmd8_buf:	db	8|040h,0,0,001h,0aah,086h|001h
.sd_cmd8_len:	equ	$-.sd_cmd8_buf
	
; DEBUG CODE BELOW
if .sd_debug >= 1


endif
	
end
