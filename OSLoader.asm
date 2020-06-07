;=================================================
; 0x500 through 0x7bff is unused above the BIOS data area.
; Loaded at 0x500 (0x50:0)
;=================================================

bits 	16
org	0x500

jmp	Main				; go to start
 
;=================================================
; Preprocessor directives
;=================================================
 
%include	"sources/include/stdio.inc"	; basic i/o routines
%include	"sources/include/gdt.inc"	; gdt routines
%include	"sources/include/a20.inc"	; enable A20 routines
 
;=================================================
; Data Section
;=================================================
 
msgPrepOS	db	"Preparing to load the Operating System", 0x0D, 0x0A, 0x00
msgGDTReady	db	"Global Descriptor Table installed", 0x0D, 0x0A, 0x00
msgA20Enabled	db	"A20 Address line enabled", 0x0D, 0x0A, 0x00
msgStartOS	db	"Starting the OS", 0x0A, 0x00

;=================================================
; Prepare to enter 32 bit mode (Protected Mode)
;=================================================

Main:
 
	;===============================================
	;   Setup segments and stack
	;===============================================
 
	cli				; disable interrupts
	xor	ax, ax			; clear segment registers
	mov	ds, ax			;
	mov	es, ax			;
	mov	ax, 0x9000		; stack begins at 0x9000
	mov	ss, ax			;
	mov	sp, 0xFFFF		; stack ends at 0xFFFF
	sti				; enable interrupts
 
	;===============================================
	; Display loading message
	;===============================================
 
	mov	si, msgPrepOS
	call	puts16
 
	;===============================================
	; Install the GDT
	;===============================================
 
	call	installGDT		; install the GDT

	mov	si, msgGDTReady
	call	puts16


	;===============================================
	; Enable A20
	;===============================================

	call	enableA20KbrdOut

 	mov	si, msgA20Enabled
	call	puts16

	;===============================================
	; Go into pmode
	;===============================================
 
	cli				; clear interrupts
	mov	eax, cr0		; set bit 0 in cr0 (enter PMode)
	or	eax, 1			; set the bit
	mov	cr0, eax		;

	jmp	GDT_CODE_DESC:PMode	; far jump to fix CS (the code selector is 0x08, offset of gdt for code)
 
;=================================================
; PMode entered
;=================================================

bits	32					; 32 bit mode
 
PMode:

	;===============================================
	; Set registers
	;===============================================
 
	mov		ax, GDT_DATA_DESC	; set data segments to data selector (0x10)
	mov		ds, ax			;
	mov		ss, ax			;
	mov		es, ax			;
	mov		esp, 0x90000		; stack begins at 0x90000
	;call		clrscr32		; clear screen and dislay start OS
	mov		ebx, msgStartOS		;
	call		puts32			;
 
;=================================================
; Stop execution
;=================================================
 
Stop:
 
	cli
	hlt
