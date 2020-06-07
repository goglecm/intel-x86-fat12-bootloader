;=================================================
; GDT Routines
;=================================================
 
%ifndef __GDT_INC_12_INCLUDED__
	
	%define	__GDT_INC_12_INCLUDED__
	
	%define	GDT_NULL_DESC 0x00
	%define	GDT_CODE_DESC 0x08
	%define	GDT_DATA_DESC 0x10

	bits	16
 
	;===============================================
	; Command: installGDT()
	; Description: Install The Global Descriptor Table
	; Parameters: (toc) -> pointer to GDT
	;===============================================
 
 	installGDT:
 
		cli				; clear interrupts
		pusha				; save registers
		lgdt	[toc]			; load GDT into GDTR
		sti				; enable interrupts
		popa				; restore registers

		ret				; return
 
	;===============================================
	; Global Descriptor Table (GDT)
	;===============================================
 
	GDTDetails: 

		;==============================================
		; Null descriptor, offset 0x00
		;==============================================

		dd	0x00000000
		dd	0x00000000
 
 		;=============================================
		; Code descriptor, offset 0x08
		;=============================================
		
		dw	0xFFFF 			; limit low
		dw	0x0000			; base low
		db	0x00			; base middle
		db	10011010b		; access
		db	11001111b		; granularity
		db	0x00			; base high
 
		;==============================================
		; Data descriptor, offset 0x10
		;==============================================

		dw	0xFFFF 			; limit low (Same as code)
		dw	0x0000			; base low
		db	0x00			; base middle
		db	10010010b 		; access (Same as code except bit 41 is unset)
		db	11001111b 		; granularity
		db	0x00			; base high
 
	EndGDT:

	toc: 
		dw	EndGDT - GDTDetails - 1 ; limit (Size of GDT)
		dd	GDTDetails 		; base of GDT
 
%endif
