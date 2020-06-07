;=================================================
; Routines to enable A20 address line
;=================================================

%ifndef __A20_INC_12_INCLUDED__

	%define	__A20_INC_12_INCLUDED__

	bits	16

	;=================================================
	; Enables a20 line through keyboard controller
	;=================================================
	
	enableA20KbrdCtrlr:
	
		cli			; disable interrupts
		push	ax		; stack ax
		mov	al, 0xDD	; send enable a20 address line command to controller
		out	0x64, al	; write to keyboard 0x64 buffer
		pop	ax		; restore ax
	
		ret			; return
	
	;=================================================
	; Enables a20 line through output port
	;=================================================
	
	enableA20KbrdOut:
	
		cli			; disable interrupts
		pusha			; stack all registers
	
	        call	waitInput	;
	        mov	al, 0xAD	;
	        out	0x64, al	; disable keyboard
	        call	waitInput	;
	
	        mov	al, 0xD0	;
	        out	0x64, al	; tell controller to read output port
	        call	waitOutput	;
	
		in	al, 0x60	;
	        push	eax		; get output port data and store it
	        call	waitInput	;
	
		mov	al, 0xD1	;
	        out	0x64, al	; tell controller to write output port
	        call	waitInput	;
	
		pop	eax		;
	        or	al, 2		; set bit 1 (enable a20)
	        out	0x60, al	; write out data back to the output port
	
		call	waitInput	;
		mov	al, 0xAE	; enable keyboard
		out	0x64, al	;
	
		call	waitInput	;
		popa			; restore registers
	        sti			; enable interrupts
	
	        ret			; return
	
		;===============================================
		; Wait for input buffer to be clear
		;===============================================
	
		waitInput:
	
			in	al, 0x64
			test	al, 2
			jnz	waitInput
			
			ret
	
		;===============================================
		; Wait for output buffer to be clear
		;===============================================
	
		waitOutput:
	
			in	al, 0x64
			test	al, 1
			jz	waitOutput
			
			ret
	
	;=================================================
	; Enables a20 line through bios
	;=================================================
	
	enableA20BIOS:
	
		pusha			; stack all regsiters
		mov	ax, 0x2401	;
		int	0x15		;
		popa			; restore all registers
	
		ret			; return
	
	;=================================================
	; Enables a20 line through system control port A
	;=================================================
	
	enableA20SysCtrlA:
	
		push	ax		; stack ax
		mov	al, 2		;
		out	0x92, al	;
		pop	ax		; restore ax
		
		ret			; return
	
%endif
