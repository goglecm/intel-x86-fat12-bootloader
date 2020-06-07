;=================================================
; Input/Output routines
;=================================================

%ifndef __STDIO_INC_12_INCLUDED__

	%define	__STDIO_INC_12_INCLUDED__
	%define	VIDMEM_COLOR_TEXT_M7	0xB8000			; video memory for color text
	%define	SCRN_WDTH		80			; width of the screen
	%define	SCRN_HGT		25			; height of the screen
	%define	CHAR_ATTRIB		15			; character attribute (White text on black background)
	%define NULL			0x00			; null character
 
	currentLocX	db	0x00				; current x location
	currentLocY	db	0x00				; current y location
 
	bits	16

	;=================================================
	; Command: puts16 ()
	; Description: Prints a null terminated string
	; Parameters: (DS:SI) -> null terminated string
	;=================================================	
	
	puts16:

		pusha					; stack registers
		.loop:					;
			lodsb				; load next byte from string from SI to AL
			or	al, al			; is al == 0 ?
			jz	.done			; yes, then finish
			mov	ah, 0x0E		; no, then print the character
			int	0x10			; call BIOS to print al
			jmp	.loop			; repeat until null terminator found
		.done:					;
			popa				; restore registers

		ret					; return
	 
	 
	 
	 
	bits	32
	
	;=============================================
	; Command: clrscr32 ()
	; Description: Clears the screen
	; Parameters: BL -> Character to print, BH -> Character attribute byte
	;=============================================

	clrscr32:
 
		pusha					; save registers
		cld					; direction flag is cleared, so left to right
		mov	edi, VIDMEM_COLOR_TEXT_M7
		mov	cx, 2000
		mov	ah, CHAR_ATTRIB
		mov	al, ' '	
		rep	stosw
	 
		mov	byte [currentLocX], 0
		mov	byte [currentLocY], 0
		popa
		ret
	 
	;=============================================
	; Command: putchar32 ()
	; Description: Prints a character
	; Parameters: BL -> Character to print, BH -> Character attribute byte
	;=============================================

	putchar32:
 
		pusha					; save registers
		mov	edi, VIDMEM_COLOR_TEXT_M7	; edi = video memory base address
	
		;=============================================
		; Get current character position
		;=============================================
 
		mov	ecx, SCRN_WDTH * 2		; (ecx = screenWidth * 2) bytes per row, as a char in mode 7 has 2 bytes
		xor	eax, eax			; clear eax
		mov	al, byte [currentLocY]		; 
		mul	ecx				; (eax = currentY  * ecx) to get absolute y position
		push	eax				; save eax
		mov	al, byte [currentLocX]		; 
		mov	cl, 2				;
		mul	cl				; (al = currentX * 2) because there 2 bytes per char
		pop	ecx				; pop absolute Y
		add	eax, ecx			; (eax = absoluteY + Xoffset)
		xor	ecx, ecx			; clear ecx
		add	edi, eax			; add Video mem base address to char position
	
		;=============================================
		; Check if character is new line
		;=============================================
		
		cmp	bl, 0x0A			; check if bl is newline
		je	.NewLine			; if it is, then go to next row, else print the character
 		
 		;=============================================
 		; Print the character
 		;=============================================
 		
 		mov	dl, bl				; Get the character in dl
 		mov	dh, CHAR_ATTRIB			; Get the character attribute in dh
		mov	word [edi], dx			; Write the character to video memory (i.e. display it)
 
 		;=============================================
 		; Update next position
 		;=============================================
 		 
 		inc	byte [currentLocX]		; go to next character
 		cmp	byte [currentLocX], SCRN_WDTH	; check if currentXposition is end of line
		je	.NewLine			; if it is, then go to next row
		jmp	.End				; if not, then finish
	
		;=============================================
		; Go to next row
		;=============================================
		
		.NewLine:
			mov	byte [currentLocX], 0x00; go first column
			inc	byte [currentLocY]	; go to next row
 
 		;=============================================
 		; Restore registers and finish
 		;=============================================
 		 
		.End:
			popa				; restore registers
	
	ret						; return
	
	;=============================================
	; Command: puts32 ()
	; Description: Prints a 32 bit null terminated string
	; Parameters: EBX -> base address of the null terminated string
	;=============================================
	
	puts32:
	
		;=============================================
		; Store registers
		;=============================================
		
		pusha					; save registers
		push	ebx				; stack the string address
		pop	edi				; (edi = string address)
		
		;=============================================
		; Get character
		;=============================================
		 
		.getNChar:
		
			mov	bl, byte [edi]          ; get next character
			cmp	bl, NULL		; check if char = NULL
			je	.End			; if it is, then finish
		
		;=============================================
		; Print character
		;============================================= 
		
		call	putchar32			; else print the character

		;=============================================
		; Go to next character
		;=============================================
		
		.goNChar:
			inc	edi			; go to next character
			jmp	.getNChar		;
			
		;=============================================
		; Update hardware cursor
		;=============================================
		
		.End:
			mov	bh, byte [currentLocY]	; get current Y location
			mov	bl, byte [currentLocX]	; get current X location
			call	movCursor		; update cursor
		popa					; restore registers
		
		ret					; return
		
	;=============================================
	; Command: movCursor ()
	; Description: Update hardware cursor to current location
	; Parameter: BH -> cursor Y pos, BL -> cursor X pos
	;=============================================
	
	movCursor:
		pusha					; save registers
		
		;=============================================
		; Determine current position
		;=============================================
		
		xor	eax, eax			; clear eax
		mov	ecx, SCRN_WDTH			; the position of the cursor is relative to the screen, not memory
		mov	al, bh				; get y position
		mul	ecx				; (ecx = SCRN_WDTH * currentY)
		add	al, bl				; Now add x
		mov	ebx, eax			;
 
	;--------------------------------------;
	;   Set low byte index to VGA register ;
	;--------------------------------------;
 
	mov	al, 0x0f		; Cursor location low byte index
	mov	dx, 0x03D4		; Write it to the CRT index register
	out	dx, al
 
	mov	al, bl			; The current location is in EBX. BL contains the low byte, BH high byte
	mov	dx, 0x03D5		; Write it to the data register
	out	dx, al			; low byte
 
	;---------------------------------------;
	;   Set high byte index to VGA register ;
	;---------------------------------------;
 
	xor	eax, eax
 
	mov	al, 0x0e		; Cursor location high byte index
	mov	dx, 0x03D4		; Write to the CRT index register
	out	dx, al
 
	mov	al, bh			; the current location is in EBX. BL contains low byte, BH high byte
	mov	dx, 0x03D5		; Write it to the data register
	out	dx, al			; high byte
 
	popa
	ret	
		
%endif
