;=================================================
; FAT12 FS BootLoader - it loads KRNLDR.SYS
;=================================================

org	0x0
bits	16						; 16 bit Real Mode

Start:
	jmp	Main

;================================================
; OEM / BIOS Parameter Block
;================================================

bpbOEM			db	"Main OEM"		; OEM identifier (8 bytes)

bpbBytesPerSector:  	DW	512
bpbSectorsPerCluster: 	DB	0x01
bpbReservedSectors: 	DW	0x0001
bpbNumberOfFATs: 	DB	0x02
bpbRootEntries: 	DW	224			; max 224
bpbTotalSectors: 	DW	2880
bpbMedia: 		DB	0xF8
bpbSectorsPerFAT: 	DW	0x0009
bpbSectorsPerTrack: 	DW	18
bpbHeadsPerCylinder: 	DW	0x0002
bpbHiddenSectors: 	DD	0x00000000
bpbTotalSectorsBig:     DD	0x00000000
bsDriveNumber: 	        DB	0x00
bsUnused: 		DB	0x00
bsExtBootSignature: 	DB	0x29
bsSerialNumber:	        DD	0x89ABCDEF		; any 8 bytes
bsVolumeLabel: 	        DB	"MOS FLOPPY "		; 11 bytes
bsFileSystem: 	        DB	"FAT12   "		; 8 bytes


;=================================================
;	Prints a string
;	DS:SI = null terminated string
;=================================================

Print:
	lodsb						; load next byte from string from SI to AL
	or	al, al					; is al == 0 ?
	jz	.PrintDone				; Yes, null terminator found, exit
	mov	ah, 0x0E				; No, Print the character, function (0E)
	int	0x10					; Call BIOS to print char
	jmp	Print					; Repeat until null terminator found
	
	.PrintDone:
		ret					; return

;=================================================
; Reads a series of sectors
; CX = Number of sectors to read
; AX = Starting sector
; ES:BX = Location to read to
;=================================================

ReadSectors:

	.MAIN:
		mov	di, 0x000A				; after 10 attempts, error, i.e. error counter
		.SECTORLOOP:
		push	ax					; put the current parameters, ax, bx and cx in the stack
		push	bx					;
		push	cx					;
		call	LBACHS					; convert starting sector to CHS where ax = startLBAaddress
		mov	ah, 0x02				; BIOS read sector, function (2)
		mov	al, 0x01				; read one sector
		mov	ch, BYTE [absoluteTrack]		; track
		mov	cl, BYTE [absoluteSector]		; sector
		mov	dh, BYTE [absoluteHead]			; head
		mov	dl, BYTE [bsDriveNumber]		; drive
		int	0x13					; call BIOS to read sector, this will make carry = 1 on error
		jnc	.SUCCESS				; test for read error, jump is carry = 0
		xor	ax, ax					; BIOS reset disk, function (0)
		int     0x13					; call BIOS to reset floppy
		pop	cx					; restore ax, bx and cx from stack
		pop	bx					; this will recopy the values for ax, bx and cx
		pop	ax					;
		dec	di					; decrement error counter
		jnz	.SECTORLOOP				; attempt to read again, jump if error counter > 0
		int     0x18					; error, no ROM, cannot read
		.SUCCESS:
			mov	si, msgProgress			; Display progress
			call	Print				;
			pop	cx				; restore ax, bx and cx from stack
			pop	bx				;
			pop	ax				;
			add	bx, WORD [bpbBytesPerSector]	; queue next Buffer to read
			inc	ax				; queue next sector
			loop	.MAIN				; read next sector and decrement CX (the loop does it)

	ret							; return

;=================================================
; Convert CHS (cluster in ax) to LBA
; LBA = (cluster - 2) * (sectors per cluster)
; Returns the sector, after the root dir, of the cluster
;=================================================

ClusterLBA:

	sub	ax, 0x0002				; zero base cluster number (AX = AX - 2), ax has no base here
	xor     cx, cx					; clear cx
	mov	cl, BYTE [bpbSectorsPerCluster]		; convert byte to word
	mul     cx					; CX = AX x CX 
	add	ax, WORD [datasector]			; base data sector, i.e. add base to new AX
	
	ret						; return

;=================================================
; Convert LBA to CHS
; AX = LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;=================================================

LBACHS:

	xor	dx, dx					; prepare dx:ax for operation, clear dx
	div	WORD [bpbSectorsPerTrack]		; AX = AX / SectorsPerTrack
	inc	dl					; adjust for first sector
	mov	BYTE [absoluteSector], dl		; absoluteSector = 1
	
	xor	dx, dx					; prepare dx:ax for operation, clear dx
	div	WORD [bpbHeadsPerCylinder]		; AX = AX / HeadsPerCyl
	mov	BYTE [absoluteHead], dl			; absoluteHead = 0
	
	mov	BYTE [absoluteTrack], al		; absoluteTrack = AX
	
	ret						; return

;==================================================================================================

	Main:
		;==============================================
		; BIOS loads the bootloader at 0x7c00
		; Adjust segment registers
		;==============================================

		cli						; disable interrupts
		mov	ax, 0x07C0				; setup registers to point to the correct segment
		mov	ds, ax					; point data segment to ax
		mov	es, ax					; point extra segment to ax
		mov	fs, ax					; point far segment to ax
		mov	gs, ax					; point general purpose segment to ax

		;==============================================
		; Prepare the stack in the first segment
		;==============================================

		mov	ax, 0x0000				; set the stack
		mov	ss, ax					; at segment 0x0
		mov	sp, 0xFFFF				; set stack pointer at the top of the segment
		sti						; restore interrupts
		mov	si, msgLoading				; Display loading message
		call	Print					;
		;==============================================
		; Load root directory table
		;==============================================
		
		LoadRootDir:
			
			;=============================================
			; Compute size of root directory and store it in cx
			;=============================================

			xor	cx, cx				; clear cx
			xor	dx, dx				; clear dx
			mov	ax, 0x0020			; 32 byte directory entry
			mul	WORD [bpbRootEntries]		; total size of directory (rootDirEntries x 32d = totalBytesRootDir)
			div	WORD [bpbBytesPerSector]	; sectors used by directory (totalBytesRootDir / bytesPerSector = totSectorsRootDir)
			xchg	ax, cx				; swap ax with cx, i.e. store result in cx and clear ax
			
			;=============================================
			; Compute the sector (location) of root directory and store in ax
			; Compute the data sector location and store into datasector (located after the root dir)
			;=============================================

			mov	al, BYTE [bpbNumberOfFATs]	; number of FATs in ax
			mul	WORD [bpbSectorsPerFAT]		; sectors used by FATs in ax
			add	ax, WORD [bpbReservedSectors]	; adjust for bootsector, add reserved sectors to ax
			mov	WORD [datasector], ax		; base of root directory
			add	WORD [datasector], cx		; add base of rootDir to rootDirSize and store into data sector
			
			;=============================================
			; Read root directory into memory (07C0:0200), after the bootloader
			;=============================================
			
			mov	bx, 0x0200			; location of root dir straight after the bootcode (512th byte)
			call	ReadSectors			; read the roorDir, cx = numSectorsInRootDir, ax = rootDirBase and bx = offset

		;=============================================
		; Find stage 2 bootloader file
		;=============================================

		FindScndStg:
		
			;=============================================
			; Browse root directory for binary image
			;=============================================

			mov	cx, WORD [bpbRootEntries]	; load loop counter with the number of entries in rootDir
			mov	di, 0x0200			; locate first root entry
			.LOOP:
				push	cx			; moves cx to stack
				mov	cx, 0x000B		; eleven character name, so cx is set to 11 as a counter
				mov	si, ImageName		; image name to find
				push	di			; moves di (dest index) to stack
				rep	cmpsb			; compares each of the 11 bytes from si and di then inc(si), inc(di) and dec(cx) while cx <> 0
								; there is an inc of si and di if bit 10 of FLAGS is 0, otherwise, there is a dec of si and di
								; if the strings do not match, ZF is set to 0
				pop	di			; recovers di from stack
				je	LoadFAT			; if the operands of last cmp* are equal, then jump
				pop	cx			; recovers cx from stack
				add	di, 0x0020		; queue next directory entry (di + 32d)
				loop	.LOOP			; jumps to .LOOP and decrements cx

			jmp	FAILURE				; no more entries, file not found

		;==============================================
		; Load FAT
		;==============================================
		
		LoadFAT:
			
			;=============================================
			; Save the starting cluster of file in cluster
			;=============================================

			mov	dx, WORD [di + 0x001A]		; di + 26d = first cluster (bytes 26 and 27 or rootDir)
			mov	WORD [cluster], dx		; file's first cluster
			
			;=============================================
			; Compute size of FAT and store in cx
			;=============================================

			xor	ax, ax				; clear ax
			mov     al, BYTE [bpbNumberOfFATs]	; number of FATs
			mul	WORD [bpbSectorsPerFAT]		; compute number sectors used by FATs
			mov	cx, ax				; move result in cx
			
			;=============================================
			; Compute location of FAT and store in ax
			;=============================================

			mov	ax, WORD [bpbReservedSectors]	; FATS are located straight after the reserved sectors
			
			;=============================================
			; Read FAT into memory (07C0:0200), i.e. overlap the root directory
			;=============================================

			mov	bx, 0x0200			;  location of root dir straight after the bootcode (512th byte)
			call	ReadSectors

		;=============================================
		; Read file at (0050:0000), (es:bx)
		;=============================================
			
		mov	ax, 0x0050				; segment 0x50
		mov	es, ax					; setup
		mov	bx, 0x0000				; segment:offset
		push	bx					; stack bx
			
		;=============================================
		; Load file
		;=============================================
			
		LoadFileCluster:

			;=============================================
			; Load current cluster
			;=============================================
			mov	ax, WORD [cluster]		; cluster to read
			pop	bx				; location to read cluster
			call	ClusterLBA			; convert cluster to LBA
			xor     cx, cx				; clear cx
			mov	cl, BYTE [bpbSectorsPerCluster]	; number of sectors to read
			call	ReadSectors			; read the clusters
			push	bx				; stack bx
			
			;=============================================
			; Compute next cluster
			;=============================================

			mov	ax, WORD [cluster]		; get current cluster in ax
			mov	cx, ax				; copy current cluster in cx
			mov	dx, ax				; copy current cluster in dx
			shr	dx, 0x0001			; divide dx by two
			add	cx, dx				; cx becomes (1.5 x currentCluster)
			mov	bx, 0x0200			; location of FAT in memory
			add	bx, cx				; index into FAT, add FAT location to cx
			mov	dx, WORD [bx]			; read two bytes from FAT in dx
			test    ax, 0x0001			; tests if current cluster is odd and sets
			jnz     .OddCluster			; jump if odd, i.e. (AX and 0x0001 = 1)

			.EvenCluster:
				and	dx, 0000111111111111b	; take low twelve bits
				jmp	.GetNextCluster
			
			.OddCluster:
				shr	dx, 0x0004		; take high twelve bits
			
			.GetNextCluster:
				mov	WORD [cluster], dx	; store new cluster
				cmp	dx, 0x0FF0		; test for end of file
				jb	LoadFileCluster		; jumps if current cluster is less than 0x0ff0 (end of cluster in use area)
		
		;==============================================
		;Everything successfull, hand over to file
		;==============================================

		DONE:
			mov	si, msgCRLF			; Display image loaded
			call	Print				;
			push	WORD 0x0050			; Address to start execution
			push	WORD 0x0000			; Segment to start execution
		
			retf					; Far return, pops IP = 0x0000 and CS = 0x0050
		
		;==============================================
		;Failed to load file, reboot
		;==============================================

		FAILURE:
			mov	si, msgFailure			; Display failure message
			call	Print				;
			mov     ah, 0x00			; Function (0), read char
			int     0x16				; await keypress
			int     0x19				; warm boot computer

;=================================================
; Data and error messages
;=================================================

absoluteSector	db	0x00
absoluteHead	db	0x00
absoluteTrack	db	0x00
datasector	dw	0x0000					; pointer to the data area, after the root directory
cluster		dw	0x0000					; current cluster
ImageName	db	"KRNLDR  SYS"				; filename to load

msgLoading	db	"Loading Boot Image", 0x0D, 0x0A, 0x00
msgCRLF		db	0x0D, 0x0A, "Boot Image Loaded", 0x0D, 0x0A, 0x00
msgProgress	db	" +", 0x00
msgFailure	db	0x0D, 0x0A, "ERROR : Press Any Key to Reboot",0x0D, 0x0A, 0x00

;=================================================
; Signature
;=================================================

times 510 - ($-$$) db 0x00					; Max 512 bytes, zero all other
dw 0xAA55							; Boot Signiture
