; file: ext
; by august0815
; 19.12.2009

; TODO clean up 
;
; remove not used code
section .text
defvar "text_buff", text_buff, 0 ,0
; function: ZEILE  ; einlesen einer Zeile bis CR   TESTED_OK
;
; edi  push base address
; ecx		 push length
;zeile_buffer:  ist 1024 byte lang
defword  "zeile", zeile, 0
       LITN 1
        begin
        while
        dd getchar 
        dd DUP
        dd EMIT
        dd DUP
        LITN 0x0D
        dd EQU
        if
        dd DROP
        LITN 0x20
        dd SWAP
        dd DUP
        dd INCR
        dd ROT
        dd STOREBYTE
        LITN 0
        dd SWAP
        dd DUP 
        dd INCR 
        dd ROT
        dd STOREBYTE
        dd DROP
        dd EXIT
        then
         dd  
        dd SWAP 
        dd DUP 
        dd INCR 
        dd ROT
        dd STOREBYTE 
        LITN 1
        repeat
        dd EXIT
text_buffer: times 1024 db 0
  
section .text
;rubout:
;		dec edi
;		push    eax
;		push    ebx
;        push    ecx
;        dec dword [var_CURSOR_POS_X]
;        mov al,' '
;        and     eax,0x000000FF
;        or      eax,[var_SCREEN_COLOR]
;        mov     ecx,eax
;        mov     eax,[var_CURSOR_POS_X]
;        mov     ebx,[var_CURSOR_POS_Y]
;        push    ebx
;        imul    ebx,[video_width]
;        add     eax,ebx
;        shl     eax,1
;        add     eax,[video_base]
;        pop     ebx
;        mov     [eax],cx
;        pop     ecx
;        pop     ebx
;        pop     eax
;	ret	
	
; function:  NUMBER  TESTED_OK
;
; IN : ecx 	 length of string
;
;     edi 	 start address of string
;
; OUT:eax parsed number
;
;     ecx number of unparsed characters (0 = no error)
defcode "NUMBER",NUMBER,0
	pop ecx		; length of string
	pop edi		; start address of string
	call _NUMBER
	push eax		; parsed number
	push ecx		; number of unparsed characters (0 = no error)
	NEXT
_NUMBER:
	xor eax,eax
	xor ebx,ebx
	test ecx,ecx		; trying to parse a zero-length string is an error, but will return 0.
	jz .5
	mov edx,[var_BASE]	; get BASE (in %dl)
	; Check if first character is '-'.
	mov bl,[edi]		; %bl = first character in string
	inc edi
	push eax		; push 0 on stack
	cmp bl,'-'		; negative number?
	jnz .2
	pop eax
	push ebx		; push <> 0 on stack, indicating negative
	dec ecx
	jnz .1
	pop ebx		; error: string is only '-'.
	mov ecx, $1
	ret
	; Loop reading digits.
.1:	imul eax,edx		; %eax *= BASE
	mov bl,[edi]		; %bl = next character in string
	inc edi
	; Convert 0-9, A-Z to a number 0-35.
.2:	sub bl,'0'		; < '0'?
	jb .4
	cmp bl,$10		; <= '9'?
	jb .3
	sub bl,$17		; < 'A'? (17 is 'A'-'0')
	jb .4
	add bl,$10
.3:	cmp bl,dl		; >= BASE?
	jge .4
	; OK, so add it to %eax and loop.
	add eax,ebx
	dec ecx
	jnz .1
	; Negate the result if first character was '-' (saved on the stack).
.4:	pop ebx
	test ebx,ebx
	jz .5
	neg eax
.5:	ret


; function: FIND   TESTED_OK
;
; IN: ecx = length
; edi = address
;
;OUT: ; eax = address of dictionary entry (or NULL)
defcode "FIND",FIND,0
	pop ecx		; ecx = length
	pop edi		; edi = address
	call _FIND
	
	push eax		; eax = address of dictionary entry (or NULL)
	NEXT
_FIND:
    push esi		; Save esi so we can use it in string comparison.
	; Now we start searching backwards through the dictionary for this word.
	mov edx,[var_LATEST]	; LATEST points to name header of the latest word in the dictionary
.1:	test edx,edx		; NULL pointer?  (end of the linked list)
	je .4
	; Compare the length expected and the length of the word.
	; Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
	; this won't pick the word (the length will appear to be wrong).
	xor eax,eax
	mov al,[edx+4]	; %al = flags+length field
	and al,(F_HIDDEN|F_LENMASK) ; %al = name length
	cmp byte al,cl		; Length is the same?
	jne .2
	; Compare the strings in detail.
	push ecx		; Save the length
	push edi		; Save the address (repe cmpsb will move this pointer)
	lea esi,[edx+5]	; Dictionary string we are checking against.
	repe cmpsb		; Compare the strings.
	pop edi
	pop ecx
	jne .2			; Not the same.
	; The strings are the same - return the header pointer in %eax
	pop esi
	mov eax,edx
	ret
.2:	mov edx,[edx]		; Move back through the link field to the previous word
	jmp .1			; .. and loop.
.4:	; Not found.
	pop esi
	xor eax,eax		; Return zero to indicate not found.
	ret



; function: ">CFA"  TESTED_OK
	defcode ">CFA",TCFA,0
	pop edi
	call _TCFA
	push edi
	NEXT
_TCFA:
	xor eax,eax
	add edi,4		; Skip link pointer.
	mov al,[edi]		; Load flags+len into %al.
	inc edi		; Skip flags+len byte.
	and al,F_LENMASK	; Just the length, not the flags.
	add edi,eax		; Skip the name.
	add edi,3		; The codeword is 4-byte aligned.
	and edi,~3
	ret



; function: >DFA
defword ">DFA",TDFA,0
	dd TCFA		; >CFA		(get code field address)
	dd INCR4		; 4+		(add 4 to it to get to next word)
	dd EXIT		; EXIT		(return from FORTH word)
	
; function: CREATE TESTED_OK
defcode "CREATE", CREATE, 0
	
    pop ecx		; %ecx = length
	pop ebx		; %ebx = address of name
	; Link pointer.
	mov  edi,[var_HERE]	; %edi is the address of the header
	mov  eax,[var_LATEST]	; Get link pointer
	
	stosd			; and store it in the header.
	; Length byte and the word itself.
	mov al,cl		; Get the length.
	stosb			; Store the length/flags byte.
	push esi
	mov esi,ebx		; %esi = word
	rep movsb		; Copy the word
	pop esi
	add edi,3		; Align to next 4 byte boundary.
	and edi,~3
	
	; Update LATEST and HERE.
	mov  eax,[var_HERE]
	mov dword [var_LATEST], eax
	mov dword [var_HERE],edi
    NEXT


; defcode; "," TESTED_OK
	defcode "," ,COMMA ,0
	pop eax		; Code pointer to store.
	call _COMMA
	NEXT
_COMMA:
  	 mov edi,[var_HERE]	; HERE
	stosd			; Store it.
	mov dword [var_HERE],edi	; HERE
	ret

; function: [   TESTED_OK
defcode "[" ,LBRAC,F_IMMED ;;F_IMMED,LBRAC,0
	mov dword [var_STATE],0	; Set STATE to 0.
	NEXT
; defcode ]	   TESTED_OK
defcode "]",RBRAC,0
	mov dword [var_STATE],1	; Set STATE to 1.
	NEXT

; function: ":"   TESTED_OK
defword ":" , COLON  ,0
	dd TEILWORT		; Get the name of the new word
    dd CREATE		; CREATE the dictionary entry / header
	dd LIT, DOCOL, COMMA	; Append DOCOL  (the codeword).
	dd LATEST, FETCH, HIDDEN ; Make the word hidden (see below for definition).
	dd RBRAC		; Go into compile mode.
	dd EXIT		; Return from the function.

; function: ;     TESTED_OK 
defword ";",SEMICOLON,F_IMMED 
	dd LIT, EXIT, COMMA	; Append EXIT (so the word will return).
	dd LATEST, FETCH, HIDDEN ; Toggle hidden flag -- unhide the word (see below for definition).
	dd LBRAC		; Go back to IMMEDIATE mode.
	dd EXIT		; Return from the function.

; function: IMMEDIATE  not tested
defcode "IMMEDIATE" , IMMEDIATE , F_IMMED
	mov edi,[var_LATEST]	; LATEST word.
	add edi,4		; Point to name/flags byte.
	xor	byte [edi], F_IMMED	; Toggle the IMMED bit.
	NEXT

; function: HIDDEN 
defcode "HIDDEN",HIDDEN,0
	pop edi		; Dictionary entry.
	add edi,4		; Point to name/flags byte.
	xor byte [edi],F_HIDDEN	; Toggle the HIDDEN bit.
	NEXT
; function: HIDE	
defword "HIDE",HIDE,0
	dd TEILWORT		; Get the word (after HIDE).
	dd FIND		; Look up in the dictionary.
	dd HIDDEN		; Set F_HIDDEN flag.
	dd EXIT		; Retur



; function: "'"  TESTED_OK
defcode "'",TICK,0 
	lodsd			; Get the address of the next word and skip it.
	push eax		; Push it on the stack.
	NEXT
	
; TODO Branching??

; function: LITSTRING
defcode "LITSTRING",LITSTRING,0
	lodsd			; get the length of the string
	push esi		; push the address of the start of the string
	push eax		; push it on the stack
	add esi,eax		; skip past the string
 	add esi,3		; but round up to next 4 byte boundary
	and esi,~3
	NEXT

	

; function: TEILWORT  rename later to WORD ; TESTED_OK 
;
; gibt den pointer des strings aus zeilenbuffer bis zum Leerzeichen
; zurück , PPTR zeigt danach auf das nächste Wort
; edi  		; push base address
; ecx		; push length

defcode "TEILWORT" , TEILWORT , 0
	call _tlwd
	push edi		; push base address
	push ecx		; push length
	NEXT

_tlwd:
	;/* Search for first non-blank character.  Also skip \ comments. */
    mov ebx,[var_PPTR]
.1:
	mov al,[ebx] ;_KEY		; get next key, returned in %eax
	test al,al
	jnz .5
	mov dword [var_TST1],0xffff
	ret
.5:	inc ebx
	cmp al,'\'		; start of a comment?
	je .3			; if so, skip the comment
	cmp al,' '
	jbe .1			; if so, keep looking
	;/* Search for the end of the word, storing chars as we go. */
	mov edi,ptr_buff	; pointer to return buffer
.2:
	stosb			; add character to return buffer
	mov al,[ebx] ;_KEY		; get next key, returned in %eax
	inc ebx; _KEY		; get next key, returned in %al
	cmp al,' '		; is blank?
	ja .2			; if not, keep looping
	
	;/* Return the word (well, the static buffer) and length. */
	sub edi,ptr_buff
	mov ecx,edi		; return length of the word
	mov edi,ptr_buff	; return address of the word
	mov dword [var_PPTR],ebx
	ret
.4:	
	;/* Code to skip \ comments to end of the current line. */
.3:
	mov al,[ebx] ;_KEY		; get next key, returned in %eax
	inc ebx ;_KEY
	cmp al,$13	; end of line yet?
	jne .3
	jmp .1
section .data			; NB: easier to fit in the .data section
	; A static buffer where WORD returns.  Subsequent calls
	; overwrite this buffer.  Maximum word length is 256 chars.
ptr_buff: times 256 db 0
		
section .text
;defcode: INTERPRET    better now 
defcode "INTERPRET",INTERPRET,0  
	mov	dword [var_TST],0	
	call _tlwd ; Returns %ecx = length, %edi = pointer to word.
	; Is it in the dictionary?
	xor eax,eax
	mov dword [interpret_is_lit],eax ; Not a literal number (not yet anyway ...)
	call _FIND		; Returns %eax = pointer to header or 0 if not found.
	test eax,eax		; Found?
	jz .1
	
	; In the dictionary.  Is it an IMMEDIATE codeword?
	mov edi,eax		; %edi = dictionary entry
	mov al,[edi+4]	; Get name+flags.
	push ax		; Just save it for now.
	call _TCFA		; Convert dictionary entry (in %edi) to codeword pointer.
	pop ax
	and al,0x80     ;F_IMMED	; Is IMMED flag set?
	mov eax,edi
	
	jnz .4 			; If IMMED, jump straight to executing.
    
	jmp .2

.1:	; Not in the dictionary (not a word) so assume it's a literal number.
    ;
	inc dword [interpret_is_lit]
	call _NUMBER		; Returns the parsed number in %eax, %ecx > 0 if error
	test ecx,ecx
	jnz .6
	mov ebx,eax
	mov eax,LIT		; The word is LIT

.2:	; Are we compiling or executing?
	;--------------NOW COMPILING !!-----------------------------
	mov	dword edx, [var_STATE]
	test	edx, edx
	jz	.4			; Jump if executing.

	; Compiling - just append the word to the current dictionary definition.
	call	 _COMMA
	mov	ecx, [interpret_is_lit] ; Was it a literal?
	test	ecx, ecx
	jz	.3
	mov eax,ebx		; Yes, so LIT is followed by a number. 
	call	 _COMMA
.3:	NEXT

.4:	; Executing - run it!
	mov ecx,[interpret_is_lit] ; Literal?
	test ecx,ecx		; Literal?
	jnz .5
    ; Not a literal, execute it now.  This never returns, but the codeword will
	; eventually call NEXT which will reenter the loop in QUIT.
	jmp [eax]

.5:	; Executing a literal, which means push it on the stack.
	push ebx
	NEXT

.6:	; Parse error (not a known word or a number in the current BASE).
	; Print an error message followed by up to 40 characters of context.
	;mov ebx,2		; 1st param: stderr
	mov	dword [var_TST] ,0xffff
	NEXT


            
defword "inter" , inter ,0	
 			LITN 0
			dd TST , STORE
			dd CR
inter1:	    dd INTERPRET
			dd TST1,FETCH    ; endof line Interprt was OK
 			dd ZNEQU
			if
				dd CR
				LITN ok
				dd PRINTCSTRING
			 	dd CR  	
				branch next1
			then	 
			dd TST,FETCH      ; error in einput stream
 			dd ZNEQU
			if  	
				dd CR
				LITN 10
				dd INK
				dd AT_HW
				LITN text_buffer
				dd PRINTCSTRING , CR	
				LITN 12
				dd INK
			 	LITN errmsg
			 	dd PRINTCSTRING 
			 	dd PPTR_LAST,FETCH
			 	LITN 6 ;dd LENTEILW ,FETCH
			 	dd printt
			 	dd CR ;dd PRESSKEY
			 	LITN 15
			 	dd INK
			 	dd AT_HW
			 	branch next1
			then	 
			dd PPTR , FETCH 
            dd PPTR_LAST , STORE
			branch inter1
next1:		LITN 0
            dd DUP
			dd TST , STORE  ;clear Error_flag
            dd TST1 , STORE ;clear End_of_Line fla
            
			dd EXIT  

defword "ZEIL" , ZEIL ,0
       	LITN text_buffer
        dd DUP
        dd text_buff
        dd STORE 
        dd zeile
       ; dd ZEILEMIT 
        dd inter
        LITN text_buffer
        dd DUP
        dd PPTR_LAST
        dd STORE
        dd PPTR
        dd STORE
        ;  clsstack drop
 			dd EXIT		; EXIT		(return from FORTH word)

%include "ext1.s"
