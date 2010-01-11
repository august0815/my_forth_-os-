; file: kernel_video

[BITS 32]
defcode "OUTB", OUTB, 0
            ; ( val addr -- )
            pop edx
            pop eax
            out dx, al
            NEXT
		
; Screen words
;defconst SCREEN, SCREEN, 0, 0xB8000
; var: CURSOR_POS_X
defvar "CURSOR_POS_X", CURSOR_POS_X, 0 , 0
; var: CURSOR_POS_Y
defvar "CURSOR_POS_Y", CURSOR_POS_Y, 0 , 0
; var: SCREEN_COLOR
defvar "SCREEN_COLOR", SCREEN_COLOR, 0, 0x0f00
; var: KEYBUFF
defvar "KEYBUFF" , KEYBUFF , 0 , 0

section .text
; function: AT_HW
defword "ATHW", AT_HW , 0
            dd CURSOR_POS_REL
            LITN 14             ; Tell we'll send high byte of position
            LITN 0x3D4          ;
            dd OUTB             ;

            dd DUP, FETCH       ; Send high byte
            LITN 1              ;
            dd N_BYTE           ;
            LITN 0x3D5          ;
            dd OUTB             ;

            LITN 15             ; Tell we'll send low byte of position
            LITN 0x3D4          ;
            dd OUTB             ;

            dd FETCH            ; Send low byte
            LITN 0x3D5          ;
            dd OUTB             ;
            dd EXIT
          

; function: atx  (y:line x:col --)
defword "atx", atx, 0
            dd CURSOR_POS_X, STORE
            dd CURSOR_POS_Y, STORE
            ;dd AT_HW
            dd EXIT

; function: INK   (ink --)
defcode "INK", INK, 0
            pop eax
            and eax, 0x0f
            shl eax, 8
            mov ebx, [var_SCREEN_COLOR]
            and ebx, 0xf000
            or eax, ebx
            mov [var_SCREEN_COLOR], eax
            NEXT

; function: BG   (ink --)
defcode "BG", BG, 0
            pop eax
            and eax, 0x0f
            shl eax, 12
            mov ebx, [var_SCREEN_COLOR]
            and ebx, 0x0f00
            or eax, ebx
            mov [var_SCREEN_COLOR], eax
            NEXT

; function: C>CW, CHAR_TO_CHARWORD   ( char -- charword )
; Converts a character in a charword (attributes+character)
defcode "C>CW", CHAR_TO_CHARWORD, 0
            pop eax
            and eax, 0xff
            mov ebx, [var_SCREEN_COLOR]
            ;shl ebx, 8
            or eax, ebx
            push eax
            NEXT

; function:  BRIGHT
defword "BRIGHT", BRIGHT, 0
            dd LIT
            dd 8
            dd ADD
            dd EXIT

; function: CURSOR_POS_REL  ( -- cursorpos)
defword "CURSOR_POS_REL", CURSOR_POS_REL, 0
            ; ( -- cursorpos)
            dd CURSOR_POS_Y, FETCH
            LITN 160
            dd MUL
            dd CURSOR_POS_X, FETCH
            LITN 2
            dd MUL
            dd ADD
            dd EXIT

; function: CURSOR_POS
defword "CURSOR_POS", CURSOR_POS, 0
            dd CURSOR_POS_REL
            dd SCREEN   ; constante
            dd ADD
            dd EXIT

; function: SCREEN_SCROLL
defcode "SCREEN_SCROLL", SCREEN_SCROLL, 0
            call scroll_up
            dd NEXT

; function: SCREEN_SCROLL_
defword "SCREEN_SCROLL_", SCREEN_SCROLL_, 0
            dd CURSOR_POS_Y, FETCH
            LITN 24
            dd GT		;CURSOR_POS_Y>24 !!
            if
                LITN 24
                dd CURSOR_POS_Y, STORE
                LITN 0
                dd CURSOR_POS_X, STORE
                dd SCREEN_SCROLL
            then
            dd EXIT

; function: CURSOR_FORWARD
defword "CURSOR_FORWARD", CURSOR_FORWARD, 0
            LITN 1
            dd CURSOR_POS_X, ADDSTORE
            dd CURSOR_POS_X, FETCH
            LITN 80
            dd DIVMOD
            dd CURSOR_POS_Y, ADDSTORE
            dd CURSOR_POS_X, STORE
            dd SCREEN_SCROLL_
            dd AT_HW
            dd EXIT

; function: EMITCW (charword --)
; prints a charword (attributes+character)
defword "EMITCW", EMITCW, 0
             dd CURSOR_POS
            dd STOREWORD
            dd CURSOR_FORWARD
            dd EXIT

; function: EMIT (char --)
defword "EMIT" , EMIT , 0
            dd CHAR_TO_CHARWORD
            dd EMITCW
            dd EXIT

; function: PRINTCSTRING(&cstring --)
defword "PRINTCSTRING", PRINTCSTRING, 0
            begin
                dd DUP, FETCHBYTE, DUP
            while
                dd EMIT, INCR
            repeat
            dd DROP, DROP
            dd EXIT
            
; function: CLEAR   (--)           
defword "CLEAR", CLEAR, 0
            LITN 0                  ; Cursor at 0,0
            LITN 0                  ;
            dd atx                  ;
            LITN 80*25              ; for i = 0 to 80*25
            LITN 0                  ;
            do
                LITN ' '            ;   emit ' '
                dd EMIT             ;
            loop
            LITN 0                  ; Cursor at 0,0
            LITN 0                  ;
            dd atx                  ;
            dd EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; new code
            
; function: CR            
defword "CR", CR , 0 ; TESTED_OK
			LITN 1
            dd CURSOR_POS_Y, ADDSTORE
            LITN 0
            dd CURSOR_POS_X, STORE
			dd AT_HW
			dd SCREEN_SCROLL_
		 	dd EXIT
		 	

; function: TAB
defword "TAB" , TAB ,0	
			LITN 8
            dd CURSOR_POS_X, ADDSTORE
			dd AT_HW
			dd SCREEN_SCROLL_
			dd EXIT		

;THIS IS CODE FORM retro8 by crc
;			
; function: IN => KEY		 TESTED_OK
defcode "IN" ,IN ,0 ; 
		call sys_key
		NEXT

;THIS IS CODE FORM retro8 by crc
;	rewrite it? some day?		
sys_key:
 	xor   ebx,ebx  		; Show the coursor
	mov   bl,[var_CURSOR_POS_X]				     
	mov   ecx,ebx
	mov   bl,[var_CURSOR_POS_Y]				      
	mov   eax,80
	mul   bx
	add   eax,ecx				       
    mov   edx,0x3d4 
	mov   ecx,eax
	mov   al,15
	out   dx,al
	mov   eax,ecx
	inc   edx
	out   dx,al
	mov   al,14
	dec   edx
	out   dx,al
	mov   eax,ecx
	mov   al,ah
	inc   edx
	out   dx,al			; Show the coursor end
	xor eax,eax	        ;  clear eax
.1:	in al,64h		;  Is any data waiting?
	test al,1	        ;  Is character = ASCII 0?
	jz .1		        ;  Yes? Try again
	in al,60h	        ;  Otherwise, read scancode
	xor edx,edx	        ;  edx: 0=make, 1=break
	test al,80h	        ;  Is character = HEX 80h?
	jz .2		        ;  Skip the next line
	inc edx 	        ;  Update edx
.2:	 and al,7Fh		;  Filters to handle
	cmp al,39h	        ;  the ignored keys
	  ja .1 	        ;  We just try another key
	mov ecx,[board]         ;  Load the keymap
	mov al,[ecx+eax]        ;  Get the key ASCII char
	  or al,al		        ;  Is is = 0?
	js .shift		        ;  No, use CAPITALS
	jz .1		        ;  Ignore 0's
	or dl,dl		        ;  Filter for break code
	jnz .1		        ;  Ignore break code
	;THIS IS CODE FORM retro8 by crc  END
	mov dword [var_KEYBUFF],eax
	   ; echo
        push    eax
        push    ebx
        push    ecx
        cmp		al,0x08 ;don't display BS
        jbe .3
        cmp		al,0x0D ;don't display CR
        jbe .3
        and     eax,0x000000FF
        or      eax,[var_SCREEN_COLOR]
        mov     ecx,eax
        mov     eax,[var_CURSOR_POS_X]
        inc dword [var_CURSOR_POS_X]
        mov     ebx,[var_CURSOR_POS_Y]
     	push    ebx
        imul    ebx,[video_width]
        add     eax,ebx
        shl     eax,1
        add     eax,[video_base]
        pop     ebx
        mov     [eax],cx
.3:
       
        pop     ecx
        pop     ebx
        pop     eax
	ret

;THIS IS CODE FORM retro8 by crc	
.shift:  mov ecx,[edx*4 + .shifts]	 ;  Load the CAPITAL keymap
	mov [board],ecx 	        ;  Store into BOARD pointer
	jmp .1			  ;  And try again
.shifts dd shift,alpha
board dd alpha
alpha:
  db 0,27,"1234567890-=",8	        ;00-0E
  db 9,"qwertyuiop[]",10	        ;0F-1C
  db 0,"asdfghjkl;'`"		        ;1D-29
  db -1,"\zxcvbnm,./",-1,"+",0,32,-2    ;2A-3A
shift:
  db 0,27,"!@#$%^&*()_+",8	        ;00-0E
  db 9,"QWERTYUIOP{}",10	        ;0F-1C
  db 0,'ASDFGHJKL:"~'		        ;1D-29
  db -1,"|ZXCVBNM<>?",-1,"=",0,32,-2    ;2A-3A

;THIS IS CODE FORM retro8 by crc end
;---------------------------------------------------------------
video_base:     dd      0xB8000
video_width:    dd      80
video_height:   dd      25
;--------------scroll up---------------------------------------
;-------------in asm , maybe sometime in defwords ??
;-------------smoetimes pos (0,24) after scroll is lost !?? why
;-------------calling routines ?
scroll_up:								;
        push    eax
        push    ebx
        push    ecx
		mov     eax,0                	 ; Get offset for (0,1)
        lea     ebx,[eax+1]
        push    ebx
        imul    ebx,[video_width]
        add     eax,ebx
        shl     eax,1        
        add     eax,[video_base]        
        pop     ebx             		; EAX  source
	    mov     ebx,[video_base]       	; EBX  destination address
	    mov     ecx,0xf00			   	; ECX  number of bytes to move
	    mov edx, esi				 	; memory_move     				
        mov edi,ebx      
        mov esi,eax       
        rep movsb     
        mov esi, edx        			; Overwrite  bottom.
        mov     eax,0
        mov     ebx,[video_height]
        dec     ebx
        push    ebx
        imul    ebx,[video_width]
        add     eax,ebx
        shl     eax,1        
        add     eax,[video_base]        
        pop     ebx
        mov     ebx,[video_width]
        mov     ecx,[var_SCREEN_COLOR]
   		mov     cx,' '
.1:		mov     [eax],cx
        add     eax,2
        dec     ebx
        or      ebx,ebx
        jnz     .1
        pop     ecx
        pop     ebx
        pop     eax
        ret





%include "ext.s"	  ; the new files for include		
				
;%endif
