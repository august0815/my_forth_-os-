; program: kernel_video
; Words related to screen driver.
;| by august0815 
;| 30.01.2010
;| based on kernel_video.fth from http://github.com/jdinuncio/forthos
;| License: GPL
;| José Dinuncio <jdinunci@uc.edu.ve>, 12/2009.
;| This file is based on Bran's kernel development tutorial file start.asm
[BITS 32]

		
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
; function: outb
;   Executes an out assembly instruction
;
; Stack:
;   val port --
;
; Parameters:
;   val - The value to out. Byte.
;   port - The port to output the value. int16.
defcode "outb", outb, 0
        pop edx
        pop eax
        out dx, al
        NEXT

; function: inb
;   Executes an IN assembly instruction
;
; Stack:
;   port -- val
defcode "inb", inb, 0
        pop edx
        xor eax, eax
        in  al, dx
        push eax
        NEXT
        
; function: cursor_pos_rel
;   Returns the cursor relative position respect to the origin of the screen
;
; Stack:
;   -- cursor_pos_rel
defword  "cursor_pos_rel", cursor_pos_rel, 0
        dd CURSOR_POS_Y
        dd FETCH
        LITN 160 
        dd MUL
        dd CURSOR_POS_X
        dd FETCH
        LITN 2 
        dd MUL
        dd ADD
        dd EXIT

; function: cursor_pos
;   Returns the absolute address of the cursor.
;
; Stack:
;   -- cursor_pos
defword  "cursor_pos", cursor_pos, 0
        dd cursor_pos_rel
        dd SCREEN
        dd ADD
        dd EXIT

; function: at_hw
;   Moves the cursor to the position indicated by cursor_pos variables.
;
; Stack:
;   --
defword  "AT_HW", AT_HW, 0
        dd cursor_pos_rel
        ;  Get the position of the cursor
        LITN 14 
        LITN 0x3D4
        dd outb
        ;  Say you're going to send the high byte
        dd DUP
        LITN 1 
        dd N_BYTE
        ;  ... get the higer byte
        LITN 0x3D5
        dd outb
        ;  ... and send it
        LITN 15 
        LITN 0x3D4
        dd outb
        ;  Say you're going to send the low bye
        LITN 0x3D5
        dd outb
        ;  ... and send it
        dd EXIT

; function: atx
;   Moves the cursor thoe the coordinates indicated. It updates the cursor_pos
;   variables.
; 
; Stack:
;   y x --
defword  "atx", atx, 0
        dd CURSOR_POS_X
        dd STORE
        dd CURSOR_POS_Y
        dd STORE
        dd EXIT

; function: ink
;   Set the ink color.
;
; Stack:
;   color --
defcode "INK", INK, 0
        pop eax
        and eax, 0x0f
        shl eax, 8
        mov ebx, [var_SCREEN_COLOR]
        and ebx, 0xf000
        or eax, ebx
        mov [var_SCREEN_COLOR], eax
        NEXT

; function: bg
;   Sets the background color.
;
; Stack:
;   color --
defcode "bg", bg, 0
        pop eax
        and eax, 0x0f
        shl eax, 12
        mov ebx, [var_SCREEN_COLOR]
        and ebx, 0x0f00
        or eax, ebx
        mov [var_SCREEN_COLOR], eax
        NEXT

; function:  bright
;   Takes a color and returns its brighter version.
;
; Stack:
;   color -- color
defword  "bright", bright, 0
        LITN 8 
        dd ADD
        dd EXIT

; function screen_scroll
;
; Stack:
;   --
defword  "screen_scroll", screen_scroll, 0
        dd SCREEN
        dd DUP
        LITN 160 
        dd ADD
        dd SWAP
        LITN 3840 
        dd CMOVE
        ;  TODO - Clean last line, move cursor
        dd _clean_last_line
        dd EXIT

defword  "_clean_last_line", _clean_last_line, 0
        dd SCREEN
        dd DUP
        LITN 4000 
        dd ADD
        dd SWAP
        LITN 3840 
        dd ADD
        do
        dd SCREEN_COLOR
        dd FETCH
        dd OVER
        dd STOREWORD
        dd INCR
        loop
        dd EXIT

; function: screen_scroll_
;   Scrolls the screen if the cursor goes beyond line 24.
;
; Stack:
;   --
defword  "screen_scroll_", screen_scroll_, 0
        dd CURSOR_POS_Y
        dd FETCH
        LITN 24 
        dd GT
        if
        dd screen_scroll
        LITN 24 
        dd CURSOR_POS_Y
        dd STORE
        then
        dd EXIT

; function: cursor_forward
;   Moves the cursor forward.
;
; Stack:
;   --
defword  "cursor_forward", cursor_forward, 0
        LITN 1 
        dd CURSOR_POS_X
        dd FETCH
        dd ADD
        LITN 80 
        dd DIVMOD
        dd CURSOR_POS_Y
        dd ADDSTORE
        dd CURSOR_POS_X
        dd STORE
        dd screen_scroll_
        dd EXIT
; function: cursor_back
;   Moves the cursor back.
;
; Stack:
;   --
defword  "cursor_back", cursor_back, 0
        LITN -1 
        dd CURSOR_POS_X
        dd FETCH
        dd ADD
        LITN 80 
        dd DIVMOD
        dd CURSOR_POS_Y
        dd ADDSTORE
        dd CURSOR_POS_X
        dd STORE
        dd screen_scroll_
        dd EXIT
; function: c>cw, char_to_charword
;   Converts a character in a charword.
;
;   A charword is a 16bits word with information about the character to be 
;   printed and its colors.
;
; Stack:
;   char -- charword
defcode "c>cw", char_to_charword, 0
        pop eax
        and eax, 0xff
        mov ebx, [var_SCREEN_COLOR]
        or eax, ebx
        push eax
        NEXT


; function: emitcw
;   Prinst a character word
;
; Stack:
;   charword --
defword  "emitcw", emitcw, 0
        dd cursor_pos
        dd STOREWORD
        dd cursor_forward
        dd EXIT

; function: emit
;   Prinst a character. (only if char on stack is ascii)
;
; Stack:
;   char --
defword  "EMIT", EMIT, 0
		dd DUP
		LITN ' '
		dd LT
		if
		drop
		else
        dd char_to_charword
        dd emitcw
        then
        dd EXIT

; function: printcstring
;   Prints a C string
;
; Stack:
;   &string --
defword  "PRINTCSTRING", PRINTCSTRING, 0
        begin
        dd DUP
        dd FETCHBYTE
        dd DUP
        while
        dd EMIT
        dd INCR
        repeat
        dd TWODROP
        dd EXIT
            
; function: clear 
;   Clear the screen
;
; Stack:
;   char --
defword  "CLEAR", CLEAR, 0
        LITN 0 
        LITN 0 
        dd atx
        LITN 2000 
        LITN 0 
        do
        dd spc
        loop
        LITN 0 
        LITN 0 
        dd atx
        dd EXIT

; function: cr            
;   Prints a cr
;
; Stack:
;    --
defword  "CR", CR, 0
        LITN 1 
        dd CURSOR_POS_Y
        dd ADDSTORE
        LITN 0 
        dd CURSOR_POS_X
        dd STORE
        dd AT_HW
        dd screen_scroll_
        dd EXIT

; function: spc
;   Prints a space
defword  "spc", spc, 0
        LITN 32 
        dd EMIT
        dd EXIT
		 	
; function: tab
;   Prints a tab.
;
; Stack:
;   --
defword  "TAB", TAB, 0
        ;  TODO - Move to the next column multiple of 8
        LITN 8 
        dd CURSOR_POS_X
        dd ADDSTORE
        dd AT_HW
        dd EXIT




%include "kernel_keyboard.s"	  ; the new files for include		
				
;%endif
