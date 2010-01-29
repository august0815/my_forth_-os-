; file: kernel_video

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
; Stack
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
; Stack
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
; Stack
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
;   Prinst a character.
;
; Stack:
;   char --
defword  "EMIT", EMIT, 0
        dd char_to_charword
        dd emitcw
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


%define _key_stat_caps 0x01
%define _key_stat_shift 0x02

; variable: key_status
;   Store the status of caps, shift and CONTROL keys.
defvar "key_status", key_status, 0, 0

; function: kbd_flags
;   Returns the keyboard status code.
;
; Stack:
;   -- kbd_status
defword "kbd_flags", kbd_flags, 0
     LITN 0x64 
     dd  inb
dd EXIT

; function: kbd_buffer_full
;   true if there is a scancode waiting to be readed
;
; Stack:
;   -- bool
defword  "kbd_buffer_full", kbd_buffer_full, 0
    dd kbd_flags 
    LITN 1
    dd AND
dd EXIT

; function: kbd_scancode_now
;   Returns the scancode readed on the keyboard at this moment.
;
; Stack:
;   -- scancode
defword  "kbd_scancode_now", kbd_scancode_now, 0
        LITN 0x60
        dd inb
        dd EXIT

; function: kbd_scancode
;   Waits for a key pressed and returns its sacancode.
;
; Stack:
;   -- scancode
defword  "kbd_scancode", kbd_scancode, 0
        begin
        dd kbd_buffer_full
        until
        dd kbd_scancode_now
        LITN 0xFF
        dd AND
        dd EXIT


; function _tx_key_status
;   Test and xor the key_status variable.
;
;   If the scancode is equal to the given test, makes an xor
;   between key_status and flags.
;
; stack:
;   scancode test flag --
defword  "_tx_key_status", _tx_key_status, 0
        dd ROT
        dd EQU
        if
        dd key_status
        dd FETCH
        dd XOR
        dd key_status
        dd STORE
        else
        dd DROP
        then
        dd EXIT

; function: _update_key_status
;   Updates the kbd_flags variable according with the scancode given.
;
; Stack:
;   scancode --
defword  "_update_key_status", _update_key_status, 0
        ;  TODO - xor could fail in some cases. Set o clear the bit.
        dd DUP
        LITN 58 
        LITN _key_stat_caps
        dd _tx_key_status
        ;  caps   down
        dd DUP
        LITN 42 
        LITN _key_stat_shift
        dd _tx_key_status
        ;  lshift down
        dd DUP
        LITN 170 
        LITN _key_stat_shift
        dd _tx_key_status
        ;  lshift up
        dd DUP
        LITN 54 
        LITN _key_stat_shift
        dd _tx_key_status
        ;  rshift down
        dd DUP
        LITN 182 
        LITN _key_stat_shift
        dd _tx_key_status
        ;  rshift up
        dd DROP
        dd EXIT

; stack:
;   scancode -- bool
defword  "_key_down?", _key_down?, 0
        LITN 0x80
        dd AND
        dd ZEQU
        ; 0x79 and 0<>
        dd EXIT

; function: sc>c (SCANCODE2CHAR)
;   Converts a scancode to an ASCII character.
; 
;   If the scancode correspond to keyup or to a non-character
;   it returns 0
;
; stack:
;   scancode -- char
defword  "sc>c", scancode2char, 0
        dd DUP
        dd _key_down?
        if
        LITN 4 
        dd MUL
        dd key_status
        dd FETCH
        dd ADD
        LITN keymap
        dd ADD
        dd FETCHBYTE
        else
        dd DROP
        LITN 0 
        then
        dd EXIT
;%undef LINK             ;   previous one
;%xdefine LINK  _video_end ;
; function: getchar
;   Waits for a key to be pressed and then returns its ASCII code.
;
; Stack:
;   -- c
defword  "getchar", getchar, 0
        LITN 0
        begin
        dd DROP
        dd kbd_scancode
        dd DUP
        dd _update_key_status
         dd scancode2char
        dd DUP
        until
        dd EXIT



%include "kbd_map.s"	  ; the new files for include		
				
;%endif
