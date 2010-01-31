; file: keyboard
; by august0815 
;| 30.01.2010
;| based on kernel_kbd.fth from http://github.com/jdinuncio/forthos
;| License: GPL
;| José Dinuncio <jdinunci@uc.edu.ve>, 12/2009.

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

%include "kbd_map.s"
