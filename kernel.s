;  program: kernel
;**   ___________________________________________________________________________

; func:   Version 0.01a

; section:   Copyright (C) 2010 august0815 (Mario Marcec)

 
  
%include "forth_words.s"
;%include "kernel_video.s"

[BITS 32]
defconst "SCREEN", SCREEN, 0, 0xB8000
defvar "PPTR", PPTR, 0 , 0
defvar "ISLIT", ISLIT, 0 , 0
defvar "END_OF_LINE", END_OF_LINE, 0 , 0
defvar "PARS_ERROR", PARS_ERROR, 0 , 0
defvar "TFFA" , TFFA , 0, 0
defvar "TNFA" , TNFA , 0, 0
defvar "RR" , RR , 0, 0
defvar "ONLYBODY" , ONLYBODY , 0, 0
defvar "CONTEXT" , CONTEXT , 0, 0
defvar "IMMED" , IMMED , 0, 0
defvar "LENTEILW", LENTEILW , 0, 0
defvar "PPTR_LAST", PPTR_LAST, 0 , 0
defvar "GRUB", GRUB, 0, 0
global var_GRUB
defvar "text_buff", text_buff, 0 ,0
defvar "SRC", SRC, 0 , 0
defvar "SRC_END", SRC_END, 0 , 0
defvar "FILP", FILP, 0 , 0
; Assembly Entry point
section .text
extern module
GLOBAL main
main:
			mov [var_S0],esp 			; Save the initial data stack pointer in FORTH variable S0.
            mov ebp, return_stack_top   ; init the return stack
            mov	dword [var_END_OF_LINE],0
            mov dword [var_STATE],0
            ;mov eax,[label_ENDE]
            ;mov dword[var_LATEST],eax
            ;mov	dword [currkey], buffer
			;mov  dword [bufftop] ,buffer
            mov eax,point_HERE
            mov dword [var_HERE],eax
            mov esi, cold_start         ; fist foth word to exec
            NEXT
            ret

; Forth Entry point
section .rodata
cold_start: 
			dd WELCOM
			dd CLEAR 
mes:        dd MES2
			dd show ,CR
int: 			 	 
			dd ZEIL
        	branch int
  		    dd STOP
   
interpret_is_lit db 0     
pptr: dw 0            

section .data
bienvenida:     db 'Welcome to ', 0
osname          db 'Goyo-OS-FORTH-0.0.1', 0
colofon         db 'Wait good thing in the future...', 0
ok: 			db '  OK ... ' ,0
key_press: 		db '   PRESS ANY KEY  .... ' , 0
outputmes 		db 'Words of forth' , 0
inputloop		db 'Enter  words' , 0
errmsg: 		db 'PARSE ERROR: AT ->' ,0
gef: 			db 'GEFUNDEN' , 0
ngef: 			db 'NICHT IN TABELLE' , 0
stackmes:		db 'STACK> ', 0
in_key:         times 256 db 0

; stacks
section   .bss
align 4096
            RETURN_STACK_SIZE equ 8192
return_stack:
            resb RETURN_STACK_SIZE
return_stack_top:

align 4096
BUFFER_SIZE equ 4096
buffer:
	resb BUFFER_SIZE

align 4096

point_HERE: resb 2048

point: resb 363748
top: resb 0
point2: resb 363748
top2: resb 0
