; program: forth_words

; myforth: My own forth system.
; This file is a translation of jonesforth
; [http:;www.annexia.org/_file/jonesforth.s.txt] for being compiled with nasm

%ifndef forth_words
%define forth_words
%include "forth_core.s"

[BITS 32]
; forthword ptrs contains the basic words of a forth interpreter. The escential
; routines and word ptrs are in forthcore.

; function: STOP endless loop
defcode "STOP", STOP, 0
            jmp $

;MINIMALER BASIS-BEFEHLSSATZ
; function: LIT takes the next word (a literal value) in a word definition and stores it in the stack.
defcode "LIT", LIT, 0
            lodsd               ; Load the next word in the current definition
            push eax            ; pushes it on the stack
            NEXT                ; and executes the following word
;MINIMALER BASIS-BEFEHLSSATZ            
;Basic stack word ptrs
; function: DROP  (x -- )
;
; Discard the top data stack item and promote NOS to TOS
defcode "DROP", DROP,0
            pop eax       
            NEXT
; function: SWAP  ( x1 x2 -- x2 x1 )
;
; Exchange the top two data stack items.
defcode "SWAP", SWAP,0
            pop eax       
            pop ebx
            push eax
            push ebx
            NEXT
; function: DUP ( x -- x x ) 
;
; DUPlicate the top stack item.
defcode "DUP", DUP, 0
            mov eax, [esp]    
            push eax
            NEXT
; function: OVER  ( x1 x2 -- x1 x2 x1 )
;
; Make a copy of the second item on the stack.
defcode "OVER", OVER, 0
            mov eax, [esp + 4]   
            push eax      
            NEXT
; function: ROT (n1 n2 n3 -- n2 n3 n1 )
;
; ROTate the positions of the top three stack items such that the current top of stack becomes the second item.
defcode "ROT", ROT, 0
            pop eax
            pop ebx
            pop ecx
            push eax
            push ecx
            push ebx
            NEXT
; function: -ROT (x1 x2 x3 -- x3 x1 x2 )
;
; The reciprocal of ROT. Non ANS, but widely available.
defcode "-ROT", NROT, 0
            pop eax
            pop ebx
            pop ecx
            push ebx
            push eax
            push ecx
            NEXT
; function: 2DROP (  x1 x2 -- )
;
; Discard the top two data stack items.
defcode "2DROP", TWODROP, 0
            pop eax
            pop eax
            NEXT
; function: 2DUP ( x1 x2 -- x1 x2 x1 x2 )
;
; DUPlicate the top cell-pair on the data stack.   
defcode "2DUP", TWODUP, 0
            mov eax, [esp]
            mov ebx, [esp + 4]
            push ebx
            push eax
            NEXT
; function: 2SWAP (x1 x2 x3 x4 -- x3 x4 x1 x2 )
;
; Exchange the top two cell-pairs on the data stack.
defcode "2SWAP", TWOSWAP, 0
            pop eax
            pop ebx
            pop ecx
            pop edx
            push ebx
            push eax
            push edx
            push ecx
            NEXT
; function ?DUP  ( x --      0 | x x )
;
;DUPplicate the top stack item only if it is non-zero. Nearly always used before a conditional branch.
defcode "?DUP", QDUP, 0
            mov eax, [esp]
            test eax, eax
            jz .1
            push eax
.1: NEXT

; function: 1+
defcode "1+", INCR, 0
            ; ( n -- n+1 )
            inc dword [esp]    
            NEXT

; function: 1-
defcode "1-", DECR, 0
            dec dword [esp]    
            NEXT

; function: 4+
defcode "4+", INCR4, 0
            add dword [esp], 4     
            NEXT

; function: 4-
defcode "4-", DECR4, 0
            sub dword [esp], 4     
            NEXT

; function: +
defcode "+", ADD, 0
            pop eax       
            add [esp], eax   
            NEXT

; function: -                                                                   
defcode "-", SUB, 0
            pop eax       
            sub [esp], eax   
            NEXT

; function: *
defcode "*", MUL, 0
            pop eax
            pop ebx
            imul eax, ebx
            push eax      
            NEXT


;            In this FORTH, only /MOD is primitive.  Later we will define the / and MOD word ptrs in
;            terms of the primitive /MOD.  The design of the i386 assembly instruction idiv which
;            leaves both quotient and remainder makes this the obvious choice.

; function: /MOD
defcode "/MOD", DIVMOD, 0
            xor edx, edx
            pop ebx
            pop eax
            idiv ebx
            push edx      
            push eax      
            NEXT

; function: /
defword "/", DIV, 0
            dd DIVMOD
            dd SWAP
            dd DROP
            dd EXIT

; function: MOD
defword "MOD", MOD, 0
            dd DIVMOD
            dd DROP
            dd EXIT

; Comparisons
; function: =
defcode "=", EQU, 0
            pop eax
            pop ebx
            cmp eax, ebx
            sete al
            movzx eax, al
            push eax
            NEXT

; function: <>
defcode "<>", NEQU, 0
            pop eax
            pop ebx
            cmp eax, ebx
            setne al
            movzx eax, al
            push eax
            NEXT

; function: <
defcode "<", LT, 0
            pop eax
            pop ebx
            cmp ebx, eax
            setl al
            movzx eax, al
            push eax
            NEXT

; function: >
defcode ">", GT, 0
            pop eax
            pop ebx
            cmp ebx, eax
            setg al
            movzx eax, al
            push eax
            NEXT

; function: <=
defcode "<=", LE, 0
            pop eax
            pop ebx
            cmp ebx, eax
            setle al
            movzx eax, al
            push eax
            NEXT

; function: >=
defcode ">=", GE, 0
            pop eax
            pop ebx
            cmp ebx, eax
            setge al
            movzx eax, al
            push eax
            NEXT

; function: 0=
defcode "0=", ZEQU, 0
            pop eax
            test eax, eax
            setz al
            movzx eax, al
            push eax
            NEXT

; function: 0<>
defcode "0<>", ZNEQU, 0
            pop eax
            test eax, eax
            setnz al
            movzx eax, al
            push eax
            NEXT

; function: 0<
defcode "0<", ZLT, 0
            pop eax
            test eax, eax
            setl al
            movzx eax, al
            push eax
            NEXT

defcode "0>", ZGT, 0
            pop eax
            test eax, eax
            setg al
            movzx eax, al
            push eax
            NEXT

; function: 0<=
defcode "0<=", ZLE, 0
            pop eax
            test eax, eax
            setle al
            movzx eax, al
            push eax
            NEXT

; function: 0>=
defcode "0>=", ZGE, 0
            pop eax
            test eax, eax
            setge al
            movzx eax, al
            push eax
            NEXT

; function: AND
defcode "AND", AND, 0   
            pop eax
            and [esp], eax
            NEXT

; function: OR
defcode "OR", OR, 0 
            pop eax
            or [esp], eax
            NEXT

; function: XOR
defcode "XOR", XOR, 0   
            pop eax
            xor [esp], eax
            NEXT

; function: INVERT
defcode "INVERT", INVERT, 0
            not dword [esp]
            NEXT

;MINIMALER BASIS-BEFEHLSSATZ
; function: !
defcode "!", STORE, 0
            ; ( n addr -- )
            pop ebx       
            pop eax       
            mov [ebx], eax    
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: @
defcode "@", FETCH, 0
            pop ebx       
            mov eax, [ebx]    
            push eax      
            NEXT

; function: +!
defcode "+!", ADDSTORE, 0
            pop ebx       
            pop eax       
            add [ebx], eax   
            NEXT

; function: -!
defcode "-!", SUBSTORE, 0
            pop ebx       
            pop eax       
            sub [ebx], eax   
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: C!
defcode "C!", STOREBYTE, 0
            pop ebx       
            pop eax       
            mov [ebx], al    
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: C@
defcode "C@", FETCHBYTE, 0
            pop ebx       
            xor eax, eax
            mov al, [ebx]    
            push eax      
            NEXT

; function: W!
defcode "W!", STOREWORD, 0
            pop ebx       
            pop eax       
            mov [ebx], ax    
            NEXT

; function: W@
defcode "W@", FETCHWORD, 0
            pop ebx       
            xor eax, eax
            mov ax, [ebx]    
            push eax      
            NEXT

; C@C! is a useful byte copy primitive. */
; function: C@C!
defcode "C@C!", CCOPY, 0
            mov ebx, [esp + 4]  	;movl 4(%esp),%ebx	// source address
            mov al, [ebx]    		;movb (%ebx),%al		// get source character
            pop edi       			;pop %edi		// destination address
            stosb          			;stosb			// copy to destination
            push edi    			;push %edi		// increment destination address
            inc dword [esp + 4]     ;incl 4(%esp)		// increment source address  
            NEXT

; function: CMOVE
; and CMOVE is a block copy operation. */
defcode "CMOVE", CMOVE, 0
            mov edx, esi      
            pop ecx       
            pop edi       
            pop esi       
            rep movsb      
            mov esi, edx      
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function:  >R  Return Stack
 defcode ">R", TOR, 0
            pop eax       
            PUSHRSP eax       
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: R>
defcode "R>", FROMR, 0
            POPRSP eax    
            push eax      
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: RSP@
defcode "RSP@", RSPFETCH, 0
            push ebp
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: RSP!
defcode "RSP!", RSPSTORE, 0
            pop ebp
            NEXT

; function: RDROP
defcode "RDROP", RDROP, 0
            add ebp, 4       
            NEXT

;MINIMALER BASIS-BEFEHLSSATZ
; function: BRANCH
defcode "BRANCH", BRANCH, 0
            add esi, [esi]
            NEXT
;MINIMALER BASIS-BEFEHLSSATZ
; function: 0BRANCH oder ?BRANCH QBRANCH
defcode "0BRANCH", ZBRANCH, 0
            pop eax
            test eax, eax
            jz code_BRANCH 
            lodsd           
            NEXT

; Data stack manipulation
; function: DSP@
defcode "DSP@", DSPFETCH, 0
    mov eax, esp
    push eax
    NEXT

; function: DSP!
defcode "DSP!", DSPSTORE, 0
    pop esp
    NEXT

; Shift and Rotate
; function: SHL
defcode "SHL", SHL, 0
    ; ( n1 n2 -- n1 << n2)
    pop ecx
    pop eax
    shl eax, cl
    push eax
    NEXT

; function: SHR
defcode "SHR", SHR, 0
    ; ( n1 n2 -- n1 >> n2)
    pop ecx
    pop eax
    shr eax, cl
    push eax
    NEXT

; function: N_BYTE
defword "N_BYTE", N_BYTE, 0
    ; Gives the n-th byte of a cell
    ; ( b3b2b1b0 n -- bn)
    LITN 8
    dd MUL
    dd SHR
    LITN 0xff
    dd AND
    dd EXIT
;MINIMALER BASIS-BEFEHLSSATZ    
; function: EXECUTE
;   Executes the word which address in in the stack
;
; stack:
;   addr -- ??
defcode "EXECUTE", EXECUTE, 0
        pop eax
        jmp [eax]
        
;-------------This Code is taken from bb4wforth.f---see--README-------
       
defcode "D+",  Dadd, 0 ; Added by RTR
        pop edx                 ; MS 32-bits
        pop eax                 ; LS 32-bits (Forth is big-endian)
        add dword [esp+4],eax
        adc dword [esp],edx
        NEXT
         
defcode "D-", Dsub,0 ; Added by RTR
        pop edx                 ; MS 32-bits
        pop eax                 ; LS 32-bits (Forth is big-endian)
        sub dword [esp+4],eax
        sbb dword [esp],edx
        NEXT

defcode "UM/MOD", UMdivmod , 0 ; Added by RTR (unsigned)
        pop ebx
        pop edx
        pop eax
        div ebx
        push edx                ; push remainder
        push eax                ; push quotient
        NEXT
        
defcode "UM*",  Umul64 ,0  ; Added by RTR
        pop eax
        pop ebx
        mul ebx                 ; unsigned multiply
        push eax                ; LS 32 bits (Forth is big-endian)
        push edx                ; MS 32 bits
        NEXT 
        
defcode "R@", Rfetch, 0 ; Added by RTR
        mov eax,[ebp]
        push eax
        NEXT  
              
defcode "M*", Mul64 ,0 ; Added by RTR
        pop eax
        pop ebx
        imul ebx                ; signed multiply
        push eax                ; LS 32 bits (Forth is big-endian)
        push edx                ; MS 32 bits
        NEXT
        
defcode "SM/REM", SMdivrem, 0 ; Added by RTR (signed)
        pop ebx
        pop edx
        pop eax
        idiv ebx
        push edx                ; push remainder
        push eax                ; push quotient
        NEXT
        
defcode "FM/MOD", FMdivmod,0 ; Added by RTR (floored)
        pop ebx
        pop edx
        pop eax
        idiv ebx
        or edx,edx
        jz _fmmodx              ; No remainder
        or eax,eax
        jns _fmmodx             ; Quotient is positive
        dec eax
        add edx,ebx
_fmmodx:
        push edx                ; push remainder
        push eax                ; push quotient
        NEXT
        
defcode "*/", Muldiv,0 ; Added by RTR
        pop ecx
        pop ebx
        pop eax
        imul ebx
        idiv ecx
        push eax
        NEXT
        
defcode "*/MOD", Muldivmod ,0 ; Added by RTR
        pop ecx
        pop ebx
        pop eax
        imul ebx
        idiv ecx
        push edx                ; push remainder
        push eax                ; push quotient
        NEXT
        
defcode "2*", Twomul, 0 ; Added by RTR
        shl dword [esp],1
        NEXT
        
defcode "2/", Twodiv, 0 ; Added by RTR
        sar dword [esp],1
        NEXT
        
defcode "U2/", Utwodiv,0 ; Added by RTR
        shr dword [esp],1
        NEXT

defcode "U<", Ult,0 ; Added by RTR
        pop eax
        pop ebx
        cmp ebx,eax
        setb al
        movzx eax,al
        neg eax
        push eax
       NEXT
  
 defcode "U>", Ugt,0 ; Added by RTR
        pop eax
        pop ebx
        cmp ebx,eax
        seta al
        movzx eax,al
        neg eax
        push eax
       NEXT

defcode "S>D", Stod,0   ; Added by RTR
        pop eax
        cdq
        push eax                ; LS 32 bits (Forth is big-endian)
        push edx                ; MS 32 bits
       NEXT
        
defcode "ROLL", Roll,0 ; Added by RTR
        pop ecx
        jecxz _roll_next
        lea edi,[esp+ecx*4]
        lea ebx,[edi-4]
        mov eax,[edi]
        std
        xchg esi,ebx
        rep movsD
        xchg esi,ebx
        cld
        mov [esp],eax
_roll_next:
       NEXT  
defcode "doLIST", doLIST, 0;   
	 XCHG  eBP,eSP                 

      PUSH  eSI                     

      XCHG  eBP,eSP                  

      POP   eSI                    

 
        NEXT
defcode "(DOES)" , dodoes,0
    
_does:
        PUSHRSP esi           ; // push %esi on to the return stack
        pop esi               ; // point %esi at the does> part
        add eax,4             ;// push the CFA of the defined word
        push eax
        NEXT

    	

defcode  "doCREATE" , doCREATE ,0
		nop
		nop
		nop
		nop
		nop
		call _doCREATE
		NEXT
_doCREATE:	
		mov     edi,[var_HERE] 
      ;  pop     edi
        mov     eax,    [edi]
        lea     ebx,    [edi + 4]
        push    ebx
        test    eax,    eax
        jnz     .L1
        NEXT
.L1:
        jmp     eax
		ret

defcode "dodoes1", Dodoes1, 0;
__doCREATE:	; mov eax, [var_LATEST]
       	LEA     eBP,[eBP - 4] ;Push HIP.
        MOV     [ebp],esi
        MOV     eSI,[eBX+4] ;NEW IP 
        LEA     eAX,[eSI+4]
        MOV     eSI,[eSI]
        push eax
        NEXT
.L1:
        jmp     eax

defcode "dodoes", Dodoes, 0;
_dodoes:
        cmp dword [eax+4],0     ; Has DOES> been executed ?
        jz _nodoes
        lea ebp,[ebp-4]
        mov [ebp],esi
        mov esi,[eax+4]         ; Get pointer stored by DOES>
_nodoes:
        lea eax,[eax+8]
        push eax                ; Push user data area address
        NEXT
defcode "lit",lit,0
		mov eax,[var_LATEST]
		NEXT      
 	
                                
defcode "LEAVE", Leave ,0  ; Added by RTR
        lea ebp,[ebp+12]        ; pop return stack
        jmp _leave
        
defcode "?DO",  Qdo, 0 ; Added by RTR
        pop ecx                 ; initial index
        pop edx                 ; limit
        cmp ecx,edx
        jne _dogo
_leave:
        mov ecx,1
        xor ebx,ebx
_qdo_loop:
        lodsd
        cmp eax,Do              ; Nested loop ?
        setz bl
        add ecx,ebx
        cmp eax,Qdo
        setz bl
        add ecx,ebx
        cmp eax,Loop
        setz bl
        sub ecx,ebx
        cmp eax,Ploop
        setz bl
        sub ecx,ebx
        or ecx,ecx
        jnz _qdo_loop
        NEXT
        
defcode "DO",  Do, 0 ; Added by RTR
        pop ecx                 ; initial index
        pop edx                 ; limit
_dogo:
        lea ebp,[ebp-12]        ; make room on return stack
        mov [ebp+8],esi
        mov [ebp+4],edx
        mov [ebp],ecx
        NEXT
        
defcode"+LOOP", Ploop, 0 ; Added by RTR
        pop eax                 ; step
        jmp _loop_step
        
defcode "LOOP", Loop, 0 ; Added by RTR
        mov eax,1               ; default step
_loop_step:
        ;test byte [bflags],&81
        ;jnz near _escape        ; ESCape key pressed or Close
        mov ebx,[ebp]           ; index
        sub ebx,[ebp+4]         ; subtract limit
        btc ebx,31              ; invert MSB
        add ebx,eax             ; step
        jo _unloop              ; overflow signals loop end
        btc ebx,31              ; invert MSB again
        add ebx,[ebp+4]         ; add limit back
        mov [ebp],ebx           ; new index
        mov esi,[ebp+8]
        NEXT       ; continue looping
        
defcode "UNLOOP", Unloop, 0  ; Added by RTR
_unloop:
        lea ebp,[ebp+12]        ; pop return stack
        NEXT       ; exit loop
        
defcode "I", I , 0 ; Added by RTR
        push dword [ebp]        ; index
        NEXT       ; exit loop
        
defcode "J", J, 0 ; Added by RTR
        push dword [ebp+12]     ; outer index
        NEXT       ; exit loop

;-------------This Code is taken from bb4wforth.f---see--README-------
   

%include "kernel_video.s"

	
%endif

