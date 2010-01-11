
; file: rest
; must be the last file
; by august0815
; 01.12.2009


; function: MES1   TESTED_OK
defword "MES1",MES1,0
			dd CR
            LITN 79             ; for i = 0 to 80*25
            LITN 0                  
            do
 			LITN '*'
 			dd EMIT
 			loop
 			dd CR
            LITN  outputmes
            dd PRINTCSTRING
            dd CR
            dd EXIT		; EXIT		(return from FORTH word)
                       
; function: MES2   TESTED_OK
defword "MES2",MES2,0   
			dd CR
            LITN 79             ; for i = 0 to 80*25
            LITN 0                  
            do
 			LITN '*'
 			dd EMIT
 			loop
 			dd CR
            LITN  inputloop
            dd PRINTCSTRING
            dd CR
            
            
dd EXIT		; EXIT		(return from FORTH word) 
          
; function: ZEILEMIT    TESTED_OK
defword "ZEILEMIT",ZEILEMIT,0
            dd CR
            LITN 10             
            LITN 0                  
            do
 			LITN '-'
 			dd EMIT
 			loop
 			dd CR
 			LITN '>'
 			dd EMIT
 			LITN zeile_buffer
            ;dd KEYBUFF
            dd PRINTCSTRING
            LITN '<'
 			dd EMIT
 			
 			;dd DOTS
dd EXIT		; EXIT		(return from FORTH word)  

; function: TEILEMIT   TESTED_OK
defword "TEILEMIT",TEILEMIT,0
			;dd CR
			;dd DOTS
			;dd CR
            LITN 10             
            LITN 0                  
            do
 			LITN '*'
 			dd EMIT
 			loop
 			dd CR
 			LITN '>'
 			dd EMIT
 			LITN ptr_buff
            dd PRINTCSTRING
            LITN '<'
 			dd EMIT
 			
 			;dd DOTS
dd EXIT		; EXIT		(return from FORTH word)			


; function: TELL   rewrite it !!!! still for linux
defword "TELL" ,TELL , 0
	dd DOTS, EMIT, STOP
	dd EXIT

; function: PRESSKEY   TESTED_OK
defword "PRESSKEY" , PRESSKEY ,0
      		LITN key_press
            dd PRINTCSTRING
            dd TAB
            LITN '!'
            dd EMIT
            dd IN
            dd CLEAR
            dd EXIT		; EXIT		(return from FORTH word)	
            
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
				LITN zeile_buffer
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

; function: ZEIL   TESTED_OK
defword "ZEIL" , ZEIL ,0

			
			dd ZEILE ;, TWODROP
			dd inter

            LITN zeile_buffer
            dd DUP
            dd PPTR_LAST , STORE
			dd PPTR , STORE
           ; dd CLEAR
            dd CLSSTACK
            dd DROP
 			dd EXIT		; EXIT		(return from FORTH word)

; function: DUMP   NOT TESTED_OK (not very usefull)
defword "DUMP" , DUMP ,0
;: DUMP		( addr len -- )
;	BASE @ -ROT		( save the current BASE at the bottom of the stack )
;	HEX			( and switch to hexadecimal mode )
;	BEGIN
;		?DUP		( while len > 0 )
;	WHILE
;		OVER 8 U.R	( print the address )
;		SPACE		( print up to 16 words on this line )
;		2DUP		( addr len addr len )
;		1- 15 AND 1+	( addr len addr linelen )
;		BEGIN
;			?DUP		( while linelen > 0 )
;		WHILE
;			SWAP		( addr len linelen addr )
;			DUP C@		( addr len linelen addr byte )
;			2 .R SPACE	( print the byte )
;			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
;		REPEAT
;		DROP		( addr len )
;		( print the ASCII equivalents )
;		2DUP 1- 15 AND 1+ ( addr len addr linelen )
;		BEGIN
;			?DUP		( while linelen > 0)
	;	WHILE
;			SWAP		( addr len linelen addr )
;			DUP C@		( addr len linelen addr byte )
;			DUP 32 128 WITHIN
;          ( b   a  c	)		
; 			IF	( 32 <= c < 128? )

;				EMIT
;			ELSE
;				DROP '.' EMIT
;			THEN
;			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
;		REPEAT
;		DROP		( addr len )
;		CR
;		DUP 1- 15 AND 1+ ( addr len linelen )
;		TUCK		( addr linelen len linelen )
;		-		( addr linelen len-linelen )
;		>R + R>		( addr+linelen len-linelen )
;	REPEAT
;	DROP			( restore stack )
;	BASE !			( restore saved BASE )
        dd BASE , FETCH , NROT , HEX
        begin
    	    dd QDUP
	    while
        	dd OVER
        	LITN 3
        	dd DOTR , SPACE , TWODUP , DECR
        	LITN 15
        	dd AND , INCR
        begin
        	dd QDUP
        while
        	dd SWAP , DUP , FETCHBYTE
        	LITN 2
        	dd DOTR , SPACE , INCR , SWAP , DECR
       repeat
        	dd DROP , TWODUP , DECR
        	LITN 15
        	dd AND , INCR
      begin
        	dd QDUP
      while
      	  dd SWAP , DUP , FETCHBYTE , DUP
        	LITN 32
       		LITN 128
        	dd WITHIN
       		if
        		dd EMIT
        	else
        		dd DROP
        		LITN '.'
        		dd EMIT
        	then
        	dd INCR , SWAP , DECR
       	repeat
       	dd DROP , CR , DUP , DECR
       	LITN 15
        dd AND , INCR , TUCK , SUB , TOR , ADD , FROMR
	repeat
        dd DROP , BASE , STORE
	dd EXIT		; EXIT		(return from FORTH word)

; ; function: STRLEN only tesing
defword "STRLEN " , STRLEN  ,0
;: STRLEN 	( str -- len )
;	DUP		( save start address )
;	BEGIN
;		DUP C@ 0<>	( zero byte found? )
;	WHILE
;		1+
;	REPEAT

;	SWAP -		( calculate the length )
dd DUP
begin
dd DUP ,FETCHBYTE,ZNEQU
while
dd INCR 
repeat
dd SWAP ,SUB
dd EXIT		; EXIT		(return from FORTH word)



; function: WELCOME must be the LAST WORD !! LATEST points here <==
wel:
defword "WELCOM", WELCOM ,0  
			LITN zeile_buffer
			dd DUP
            dd PPTR_LAST , STORE
			dd PPTR , STORE
			dd CLEAR
            dd CR
			dd CR
            LITN 15
            dd INK
            LITN osname
            dd PRINTCSTRING
            LITN 10              ; for i = 10 to 0
            LITN 0                  ;
            do
             dd CR
            loop
            LITN colofon
            dd PRINTCSTRING
            LITN 10              ; for i = 10 to 0
            LITN 0                  ;
            do
             dd CR
            loop
            dd PRESSKEY
 			dd EXIT		; EXIT		(return from FORTH word)  
 			 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; no defcode or defword afther this line only for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; function: all_INK
; prints test string in 16 ink colors
defword "all_INK", all_INK,0
			LITN 15             
            LITN 0                  
            do
            dd TST , FETCH
            dd INCR 
            dd DUP
 			dd INK
 			LITN osname
 			dd PRINTCSTRING
 			dd TAB 
 			dd DUP
 			dd DOT, CR
 			dd TST ,STORE
 			loop
 			dd CR
 			LITN 0
 			dd TST, STORE
 			dd PRESSKEY
dd EXIT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; function: linemove
; moves the 'string' to zeile_buffer the line input buffer	
defword "linemove", linemove ,0
	LITN zeile_buffer   ;   destination address
	LITN 300			;   number of bytes to move
	dd CMOVE			; memory_move 
	dd inter    
	LITN zeile_buffer
	dd PPTR , STORE
	dd EXIT
; function: highlevel 
; define some words in pseudo line input compiling. Same as typing the words on keyboard
defword "highlevel" , highlevel ,0
;At the moment there is no file system, so we have to simulate lineinput
	dd DECIMAL
    LITN zeile_buffer
	dd PPTR , STORE
	LITN comp			;   source
	dd linemove
	LITN iff			;   source
	dd linemove
	LITN thenn			;   source
	dd linemove
	LITN elsee			;   source
	dd linemove
	LITN beginn			;   source
	dd linemove
	LITN untill			;   source
	dd linemove
	LITN againn			;   source
	dd linemove
	LITN  whilee		;   source
	dd linemove
	LITN repeatt		;   source
	dd linemove
	LITN casee			;   source
	dd linemove
	LITN off			;   source
	dd linemove
	LITN endoff			;   source
	dd linemove
	LITN endcasee		;   source
	dd linemove
	LITN ckk			;   source
	dd linemove
	LITN alli			;   source
	dd linemove
	LITN saa			;   source
	dd linemove
	LITN dotaa			;   source
	dd linemove
	LITN remm			;   source
	dd linemove
	dd EXIT

comp: db ': [COMPILE] IMMEDIATE TEILWORT FIND >CFA , ; ',0x0d,0	
iff: db ': IF IMMEDIATE ', 0x27,' 0BRANCH , HERE @ 0 , ;',0x0d,0
thenn: db ': THEN IMMEDIATE DUP HERE @ SWAP - SWAP ! ;' , 0x0d,0	
elsee: db ': ELSE IMMEDIATE ',0x27,' BRANCH , HERE @ 0 , SWAP DUP HERE @ SWAP - SWAP ! ; ',0x0d,0	
beginn: db ': BEGIN IMMEDIATE HERE @ ; ' , 0x0d,0
untill: db ': UNTIL IMMEDIATE ',0x27,' 0BRANCH , HERE @ - , ; ' , 0x0d,0
againn: db ': AGAIN IMMEDIATE ',0x27,' BRANCH , HERE @ - , ; ' , 0x0d,0
whilee: db ': WHILE IMMEDIATE ',0x27,' 0BRANCH , HERE @ 0 , ; ' , 0x0d,0
repeatt: db ': REPEAT IMMEDIATE ',0x27,' BRANCH , SWAP HERE @ - , DUP HERE @ SWAP - SWAP ! ; ' , 0x0d,0
casee: db ': CASE IMMEDIATE 0 ; ', 0x0d,0
off: db ': OF IMMEDIATE ',0x27,' OVER , ',0x27,' = , [COMPILE] IF ',0x27,' DROP , ; ', 0x0d,0
endoff: db ': ENDOF IMMEDIATE [COMPILE] ELSE ; ', 0x0d,0
endcasee: db ': ENDCASE IMMEDIATE ',0x27,' DROP , BEGIN ?DUP WHILE [COMPILE] THEN REPEAT ; ', 0x0d,0
ckk: db ': C, HERE @ C! 1 HERE +! ; ', 0x0d,0
alli: db ': ALIGN HERE @ ALIGNED HERE ! ; ', 0x0d,0
; S" and ." nocht nicht richtig
saa: db ': S" IMMEDIATE STATE @ .S IF 55 EMIT ',0x27,' LITSTRING , HERE @ 0 , BEGIN KEY1 DUP 22 <> WHILE C, REPEAT DROP DUP HERE @ SWAP - SWAP ! ALIGN ELSE 66 EMIT HERE @ BEGIN KEY1 DUP 22 <> WHILE OVER C! 1+ REPEAT DROP HERE @ - HERE @ SWAP THEN ; ', 0x0d,0
dotaa: db ': ." IMMEDIATE STATE @ IF [COMPILE] S" ',0x27,' TELL ,  ELSE BEGIN KEY1 DUP 22 <> WHILE EMIT REPEAT THEN ; ', 0x0d,0
remm: db ': ( DECIMAL IMMEDIATE 1 BEGIN KEY1 DUP 40 = IF DROP 1+ ELSE 41 = IF 1- THEN THEN DUP 0= UNTIL DROP ; ', 0x0d,0


