
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
 			LITN text_buffer
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
	dd EMIT, STOP
	dd EXIT
; function: echooff   TESTED_OK
defword "echooff" , echooff ,0
			LITN 0
      		dd NOECHO,STORE
      		dd  EXIT	;		(return from FORTH word)	
; function: echoon   TESTED_OK
defword "echoon" , echoon ,0
			LITN 1
      		dd NOECHO,STORE
      		dd  EXIT	;		(return from FORTH word)
; function: PRESSKEY   TESTED_OK
defword "PRESSKEY" , PRESSKEY ,0
      		LITN key_press
            dd PRINTCSTRING
            dd TAB
            LITN '!'
            dd EMIT
            dd getchar
            dd CLEAR
            dd EXIT		; EXIT		(return from FORTH word)	; function: ZEIL   TESTED_OK

defword "show", show, 0
  dd CR
        dd CR
        dd GRUB
        dd FETCH
        LITN 0x14
        dd ADD
        dd FETCH 
        dd GRUB
        dd FETCH
        LITN 0x18
        dd ADD
        dd FETCH
        dd DUP
        dd FETCH
        dd SWAP
        dd INCR4 
        dd FETCH 
        dd SWAP
        dd TWODUP
        dd SUB
        dd NROT , INCR
        dd SRC_END
        dd STORE
        LITN 0
        dd SRC_END ,STOREBYTE
        dd SWAP
        dd DUP
        dd SRC
        dd STORE
        dd SWAP
        dd TWODROP
        dd DROP
        dd interforth
        dd DROP 
        dd printt
        LITN text_buffer
        dd text_buff
        dd STORE
        LITN 1 
        dd text_buff
        dd FETCH
        dd STOREBYTE
      
dd EXIT

; function: WELCOME must be the LAST WORD !! LATEST points here <==
wel:
defword "WELCOM", WELCOM ,0  
			LITN text_buffer
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


