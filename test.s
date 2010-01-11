defword test_scroll , test_scroll ,0
		dd DECIMAL
scr:	LITN tstmes
    	dd  PRINTCSTRING , TAB
		dd CURSOR_POS_X,FETCH,DOT,TAB
		dd CURSOR_POS_Y,FETCH,DOT
		dd IN 
		LITN 'q'
		dd KEYBUFF , FETCH , EQU
		if 
		branch out
		then		
    	dd CR
		branch scr
out:	dd WORDS
		dd EXIT		; EXIT		(return from FORTH word)
		
tstmes: db '     THIS IS SCROLL TEST  X , Y POS IS : ' ,0

defword test_CON , test_CON ,0
      ;--------TESTING words------------------
      ;--------CON--------------------------
      ;-------still not working well !!-------
      dd keydum ;dummy "keyboard input"
      LITN zeile_buffer ; SHOWIT
      dd  PRINTCSTRING ,CR
      dd HEX
      ;---------print Var. befor
      LITN latest
      dd  PRINTCSTRING ,TAB
      dd LATEST ,FETCH , DOT , CR
      LITN here
      dd  PRINTCSTRING ,TAB
      dd HERE ,FETCH , DOT , CR
      LITN state
      dd  PRINTCSTRING ,TAB
      dd STATE ,FETCH , DOT , CR
      ;--------now -define new word---------
      LITN 22
      dd CONSTANT
      
     ; dd STOP
      dd SEMICOLON
      ;---------print Var. after
      
      dd CR,CR
      LITN latest
      dd  PRINTCSTRING ,TAB
      dd LATEST ,FETCH , DOT , CR
      LITN here
      dd  PRINTCSTRING ,TAB
      dd HERE ,FETCH , DOT , CR
      LITN state
      dd  PRINTCSTRING ,TAB
      dd STATE ,FETCH , DOT , CR
      ;dd PRESSKEY
      dd CR
      dd WORDS ; now look if new word is on top?
      LITN zeile_buffer
	  dd PPTR , STORE
      ;dd d
      ;dd DOTS
     ; dd STOP
dd EXIT		; EXIT		(return from FORTH word)

defword test_COLON , test_COLON ,0
      ;--------TESTING words------------------
      ;--------COLON--------------------------
      ;-------still not working well !!-------
      dd keydum ;dummy "keyboard input"
      LITN zeile_buffer ; SHOWIT
      dd  PRINTCSTRING ,CR
      dd HEX
      ;---------print Var. befor
      LITN latest
      dd  PRINTCSTRING ,TAB
      dd LATEST ,FETCH , DOT , CR
      LITN here
      dd  PRINTCSTRING ,TAB
      dd HERE ,FETCH , DOT , CR
      LITN state
      dd  PRINTCSTRING ,TAB
      dd STATE ,FETCH , DOT , CR
      ;--------now -define new word---------
      
      dd COLON
      dd WORDS
     ; dd STOP
      dd SEMICOLON
      ;---------print Var. after
      
      dd CR,CR
      LITN latest
      dd  PRINTCSTRING ,TAB
      dd LATEST ,FETCH , DOT , CR
      LITN here
      dd  PRINTCSTRING ,TAB
      dd HERE ,FETCH , DOT , CR
      LITN state
      dd  PRINTCSTRING ,TAB
      dd STATE ,FETCH , DOT , CR
      dd PRESSKEY
      dd CR
      dd WORDS ; now look if new word is on top?
      LITN zeile_buffer
	  dd PPTR , STORE
      ;dd d
      ;dd DOTS
     ; dd STOP
dd EXIT		; EXIT		(return from FORTH word)
latest: db 'ADR of LATEST : ' ,0
here: db 'ADR of HERE : ' ,0
state: db ' STATE is _' ,0
ee:
defword within_test , within_test ,0
      ;--------TESTING words------------------
      ;********WITHIN****OK*******************
      ;( c a b WITHIN returns true if a <= c and c < b )
      LITN 5    ; b
      LITN 10   ; a
      LITN 30   ; b
      dd DOTS   ; ( c a b )
      dd WITHIN 
      dd DOTS  ; TRUE ? NO
      dd DROP
      dd CR
      LITN 05   ; c   ------OK TRUE
      LITN 30   ; a
      LITN 10   ; b
      dd DOTS   ; ( c a b )
      dd WITHIN 
      dd DOTS  ; TRUE ? YES
      dd DROP 
      dd CR
      LITN 30   ; c
      LITN 10   ; a
      LITN  5   ; b
      dd DOTS   ; ( c a b )
      dd WITHIN 
      dd DOTS  ; TRUE ? NO
      dd DROP 
      dd CR
dd EXIT		; EXIT		(return from FORTH word)
