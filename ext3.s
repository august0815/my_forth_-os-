
; file: ext3
; by august0815
; 01.12.2009

section .text
;;;;;;;;;;;;;; SOME WORDS

; function: CONSTANT  TESTED_OK
;	CONSTANT
;|  : CONSTANT
;|	WORD		( get the name (the name follows CONSTANT) )
;|	CREATE		( make the dictionary entry )
;|	DOCOL ,		( append DOCOL (the codeword field of this word) )
;|	' LIT ,		( append the codeword LIT )
;|	,		( append the value on the top of the stack )
;|	' EXIT ,	( append the codeword EXIT )
defword "CONSTANT",CONSTANT,0
	dd TEILWORT
	dd CREATE ,LIT
	dd DOCOL ,COMMA
	dd TICK ,LIT , COMMA ,COMMA
    dd TICK , EXIT,COMMA
	dd EXIT		; EXIT		(return from FORTH word

; function: ALLOT ( n -- ) TESTED_OK
;		Allocate n bytes to the code dictionary. 
;|	First ALLOT, where n ALLOT allocates n bytes of memory.  (Note when calling this that
;|	it's a very good idea to make sure that n is a multiple of 4, or at least that next time
;|	a word is compiled that HERE has been left as a multiple of 4).
;|  :ALLOT		( n -- addr )
;|	HERE @ SWAP	( here n )
;|	HERE +!		( adds n to HERE, after this the old value of HERE is still on the stack );
defword "ALLOT",ALLOT,0
	dd HERE ,FETCH ,SWAP
	dd HERE ,ADDSTORE
	dd EXIT		; EXIT		(return from FORTH word
; function: CELLS  ( n -- n )  ; TESTED_OK
;	CELLS
;|	Second, CELLS.  In FORTH the phrase 'n CELLS ALLOT' means allocate n integers of whatever size
;|	is the natural size for integers on this machine architecture.  On this 32 bit machine therefore
;|	CELLS just multiplies the top of stack by 4.
;|	: CELLS ( n -- n ) 4 *; 
defword "CELLS",CELLS,0,
    LITN 4
    dd MUL
	dd EXIT		; EXIT		(return from FORTH word

; function: VARIABLE  ( -- ; <string> ) TESTED_OK
;		Compile a new variable . 
;|	So now we can define VARIABLE easily in much the same way as CONSTANT above.  Refer to the
;|	diagram above to see what the word that this creates will look like.
;|  : VARIABLE
;|	1 CELLS ALLOT	( allocate 1 cell of memory, push the pointer to this memory )
;|	WORD CREATE	( make the dictionary entry (the name follows VARIABLE) )
;|	DOCOL ,		( append DOCOL (the codeword field of this word) )
;|	' LIT ,		( append the codeword LIT )
;|	,		( append the pointer to the new memory )
;|	' EXIT ,	( append the codeword EXIT )
;
defword "VARIABLE" ,VARIABLE ,0
	LITN 1 
	dd  CELLS , ALLOT
    dd TEILWORT , CREATE ,LIT
    dd DOCOL , COMMA
    dd TICK , LIT , COMMA , COMMA
    dd TICK , EXIT ,COMMA
	dd EXIT		; EXIT		(return from FORTH word
	
; function: VALUE	( -- n ) TESTED_OK
;	VALUEs are like VARIABLEs but with a simpler syntax.
;|	20 VALUE VAL 	creates VAL with initial value 20
;|	VAL		pushes the value (20) directly on the stack
;|	30 TO VAL	updates VAL, setting it to 30
;|	VAL		pushes the value (30) directly on the stack
;|	: VALUE		( n -- )
;|	WORD CREATE	( make the dictionary entry (the name follows VALUE) )
;|	DOCOL ,		( append DOCOL )
;|	' LIT ,		( append the codeword LIT )
;|	,		( append the initial value )
;|	' EXIT ,	( append the codeword EXIT )
defword "VALUE" , VALUE , 0	
		dd TEILWORT , CREATE ,LIT , DOCOL ,COMMA
		dd TICK ,LIT , COMMA
		dd COMMA , TICK , EXIT , COMMA
		dd EXIT		; EXIT		(return from FORTH word	
		
; function: TOO 	( n -- ) TESTED_OK remane  => TO
;	VALUEs are like VARIABLEs but with a simpler syntax.
;|	: TO IMMEDIATE	( n -- )
;|	WORD		( get the name of the value )
;|	FIND		( look it up in the dictionary )
;|	>DFA		( get a pointer to the first data field (the 'LIT') )
;|	4+		( increment to point at the value )
;|	STATE @ IF	( compiling? )
;|		' LIT ,		( compile LIT )
;|		,		( compile the address of the value )
;|		' ! ,		( compile ! )
;|	ELSE		( immediate mode )
;|		!		( update it straightaway )
;|	THEN
defword "TO" , TOO , 0	
		dd IMMEDIATE
		dd TEILWORT , FIND , TDFA , INCR4
		dd STATE , FETCH 
		if 
			dd TICK , LIT , COMMA ,COMMA , TICK , STORE , COMMA
		else
		 	dd STORE
		then
		dd EXIT		; EXIT		(return from FORTH word	

; function: +TO	 TESTED_OK   
;	( x +TO VAL adds x to VAL )
;|	: +TO IMMEDIATE
;|	WORD		( get the name of the value )
;|	FIND		( look it up in the dictionary )
;|	>DFA		( get a pointer to the first data field (the 'LIT') )
;|	4+		( increment to point at the value )
;|	STATE @ IF	( compiling? )
;|		' LIT ,		( compile LIT )
;|		,		( compile the address of the value )
;|		' +! ,		( compile +! )
;|	ELSE		( immediate mode )
;|		+!		( update it straightaway )
;|	THEN
defword "+TO" , ADDTO , 0	
		dd IMMEDIATE
		dd TEILWORT , FIND , TDFA , INCR4
		dd STATE , FETCH 
		if 
			dd TICK , LIT , COMMA ,COMMA , TICK , ADDSTORE , COMMA
		else
		 	dd ADDSTORE
		then
		dd EXIT		; EXIT		(return from FORTH word	
	
; function: DEPTH	( -- n ) TESTED_OK
;	 ( DEPTH returns the depth of the stack. )
;| : DEPTH		( -- n )
;| S0 @ DSP@ -
;| 4-			( adjust because S0 was on the stack when we pushed DSP )
defword "DEPTH",DEPTH	,0	
		dd S0 , FETCH
		dd DSPFETCH , SUB
		dd DECR4
		dd EXIT		; EXIT		(return from FORTH word

; function: CLSSTACK ( x0 .. xn -- ) TESTED_OK	
;	CLSSTACK
;| : cls  ( x0 .. xn -- ) \ clear stack
;| depth 0= IF ( noop) ELSE  depth 0 do drop loop THEN ;
;| DEPTH IF  .S  DEPTH 0 DO DROP LOOP  THEN
defword "CLSSTACK",CLSSTACK	,0	
		dd DEPTH 
		dd QDUP
		if 
clslp:		dd DEPTH
			zbranch clsexit
			dd DROP
			branch clslp
clsexit:		  
		then 
		dd EXIT		; EXIT		(return from FORTH word
		
		
;*******************some words for debug************************
;
; defword: printt  TESTED_OK
;
; prints an string of len , pointer to string
defword "printt1", printt1,0
	        dd DECR
            LITN 0                  
            do
                dd NROT
                dd DUP 
                dd FETCH
                dd EMIT
                dd INCR
                dd ROT             
            loop
            dd DROP     
	        dd EXIT
; defword: CFA>  TESTED_OK
;
; prints an string of len , pointer to string
defword "CFA>", CFAT ,0	        
;: CFA>
;	LATEST @	( start at LATEST dictionary entry )
;	BEGIN
;		?DUP		( while link pointer is not null )
;	WHILE
;		2DUP SWAP	( cfa curr curr cfa )
;		< IF		( current dictionary entry < cfa? )
;			NIP		( leave curr dictionary entry on the stack )
;			EXIT
;		THEN
;		@		( follow link pointer back )
;	REPEAT
;	DROP		( restore stack )
;	0		( sorry, nothing found )
 	dd LATEST,FETCH
 	begin
 	dd QDUP
 	while
 	dd TWODUP,SWAP,LT
 	if
 		dd NIP , EXIT
 	then
 	dd FETCH	
 	repeat
 	dd DROP
 	LITN 0
 	dd EXIT

; defword: se ; my see ,decompiles name and adr and shows it	
;   SEE		( -- ; <string> )
;		A simple decompiler
defword "se" , se,0
    dd CR,CR
	LITN nam
	dd PRINTCSTRING ,TAB
	dd TEILWORT
	dd TWODUP
	dd printt ,CR
	dd FIND , QDUP
	if 
	 dd TCFA
	 LITN adr
	 dd PRINTCSTRING ,TAB
	 dd DOT , CR
	else
	 LITN er
	 dd PRINTCSTRING ,CR
	then
	dd EXIT	
nam: db ' NAME DES WORTES      ' ,0
adr: db ' CODE DES WORTES ANF. ' ,0	
er: db  ' FEHLERHAFTES WORT    ' ,0
;   SEE		( -- ; <string> )
;		A simple decompiler.
defword "see" , see,0
		dd TEILWORT
		dd FIND
		dd	TICK			;starting address
		dd	CR
SEE1:	dd	INCR4,DUP,FETCH,DUP	;?does it contain a zero
		zbranch SEE2 
		dd	CFAT			;?is it a name
SEE2:	dd	DUP			;name address or zero
		zbranch SEE3
		dd	SPACE,IDDOT		;display name
		branch SEE4
SEE3:	dd	DUP,FETCH,UDOT		;display number
SEE4:	dd PRESSKEY			;user control
		zbranch SEE1
		dd	DROP,EXIT		
%include "rest.s"
