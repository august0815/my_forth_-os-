
; file: ext2
; by august0815
; 19.12.2009

section .text
;;;;;;;;;;;;;; SOME WORDS

; function: U. ( u -- ) TESTED_OK
;	Displays an unsigned number
;|	( This is the underlying recursive definition of U. )
;|	: U.		( u -- )
;|	BASE @ /MOD	( width rem quot )
;|	?DUP IF			( if quotient <> 0 then )
;|		RECURSE		( print the quotient )
;|	THEN
;|	( print the remainder )
;|	DUP 10 < IF
;|		'0'		( decimal digits 0..9 )
;|	ELSE
;|		10 -		( hex and beyond digits A..Z )
;|		'A'
;|	THEN
;|	+
;|	EMIT
defword "U.",UDOT,0
	dd BASE, FETCH 	;( width rem quot )
	dd DIVMOD		
	dd QDUP
	if 				;( if quotient <> 0 then )
	 	dd UDOT
	else
	then
		dd DUP		;( print the remainder )
		LITN 10 
		dd LT
		if
	 		LITN '0'  ;(decimal digits 0..9 )
		else
		LITN 10 
	 	dd  SUB		;( hex and beyond digits A..Z )
	 	LITN 'A'
	 	then
	 dd ADD
	 dd EMIT	
	 dd EXIT		; EXIT		(return from FORTH word)
	 
; function: .S  ( -- )  TESTED_OK
;	FORTH word .S prints the contents of the stack.  It doesn't alter the stack.
;	Very useful for debugging. ;
;|	: .S		( -- )
;|	DSP@		( get current stack pointer )
;|	BEGIN
;|		DUP S0 @ <
;|	WHILE
;|		DUP @ U.	( print the stack element )
;|		SPACE
;|		4+		( move up )
;|	REPEAT
;|	DROP
defword ".S"  , DOTS ,0
	LITN '>'
	dd EMIT
	dd DSPFETCH ;( get current stack pointer )
	begin
		dd DUP 
		dd S0 ,FETCH , LT
	while
		dd DUP ,FETCH 
		dd UDOT ;( print the stack element )
		dd SPACE
		dd INCR4			 ;(move up )
	repeat
	dd DROP
	LITN '<'
	dd EMIT
	dd EXIT		; EXIT		(return from FORTH word)



; function: ID. ( anf -- )  TESTED_OK
;   Displays the name of a word from its name field address
;|	: ID. 4+		( skip over the link pointer )
;|	DUP C@		( get the flags/length byte )
;|	F_LENMASK AND	( mask out the flags - just want the length )
;|	BEGIN
;|		DUP 0>		( length > 0? )
;|	WHILE
;|		SWAP 1+		( addr len -- len addr+1 )
;|		DUP C@		( len addr -- len addr char | get the next character)
;|		EMIT		( len addr char -- len addr | and print it)
;|		SWAP 1-		( len addr -- addr len-1    | subtract one from length )
;|	REPEAT
;|	2DROP		( len addr -- )
defword "ID.",IDDOT,0
	dd INCR4
	dd DUP 
	dd  FETCHBYTE 
	LITN 0x1F  ;  F_LENMASK( mask out the flags - just want the length )
	dd AND
	begin	
		dd DUP , ZGT
	while
	  	dd SWAP , INCR
		dd DUP , FETCH
		dd EMIT
		dd SWAP , DECR
	repeat
    dd TWODROP
	dd EXIT		; EXIT		(return from FORTH word)
	
; function: ?HIDDEN	 ( --) NOT TESTED_OK
;	Vocabulary containing words used only to implement other words
;|	: ?HIDDEN
;|	4+		( skip over the link pointer )
;|	C@		( get the flags/length byte )
;|	F_HIDDEN AND	( mask the F_HIDDEN flag and return it (as a truth value) 
defword "?HIDDEN" ,?HIDDEN ,0
	dd INCR4 , FETCHBYTE
	LITN 0x20
	dd AND
	dd EXIT		; EXIT		(return from FORTH word)
	
defword "?IMMEDIATE" ,?IMMEDIATE ,0
; function: ?IMMEDIATE NOT TESTED_OK
;	?IMMEDIATE
;|	: ?IMMEDIATE
;|	4+		( skip over the link pointer )
;|	C@		( get the flags/length byte )
;|	F_IMMED AND	( mask the F_IMMED flag and return it (as a truth value) )
	dd INCR4 , FETCHBYTE
	LITN 0x80
	dd AND
	dd EXIT		; EXIT		(return from FORTH word)


; function: WORDS ( -- )  TESTED_OK
;	Displays the words in the context vocabulary
;|	: WORDS
;|	LATEST @	( start at LATEST dictionary entry )
;|	BEGIN
;|		?DUP		( while link pointer is not null )
;|	WHILE
;|		DUP ?HIDDEN NOT IF	( ignore hidden words )
;|			DUP ID.		( but if not hidden, print the word )
;|			SPACE
;|		THEN
;|		@		( dereference the link pointer - go to previous word )
;|	REPEAT
;|	CR
defword "WORDS",WORDS,0
	dd LATEST , FETCH
	begin 
		dd QDUP
	while
		dd DUP 
		dd ?HIDDEN ,NOT
		if 
		;dd DUP
		;dd UDOT	
		
			dd DUP , IDDOT
			dd SPACE
		then
		dd FETCH
	repeat
	dd CR
	dd EXIT		; EXIT		(return from FORTH word)

; function: FORGET ; (NOT) TESTED_OK 
; 'FORGET word' deletes the definition of 'word' from the dictionary and everything defined
;	after it, including any variables and other memory allocated after.
;| ***********NOTE  to simple works well only for the lastest word. 'FORGET' words from middle of
;| ***********dictionary , and the words after could not be used !!
;|	: FORGET
;|	WORD FIND	( find the word, gets the dictionary entry address )
;|	DUP @ LATEST !	( set LATEST to point to the previous word )
;|	HERE !		( and store HERE with the dictionary address )
defword "FORGET" , FORGET ,0
    dd TEILWORT , FIND 
    dd DUP , FETCH , LATEST , STORE
    dd HERE ,STORE
	dd EXIT		; EXIT		(return from FORTH word)

; function: UWIDTH ; NOT TESTED_OK
;	( This word returns the width (in characters) of an unsigned number in the current base )
;|	: UWIDTH	( u -- width )
;|	BASE @ /	( rem quot )
;|	?DUP IF		( if quotient <> 0 then )
;|		RECURSE 1+	( return 1+recursive call )
;|	ELSE
;|		1		( return 1 )
;|	THEN
defword "UWIDTH",UWIDTH,0
    dd BASE , FETCH 
	dd DIV
	dd QDUP
	if
		dd UWIDTH , INCR
	else	
		LITN 1
	then
	dd EXIT		; EXIT		(return from FORTH word)



; function: U.R  ( u +n -- ) ; NOT TESTED_OK
;	Prints an unsigned number in fixed width field
;|	: U.R		( u width -- )
;|	SWAP		( width u )
;|	DUP		( width u u )
;|	UWIDTH		( width u uwidth )
;|	ROT		( u uwidth width )
;|	SWAP -		( u width-uwidth )
;|	( At this point if the requested width is narrower, we'll have a negative number on the stack.
;|	  Otherwise the number on the stack is the number of spaces to print.  But SPACES won't print
;|	  a negative number of spaces anyway, so it's now safe to call SPACES ... )
;|	SPACES
;|	( ... and then call the underlying implementation of U. )
;|	U.
defword "U.R",UDOTR,0
	dd SWAP , DUP , UWIDTH, ROT , SWAP , SUB
	dd SPACES 
	dd UDOT
dd EXIT		; EXIT		(return from FORTH word)

; function: .R  ( n +n -- )  TESTED_OK (noch nicht alles getestet)
;	Displays a number in a fixed width field
;|	.R prints a signed number, padded to a certain width.  We can't just print the sign
;|	and call U.R because we want the sign to be next to the number ('-123' instead of '-  123').
;|	: .R		( n width -- )
;|	SWAP		( width n )
;|	DUP 0< IF
;|		NEGATE		( width u )
;|		1		( save a flag to remember that it was negative | width n 1 )
;|		SWAP		( width 1 u )
;|		ROT		( 1 u width )
;|		1-		( 1 u width-1 )
;|	ELSE
;|		0		( width u 0 )
;|		SWAP		( width 0 u )
;|		ROT		( 0 u width )
;|	THEN
;|	SWAP		( flag width u )
;|	DUP		( flag width u u )
;|	UWIDTH		( flag width u uwidth )
;|	ROT		( flag u uwidth width )
;|	SWAP -		( flag u width-uwidth )
;|	SPACES		( flag u )
;|	SWAP		( u flag )
;|	IF			( was it negative? print the - character )
;|		'-' EMIT
;|	THEN
;|	U.
defword ".R",DOTR,0
	dd SWAP 
	dd DUP
	dd ZLT
	if
		dd NEGATE 
		LITN 1
		dd SWAP 
		dd ROT
	else
		 LITN 0
		 dd SWAP 
		dd  ROT
	then
	dd SWAP 
	dd DROP
	dd ZNEQU
	if
		LITN '-'
		dd EMIT
	else
	then
	dd UDOT
	dd EXIT		; EXIT		(return from FORTH word)


; function: .  ( n  -- ) ;  TESTED_OK
; 	Displays a number
; |: . 0 .R SPACE ;
defword "." , DOT ,0
;( Finally we can define word . in terms of .R, with a trailing space. )

	LITN 0 
	dd DOTR , SPACE
	dd EXIT		; EXIT		(return from FORTH word)
; The real U., note the trailing space. ) 
;: U. U. SPACE ;	??

; function: ?  ( adr  -- ) ;  TESTED_OK
;	( ? fetches the integer at an address and prints it. )
;|	: ? ( addr -- ) @ . ;
defword "?" , QQ ,0
	dd FETCH , DOT
	dd EXIT		; EXIT		(return from FORTH word)
	
; function: WITHIN  ( n min max -- f ) ;  TESTED_OK
;	True if n <= x < max
;| 	( c a b WITHIN returns true if a <= c and c < b )
;|	(  or define without ifs: OVER - >R - R>  U<  )
;|	: WITHIN
;|	-ROT		( b c a )
;|	OVER		( b c a c )
;|	<= IF
;|		> IF		( b c -- )
;|			TRUE
;|		ELSE
;|			FALSE
;|		THEN
;|	ELSE
;|		2DROP		( b c -- )
;|		FALSE
;|	THEN
defword "WITHIN" , WITHIN ,0
	dd NROT , OVER , LE
	if
		dd GT
		if
			dd TRUE	
		else
			dd FALSE
		then
	else
		dd TWODROP , FALSE
	then
	dd EXIT		; EXIT		(return from FORTH word)

; function: ALIGNED  ( adr1 -- adr2 ) ; NOT TESTED_OK
;	Adjusts an address to a machine word boundary
;|	ALIGNED takes an address and rounds it up (aligns it) to the next 4 byte boundary.
;|	: ALIGNED	( addr -- addr )
;|	3 + 3 INVERT AND	( (addr+3) & ~3 )
defword "ALIGNED" , ALIGNED ,0
	LITN 3
	dd ADD 
	LITN 3
	dd INVERT ,AND
	dd EXIT		; EXIT		(return from FORTH word)
	
; function: ALIGN  ( -- ) ; NOT TESTED_OK rename !!!
;	ALIGN aligns the HERE pointer, so the next word appended will be aligned properly.
;| : ALIGN HERE @ ALIGNED HERE ! ;
;defword ALI , ALI ,0
	
; function: C, ; NOT TESTED_OK rename !!!
;  define as high level word in rest.s
;	( C, appends a byte to the current compiled word. )
;|	: C,
;|	HERE @ C!	( store the character in the compiled image )
;|	1 HERE +!	( increment HERE pointer by 1 byte )


defcode "KEY1",KEY1  ,0
	xor eax,eax
	call _KEY1
	push eax
	NEXT
_KEY1:
	push ebx
	mov ebx,[var_PPTR]
	mov al,[ebx] 
	inc ebx
	mov dword [var_PPTR],ebx
	pop ebx
	ret
	dd EXIT		; EXIT		(return from FORTH word)

; function: S" ; NOT TESTED_OK rename !!!
;  define as high level word in rest.s
;|	: S" IMMEDIATE		( -- addr len )
;|	STATE @ IF	( compiling? )
;|		' LITSTRING ,	( compile LITSTRING )
;|		HERE @		( save the address of the length word on the stack )
;|		0 ,		( dummy length - we don't know what it is yet )
;|		BEGIN
;|			KEY 		( get next character of the string )
;|			DUP '"' <>
;|		WHILE
;|			C,		( copy character )
;|		REPEAT
;|		DROP		( drop the double quote character at the end )
;|		DUP		( get the saved address of the length word )
;|		HERE @ SWAP -	( calculate the length )
;|		4-		( subtract 4 (because we measured from the start of the length word) )
;|		SWAP !		( and back-fill the length location )
;|		ALIGN		( round up to next multiple of 4 bytes for the remaining code )
;|	ELSE		( immediate mode )
;|		HERE @		( get the start address of the temporary space )
;|		BEGIN
;|			KEY
;|			DUP '"' <>
;|		WHILE
;|			OVER C!		( save next character )
;|			1+		( increment address )
;|		REPEAT
;|		DROP		( drop the final " character )
;|		HERE @ -	( calculate the length )
;|		HERE @		( push the start address )
;|		SWAP 		( addr len )
;|	THEN

	
; function: ." ; NOT TESTED_OK	
; ; define as high level word in rest.s
;|	: ." IMMEDIATE		( -- )
;|	STATE @ IF	( compiling? )
;|		[COMPILE] S"	( read the string, and compile LITSTRING, etc. )
;|		' TELL ,	( compile the final TELL )
;|	ELSE
;|		( In immediate mode, just read characters and print them until we get
;|		  to the ending double quote. )
;|		BEGIN
;|			KEY1
;|			DUP '"' = IF
;|				DROP	( drop the double quote character )
;|				EXIT	( return from this function )
;|			THEN
;|			EMIT
;|		AGAIN
;|	THEN
;|		begin

%include "ext3.s"
