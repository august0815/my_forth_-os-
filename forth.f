: BL   32 ;
: '\n' 10 ;
: TRUE  1 ;
: FALSE 0 ;
: NOT   0= ;
: NEGATE 0 SWAP - ;
: SPACE BL EMIT ;
: LITERAL IMMEDIATE
	' LIT ,
	,
	;
	
: ':'
	[
	CHAR :
	]
	LITERAL	
;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;
: [COMPILE] IMMEDIATE
	TEILWORT
	FIND
	>CFA
	,
;
: RECURSE IMMEDIATE
	LATEST @
	>CFA
	,
;
: IF IMMEDIATE
	' 0BRANCH ,
	HERE @
	0 ,
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP -
	SWAP !
;

: ELSE IMMEDIATE
	' BRANCH ,
	HERE @
	0 ,
	SWAP
	DUP
	HERE @ SWAP -
	SWAP !
;

: BEGIN IMMEDIATE
	HERE @
;

: UNTIL IMMEDIATE
	' 0BRANCH ,
	HERE @ -
	,
;


: AGAIN IMMEDIATE
	' BRANCH ,
	HERE @ -
	,
;

: WHILE IMMEDIATE
	' 0BRANCH ,
	HERE @
	0 ,
;

: REPEAT IMMEDIATE
	' BRANCH ,
	SWAP
	HERE @ - ,
	DUP
	HERE @ SWAP -
	SWAP !
;

: UNLESS IMMEDIATE
	' NOT ,
	[COMPILE] IF
;
: ( IMMEDIATE
	1	BEGIN
		KEY1
		DUP 40 = IF
			DROP
			1+
		ELSE
			41 = IF
				1-
			THEN
		THEN
	DUP 0= UNTIL
	DROP
;
( From now on we can use ( ... ) for comments.)

( Some more complicated stack examples, showing the stack notation. )
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	DSP@ +		( add to the stack pointer )
	@    		( and fetch )
;
( Standard words for manipulating BASE. )
: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;


( With the looping constructs, we can now write SPACES, which writes n spaces to stdout. )
: SPACES	( n -- )
	BEGIN
		DUP 0>		( while n > 0 )
	WHILE
		SPACE		( print a space )
		1-		( until we count down to 0 )
	REPEAT
	DROP
;

: CASE IMMEDIATE
	0
;

: OF IMMEDIATE
	' OVER ,
	' = ,
	[COMPILE] IF
	' DROP ,
;

: ENDOF IMMEDIATE
	[COMPILE] ELSE
;

: ENDCASE IMMEDIATE
	' DROP ,
	BEGIN
		?DUP
	WHILE
		[COMPILE] THEN
	REPEAT
;

( This is the underlying recursive definition of U. )
: U.		( u -- )
	BASE @ /MOD	( width rem quot )
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN

	( print the remainder )
	DUP 10 < IF
		'0'		( decimal digits 0..9 )
	ELSE
		10 -		( hex and beyond digits A..Z )
		'A'
	THEN
	+
	EMIT
;

(
	FORTH word .S prints the contents of the stack.  It doesn't alter the stack.
	Very useful for debugging.
)
: .S		( -- )
	DSP@		( get current stack pointer )
	BEGIN
		DUP S0 @ <
	WHILE
		DUP @ U.	( print the stack element )
		SPACE
		4+		( move up )
	REPEAT
	DROP
;

( This word returns the width (in characters) of an unsigned number in the current base )
: UWIDTH	( u -- width )
	BASE @ /	( rem quot )
	?DUP IF		( if quotient <> 0 then )
		RECURSE 1+	( return 1+recursive call )
	ELSE
		1		( return 1 )
	THEN
;

: U.R		( u width -- )
	SWAP		( width u )
	DUP		( width u u )
	UWIDTH		( width u uwidth )
	-ROT		( u uwidth width )
	SWAP -		( u width-uwidth )
	( At this point if the requested width is narrower, we'll have a negative number on the stack.
	  Otherwise the number on the stack is the number of spaces to print.  But SPACES won't print
	  a negative number of spaces anyway, so it's now safe to call SPACES ... )
	SPACES
	( ... and then call the underlying implementation of U. )
	U.
;

(
	.R prints a signed number, padded to a certain width.  We can't just print the sign
	and call U.R because we want the sign to be next to the number ('-123' instead of '-  123').
)
: .R		( n width -- )
	SWAP		( width n )
	DUP 0< IF
		NEGATE		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		SWAP		( width 1 u )
		-ROT		( 1 u width )
		1-		( 1 u width-1 )
	ELSE
		0		( width u 0 )
		SWAP		( width 0 u )
		-ROT		( 0 u width )
	THEN
	SWAP		( flag width u )
	DUP		( flag width u u )
	UWIDTH		( flag width u uwidth )
	-ROT		( flag u uwidth width )
	SWAP -		( flag u width-uwidth )

	SPACES		( flag u )
	SWAP		( u flag )

	IF			( was it negative? print the - character )
		'-' EMIT
	THEN

	U.
;	

( Finally we can define word . in terms of .R, with a trailing space. )
: . 0 .R SPACE ;

( The real U., note the trailing space. )
: U. U. SPACE ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

: ascii  127 AND DUP 32 < IF  DROP 46 THEN EMIT ;
( Some more complicated stack examples, showing the stack notation. )
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	DSP@ +		( add to the stack pointer )
	@    		( and fetch )
;



( c a b WITHIN returns true if a <= c and c < b )
(  or define without ifs: OVER - >R - R>  U<  )
: WITHIN
	ROT		( b c a )
	OVER		( b c a c )
	<= IF
		> IF		( b c -- )
			TRUE
		ELSE
			FALSE
		THEN
	ELSE
		2DROP		( b c -- )
		FALSE
	THEN
;

( DEPTH returns the depth of the stack. )
: DEPTH		( -- n )
	S0 @ DSP@ -
	4-			( adjust because S0 was on the stack when we pushed DSP )
;


: ALIGNED	( addr -- addr )
	3 + 3 INVERT AND	( (addr+3) & ~3 )
;


: ALIGN HERE @ ALIGNED HERE ! ;
: C,
	HERE @ C!
	1 HERE +!
;

: S" IMMEDIATE		( -- addr len )
	STATE @ IF	( compiling? )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		BEGIN
			KEY1 		( get next character of the string )
			DUP '"' <>
		WHILE
			C,		( copy character )
		REPEAT
		DROP		( drop the double quote character at the end )
		DUP		( get the saved address of the length word )
		HERE @ SWAP -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		SWAP !		( and back-fill the length location )
		ALIGN		( round up to next multiple of 4 bytes for the remaining code )
	ELSE		( immediate mode )
		HERE @		( get the start address of the temporary space )
		BEGIN
			KEY1
			DUP '"' <>
		WHILE
			OVER C!		( save next character )
			1+		( increment address )
		REPEAT
		DROP		( drop the final " character )
		HERE @ -	( calculate the length )
		HERE @		( push the start address )
		SWAP 		( addr len )
	THEN
;
: ." IMMEDIATE		( -- )
	STATE @ IF	( compiling? )
		[COMPILE] S"	( read the string, and compile LITSTRING, etc. )
		' TELL ,	( compile the final TELL )
	ELSE
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		BEGIN
			KEY1
			DUP '"' = IF
				DROP	( drop the double quote character )
				EXIT	( return from this function )
			THEN
			EMIT
		AGAIN
	THEN
;

: CONSTANT
	TEILWORT		( get the name (the name follows CONSTANT) )
	HEADER		( make the dictionary entry )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		( append the value on the top of the stack )
	' EXIT ,	( append the codeword EXIT )
;
: ALLOT		( n -- addr )
	HERE @ SWAP	( here n )
	HERE +!		( adds n to HERE, after this the old value of HERE is still on the stack )
;
: CELLS ( n -- n ) 4 * ;
: VARIABLE
	1 CELLS ALLOT	( allocate 1 cell of memory, push the pointer to this memory )
	TEILWORT HEADER	( make the dictionary entry (the name follows VARIABLE) )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		( append the pointer to the new memory )
	' EXIT ,	( append the codeword EXIT )
;
: VALUE		( n -- )
	TEILWORT HEADER	( make the dictionary entry (the name follows VALUE) )
	DOCOL ,		( append DOCOL )
	' LIT ,		( append the codeword LIT )
	,		( append the initial value )
	' EXIT ,	( append the codeword EXIT )
;

: TO IMMEDIATE	( n -- )
	TEILWORT		( get the name of the value )
	FIND		( look it up in the dictionary )
	>DFA		( get a pointer to the first data field (the 'LIT') )
	4+		( increment to point at the value )
	STATE @ IF	( compiling? )
		' LIT ,		( compile LIT )
		,		( compile the address of the value )
		' ! ,		( compile ! )
	ELSE		( immediate mode )
		!		( update it straightaway )
	THEN
;

( x +TO VAL adds x to VAL )
: +TO IMMEDIATE
	TEILWORT		( get the name of the value )
	FIND		( look it up in the dictionary )
	>DFA		( get a pointer to the first data field (the 'LIT') )
	4+		( increment to point at the value )
	STATE @ IF	( compiling? )
		' LIT ,		( compile LIT )
		,		( compile the address of the value )
		' +! ,		( compile +! )
	ELSE		( immediate mode )
		+!		( update it straightaway )
	THEN
;
: ID.
	4+		( skip over the link pointer )
	DUP C@		( get the flags/length byte )
	F_LENMASK AND	( mask out the flags - just want the length )

	BEGIN
		DUP 0>		( length > 0? )
	WHILE
		SWAP 1+		( addr len -- len addr+1 )
		DUP C@		( len addr -- len addr char | get the next character)
		EMIT		( len addr char -- len addr | and print it)
		SWAP 1-		( len addr -- addr len-1    | subtract one from length )
	REPEAT
	2DROP		( len addr -- )
;

(
	'WORD word FIND ?HIDDEN' returns true if 'word' is flagged as hidden.

	'WORD word FIND ?IMMEDIATE' returns true if 'word' is flagged as immediate.
)
: ?HIDDEN
	4+		( skip over the link pointer )
	C@		( get the flags/length byte )
	F_HIDDEN AND	( mask the F_HIDDEN flag and return it (as a truth value) )
;
: ?IMMEDIATE
	4+		( skip over the link pointer )
	C@		( get the flags/length byte )
	F_IMMED AND	( mask the F_IMMED flag and return it (as a truth value) )
;

(
	WORDS prints all the words defined in the dictionary, starting with the word defined most recently.
	However it doesn't print hidden words.

	The implementation simply iterates backwards from LATEST using the link pointers.
)
: WORDS
	LATEST @	( start at LATEST dictionary entry )
	BEGIN
		?DUP		( while link pointer is not null )
	WHILE
		DUP ?HIDDEN NOT IF	( ignore hidden words )
			DUP ID.		( but if not hidden, print the word )
			SPACE
		THEN
		@		( dereference the link pointer - go to previous word )
	REPEAT
	CR
;


: FORGET
	TEILWORT FIND	( find the word, gets the dictionary entry address )
	DUP @ LATEST !	( set LATEST to point to the previous word )
	HERE !		( and store HERE with the dictionary address )
;

: CFA>
	LATEST @	( start at LATEST dictionary entry )
	BEGIN
		?DUP		( while link pointer is not null )
	WHILE
		2DUP SWAP	( cfa curr curr cfa )
		< IF		( current dictionary entry < cfa? )
			NIP		( leave curr dictionary entry on the stack )
			EXIT
		THEN
		@		( follow link pointer back )
	REPEAT
	DROP		( restore stack )
	0		( sorry, nothing found )
;

: SEE
	CR
	TEILWORT
	FIND
	HERE @	
	LATEST @	
	BEGIN
		2 PICK	
		OVER
		<>	
	WHILE
		NIP	
		DUP @
	REPEAT

	DROP
	SWAP	
	':' EMIT SPACE DUP ID. SPACE
	DUP ?IMMEDIATE IF ." IMMEDIATE " THEN

	>DFA		
	BEGIN	
		2DUP >
	WHILE
		DUP @

		CASE
		' LIT OF
			4 + DUP @	
			.
		ENDOF
		' LITSTRING OF	
			[ CHAR S ] LITERAL EMIT '"' EMIT SPACE
			4 + DUP @	
			SWAP 4 + SWAP	
			2DUP TELL	
			'"' EMIT SPACE	
			+ ALIGNED	
			4 -	
		ENDOF
		' 0BRANCH OF
			." 0BRANCH >"
			4 + DUP @		
			.
			." < "
		ENDOF
		' BRANCH OF		
			." BRANCH >"
			4 + DUP @
			.
			." < "
		ENDOF
		' ' OF		
			[ CHAR ' ] LITERAL EMIT SPACE
			4 + DUP @		
			CFA>	
			ID. SPACE
		ENDOF
		' EXIT OF	
				2DUP	
			4 +	
			<> IF	
				." EXIT "
			THEN
		ENDOF
					
			DUP		
			CFA>	
			ID. SPACE		
		ENDCASE

		4 +		
	REPEAT

	59 EMIT CR

	2DROP		( restore stack )
;

: NONAME
	0 0 HEADER	
	HERE @	
	DOCOL ,	
	]
;


: ['] IMMEDIATE
	' LIT ,		
;
: EXCEPTION-MARKER
	RDROP
	0
;

: CATCH	
	DSP@ 4+ >R		
	' EXCEPTION-MARKER 4+
	>R
	EXECUTE
;

: THROW		( n -- )
	?DUP IF			
		RSP@ 			( get return stack pointer )
		BEGIN
			DUP R0 4- <		
		WHILE
			DUP @			( get the return stack entry )
			' EXCEPTION-MARKER 4+ = IF	( found the EXCEPTION-MARKER on the return stack )
				4+			( skip the EXCEPTION-MARKER on the return stack )
				RSP!			( restore the return stack pointer )

				( Restore the parameter stack. )
				DUP DUP DUP		( reserve some working space so the stack for this word
							  doesn't coincide with the part of the stack being restored )
				R>			
				4-			( reserve space on the stack to store n )
				SWAP OVER		( dsp n dsp )
				!			( write n on the stack )
				DSP! EXIT		( restore the parameter stack pointer, immediately exit )
			THEN
			4+
		REPEAT

		
		DROP

		CASE
		0 1- OF	( ABORT )
			." ABORTED" CR
		ENDOF
			( default case )
			." UNCAUGHT THROW "
			DUP . CR
		ENDCASE
		QUIT
	THEN
;

: ABORT	
	0 1- THROW
;

: DUMP
	BASE @ ROT		( save the current BASE at the bottom of the stack )
	HEX			( and switch to hexadecimal mode )

	BEGIN
		?DUP		( while len > 0 )
	WHILE
		OVER 8 .R	( print the address )
		SPACE

		2DUP	
		1- 15 AND 1+
		BEGIN
			?DUP	
		WHILE
			SWAP
			DUP C@	
			2 .R SPACE	( print the byte )
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		DROP		( addr len )

		( print the ASCII equivalents )
		2DUP 1- 15 AND 1+ ( addr len addr linelen )
		BEGIN
			?DUP		( while linelen > 0)
		WHILE
			SWAP		( addr len linelen addr )
			DUP C@		( addr len linelen addr byte )
			DUP 32 128 WITHIN IF	( 32 <= c < 128? )
				EMIT
			ELSE
				DROP '.' EMIT
			THEN
			1+ SWAP 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		REPEAT
		DROP		( addr len )
		CR

		DUP 1- 15 AND 1+ ( addr len linelen )
		TUCK		( addr linelen len linelen )
		-		( addr linelen len-linelen )
		>R + R>		( addr+linelen len-linelen )
	REPEAT

	DROP			( restore stack )
	BASE !			( restore saved BASE )
;

: STRLEN 	( str -- len )
	DUP		( save start address )
	BEGIN
		DUP C@ 0<>	( zero byte found? )
	WHILE
		1+
	REPEAT

	SWAP -		( calculate the length )
;

: CSTRING	( addr len -- c-addr )
	SWAP OVER	( len saddr len )
	HERE @ SWAP	( len saddr daddr len )
	CMOVE		( len )

	HERE @ +	( daddr+len )
	0 SWAP C!	( store terminating NUL char )

	HERE @ 		( push start address )
;


: UNUSED	( -- n )
	TOPMEM @		( get end of data segment according to the kernel )
	HERE @		( get current position in data segment )
	-
	4 /		( returns number of cells )
;


: WEL
	CLEAR CR ." MY-FORTH version 0." 1 . 
	."  adapted from Jonesforth version 47"  CR
	." Corrections and additions by Richard Russell, 19-Oct-2009 " CR
	." adapted by august0815, 01-FEB-2010" CR
	UNUSED . ." cells remaining" CR
	
;
4 CONSTANT CELL
: NOOP ( -- ) ;  


( ----------------------------------------------------------------------- )

( The preceding code is standard Jonesforth (except U. using U/MOD). )
( Below are the BB4Wforth additions and corrections to Jonesforth. )
( These improve ISO compliance and provide access to the BB4W host. )
( (C) Copyright Richard T. Russell 2009, http://www.rtrussell.co.uk/ )
( You may freely adapt this code so long as acknowledgement is made. )
( some words adapted by august0815 , 06.02.2010 )
( STILL TESTING)
;
0 VALUE PNSPTR ;
: SOURCE text_buff @ DUP PPTR @ - ;
: >IN PPTR @ ;
: ALIAS
  TEILWORT HEADER TEILWORT FIND >CFA @
  DUP DOCOL = IF ." Cannot alias a non-code word" THEN
  ,
; 
: CELL+ ( a-addr1 -- a-addr2 ) 1 CELLS + ;
: CELL- ( a-addr1 -- a-addr2 ) 1 CELLS - ;
: CHARS ( n1 -- n2 ) ;
IMMEDIATE ;

ALIAS (HERE) HERE ;

ALIAS (FIND) FIND ;

ALIAS (KEY) KEY1 ;

HIDE DEPTH ;
HIDE .S ;
HIDE HERE ;
HIDE ALLOT ;

HIDE TRUE ;
HIDE FIND ;
HIDE WHILE ;
HIDE REPEAT ;

( HIDE ' ( LIT is identical ) )


: DOES> R> LATEST @ >DFA ! ; 
: DEPTH ( -- +n ) S0 @ 4- DSP@ - 4 / ;
: .S ( -- ) S0 @ DEPTH 1 ?DO 4- DUP @ . LOOP DROP ;
: HERE ( -- addr ) (HERE) @ ;
: ALLOT ( n -- ) HERE + (HERE) ! ;

echoon ;

: CREATE ( "<spaces>name" -- ) 
	1 CELLS ALLOT 	( HERE push the pointer to this memory )
	TEILWORT HEADER	( make the dictionary entry  )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		( append the pointer to the new memory )
	' EXIT , 	( append the codeword EXIT ) ;
IMMEDIATE ;
echooff ;

: TRUE ( -- true ) -1 ;
: COUNT ( caddr1 -- caddr2 u ) DUP C@ SWAP 1+ SWAP ;

: ' ( "<spaces>name" -- xt ) TEILWORT (FIND) >CFA ; 

: FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
  DUP COUNT (FIND) DUP 0= IF FALSE EXIT THEN
  NIP DUP 4+ @ F_IMMED AND 0= IF >CFA -1 EXIT THEN
  >CFA 1 
;


: WHILE ( C: dest -- orig dest )
	['] 0BRANCH ,
	HERE 
	SWAP
	0 ,	
; 
IMMEDIATE ;

: REPEAT ( C: orig dest -- )
	['] BRANCH ,
	HERE - ,
	DUP
	HERE SWAP -
	SWAP !
; 
IMMEDIATE ;

: WORD 
  0 BEGIN DROP
  SOURCE NIP >IN @ <= IF DROP 0 HERE C! HERE EXIT THEN 
  (KEY) 2DUP <> UNTIL 
  HERE -ROT BEGIN ROT 1+ 2DUP C! -ROT DROP 
  SOURCE NIP >IN @ <= IF DROP HERE - HERE C! HERE EXIT THEN 
  (KEY) 2DUP = UNTIL 2DROP HERE - HERE C! HERE
  
;

( Add standard Forth words )
: 2* ( n -- [n*2] ) DUP + ;
: U2/ ( n -- [n/2] ) 1 SHR ;

: <BUILDS TEILWORT HEADER dodoes , 0 , ;
( DOES> R> LATEST @ >DFA ! ) 
: >BODY ( xt -- a-addr ) 2 CELLS + ;

: ABS ( n -- u ) DUP 0< IF NEGATE THEN ;
: MAX ( n1 n2 -- n3 ) 2DUP < IF SWAP THEN DROP ;
: MIN ( n1 n2 -- n3 ) 2DUP > IF SWAP THEN DROP ;

(  MS ( u -- ) Sleep SYSCALL DROP )
: TYPE ( c-addr u -- ) TELL ;
( INCLUDED ( c-addr u -- ) EXEC )
: [CHAR] ( "<spaces>name" -- ) CHAR ['] LIT , , ; 
IMMEDIATE ;
: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) 3 PICK 3 PICK ;
: CHAR+ ( c-addr1 -- c-addr2 ) 1 CHARS + ;
: 2! ( x1 x2 a-addr -- ) SWAP OVER ! CELL+ ! ;
: 2@ ( a-addr -- x1 x2 ) DUP CELL+ @ SWAP @ ;
: LSHIFT ( x1 u -- x2 ) 0 ?DO 2* LOOP ;
: RSHIFT ( x1 u -- x2 ) 0 ?DO U2/ LOOP ;
: MOVE ( addr1 addr2 u -- ) CMOVE ;
: 2>R ( x1 x2 -- ) ( R: -- x1 x2 ) ['] SWAP , ['] >R , ['] >R , ; 
IMMEDIATE ;
: 2R> ( -- x1 x2 ) ( R: x1 x2 -- ) ['] R> , ['] R> , ['] SWAP , ; 
IMMEDIATE ;

: <# ( -- ) HERE 128 + PNSPTR ! ;
: #> ( xd -- c-addr u ) DROP DROP HERE 128 + PNSPTR @ - PNSPTR @ SWAP ;
: M/MOD ( n1 n2 n3 -- n4 n5 n6 ) >R 0 R@ UM/MOD R> SWAP >R UM/MOD R> ;
: HOLD ( char -- ) -1 PNSPTR +! PNSPTR @ C! ;
: SIGN ( n -- ) 0< IF 45 HOLD THEN ;
: # ( ud1 -- ud2 ) BASE @ M/MOD ROT 9 OVER < IF 7 + THEN 48 + HOLD ;
: #S ( ud1 -- ud2 ) BEGIN # OVER OVER OR 0= UNTIL ;

: .( ( "ccc<paren>" -- )
  BEGIN (KEY) DUP ')' = IF DROP EXIT THEN EMIT AGAIN
; 
IMMEDIATE ;

: DIGIT ( char base -- u valid )
  SWAP 48 -
  DUP 0< IF DROP FALSE EXIT THEN
  DUP 16 > IF 7 - THEN
  TUCK <= IF FALSE EXIT THEN
  TRUE
; 

: >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
  2DUP + >R 0
  ?DO DUP C@ BASE @ DIGIT 0= IF DROP LEAVE THEN
  SWAP >R SWAP BASE @ UM* DROP ROT BASE @ UM* D+ R> 1+
  LOOP R> OVER -
;

: CONVERT ( ud1 c-addr1 -- ud2 c-addr2 ) CHAR+ 65535 >NUMBER DROP ;

: POSTPONE ( "<spaces>name" -- )
  BL WORD FIND DUP 0= IF ABORT THEN
  -1 = IF
    ['] LIT , , ['] , ,
  ELSE
    ,
  THEN
; 
IMMEDIATE ;
: C" ( -- c-addr )
  STATE @ IF ( compiling? )
    ['] LITSTRING , HERE 0 , 0 C, BEGIN (KEY) DUP '"' <> WHILE C, REPEAT
    DROP DUP HERE SWAP - 4- SWAP 2DUP ! 4+ SWAP 1- SWAP C! ALIGN ['] DROP ,
  ELSE ( immediate mode )
    HERE BEGIN 1+ (KEY) DUP '"' <> WHILE OVER C! REPEAT
    DROP HERE - 1- HERE C! HERE
  THEN
; 

\  see ftp://ccreweb.org/software/kforth/examples/ansi.4th
\ ansi.4th
\
\ ANSI Terminal words for kForth
\
\ Copyright (c) 1999--2004 Krishna Myneni
\ Creative Consulting for Research and Education
\
\ This software is provided under the terms of the GNU
\ General Public License.
\
\ ====> Requires that the file strings.4th be included first
\      (not now ! august0815)
\ Revisions: 
\    06-10-1999
\    10-11-1999 force cursor to 0 0 on page define at-xy  KM
\    01-23-2000 replaced char with [char] for ANS Forth compatibility KM
\    08-29-2002 use 0,0 as top left for AT-XY in accord with ANS Forth  KM
\    09-08-2004 added console query words provided by Charley Shattuck: 
\                 AT-XY?  ROWS  COLS   
\               Note that ROWS and COLS are also provided in gforth and PFE
\    09-10-2004 added scrolling words -- CS
\ Colors
\ at moment many words 'foo'

0 CONSTANT BLACK  ;
1 CONSTANT RED ;
2 CONSTANT GREEN ;
3 CONSTANT YELLOW ;
4 CONSTANT BLUE ;
5 CONSTANT MAGENTA ;
6 CONSTANT CYAN ;
7 CONSTANT WHITE ;
VARIABLE TMP_X ;
VARIABLE TMP_Y ;

VARIABLE ORIG_BASE ;

: SAVE_BASE ( -- | STORE CURRENT BASE AND SET TO DECIMAL )
	BASE @ ORIG_BASE ! 
	DECIMAL ;

: RESTORE_BASE ( -- | RESTORE ORIGINAL BASE )
	ORIG_BASE @ BASE ! ;

SAVE_BASE ;

: ANSI_ESCAPE ( -- | OUTPUT ESCAPE CODE ) 
	48 EMIT ;


: CLRTOEOL ( -- | CLEAR TO END OF LINE )
	49 EMIT ;

: GOTOXY ( X Y -- | POSITION CURSOR AT COL X ROW Y, ORIGIN IS 1,1 )
	SAVE_BASE
	atx
	RESTORE_BASE ;

: AT-XY ( X Y -- |  ANS COMPATIBLE VERSION OF GOTOXY, ORIGIN IS 0,0 )
	SAVE_BASE
	atx
	RESTORE_BASE ;

: PAGE ( -- | CLEAR THE SCREEN AND PUT CURSOR AT TOP LEFT )
	CLEAR ;

: CUR_UP ( N -- | MOVE CURSOR UP BY N LINES )
	SAVE_BASE  
	CURSOR_POS_Y -!
	RESTORE_BASE ;

: CUR_DOWN ( N -- | MOVE CURSOR DOWN BY N LINES )
	SAVE_BASE 
	CURSOR_POS_Y +!
	RESTORE_BASE ;

: CUR_LEFT ( N -- | MOVE CURSOR LEFT BY N COLUMNS )
	SAVE_BASE
	cursor_back
	RESTORE_BASE ;

: CUR_RIGHT ( N -- | MOVE CURSOR RIGHT BY N COLUMNS )
	SAVE_BASE
	cursor_forward
	RESTORE_BASE ;

: SAVE_CURSOR ( -- | SAVE CURRENT CURSOR POSITION )
	CURSOR_POS_X @ TMP_X ! CURSOR_POS_Y @ TMP_Y ! ;

: RESTORE_CURSOR ( -- | RESTORE CURSOR TO PREVIOUSLY SAVED POSITION )
	TMP_X @ CURSOR_POS_X !  TMP_Y @ CURSOR_POS_Y !  ;

: FOREGROUND ( N -- | SET FOREGROUND COLOR TO N )
	SAVE_BASE
	INK
	RESTORE_BASE ;

: BACKGROUND ( N -- | SET BACKGROUND COLOR TO N )
	SAVE_BASE
	bg 
	RESTORE_BASE ;

: TEXT_NORMAL ( -- | SET NORMAL TEXT DISPLAY )
	50 EMIT ;

: TEXT_BOLD ( -- | SET BOLD TEXT )
	51 EMIT ;

: TEXT_UNDERLINE ( -- | SET UNDERLINED TEXT )
	SAVE_BASE
	51 EMIT
	RESTORE_BASE ;

: TEXT_BLINK ( -- | SET BLINKING TEXT )
	SAVE_BASE
	53 EMIT
	RESTORE_BASE ;

: TEXT_REVERSE ( -- | SET REVERSE VIDEO TEXT )
	SAVE_BASE
	54 EMIT
	RESTORE_BASE ;  

: READ-CDNUMBER  ( C - N | READ A NUMERIC ENTRY DELIMITED BY CHARACTER C)
	>R 0 BEGIN
		KEY1 DUP R@ - WHILE
		SWAP 10 * SWAP [CHAR] 0 - +
	REPEAT
	R> 2DROP ;

: AT-XY?  ( -- X Y | RETURN THE CURRENT CURSOR COORDINATES)
	 CURSOR_POS_X @  CURSOR_POS_Y @ ;

: ROWS  ( -- N | RETURN ROW SIZE OF CONSOLE) 
   ( SAVE_CURSOR  0 100 AT-XY AT-XY? NIP RESTORE_CURSOR  ) 80 ;

: COLS  ( -- N | RETURN COLUMN SIZE OF CONSOLE)
   ( SAVE_CURSOR  200 0 AT-XY  AT-XY? DROP RESTORE_CURSOR ) 24 ;  

: RESET-SCROLLING  (  - )
	55 EMIT ;

: SCROLL-WINDOW  ( START END - )
	56 EMIT ;

: SCROLL-UP  (  - ) 57 EMIT ;

: SCROLL-DOWN  (  - ) 58 EMIT ;


RESTORE_BASE ;

( ------------------------------------------------------------------ )
( ------------------some tests-------------------------------------- )

: MULT_TABLE  CR 11 1 DO   
                      11 1 DO  
                      		I J *  5 U.R  
                      		LOOP
                      CR LOOP ;


: /STRING ( A1 U1 N -- A2 U2 | ADJUST SIZE OF STRING BY N CHARACTERS)
	DUP >R - SWAP R> + SWAP ;

IMMEDIATE ; 
: SEARCH-TABLE           ( N1 A1 N2 N3 -- N4 F)
      SWAP >R            ( N1 A1 N3)
      ROT ROT            ( N3 N1 A1)
      OVER OVER          ( N3 N1 A1 N1 A1)
      0                  ( N3 N1 A1 N1 A1 N2)
      BEGIN              ( N3 N1 A1 N1 A1 N2)
            SWAP OVER    ( N3 N1 A1 N1 N2 A1 N2)
            CELLS +      ( N3 N1 A1 N1 N2 A2)
            @ DUP        ( N3 N1 A1 N1 N2 N3 N3)
            0> >R        ( N3 N1 A1 N1 N2 N3)
            ROT <>       ( N3 N1 A1 N2 F)
            R@ AND       ( N3 N1 A1 N2 F)
      WHILE              ( N3 N1 A1 N2)
            R> DROP      ( N3 N1 A1 N2)
            R@ +         ( N3 N1 A1 N2+2)
            >R OVER OVER ( N3 N1 A1 N1 A1)
            R>           ( N3 N1 A1 N1 A1 N2+2)
      REPEAT             ( N3 N1 A1 N2)
      R@ IF
            >R ROT R>    ( N1 A1 N3 N2)
       + CELLS + @    ( N1 N4)
       SWAP DROP      ( N3)
  ELSE
       DROP DROP DROP ( N1)
  THEN
  R>                  ( N F)
  R> DROP             ( N F)
;


0 CONSTANT NULL
3 CONSTANT MF

CREATE MONTHTABLE ;
      1 , S" JANUARY " , 31 , ;
      2 , S" FEBRUARY " , 28 , ;
      3 , S"   MARCH " , 31 , ; 
      4 , S"   APRIL " , 30 , ;
      5 , S"    MAY   " , 31 , ;
      6 , S"   JUNE   " , 30 , ;
      7 , S"   JULY   " , 31 , ;
      8 , S" AUGUST " , 31 , ;
      9 , S" SEPTEMBER" , 30 , ; 
      10 , S" OCTOBER " , 31 , ;
      11 , S" NOVEMBER " , 30 , ;
      12 , S" DECEMBER " , 31 , ;
      NULL , ;

: SM MONTHTABLE MF 1 SEARCH-TABLE ;
: GM        ( N --)
      SM
  IF                       \ IF MONTH IS FOUND
        TYPE \ PRINT ITS NAME
  ELSE                     \ IF MONTH IS NOT FOUND
       DROP ." NOT FOUND"  \ DROP VALUE
  THEN                     \ AND SHOW MESSAGE
  CR
;


IMMEDIATE ; 
echoon ;
