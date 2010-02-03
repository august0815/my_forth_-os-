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
	ROT		( u uwidth width )
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
		ROT		( 1 u width )
		( 1- )		( 1 u width-1 )
	ELSE
		0		( width u 0 )
		SWAP		( width 0 u )
		ROT		( 0 u width )
	THEN
	SWAP	( flag width u )
	DROP
	0<>	
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
	-ROT		( b c a )
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
	CREATE		( make the dictionary entry )
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
	TEILWORT CREATE	( make the dictionary entry (the name follows VARIABLE) )
	DOCOL ,		( append DOCOL (the codeword field of this word) )
	' LIT ,		( append the codeword LIT )
	,		( append the pointer to the new memory )
	' EXIT ,	( append the codeword EXIT )
;
: VALUE		( n -- )
	TEILWORT CREATE	( make the dictionary entry (the name follows VALUE) )
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
	TEILWORT FIND
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
	59 EMIT SPACE DUP ID. SPACE
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
		
				
		
		ENDCASE
			DUP	
			CFA>
			ID. SPACE
		ENDCASE
	
		4 +
	REPEAT
	59 EMIT CR
	2DROP
	
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
	." MY-FORTH version " 1 .
	."  adapted from Jonesforth version " VERSION . CR
	." Corrections and additions "
	." by august0815, 01-FEB-2010" CR
	UNUSED . ." cells remaining" CR
	." OK" CR


	
;

IMMEDIATE ; 
echoon ;
WEL ;

