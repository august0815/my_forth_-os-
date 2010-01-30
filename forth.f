\ DAS ist ein TESt
: BL   32 ;
: '\n' 10 ;
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
	DECIMAL
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
	HEX
;
( From now on we can use ( ... ) for comments.)

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





( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

: ascii  127 AND DUP 32 < IF  DROP 46 THEN EMIT ;



: ALIGNED	( addr -- addr )
	3 + 3 INVERT AND	( (addr+3) & ~3 )
;


: ALIGN HERE @ ALIGNED HERE ! ;
: C,
	HERE @ C!
	1 HERE +!
;
CLSSTACK


: S" IMMEDIATE
	STATE @ IF
		' LITSTRING ,
		HERE @
		0 ,
		BEGIN
			KEY1
			DUP 22 <>
		WHILE
			C,
		REPEAT
		DROP
		DUP	
		HERE @ SWAP -
		4-
		SWAP !
		ALIGN
	ELSE
		HERE @
		BEGIN
			KEY1
			DUP 22 <>
		WHILE
			OVER C!
			1+
		REPEAT
		DROP	
		HERE @ -
		HERE @
		SWAP
	THEN
;

: ." IMMEDIATE
	 STATE @ IF
		[COMPILE] S"
	' TELL , 
	ELSE 
		BEGIN
			KEY1
			DUP 22 = IF
				DROP
				EXIT
			THEN
			EMIT
		AGAIN
	THEN 
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
		
			DUP	
			CFA>
			ID. SPACE
		ENDCASE
	
		4 +
	REPEAT
	59 EMIT CR
	2DROP
	
;

