/*  *****************************************************************************
 *  COMMAND         INITIAL STACK (bot->top)RESULT (STACK)
 *	-------         -------------           -----------------
 *	+ (add)         <value1> <value2>       <value1 + value2>
 *	- (subtract)    <value1> <value2>       <value1 - value2>
 *	* (multiply)    <value1> <value2>       <value1 * value2>
 *	/ (divide)      <value1> <value2>       <value1 / value2> (nb. integer)
 *	% (modulo)      <value1> <value2>       <value1 mod value2>
 *	! (not)         <value>                 <0 if value non-zero, 1 otherwise>
 *	` (greater)     <value1> <value2>       <1 if value1 > value2, 0 otherwise>
 *	> (right)                               PC -> right
 *	< (left)                                PC -> left
 *	^ (up)                                  PC -> up
 *	v (down)                                PC -> down
 *	? (random)                              PC -> right? left? up? down? ???
 *	_ (horizontal if) <boolean value>       PC->left if <value>, else PC->right
 *	| (vertical if)   <boolean value>       PC->up if <value>, else PC->down
 *	" (stringmode)                          Toggles 'stringmode'
 *	: (dup)         <value>                 <value> <value>
 *	\ (swap)        <value1> <value2>       <value2> <value1>
 *	$ (pop)         <value>                 pops <value> but does nothing
 *	. (pop)         <value>                 outputs <value> as integer
 *	, (pop)         <value>                 outputs <value> as ASCII
 *	# (bridge)                              'jumps' PC one farther; skips
 *	                                        over next command
 *	g (get)         <x> <y>                 <value at (x,y)>
 *	p (put)         <value> <x> <y>         puts <value> at (x,y)
 *	& (input value)                         <value user entered>
 *	~ (input character)                     <character user entered>
 *	@ (end)                                 ends program
 *	*****************************************************************************
 * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <time.h>
#include "befunge.h" 

#define NEXT_INSTRUCTION goto next_instruction;
/* *****************
 * Global variables
 * *****************
 * */
char **codetext ;
struct stack *st ;
int  curcol, // current column number
     currow; // current row number
signed long int a,b,c;		 // help variables
char d;

char curdir,
	 curcmd; // current direction

char *buffer = NULL;
size_t ssize = 0;

extern void	NEXT_PC();
extern int UP_PC();
extern int DOWN_PC();
extern int LEFT_PC();
extern int RIGHT_PC();
extern int RANDOM_PC();
extern int NULL_CMD();
extern int ADD();
extern int SUB();
extern int MUL();
extern int DIV();
extern int NOT();
extern int MOD();
extern int STRINGMODE();
extern int HORIZONTIAL_IF();
extern int VERTICAL_IF();
extern int READ_INT();
extern int READ_ASCII();
extern int PRINT_INT();
extern int PRINT_ASCII();
extern int SKIP(); 
extern int POP_AND_SKIP();
extern int SWAP();
extern int DUPLICATE();
extern int IS_GREATER();
extern int GET();
extern int PUT();
extern int NUMBER();
extern int read_code();

int TOT_ROWS = 0, TOT_COLS = 0;

int main(int argc, char **argv) {
	if(argc != 2) {
		fprintf(stdout, "Usage: ./%s befunge93_src.bf\n", argv[0]);
		exit(0);	
	}

	/* ***********************************************
	 * Memory allocation for befunge-93 source code
	 * and its runtime stack.
	 * Initialization of 2D-torus with null commands
	 * and number of elements in stack at zero.
	 * ***********************************************
	 * */
	

	int i = 0, j = 0;		
	codetext = (char **) malloc(dimX * sizeof(char *));
    for(i=0; i < dimX; i++) 
		codetext[i] = (char *) malloc(dimY * sizeof(char));

	st = malloc(sizeof(*st));
	st->cnt = 0;
	for(i = 0; i < dimX; ++i)
		for(j = 0; j < dimY; ++j)
			codetext[i][j] = ' ';


	read_code(argv[1]);
	// Fix memory layout...	
	int k = 0; 
	i = 0; j = 0;
	while(buffer[k] != '\0') {

		while(buffer[k] != '\n' && buffer[k] != '\0') {
			codetext[i][j] = buffer[k];
			k++; j++;
		}

		if(j > TOT_COLS)
			TOT_COLS = j;

		k++;  j = 0;  i++;  TOT_ROWS++;
	}
	free(buffer);

	struct timeval start, end;
	gettimeofday(&start, NULL);
	srand(time(NULL));
	currow = 0; curcol = 0; curcmd = codetext[currow][curcol]; curdir = right;
	while(1) {
		next_instruction:		
		curcmd = codetext[currow][curcol];

		if(isdigit(curcmd)){
			NUMBER();
			continue;
		}
		
		switch(curcmd) {
			case _add:
				ADD();
				NEXT_INSTRUCTION;
			case _sub:
				SUB();
				NEXT_INSTRUCTION;
			case _mul:
				MUL();
				NEXT_INSTRUCTION;
			case _div:
				DIV();
				NEXT_INSTRUCTION;
			case _mod:
				MOD();
				NEXT_INSTRUCTION;
			case _not:
				NOT();
				NEXT_INSTRUCTION;
			case right:
				RIGHT_PC();
				NEXT_INSTRUCTION;
			case left:
				LEFT_PC();
				NEXT_INSTRUCTION;
			case up:
				UP_PC();
				NEXT_INSTRUCTION;
			case down:
				DOWN_PC();
				NEXT_INSTRUCTION;
			case _null:
				NULL_CMD();
				NEXT_INSTRUCTION;
			case random:
				RANDOM_PC();
				NEXT_INSTRUCTION;
			case horizontial_if:
				HORIZONTIAL_IF();
				NEXT_INSTRUCTION;
			case vertical_if:
				VERTICAL_IF();
				NEXT_INSTRUCTION;
			case readint:
				READ_INT();
				NEXT_INSTRUCTION;
			case readascii:
				READ_ASCII();
				NEXT_INSTRUCTION;
			case printint:
				PRINT_INT();
				NEXT_INSTRUCTION;
			case printascii:
				PRINT_ASCII();
				NEXT_INSTRUCTION;
			case skip:
				SKIP();
				NEXT_INSTRUCTION;
			case duple:
				DUPLICATE();
				NEXT_INSTRUCTION;
			case popandskip:
				POP_AND_SKIP();
				NEXT_INSTRUCTION;	
			case top2swp:
				SWAP();
				NEXT_INSTRUCTION;
			case greater:
				IS_GREATER();
				NEXT_INSTRUCTION;
			case get:
				GET();
				NEXT_INSTRUCTION;
			case put:
				PUT();
				NEXT_INSTRUCTION;
			case _string_:
				STRINGMODE();
				NEXT_INSTRUCTION;
			case endp:
				gettimeofday(&end, NULL);
				double total = 0;
				total = (end.tv_sec - start.tv_sec) + (end.tv_usec - start.tv_usec) * 0.000001;
				printf("\n\n");
				printf("Overall time = %lf\n", total);
				return 0;
			default:
				#ifdef DBG_FLAG
					printf("%c is not a befunge-93 command\n",curcmd);
				#endif
				NEXT_PC();
				NEXT_INSTRUCTION;

		}	
	
	}
	return 0;		

}



