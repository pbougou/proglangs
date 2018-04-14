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
#include <time.h>
#include <sys/time.h>
#include "befunge.h" 

/* ****************************
 * Global variable definitions
 * ****************************
 * */
char **codetext ;
struct stack *st ;
int  curcol, // current column number
     currow; // current row number

// help variables
signed long int a,b,c;		 
char d;

char curdir,
	 curcmd; // current direction

char *buffer = NULL;
size_t ssize = 0;
/* ***********************************
 * Declarations for extern functions.
 * Linker has to take care...
 * ***********************************
 */
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
	 * and its runtime stack(stack-based VM).
	 * Initialization of 2D-torus with null commands
	 * and number of elements(cnt) in stack at zero.
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
		
		TOT_ROWS++;  k++;  j = 0;  i++;
		
	}
	//printf("rows = %d, cols = %d\n", TOT_ROWS, TOT_COLS);
	free(buffer);

	struct timeval start, end;
	gettimeofday(&start, NULL);
	srand(time(NULL));
	currow = 0; curcol = 0; curcmd = codetext[currow][curcol]; curdir = right;
	while(1) {
		//print(st);		
		curcmd = codetext[currow][curcol];

		if(isdigit(curcmd)){
			NUMBER();
			continue;
		}
		
		switch(curcmd) {
			case _add:
				ADD();
				break;
			case _sub:
				SUB();
				break;
			case _mul:
				MUL();
				break;
			case _div:
				DIV();
				break;
			case _mod:
				MOD();
				break;
			case _not:
				NOT();
				break;
			case right:
				RIGHT_PC();
				break;
			case left:
				LEFT_PC();
				break;
			case up:
				UP_PC();
				break;
			case down:
				DOWN_PC();
				break;
			case _null:
				NULL_CMD();
				break;
			case random:
				RANDOM_PC();
				break;
			case horizontial_if:
				HORIZONTIAL_IF();
				break;
			case vertical_if:
				VERTICAL_IF();
				break;
			case readint:
				READ_INT();
				break;
			case readascii:
				READ_ASCII();
				break;
			case printint:
				PRINT_INT();
				break;
			case printascii:
				PRINT_ASCII();
				break;
			case skip:
				SKIP();
				break;
			case duple:
				DUPLICATE();
				break;
			case popandskip:
				POP_AND_SKIP();
				break;	
			case top2swp:
				SWAP();
				break;
			case greater:
				IS_GREATER();
				break;
			case get:
				GET();
				break;
			case put:
				PUT();
				break;
			case _string_:
				STRINGMODE();
				break;
			case endp:
				gettimeofday(&end, NULL);
				double total = 0;
				total = (end.tv_sec - start.tv_sec) + (end.tv_usec - start.tv_usec) * 0.000001;
				printf("\n\n");
				printf("Overall time = %lf\n", total);
				return 0;
			default:
				// Uncomment next line if you want, but befungex.bf won't work properly...
				#ifdef DBG_FLAG
					printf("%c is not a befunge-93 command\n",curcmd);
				#endif
				NEXT_PC();
				break;

		}	
	
	}
	return 0;		
}



