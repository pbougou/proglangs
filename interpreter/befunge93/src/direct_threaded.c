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

/* *************************
 * Pre-processor directives
 * *************************
 * */

#define FN_NUM 128 // Ascii code range

#ifdef __GNUC__
#define NEXT_INSTRUCTION \
	goto **(void **)(pc)
#else
#define NEXT_INSTRUCTION \
	goto next_instruction
#endif

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

char curdir, // current direction
	 curcmd; // current command 

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

int TOT_COLS = 0, TOT_ROWS = 0;

int main(int argc, char **argv) {
	if(argc != 2) {
		fprintf(stdout, "Usage: ./%s befunge93_src.bf\n", argv[0]);
		exit(0);	
	}
#ifdef __GNUC__
	static void *label_tab[FN_NUM] = {
		&&default_label,//0
		&&default_label,//1
		&&default_label,//2
		&&default_label,//3
		&&default_label,//4
		&&default_label,//5
		&&default_label,//6
		&&default_label,//7
		&&default_label,//8
		&&default_label,//9
		&&default_label,//10
		&&default_label,//11
		&&default_label,//12
		&&default_label,//13
		&&default_label,//14
		&&default_label,//15
		&&default_label,//16
		&&default_label,//17
		&&default_label,//18
		&&default_label,//19
		&&default_label,//20
		&&default_label,//21
		&&default_label,//22
		&&default_label,//23
		&&default_label,//24
		&&default_label,//25
		&&default_label,//26
		&&default_label,//27
		&&default_label,//28
		&&default_label,//29
		&&default_label,//30
		&&default_label,//31
		&&noop_label,//32
		&&not_label,//33
		&&string_label,//34
		&&skip_label,//35
		&&popandskip_label,//36
		&&mod_label,//37
		&&readint_label,//38
		&&default_label,//39
		&&default_label,//40
		&&default_label,//41
		&&mul_label,//42
		&&add_label,//43
		&&printascii_label,//44
		&&sub_label,//45
		&&printint_label,//46
		&&div_label,//47
		&&num_label,//48
		&&num_label,//49
		&&num_label,//50
		&&num_label,//51
		&&num_label,//52
		&&num_label,//53
		&&num_label,//54
		&&num_label,//55
		&&num_label,//56
		&&num_label,//57
		&&duple_label,//58
		&&default_label,//59
		&&left_pc_label,//60
		&&default_label,//61
		&&right_pc_label,//62
		&&rnd_pc_label,//63
		&&endp_label,//64
		&&default_label,//65
		&&default_label,//66
		&&default_label,//67
		&&default_label,//68
		&&default_label,//69
		&&default_label,//70
		&&default_label,//71
		&&default_label,//72
		&&default_label,//73
		&&default_label,//74
		&&default_label,//75
		&&default_label,//76
		&&default_label,//77
		&&default_label,//78
		&&default_label,//79
		&&default_label,//80
		&&default_label,//81
		&&default_label,//82
		&&default_label,//83
		&&default_label,//84
		&&default_label,//85
		&&default_label,//86
		&&default_label,//87
		&&default_label,//88
		&&default_label,//89
		&&default_label,//90
		&&default_label,//91
		&&top2swp_label,//92
		&&default_label,//93
		&&up_pc_label,//94
		&&horizontial_if_label,//95
		&&greater_label,//96
		&&default_label,//97
		&&default_label,//98
		&&default_label,//99
		&&default_label,//100
		&&default_label,//101
		&&default_label,//102
		&&get_label,//103
		&&default_label,//104
		&&default_label,//105
		&&default_label,//106
		&&default_label,//107
		&&default_label,//108
		&&default_label,//109
		&&default_label,//110
		&&default_label,//111
		&&put_label,//112
		&&default_label,//113
		&&default_label,//114
		&&default_label,//115
		&&default_label,//116
		&&default_label,//117
		&&down_pc_label,//118
		&&default_label,//119
		&&default_label,//120
		&&default_label,//121
		&&default_label,//122
		&&default_label,//123
		&&vertical_if_label,//124
		&&default_label,//125
		&&readascii_label,//126
		&&default_label//127
};
#endif
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

		if(TOT_COLS < j)
			TOT_COLS = j;

		k++;  j = 0;  i++;  TOT_ROWS++;
	}
	free(buffer);

	#ifdef __GNUC__
	void *direct_labels[dimX][dimY];
	for(i = 0; i < dimX; i++)
		for(j = 0; j < dimY; j++) {
			int tmp = codetext[i][j];
			direct_labels[i][j] = label_tab[tmp];
	}
	void **pc;
	#endif


	struct timeval start, end;
	gettimeofday(&start, NULL);
	srand(time(NULL));
	currow = 0; curcol = 0;  curdir = right;
	
	while(1) {
	next_instruction:
		curcmd = codetext[currow][curcol];
		switch(curcmd) {
			case '0': case '1': case '2':
			case '3': case '4': case '5':
			case '6': case '7': case '8':
			case '9':
				num_label:
					NUMBER();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _add:
				add_label:
					ADD();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _sub:
				sub_label:
					SUB();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _mul:
				mul_label:
					MUL();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _div:
				div_label:
					DIV();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _mod:
				mod_label:
					MOD();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _not:
				not_label:
					NOT();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case right:
				right_pc_label:
					RIGHT_PC();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case left:
				left_pc_label:
					LEFT_PC();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case up:
				up_pc_label:
					UP_PC();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case down:
				down_pc_label:
					DOWN_PC();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _null:
				noop_label:
					NULL_CMD();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case random:
				rnd_pc_label:
					RANDOM_PC();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case horizontial_if:
				horizontial_if_label:
					HORIZONTIAL_IF();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case vertical_if:
				vertical_if_label:
					VERTICAL_IF();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case readint:
				readint_label:
					READ_INT();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case readascii:
				readascii_label:
					READ_ASCII();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case printint:
				printint_label:
					PRINT_INT();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case printascii:
				printascii_label:
					PRINT_ASCII();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case skip:
				skip_label:
					SKIP();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case duple:
				duple_label:
					DUPLICATE();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case popandskip:
				popandskip_label:
					POP_AND_SKIP();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;	
			case top2swp:
				top2swp_label:
					SWAP();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case greater:
				greater_label:
					IS_GREATER();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case get:
				get_label:
					GET();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case put:
				put_label:
					PUT();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case _string_:
				string_label:
					STRINGMODE();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;
			case endp:
				endp_label:
					gettimeofday(&end, NULL);
					double total = 0;
					total = (end.tv_sec - start.tv_sec) + (end.tv_usec - start.tv_usec) * 0.000001;
					printf("\n\n");
					printf("Overall time = %lf\n", total);
					return 0;
			default:
				default_label:
					#ifdef DBG_FLAG
						printf("%c is not a befunge-93 command\n",curcmd);
					#endif
					NEXT_PC();
					#ifdef __GNUC__
						pc = *direct_labels + (currow * dimY + curcol);
					#endif
					NEXT_INSTRUCTION;

		}	
	
	}
	return 0;		
}



