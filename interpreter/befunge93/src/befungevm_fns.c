#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "befunge.h" 

extern char **codetext;
extern struct stack *st;
extern int curcol, 
		   currow;

extern signed long int a, b, c;
extern char d;

extern char curdir, 
	        curcmd;

extern char *buffer;
extern size_t ssize;

extern int TOT_ROWS, TOT_COLS;
/* *****************************
 * Program counter next command
 * *****************************
 * */
void NEXT_PC() {
	if(curdir == right)	 {
		curcol++;
		if(curcol >= TOT_COLS) 
			curcol = 0;
	}
	else if(curdir == left) {
		curcol--;
		if(curcol < 0) {
			curcol = TOT_COLS - 1;
		}
	}
	else if(curdir == up) {
		currow--;
		if(currow < 0)
			currow = TOT_ROWS - 1;
	}
	else if(curdir == down) {
		currow++;
		if(currow >= TOT_ROWS)
			currow = 0;
	}
}

int UP_PC() {
	curdir = up;
	NEXT_PC();
	return 0;
}

int DOWN_PC() {
	curdir = down;
	NEXT_PC();
	return 0;
}

int LEFT_PC() {
	curdir = left;
	NEXT_PC();
	return 0;
}

int RIGHT_PC() {
	curdir = right;
	NEXT_PC();
	return 0;
}

int RANDOM_PC() {
	a = rand()%4;
	if(a == 0)
		curdir = right;
	else if(a == 1)
		curdir = left;
	else if(a == 2)
		curdir = up;
	else if(a == 3)
		curdir = down;
	NEXT_PC();
	return 0;
}

int NULL_CMD() {
	NEXT_PC();
	return 0;
}


/* *****************************************
 * Functions for basic arithmetic operations
 *	ADD, SUB, MUL, DIV, MOD, NOT
 * *****************************************
 * */

int ADD() {
	a = pop(st);
	b = pop(st);
	push(st, a+b);

	NEXT_PC();
	return 0;
}


int SUB() {
	a = pop(st);
	b = pop(st);
	
	push(st, b-a);
	NEXT_PC();
	return 0; 	
}


int MUL() {
	a = pop(st);
	b = pop(st);
	
	push(st, b*a);
	NEXT_PC();
	return 0; 	
}

int DIV() {
	a = pop(st);
	b = pop(st);
	
	
	if(a)
		push(st, b/a);
	NEXT_PC();
	return 0; 	
}


int MOD() {
	a = pop(st);
	b = pop(st);
	
	if(a)
		push(st, b%a);
	NEXT_PC();
	return 0; 	
}


int NOT() {
	a = pop(st);
	int res = a ? 0 : 1;
	
	push(st, res);
	NEXT_PC();
	return 0; 	
}

/* ********************
 * String manipulation
 * ********************
 * */

int STRINGMODE() {
	NEXT_PC();

	curcmd = codetext[currow][curcol];
	while(curcmd != _string_) {
		push(st, curcmd);
		NEXT_PC();
		curcmd = codetext[currow][curcol];
	}
	NEXT_PC();
	return 0;
}

/* **********************
 * Conditions:
 *  - horizontial move
 *  - vertical move
 * **********************
 * */

int HORIZONTIAL_IF() {
	curdir = (pop(st)) ? left : right;
	NEXT_PC();
	return 0;
}

int VERTICAL_IF() {
	curdir = pop(st) ? up : down;
	NEXT_PC();
	return 0;
}

/* ****************************
 * Input and output functions
 *  - as ascii
 *  - as integers
 * **************************** 
 * */

int READ_INT() {
	scanf("%ld", &a);
	push(st, a);
	NEXT_PC();
	return 0;
}

int READ_ASCII(){
	scanf("%c", &d);
	push(st, d);
	NEXT_PC();
	return 0;
}

int PRINT_INT() {
	printf("%ld ", pop(st));
	NEXT_PC();
	return 0;
}

int PRINT_ASCII() {
	d = pop(st);
	printf("%c", d);
	NEXT_PC();
	return 0;
}

/* *********************************
 * Special commands:
 *  - skip 
 *  - duplicate top element
 *  - pop elem and skip it
 *  - swap top two elements
 *  - is greater -- between top 2
 * *********************************
 * */

int SKIP() {
	NEXT_PC();
	NEXT_PC();
	return 0;
}

int DUPLICATE() {
	push(st, gettop(st));
	NEXT_PC();
	return 0;
}

int POP_AND_SKIP() {
	pop(st);
	NEXT_PC();
	return 0;
}

int SWAP() {
	a = pop(st);
	b = pop(st);
	push(st, a);
	push(st, b);
	NEXT_PC();
	return 0;
}

int IS_GREATER() {
	a = pop(st);
	b = pop(st);
	int res = b > a ? 1 : 0;
	push(st, res);
	NEXT_PC();
	return 0;
}
/* ***************************
 * Self modification commands
 * ***************************
 * */

int GET() {
	a = pop(st); b = pop(st);

	char h = codetext[a][b];
	push(st, h);
	
	NEXT_PC();
	return 0;	
}

int PUT() {
	a = pop(st);
	b = pop(st);
	d = pop(st);	
	
	if(a >= TOT_ROWS)
		TOT_ROWS = a + 1;
	if(b > TOT_COLS)
		TOT_COLS = b + 1;
	codetext[a][b] = d;
	NEXT_PC();
	return 0;
}

int NUMBER() {
	curcmd = codetext[currow][curcol];
	push(st, curcmd - '0' );
	NEXT_PC();
	return 0;
}


void read_code(char *argv) {
	// Open file
	FILE *fp = fopen(argv, "r");
	fseek(fp, 0, SEEK_END); 
	ssize = ftell(fp); 
	rewind(fp);
	// Allocate memory to save code
	// Buffered reading using fread
	buffer = malloc((ssize + 1) * sizeof(*buffer)); 
	fread(buffer, ssize, 1, fp);
	// End sentinel... Also a string!
	buffer[ssize] = '\0';
}
