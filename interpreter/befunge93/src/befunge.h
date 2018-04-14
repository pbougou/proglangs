#include <stdio.h>
/* ****************************************************
 * If the stack is empty when you pop something off, 
 * be warned that this will not generate an underflow! 
 * It will simply push a 0 value onto the stack. 
 * Hope you can live with it! 
 * ****************************************************
 * */

/* *********************************************
 * arithmetic commands and other stack commands 
 * *********************************************
 * */
#define _string_       34
#define _add           '+'
#define _sub           '-'
#define _mul           '*'
#define _div           '/'
#define _mod           '%'
#define _not           '!'
/* ***********************************************************
 * unconditional jumps... always taken
 *	<,>,v,^ : pc wraps around to the other edge of the torus
 *	?       : random movement... 
 *	space   : is the null command
 * ***********************************************************
 * */
#define right          '>'
#define left           '<'
#define up             '^'
#define down           'v'
#define _null          ' '
#define random         '?'
/* *************************************************
 * conditional jumps: decision making commands
 * ************************************************* 
 * true             : non zero element in the stack
 * */
#define horizontial_if '_'  
#define vertical_if    '|'  
/* *******************************************
 * input : either decimal value or ascii code
 * *******************************************
 * */
#define readint        '&'
#define readascii      '~'
/* *********************************
 * output: either decimal or ascii
 * *********************************
 * */
#define printint       '.'
#define printascii     ','
/* *****************
 * special commands
 * *****************
 * */
#define skip           '#'
#define duple          ':'
#define popandskip     '$'
#define top2swp        92 
#define greater        '`' // if value1 > value2 then 1 else 0 where stack = <value1,value2>
/* ***************************
 * self modification commands
 * ***************************
 * */
#define get            'g'
#define put            'p'
/* *************
 * end command
 * *************
 * */
#define endp           '@'

/* **************************************************
 * Stack control functions:
 * **************************************************
 * stack struct:
 *	An element that holds current stack size,
 *	elems array holds stack's current contents,
 *	which is are commands for execution
 *  
 * size: returns the number of elements of stack
 *
 * isEmpty:
 *	check if stack is empty
 *		if yes then it returns 0
 *		else returns 1
 *
 * isFull:
 *	check if stack is full:
 *		yes case: returns 0
 *		no case : returns 1
 *
 * push:
 *	add element at top of the stack
 *  increment top value
 *
 * pop:
 *	pop an element from the stack
 *	decrement top value
 *
 * gettop:
 *	get first element's value
 *	don't pop it from the stack
 *
 * 
 * **************************************************
 * */
#define MAX_STACK_SIZE 262144 
#define dimX           25
#define dimY           80


struct stack {
	long int cnt;
	signed long int elems[MAX_STACK_SIZE];
};

long int size(struct stack *st); 

int isEmpty(struct stack *st);

int isFull(struct stack *st);

int push(struct stack *st, signed long int n); 

signed long int pop(struct stack *st); 

signed long int gettop(struct stack *st);





