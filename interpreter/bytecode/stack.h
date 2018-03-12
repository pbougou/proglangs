#include <stdio.h>
#include <sys/types.h>


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
 *	check if stack is full, 1 success
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
FILE *stacklogfp;

typedef struct stack {
	long int cnt;
	int32_t elems[MAX_STACK_SIZE];
} stack;

int isEmpty(struct stack *st);

int isFull(struct stack *st);

void push(struct stack *st, signed int n); 

signed int pop(struct stack *st); 

void get_ith(struct stack *st, long int i);

void print_stack(struct stack *st);



