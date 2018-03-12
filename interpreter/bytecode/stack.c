#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

int isEmpty(struct stack *st) {
    return st->cnt == -1; 
}

int isFull(struct stack *st) {
	return st->cnt == MAX_STACK_SIZE;
}

void push(struct stack *st, signed int n) {
	if(isFull(st)) {
        fprintf(stderr, "Trying to push an element in a full stack. Exiting...\n");
    }
	else 
		st->elems[++st->cnt] = n;
}

signed int pop(struct stack *st) {
	if(isEmpty(st)) {
        fprintf(stderr, "Pop an element from empty stack is undefined. Exiting...\n");
        exit(-1);
    }
    return st->elems[st->cnt--];
}

void get_ith(stack *st, long int i) {
    if(i >= st->cnt) {
        fprintf(stderr, "%lu-th does not live in the stack anymore\n", i);
        exit(-1);
    }
    push(st, st->elems[st->cnt - i]);
}


void print_stack(struct stack *st) {
    for(int i = st->cnt - 1; i >= 0; i--)
        fprintf(stacklogfp, "%c", st->elems[i]);
    fprintf(stacklogfp, "\n");
}

