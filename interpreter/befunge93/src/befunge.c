#include <stdio.h>
#include "befunge.h"

long int size(struct stack *st) {
	return st->cnt;
}

int isEmpty(struct stack *st) {
	if(!size(st))
		return 0;
	else
		return 1;
}


int isFull(struct stack *st) {
	if(size(st) == MAX_STACK_SIZE - 1) {
		return 0;
	}
	else {
		return 1;	
	}
}


int push(struct stack *st, signed long int n) {
	if(!isFull(st))
		return -1;
	else {
		st->cnt++;
		st->elems[st->cnt] = n;
		return 0;
	}
			
}

signed long int pop(struct stack *st) {
	
	if(!isEmpty(st)){
		return 0;
	}
	else {
		int res = st->elems[st->cnt];
		st->cnt--;
		return res;		
	}
}

signed long int gettop(struct stack *st) {
	if(!isEmpty(st))
		return 0;
	else
		return st->elems[st->cnt];
}

