#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stack-gc.h"

#define GC_THRESHOLD 1024 * 1024 * 20

void assert(int condition, const char* message) {
  if (!condition) {
    fprintf(stderr, "%s\n", message);
    exit(1);
  }
}

VM* newVM() {
  VM* vm = malloc(sizeof(VM));
  vm->stackSize = 0;
  vm->firstObject = NULL;
  vm->numObjects = 0;
  vm->maxObjects = GC_THRESHOLD;
  return vm;
}


void push(VM* vm, Object* value) {
  assert(vm->stackSize < STACK_MAX, "Stack overflow!");
  vm->stack[vm->stackSize++] = value;
}


Object* pop(VM* vm) {
  assert(vm->stackSize > 0, "Stack underflow!");
  return vm->stack[--vm->stackSize];
}

void mark(Object* object) {
  /* If already marked, we're done. Check this first to avoid recursing
     on cycles in the object graph. */
  if (object->marked) return;

  object->marked = 1;

  if (object->type == OBJ_PAIR) {
    mark(object->head);
    mark(object->tail);
  }
}

void markAll(VM* vm)
{
  for (int i = 0; i < vm->stackSize; i++) {
    mark(vm->stack[i]);
  }
}

void sweep(VM* vm)
{
  Object** object = &vm->firstObject;
  while (*object) {
    if (!(*object)->marked) {
      /* This object wasn't reached, so remove it from the list and free it. */
      Object* unreached = *object;

      *object = unreached->next;
      free(unreached);

      vm->numObjects--;
    } else {
      /* This object was reached, so unmark it (for the next GC) and move on to
       the next. */
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
}

void gc(VM* vm) {

  markAll(vm);
  sweep(vm);

  vm->maxObjects = vm->numObjects * 2;

}

Object* newObject(VM* vm, ObjectType type) {
  if (vm->numObjects == vm->maxObjects) gc(vm);

  Object* object = malloc(sizeof(Object));
  object->type = type;
  object->next = vm->firstObject;
  vm->firstObject = object;
  object->marked = 0;

  vm->numObjects++;

  return object;
}

void pushInt(VM* vm, int intValue) {
  Object* object = newObject(vm, OBJ_INT);
  object->value = intValue;

  push(vm, object);
}

Object* pushPair(VM* vm) {
  Object* object = newObject(vm, OBJ_PAIR);
  object->tail = pop(vm);
  object->head = pop(vm);

  push(vm, object);
  return object;
}
/*
void objectPrint(Object* object) {
  switch (object->type) {
    case OBJ_INT:
      printf("%d", object->value);
      break;

    case OBJ_PAIR:
      printf("(");
      objectPrint(object->head);
      printf(", ");
      objectPrint(object->tail);
      printf(")");
      break;
  }
}
*/
void freeVM(VM *vm) {
  vm->stackSize = 0;
  gc(vm);
  free(vm);
}

void get_ith(VM *vm, long int i) {
	if(i >= vm->stackSize) {
        fprintf(stderr, "%lu-th does not live in the stack anymore\n", i); 
        exit(-1);
    }

	Object *obj = vm->stack[vm->stackSize - i - 1];
    assert(obj->type == OBJ_INT || obj->type == OBJ_PAIR, "get_ith: type?\n");
    push(vm, obj);

}


