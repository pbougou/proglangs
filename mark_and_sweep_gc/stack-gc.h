#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>

#define STACK_MAX 256
typedef enum {
  OBJ_INT,
  OBJ_PAIR
} ObjectType;

typedef struct sObject {
  ObjectType type;
  unsigned char marked;

  /* The next object in the linked list of heap allocated objects. */
  struct sObject* next;

  union {
    /* OBJ_INT */
    int32_t value;

    /* OBJ_PAIR */
    struct {
      struct sObject* tail;
      struct sObject* head;
    };  
  };  
} Object;

typedef struct {
  Object* stack[STACK_MAX];
  int stackSize;

  /* The first object in the linked list of all objects on the heap. */
  Object* firstObject;

  /* The total number of currently allocated objects. */
  int numObjects;

  /* The number of objects required to trigger a GC. */
  int maxObjects;
} VM;


void assert(int condition, const char* message);
VM* newVM();
void push(VM* vm, Object* value);
Object* pop(VM* vm);
void mark(Object* object);
void markAll(VM* vm);
void sweep(VM* vm);
void gc(VM* vm);
Object* newObject(VM* vm, ObjectType type);
void pushInt(VM* vm, int intValue);
Object* pushPair(VM* vm);
void objectPrint(Object* object);
void freeVM(VM *vm);
void get_ith(VM* vm, long int i);


