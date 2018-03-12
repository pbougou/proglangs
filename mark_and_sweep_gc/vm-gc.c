#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>

#include "stack-gc.h"

#include "vm.h"

#define PROGRAMSZ 65536

#if defined(__GNUC__) && !defined(DBG)
    #define NEXT_INSTRUCTION goto *(void *)(label_tab[program[pc]])
#elif !defined(__GNUC__) || defined(DBG)
    #define NEXT_INSTRUCTION goto next_instruction
#endif

#ifdef LOG 
    FILE *logfp;
#endif

unsigned char program[PROGRAMSZ] = { '\0' };
VM* vm;




int main(int argc, char **argv) {
    if(argc < 2) {
        printf("Usage: ./%s pp.b\n", argv[0]);
        exit(0);
    }
#ifdef LOG 
    else if(argc != 3) {
        printf("Usage: ./%s pp.b log\n", argv[0]);
        exit(0);
    }
#endif

#define NR_OPCODES 0x2e
    static void *label_tab[NR_OPCODES] = {
        &&halt,
        &&jump,
        &&jnz,
        &&dup,
        &&drop,
        &&pushInt4,
        &&pushInt2,
        &&pushInt1,
        &&add,
        &&sub,
        &&mul,
        &&div,
        &&mod,
        &&eq,
        &&ne,
        &&lt,
        &&gt,
        &&le,
        &&ge,
        &&not,
        &&and,
        &&or,
        &&input,
        &&output,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&dummy,
        &&clock,
        &&cons,
        &&hd,
        &&tl
    };
    
    // struct timeval start, end;
    vm = newVM();
    
    FILE *fp = fopen(argv[1], "r");

#ifdef LOG 
    FILE *logfp = fopen(argv[2], "wb");
#endif


    unsigned char ch;
    unsigned int cnt = 0;

    while(fscanf(fp, "%c", &ch) != EOF) {
        program[cnt] = ch;
        cnt++;
    }
    unsigned int nr_instructions = cnt;
    
    // gettimeofday(&start, 0);
    clock_t start = clock();
    u_int16_t pc = 0;
    while (1) {
next_instruction: ;
        unsigned char current = program[pc];

        if(pc >= nr_instructions) {
            printf("Program's execution is not allowed in that address %u \n", pc) ;
            return 1;
        }

        
        switch(current) {

            
            case HALT:
halt: ;
                freeVM(vm);
#ifdef LOG 
                fprintf(logfp, "halt\n");
#endif

                return 0;
            case JUMP: {
jump: ;

#ifdef LOG 
                fprintf(logfp, "jump\n");
#endif

                unsigned int address = (program[pc + 2] << 8) + program[pc + 1];
                pc += 2;
                pc = address;
                
                NEXT_INSTRUCTION;
                continue;
            }
            case JNZ: {
jnz: ;

#ifdef LOG 
                fprintf(logfp, "jnz\n");
#endif

                Object* obj = pop(vm);

                int number = obj->value;
               

                if(!number) { 
                    
                    pc += 2;
                    pc++;
                    NEXT_INSTRUCTION;
                    break;
                }
                else {
                    unsigned int address = (program[pc + 2] << 8) + program[pc + 1];

#ifdef LOG 
                    fprintf(logfp, "address = %u \n", address);
#endif

                    pc += 2;
                    pc = address;
                    NEXT_INSTRUCTION;
                    continue;
                }
            }
            case DUP: {
dup: ;
                long int i = program[pc + 1];

#ifdef LOG 
                fprintf(logfp, "dup with address %lu and st->cnt = %d\n", i, vm->stackSize);
#endif
                get_ith(vm, i);
                pc++;
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case DROP: {
drop: ;

#ifdef LOG 
                fprintf(logfp, "drop \n");
#endif

                pop(vm);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case PUSH4: {
pushInt4: ;
                signed int number = (program[pc + 4] << 24) + (program[pc + 3] << 16) + (program[pc + 2] << 8) + program[pc + 1];

#ifdef LOG 
                fprintf(logfp, "pushInt4 with hex context %x and decimal %d.\n", number, number);
#endif

                pc += 4;
                pushInt(vm, number);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case PUSH2: {
pushInt2: ;

#ifdef LOG 
                fprintf(logfp, "pushInt2\n");
#endif

                signed int number = (program[pc + 2] << 8) + program[pc + 1];
                pc += 2;
                pushInt(vm, number);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case PUSH1: {

#ifdef LOG 
                fprintf(logfp, "pushInt1\n");
#endif

pushInt1: ;
                signed int number = program[pc + 1];
                pc++;
                pushInt(vm, number);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case ADD: {
add: ;

#ifdef LOG 
                fprintf(logfp, "add\n");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "ADD: popped pair value");

                pushInt(vm, a->value + b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case SUB: {
sub: ;

#ifdef LOG 
                fprintf(logfp, "sub:");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "SUB: popped pair value");

#ifdef LOG 
                fprintf(logfp, "%d in %d and b = %d and a = %d\n", b->value - a->value, vm->stackSize, b->value, a->value);
#endif

                pushInt(vm, b->value - a->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case MUL: {
mul: ;

#ifdef LOG 
                fprintf(logfp, "mul\n");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "MUL: popped pair value");
                pushInt(vm, a->value * b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case DIV: {
div: ;

#ifdef LOG 
                fprintf(logfp, "div\n");
#endif

                Object* b = pop(vm);
                Object* a = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "DIV: popped pair value");
                if(!b->value) {
                    fprintf(stderr, "DIV: Dividing by 0 :( \n");
                    return 1;
                }
                pushInt(vm, a->value / b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case MOD: {
mod: ;

#ifdef LOG 
                fprintf(logfp, "mod\n");
#endif

                Object* b = pop(vm);
                Object* a = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "MOD: popped pair value");
                if(!b->value) {
                    fprintf(stderr, "MOD: Dividing by 0 :( \n");
                    return 1;
                }
                pushInt(vm, a->value % b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case EQ: { 
eq: ;

#ifdef LOG 
                fprintf(logfp, "eq\n");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "EQ: popped pair value");
                
                pushInt(vm, a->value == b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case NE: { 
ne: ;

#ifdef LOG 
                fprintf(logfp, "ne\n");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                
                 assert(a->type == OBJ_INT && b->type == OBJ_INT, "NE: popped pair value\n");
                
                int32_t aval = (a->type == OBJ_INT) ? a->value : 0;
                int32_t bval = (b->type == OBJ_INT) ? b->value : 1;

                pushInt(vm, aval != bval);
                 
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case LT: { 
lt: ;

#ifdef LOG 
                fprintf(logfp, "lt\n");
#endif

                Object* b = pop(vm);
                Object* a = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "LT: popped pair value");
                
                pushInt(vm, a->value < b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case GT: { 
gt: ;

#ifdef LOG 
                fprintf(logfp, "gt\n");
#endif

                Object* b = pop(vm);
                Object* a = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "GT: popped pair value");
                
                pushInt(vm, a->value > b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case LE: { 
le: ;

#ifdef LOG 
                fprintf(logfp, "le\n");
#endif
                Object* b = pop(vm);
                Object* a = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "LE: popped pair value");
                
                pushInt(vm, a->value <= b->value);

                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case GE: { 
ge: ;

#ifdef LOG 
                fprintf(logfp, "ge\n");
#endif
                Object* b = pop(vm);
                Object* a = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "GE: popped pair value");
                
                pushInt(vm, a->value >= b->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case NOT: { 
not: ;

#ifdef LOG 
                fprintf(logfp, "not\n");
#endif

                Object* a = pop(vm);
                assert(a->type == OBJ_INT, "NOT: popped pair value");
                
                pushInt(vm, !a->value);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case AND: { 
and: ;

#ifdef LOG 
                fprintf(logfp, "and\n");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "AND: popped pair value");
                
                pushInt(vm, (a->value && b->value) ? 1 : 0);
               
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case OR: { 
or: ;

#ifdef LOG 
                fprintf(logfp, "or\n");
#endif

                Object* a = pop(vm);
                Object* b = pop(vm);
                assert(a->type == OBJ_INT && b->type == OBJ_INT, "SUB: popped pair value");
                pushInt(vm, (a->value || b->value) ? 1 : 0);
                
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case INPUT: {
input: ;

#ifdef LOG 
                fprintf(logfp, "input\n");
#endif

                unsigned char ch;
                scanf("%c", &ch);
                pushInt(vm, ch);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case OUTPUT: {
output: ;

#ifdef LOG 
                fprintf(logfp, "output\n");
#endif
                
                unsigned char ch;
                ch = (unsigned char)pop(vm)->value;
                printf("%c", ch);
                
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case CLOCK: {
clock: ;

#ifdef LOG 
                fprintf(logfp, "clock\n");
#endif
                clock_t end = clock();

                // gettimeofday(&end, 0);
                // long elapsed = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
                double elapsed = (end - start)/(double)CLOCKS_PER_SEC;
                printf("Time elapsed: %lf sec. \n", elapsed);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }

            /* *****************************************************************
             * Instructions for garbage collection
             *  * cons: pop 2 elements from stack and create a 
             *      boxed cell in the heap
             *  * hd: pop an elment from the stack. If it is address
             *      free the heap element and push the head value in the stack
             *  * tl: same as hd but with tail element
             *  ****************************************************************
             * */
            case CONS: {
cons: ;         
                pushPair(vm); 

#ifdef LOG 
                fprintf(logfp, "cons\n");
#endif

                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case HD: {
hd: ;        
                Object *obj = pop(vm);
                assert(obj->type == OBJ_PAIR, "HD: popped int value\n");
                push(vm, obj->head); 

#ifdef LOG 
                fprintf(logfp, "hd\n");
#endif

                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case TL: {
tl: ;
                Object *obj = pop(vm);
                assert(obj->type == OBJ_PAIR, "HD: popped int value\n");
                push(vm, obj->tail); 

#ifdef LOG 
                fprintf(logfp, "tl\n");
#endif

                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            default: {
dummy: ;
                printf("This command's (%x) behavior is undefined. Exiting...\n", program[pc]);
                return 1;
            } 
        }

    }


    fclose(fp);
#ifdef LOG
    fclose(logfp);
#endif

    return 0;
}
