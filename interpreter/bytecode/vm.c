/*
 * stack machine 
 *  * stack can contain 32bit integer signed or unsigned
 * program: 65536 instructions max
 *  * It terminates if halt instruction is executed 
 *      or if number of instructions reaches 65536
 *  * Instructions(opcode):
 *      * halt(0x00) : ends program execution
 *      * jump(0x01) : jumps to address (2 bytes, unsigned following opcode)
 *      * jnz(0x02)  : pops an element from stack and if it is not zero jumps
 *                      to its address(2 bytes, unsigned)
 *      * dup(0x03)  : followed by one byte unsigned. duplicates 
 *                      ith element of the stack
 *      * drop(0x04) : 
 * */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>

#include "stack.h"
#include "vm.h"

#define PROGRAMSZ 65536
#ifdef __GNUC__
#define NEXT_INSTRUCTION goto *(void *)(label_tab[program[pc]])
#else
#define NEXT_INSTRUCTION goto next_instruction
#endif

unsigned char program[PROGRAMSZ] = { '\0' };
stack *st;

int main(int argc, char **argv) {
    if(argc < 2) {
        printf("Usage: ./vm hw.b\n");
        exit(0);
    }
#ifdef LOG 
    else if(argc > 3) {
        printf("Usage: ./vm hw.b log\n");
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
        &&push4,
        &&push2,
        &&push1,
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
    };
    

    clock_t start = clock();
    
    st = malloc(sizeof(*st));
    st->cnt = -1;

    FILE *fp = fopen(argv[1], "r");
#ifdef LOG 
    FILE *logfp = fopen(argv[2], "wb");
    stacklogfp = fopen("stacklog.txt", "wb");
#endif
    unsigned char ch;
    unsigned int cnt = 0;

    while(fscanf(fp, "%c", &ch) != EOF) {
        program[cnt] = ch;
        cnt++;
#ifdef LOG 
        fprintf(logfp, "%x\n", ch);
#endif
    }
    unsigned int nr_instructions = cnt;
    
    u_int16_t pc = 0;
    while (1) {
next_instruction: ;
        unsigned char current = program[pc];

        if(pc >= nr_instructions) {
            printf("Program's execution is not allowed in that address %u \n", pc) ;
            return 1;
        }

#ifdef LOG 
        print_stack(st);
#endif
        

        switch(current) {
            case HALT:
halt: ;
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

                signed int elem = pop(st);
                if(!elem) { 
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
                fprintf(logfp, "dup with address %lu and element %x\n", i, st->elems[st->cnt -i]);
#endif
                // unsigned int dupl = 
                get_ith(st, i);
                // push(st, dupl);
                pc++;
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case DROP: {
drop: ;
#ifdef LOG 
                fprintf(logfp, "drop %d\n", pop(st));
                pc++;
                NEXT_INSTRUCTION;
#else
                pop(st);
                pc++;
                NEXT_INSTRUCTION;
#endif
                break;
            }
            case PUSH4: {
push4: ;
                signed int number = (program[pc + 4] << 24) + (program[pc + 3] << 16) + (program[pc + 2] << 8) + program[pc + 1];
#ifdef LOG 
                fprintf(logfp, "push4 with hex context %x and decimal %d.\n", number, number);
#endif
                pc += 4;
                push(st, number);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case PUSH2: {
push2: ;
#ifdef LOG 
                fprintf(logfp, "push2\n");
#endif
                signed int number = (program[pc + 2] << 8) + program[pc + 1];
                pc += 2;
                push(st, number);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case PUSH1: {
#ifdef LOG 
                fprintf(logfp, "push1\n");
#endif
push1: ;
                signed int number = program[pc + 1];
                pc++;
                push(st, number);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case ADD: {
add: ;
#ifdef LOG 
                fprintf(logfp, "add\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, a + b);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case SUB: {
sub: ;
#ifdef LOG 
                fprintf(logfp, "sub:");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
#ifdef LOG 
                fprintf(logfp, "%d in %ld and a = %d and b = %d\n", a-b, st->cnt, a, b);
#endif
                push(st, b - a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case MUL: {
mul: ;
#ifdef LOG 
                fprintf(logfp, "mul\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, a * b);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case DIV: {
div: ;
#ifdef LOG 
                fprintf(logfp, "div\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                if(!a) {
                    fprintf(stderr, "DIV: Dividing by 0 :( \n");
                    return 1;
                }
                push(st, b / a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case MOD: {
mod: ;
#ifdef LOG 
                fprintf(logfp, "mod\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                if(!a) {
                    fprintf(stderr, "MOD: Dividing by 0 :( \n");
                    return 1;
                }
                push(st, b % a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case EQ: { 
eq: ;
#ifdef LOG 
                fprintf(logfp, "eq\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                int result = (a == b);
                push(st, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case NE: { 
ne: ;
#ifdef LOG 
                fprintf(logfp, "ne\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, a != b);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case LT: { 
lt: ;
#ifdef LOG 
                fprintf(logfp, "lt\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, b < a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case GT: { 
gt: ;
#ifdef LOG 
                fprintf(logfp, "gt\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, b > a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case LE: { 
le: ;
#ifdef LOG 
                fprintf(logfp, "le\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, b <= a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case GE: { 
ge: ;
#ifdef LOG 
                fprintf(logfp, "ge\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, b >= a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case NOT: { 
not: ;
#ifdef LOG 
                fprintf(logfp, "not\n");
#endif
                signed int a = pop(st);
                push(st, !a);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case AND: { 
and: ;
#ifdef LOG 
                fprintf(logfp, "and\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, ((a * b != 0) ? 1 : 0));
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case OR: { 
or: ;
#ifdef LOG 
                fprintf(logfp, "or\n");
#endif
                signed int a = pop(st);
                signed int b = pop(st);
                push(st, ((a != 0 || b != 0) ? 1 : 0));
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
                push(st, ch);
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
                ch = (unsigned char)pop(st);
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
                double elapsed = (end - start)/(double)CLOCKS_PER_SEC;
                
                printf("Time elapsed: %lf sec. \n", elapsed);
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

        // pc++;
    }


    fclose(fp);
#ifdef LOG
    fclose(logfp);
    fclose(stacklogfp);
#endif

    return 0;
}
