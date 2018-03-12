## Stack machine interpreter for bytecode

# program: 65536 instructions max
  * stack can contain 32bit integer signed or unsigned
  * It terminates if halt instruction is executed 
      or if number of instructions reaches 65536
  * Instructions(opcode):
      * halt(0x00) : ends program execution
      * jump(0x01) : jumps to address (2 bytes, unsigned following opcode)
      * jnz(0x02)  : pops an element from stack and if it is not zero jumps
                      to its address(2 bytes, unsigned)
      * dup(0x03)  : followed by one byte unsigned. duplicates 
                      ith element of the stack
      * drop(0x04) : 


