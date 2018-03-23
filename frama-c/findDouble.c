/* Tested with:
 *  * Frama-C Aluminium
 *  * Alt-Ergo 1.01
 *  * Why3 platform, version 0.87.3
 * */
#include <stdbool.h>

#define MAXV 1000000

/*@ requires 1 <= N <= 1000000;
  @ requires \valid(a + (0..N-1));
  @ requires \forall integer j; 0 <= j < 1000000 ==> 1 <= a[j] <= N;
  @ ensures 
  @     \forall integer i, j; 0 <= i < 1000000 && 0 <= j < 1000000 && a[i] != a[j] ==> \result == 0 || 
  @     \exists integer i, j; 0 <= i < 1000000 && 0 <= j < 1000000 && a[i] == a[j] ==> \result == a[i];
  @*/
int findDouble(int N, int a[]) {
    bool f[MAXV];
 
    /*@ loop invariant 1 <= i <= (MAXV+1);
      @ loop invariant \forall integer j; 1 <= j < i ==> f[j-1] == false;
      @ loop assigns i, f[0..(MAXV-1)];
      @ loop variant MAXV - i + 1;
      @*/
    for(int i = 1; i <= MAXV; ++i)
        f[i - 1] = false;
   
    /*@ loop invariant 0 <= i <= N;
      @ loop invariant \forall integer j; 0 <= j < i ==> f[a[j] - 1] == true;
      @ loop assigns i, f[0..N-1];
      @ loop variant N - i;
      @*/
    for(int i = 0; i < N; ++i)
        if (f[a[i] - 1]) return a[i]; else f[a[i]  - 1] = true;
    
    return 0;
}
