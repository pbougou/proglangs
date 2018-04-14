/* Frama-C Aluminium 20160502 
 * Alt-Ergo 1.01
 */

/*@ predicate samecnt{L}(int *x, integer i, integer j, integer N) =
  @   0 <= i <= j <= N &&
  @   \forall integer k; i <= k < j ==> x[k] == x[i];
  @*/


/*@ requires 1 <= N <= 1000000;
  @ requires \valid(x + (0..N-1));
  @ ensures \exists integer k; samecnt(x, k, k+\result, N);
  @ ensures \forall integer k,m; samecnt(x, k, k+m, N) ==> m <= \result;
  @*/
int countSameConsecutive(int N, int *x) {
//@ assert samecnt(x, 0, 0, 0);
int best = 0, i = 0;

/*@ loop invariant 0 <= i <= N;
  @ loop invariant 0 < i < N ==> x[i] != x[i-1];
  @ loop invariant \exists integer k; samecnt(x, k, k+best, i);
  @ loop invariant \forall integer k,m; samecnt(x, k, k+m, i) ==> m <= best;
  @ loop assigns i, best;
  @ loop variant N-i;
  @*/
	while(i < N) {
	  int j = i+1;
		
		/*@ loop invariant 0 <= i < j <= N;
		  @ loop invariant samecnt(x, i, j, j);
          @ loop invariant \exists integer k; samecnt(x, k, k+best, i);
		  @ loop invariant \forall integer k,m; samecnt(x, k, k+m, i) ==> m <= best;
          @ loop assigns j;
		  @ loop variant N-j;
		  @*/
		while(j < N && x[j] == x[i]) 
			++j;
	
	  if(j-i > best) 
		best = j-i;

	  i = j;
	}
	return best;
}

/*

panagiotis@ubuntu:~$ frama-c -wp -wp-prover alt-ergo -wp-rte tested.c -then -report
[kernel] Parsing .opam/4.02.1/share/frama-c/libc/__fc_builtin_for_normalization.i (no preprocessing)
[kernel] Parsing tested.c (with preprocessing)
[rte] annotating function countSameConsecutive
[wp] 35 goals scheduled
[wp] Proved goals:   35 / 35
     Qed:            14  (4ms-9ms-48ms)
     Alt-Ergo:       21  (24ms-260ms-2.3s) (277)
[report] Computing properties status...

--------------------------------------------------------------------------------
--- Properties of Function 'countSameConsecutive'
--------------------------------------------------------------------------------

[  Valid  ] Post-condition (file tested.c, line 13)
            by Wp.typed.
[  Valid  ] Post-condition (file tested.c, line 14)
            by Wp.typed.
[  Valid  ] Loop assigns (file tested.c, line 24)
            by Wp.typed.
[  Valid  ] Loop assigns (file tested.c, line 34)
            by Wp.typed.
[  Valid  ] Loop variant at loop (file tested.c, line 27)
            by Wp.typed.
[  Valid  ] Loop variant at loop (file tested.c, line 37)
            by Wp.typed.
[  Valid  ] Assertion (file tested.c, line 17)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 20)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 21)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 22)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 23)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 30)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 31)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 32)
            by Wp.typed.
[  Valid  ] Invariant (file tested.c, line 33)
            by Wp.typed.
[  Valid  ] Assertion 'rte,signed_overflow' (file tested.c, line 28)
            by Wp.typed.
[  Valid  ] Assertion 'rte,mem_access' (file tested.c, line 37)
            by Wp.typed.
[  Valid  ] Assertion 'rte,mem_access' (file tested.c, line 37)
            by Wp.typed.
[  Valid  ] Assertion 'rte,signed_overflow' (file tested.c, line 38)
            by Wp.typed.
[  Valid  ] Assertion 'rte,signed_overflow' (file tested.c, line 40)
            by Wp.typed.
[  Valid  ] Assertion 'rte,signed_overflow' (file tested.c, line 40)
            by Wp.typed.
[  Valid  ] Assertion 'rte,signed_overflow' (file tested.c, line 41)
            by Wp.typed.
[  Valid  ] Assertion 'rte,signed_overflow' (file tested.c, line 41)
            by Wp.typed.
[  Valid  ] Default behavior
            by Frama-C kernel.

--------------------------------------------------------------------------------
--- Status Report Summary
--------------------------------------------------------------------------------
    24 Completely validated
    24 Total
--------------------------------------------------------------------------------
*/
