#define _FP_W_TYPE_SIZE 32
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

#define _FP_MUL_MEAT_S(R,X,Y) _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y) _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_Q(R,X,Y) _FP_MUL_MEAT_4_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)
#define _FP_DIV_MEAT_S(R,X,Y) _FP_DIV_MEAT_1_loop(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y) _FP_DIV_MEAT_2_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y) _FP_DIV_MEAT_4_udiv(Q,R,X,Y)
/* according to the SA/SB handbook floating point tininess happens _after_ rounding */
#define _FP_TININESS_AFTER_ROUNDING 1
/* x87 FPU comes from the i960 FPU, so it is a good reference baseline in handling behavior (i960 doesn't use a FP stack though).
 * I also can't find anything relating to negation on quiet NaNs in the Sx handbook
 */
#define _FP_QNANNEGATEDP 0
#define _FP_KEEPNANFRACP 1

#define _FP_NANSIGN_S 1
#define _FP_NANSIGN_D 1
#define _FP_NANSIGN_E 1
#define _FP_NANSIGN_Q 1
/* taken from i386's 32-bit implementation... */
#define _FP_NANFRAC_S		_FP_QNANBIT_S
#define _FP_NANFRAC_D		_FP_QNANBIT_D, -1
#define _FP_NANFRAC_E		_FP_QNANBIT_E, -1, -1, -1
#define _FP_NANFRAC_Q		_FP_QNANBIT_Q, -1, -1, -1
/* pulled from ia64 implementation... seems it is the same design as the other cpus (i386 and i960) */
/* Here is something Intel misdesigned: the specs don't define
   the case where we have two NaNs with same mantissas, but
   different sign. Different operations pick up different NaNs.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if (_FP_FRAC_GT_##wc(X, Y)					\
	|| (_FP_FRAC_EQ_##wc(X,Y) && (OP == '+' || OP == '*')))	\
      {								\
	R##_s = X##_s;						\
	_FP_FRAC_COPY_##wc(R,X);				\
      }								\
    else							\
      {								\
	R##_s = Y##_s;						\
	_FP_FRAC_COPY_##wc(R,Y);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN 4321

#define __BYTE_ORDER __LITTLE_ENDIAN

# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
