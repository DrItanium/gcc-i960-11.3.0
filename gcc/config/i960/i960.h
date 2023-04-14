/* Target Definitions for Intel i960.
   Copyright (C) 2023 Joshua Scoggins

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_I960_H
#define GCC_I960_H


/* 
 * Register map:
 * g0-g15, pfp, sp, rip, r4-r15, fp0-3, cc, ac, pc, tc, sf0-sf31
 *
 * Registers with a fixed usage:
 * g14, g15, pfp, sp, rip, cc, ac, pc, tc, sf0-sf31
 *
 * The special function registers are listed there for consistency and not
 * expected to be used at all
 */

#define FIXED_REGISTERS \
 {0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1}

/* the first psuedo register is #72 in this new port */
#define FIRST_PSEUDO_REGISTER 72

/* Taken from previous impl since it describes the calling convention with
 * some additions:
	g0..g3 are used for return values,
	g0..g7 may always be used for parameters,
	g8..g11 may be used for parameters, but are preserved if they aren't,
	g12 is the static chain if needed, otherwise is preserved
	g13 is the struct return ptr if used, or temp, but may be trashed,
	g14 is the leaf return ptr or the arg block ptr otherwise zero,
		must be reset to zero before returning if it was used,
	g15 is the frame pointer,
	r0 is the previous FP,
	r1 is the stack pointer,
	r2 is the return instruction pointer,
	r3-r15 are always available,
	r3 is clobbered by calls in functions that use the arg pointer
	r4-r11 may be clobbered by the mcount call when profiling
	r4-r15 if otherwise unused may be used for preserving global registers
	fp0-fp3 are never available.  
    cc, ac, pc, tc are never available.
    sf0-sf31 are never available.
    */

#define CALL_USED_REGISTERS \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1,   \
  1, 1, 1, 1, 1, 1, 1, 1}

/*
 * Register classes (taken from previous implementation but reformatted)
 */
enum reg_class 
{
    NO_REGS,
    GLOBAL_REGS,
    LOCAL_REGS,
    LOCAL_OR_GLOBAL_REGS,
    FP_REGS,
    SPECIAL_FUNCTION_REGISTERS,
    ALL_REGS,
    LIM_REG_CLASSES,
    /* No need to reference cc, ac, pc, tc, or SFRs for lack of allocation */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES
#define REG_CLASS_NAMES { \
    "NO_REGS", \
    "GLOBAL_REGS", \
    "LOCAL_REGS", \
    "LOCAL_OR_GLOBAL_REGS", \
    "FP_REGS", \
    "ALL_REGS" }

/*
 * Pull in the allocation order from the old implementation as a base
 */
#define	REG_ALLOC_ORDER	\
{  4, 5, 6, 7, 0, 1, 2, 3, 13,	 /* g4, g5, g6, g7, g0, g1, g2, g3, g13  */ \
  20, 21, 22, 23, 24, 25, 26, 27,/* r4, r5, r6, r7, r8, r9, r10, r11  */    \
  28, 29, 30, 31, 19, 8, 9, 10,	 /* r12, r13, r14, r15, r3, g8, g9, g10  */ \
  11, 12,			 /* g11, g12  */			    \
  32, 33, 34, 35,		 /* fp0, fp1, fp2, fp3  */		    \
  /* We can't actually allocate these.  */				    \
  16, 17, 18, 14, 15, 36, 37, 38, 39,	 /* r0 (pfp), r1 (sp), r2 (rip), g14 (lr), g15, cc, ac, pc, tc  */ \
    /* 
     * special function registers are not available on all targets so make sure
     * we can't allocate them at all!
     */ \
  40, 41, 42, 43, 44, 45, 46, 47, \
  48, 49, 50, 51, 52, 53, 54, 55, \
  56, 57, 58, 59, 60, 61, 62, 63, \
  64, 65, 66, 67, 68, 69, 70, 71 }


/**
 * @brief Describes the argument counts for register and stack based
 * operations.
 */
typedef struct i960_args 
{
    /**
     * @brief the number of parmeters passed via registers
     */
    int nreg_params;
    /**
     * @brief the number of parameters passed via registers
     */
    int nstack_params;
} CUMULATIVE_ARGS;

/* The i960 can move up to 16-bytes in a single instruction (ldq, stq)
 * across all implementations. There is the movqstr and other long form
 * instructions which doesn't really count here since you must have access to
 * protected mode architecture extensions.
 */
#define MOVE_MAX 16

/*
 * All versions of the i960 support little endian byte accesses. So big endian
 * is not a thing in general. You can configure the PMCON registers in later
 * chips to have the bus unit operate in big endian mode but that is not visible from the
 * processor itself (as I understand it). Even with this being said, we only support little endian 
 */
#define BYTES_BIG_ENDIAN 0

/* A word is 32-bits or 4-bytes */
#define UNITS_PER_WORD 4

/*
 * To maximize compatibility with _all_ i960 implementations, we want to
 * disallow unaligned accesses. On chips like the Kx and Sx, one can perform an
 * unaligned load/store but it will slow things down. On chips like the Hx
 * series, a fault will be generated! According to the i960 manuals, the choice
 * is dependent on the implementation. In this case, it does not make sense to
 * ever support unaligned memory accesses
 */
#define STRICT_ALIGNMENT 1

/* Allocation boundary (in *bits*) for the code of a function.  */
/* taken from old impl */
#define FUNCTION_BOUNDARY 128

/* Length in units of the trampoline for entering a nested function.  */
/* taken from old impl */
#define TRAMPOLINE_SIZE 20

#endif
