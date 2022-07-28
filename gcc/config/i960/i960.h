/* Target Definitions for Intel i960 
   Copyright (C) 2022 Joshua Scoggins (with values taken from gcc 3.4.6 version)

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
#include "config/i960/i960-opts.h"
#define TARET_CPU_CPP_BUILTINS() \
    do  \
{ \
    builtin_define("__i960__"); \
    if (TARGET_NUMERICS) { \
        builtin_define("__i960_numerics__"); \
    } \
    if (TARGET_PROTECTED) { \
        builtin_define("__i960_protected__"); \
    } \
    if (TARGET_CORE_EXTENDED) { \
        builtin_define("__i960_core_extended__"); \
    } \
    builtin_assert ("cpu=i960"); \
    builtin_assert ("machine=i960"); \
} \
        while (0)
/* Target machine storage layout.  */

#define DEFAULT_SIGNED_CHAR 0
#define BITS_BIG_ENDIAN 0
// while the i960 supports big endian, I will never support that feature
#define BYTES_BIG_ENDIAN 0 
#define WORDS_BIG_ENDIAN 0
#define BITS_PER_WORD 32
#define UNITS_PER_WORD 4
#define POINTER_SIZE 32
/* No data type wants to be aligned rounder than this.
   Extended precision floats gets 4-word alignment.  */
#define BIGGEST_ALIGNMENT 128
#define FUNCTION_BOUNDARY 128
/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32
/* Boundary (in *bits*) on which stack pointer should be aligned.  */
/* On the i960 it depends on the implementation when performing a call */
#define STACK_BOUNDARY 128
/* Unsure what the difference is */
#define PREFERRED_STACK_BOUNDARY 128
/* The widest mode that BLKmode objects can be promoted to. */
#define MAX_FIXED_MODE_SIZE 64
/*
 * While the Sx and Kx chips support unaligned accesses, I do not want the
 * compiler to generate such things. The i960 has to generate extra word
 * accesses in burst transactions and multiple transactions if they span a
 * 16-byte window. 
 */
#define STRICT_ALIGNMENT 1


// source language data types 
// taken from gcc 3.4.6 black box testing
#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 128
#define WCHAR_TYPE_SIZE 32

// once again, taken through observation
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned long"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Registers 0-15 are the global registers (g0-g15).
   Registers 16-31 are the local registers (r0-r15).
   Register 32-35 are the fp registers (fp0-fp3).
   Register 36 is the condition code register.
   Register 37 is unused.  */

#define FIRST_PSEUDO_REGISTER 38

/* 1 for registers that have pervasive standard uses and are not available
   for the register allocator.  On i960, this includes the frame pointer
   (g15), the previous FP (r0), the stack pointer (r1), the return
   instruction pointer (r2), and the argument pointer (g14).  */
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* On the i960, note that:
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
	fp0..fp3 are never available.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 1, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1}

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */
   
/* The i960 has four kinds of registers, global, local, floating point,
   and condition code.  The cc register is never allocated, so no class
   needs to be defined for it.  */

enum reg_class { 
    NO_REGS, 
    GLOBAL_REGS, 
    LOCAL_REGS, 
    LOCAL_OR_GLOBAL_REGS,
    FP_REGS, 
    ALL_REGS, 
    /// @todo add support for including floating point registers in the general set if numerics
    LIM_REG_CLASSES 
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES
#endif
