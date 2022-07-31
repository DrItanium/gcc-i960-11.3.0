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
#define TARGET_CPU_CPP_BUILTINS() \
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

/* number of bytes we can move from memory to memory in one instruction */
/* movq */
#define MOVE_MAX 16

/* Documentation in the old port states that it is set to one after reports of
 * slowness. This does not surprise me since the i960Kx has a 32-bit data bus
 * and the i960Sx has a 16-bit data bus. These two chips use a multiplexed bus
 * design so accessing individual bytes from RAM is _SLOW_.*/
#define SLOW_BYTE_ACCESS 1
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

/* renumber registers for dbx and gdb, 
 * according to the old code g0..g15 are registers 16..31
 * In the "960" encoding. Save this for now
 * */
#define DBX_REGISTER_NUMBER(X) \
    (((X) < 16) ? ((X) + 16) \
     : (((X) > 31) ? (X) : ((X) - 16)))
#define REGISTER_NAMES { \
    "g0", "g1", "g2", "g3", "g4", "g5", "g6", "g7", \
    "g8", "g9", "g10", "g11", "g12", "g13", "g14", "fp", \
    "pfp", "sp", "rip", "r3", "r4", "r5", "r6", "r7", \
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", \
    "fp0", "fp1", "fp2", "fp3", "cc", "fake" }
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
	r0 is the previous FP,(pfp)
	r1 is the stack pointer, (sp)
	r2 is the return instruction pointer, (rip)
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

/* The order in which to allocate registers.  */

#define	REG_ALLOC_ORDER	\
{  4, 5, 6, 7, 0, 1, 2, 3, 13,	 /* g4, g5, g6, g7, g0, g1, g2, g3, g13  */ \
  20, 21, 22, 23, 24, 25, 26, 27,/* r4, r5, r6, r7, r8, r9, r10, r11  */    \
  28, 29, 30, 31, 19, 8, 9, 10,	 /* r12, r13, r14, r15, r3, g8, g9, g10  */ \
  11, 12,			 /* g11, g12  */			    \
  32, 33, 34, 35,		 /* fp0, fp1, fp2, fp3  */		    \
  /* We can't actually allocate these.  */				    \
  16, 17, 18, 14, 15, 36, 37}	 /* r0, r1, r2, g14, g15, cc  */

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
    /// @todo support for special function registers? Don't really think so but an interesting thought
    LIM_REG_CLASSES 
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES
#define REG_CLASS_NAMES { \
    "NO_REGS",  \
    "GLOBAL_REGS", \
    "LOCAL_REGS", \
    "LOCAL_OR_GLOBAL_REGS", \
    "FP_REGS", \
    "ALL_REGS" \
}
/*
 * Bit patterns which denote which of the n ( in this case 37 ) registers
 * apply to each register class. For the i960 we need two 32-bit words to store
 * everything. In the older implementation the FP registers were given
 * everything in the upper half... thats not right. I've fixed this
 */
#define REG_CLASS_CONTENTS { \
    { 0x00000000, 0x00000000 }, \
    { 0x0000FFFF, 0x00000000 }, \
    { 0xFFFF0000, 0x00000000 }, \
    { 0xFFFFFFFF, 0x00000000 }, \
    { 0x00000000, 0x0000000F }, \
    { 0xFFFFFFFF, 0x0000003F }, \
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)	\
  ((REGNO) < 16 ? GLOBAL_REGS	\
   : (REGNO) < 32 ? LOCAL_REGS	\
   : (REGNO) < 36 ? FP_REGS	\
   : NO_REGS)
#define GENERAL_REGS ((TARGET_NUMERICS) ? ALL_REGS : LOCAL_OR_GLOBAL_REGS)
/* In 3.4.6, the cumulative args was a structure which kept track of the number
 * of stack and register parameters seen so far.
 * 
 */ 
struct i960CumulativeArguments {
    int NumberOfRegisterParameters;
    int NumberOfStackParameters;
};
#define CUMULATIVE_ARGS i960CumulativeArguments

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
    do { \
        (CUM).NumberOfRegisterParameters = 0; \
        (CUM).NumberOfStackParameters = 0; \
    } while (0)

/*
 * Length in units of the trampoline for entering a nested function
 */
#define TRAMPOLINE_SIZE 20

/* Register to use for pushing function arguments */
#define STACK_POINTER_REGNUM 17

/*
 * Specify the machine mode that pointer have. After RTL generation, the
 * compiler makes no further distinction between pointers and any other objects
 * of this machine mode.
 *
 * Taken from 3.4.6
 */
#define Pmode SImode

/*
 * Maximum number of registers that can appear in a valid memory address.
 * As I understand it, this is a reference to MEM type instructions and how
 * MEMB format operations can use up to two registers to generate a 32-bit
 * memory address (abase and index). 
 */
#define MAX_REGS_PER_ADDRESS 2

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 15

/* 
 * Base register for access to arguments of the function  (g14)
 */
#define ARG_POINTER_REGNUM 14

/* According to the calling convention, g0-g11 are function arguments */
#define FUNCTION_ARG_REGNO_P(N) ((N) < 12)

/* 
 * Register elimination definitions 
 *
 * An array of structures. Each structure initializes one pair of eliminable
 * registers. The "from" register number is given first, followed by "to."
 * Eliminations of the same "from" register are listed in order of preference.
 *
 * In this case, it is the frame pointer being replaced with references to the
 * stack pointer.
 */
#define ELIMINABLE_REGS {{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }}

/*
 * A function address in a call instruction is a byte address so give the MEM
 * rtx a byte's mode (from 3.4.6 impl)
 */
#define FUNCTION_MODE SImode

#define BASE_REG_CLASS LOCAL_OR_GLOBAL_REGS

/*
 * According to the old code, this is the same as the BASE_REG_CLASS for future
 * purposes. But! If you look in the i960 manuals, you'll see that you can use
 * any register as an "index" register. You can use any register for abase as
 * well. Thus I am going to make them the same.
 */
#define INDEX_REG_CLASS LOCAL_OR_GLOBAL_REGS

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register allocation. 

   Taken from the 3.4.6 implementation with extra and accurate documentation.
   */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_FP_P(REGNO) \
  ((REGNO) < 36 || (unsigned) reg_renumber[REGNO] < 36)

/* 
 * offset of the first parameter from the argument pointer register value
 */
#define FIRST_PARM_OFFSET(FNDECL) (0)

/*
 * Make sure we align to 2**LOG bytes. The old code just used .align but since
 * I'm only supporting gas, we should just use .p2align
 */
#define ASM_OUTPUT_ALIGN(FILE, LOG) \
    do { \
        if ((LOG) != 0) { \
            fprintf(FILE, "\t.p2align %d\n", (LOG)); \
        } \
    } while (0)

/*
 * Specify the machine mode that this machine uses for the index in the
 * tablejump instruction.
 */
#define CASE_VECTOR_MODE SImode

/*
 * Output to assembler file text saying following lines may contain character
 * constants, extra white space, comments, etc.
 *
 * We are not going to be using this
 */
#define ASM_APP_ON ""

/*
 * Output to assembler file text saying following lines no longer contain
 * unusual constructs.
 */

#define ASM_APP_OFF ""

/*
 * Define the offset between two registers, one to be eliminated, and the other
 * its replacement, at the start of a routine.
 *
 * Since the stack grows upweard on the i960, this must be a negative number.
 * This includes the 64 byte hardware register save area and the size of the
 * frame
 */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
    do { \
        (OFFSET) = i960_initial_elimination_offset((FROM), (TO)); \
    } while (0)

/* Output assembler code to FILE to increment profiler lable # LABELNO for
 * profiling a function entry. According to or1k impl, this is handled by
 * PROFILE_HOOK but it is still required. Yet avr uses this macro */

#define FUNCTION_PROFILER(STREAM, LABELNO) i960_output_function_profiler(STREAM, LABELNO)

#define GLOBAL_ASM_OP "\t.global\t"
#endif // end file
