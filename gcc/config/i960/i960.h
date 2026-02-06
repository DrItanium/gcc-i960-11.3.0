/* Definitions of target machine for GNU compiler, for Intel 80960
   Copyright (C) 1992, 1993, 1995, 1996, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Steven McGeady, Intel Corp.
   Additional Work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_I960_H
#define GCC_I960_H
#include "config/i960/i960-opts.h"
/* Note that some other tm.h files may include this one and then override
   many of the definitions that relate to assembler syntax.  */
/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__i960__");		\
    if (TARGET_NUMERICS) builtin_define("__i960_numerics__"); \
    if (TARGET_PROTECTED) builtin_define("__i960_protected__"); \
	builtin_assert ("cpu=i960");		\
	builtin_assert ("machine=i960");	\
    }						\
  while (0)

#define MULTILIB_DEFAULTS { "mnumerics" }

/* Name to predefine in the preprocessor for processor variations.
   -mic* options make characters signed by default.  */
#undef CPP_SPEC
#define	CPP_SPEC "%{msoft-float:-D_SOFT_FLOAT}\
	%{mka:-D__i960KA__ -D__i960Kx__ -mcomplex-addr}\
	%{mkb:-D__i960KB__ -D__i960Kx__ -mcomplex-addr -mnumerics}\
	%{msa:-D__i960SA__ -D__i960Sx__ -mcomplex-addr}\
	%{msb:-D__i960SB__ -D__i960Sx__ -mcomplex-addr -mnumerics}\
	%{mmc:-D__i960MC__ -D__i960Mx__ -mcomplex-addr -mnumerics -mprotected}\
	%{mca:-D__i960CA__ -D__i960Cx__ -mcomplex-addr -mbranch-predict -mcode-align}\
	%{mcc:-D__i960CC__ -D__i960Cx__ -mcomplex-addr -mbranch-predict -mcode-align}\
	%{mcf:-D__i960CF__ -D__i960Cx__ -mcomplex-addr -mbranch-predict -mcode-align}\
    %{mja:-D__i960JA__ -D__i960Jx__ -mcomplex-addr -mcode-align}\
    %{mjd:-D__i960JD__ -D__i960Jx__ -mcomplex-addr -mcode-align}\
    %{mjf:-D__i960JF__ -D__i960Jx__ -mcomplex-addr -mcode-align}\
	%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:\
		%{!mcc:%{!mcf:-D__i960_KA -D__i960KA__}}}}}}}}"

/* Specs for the compiler, to handle processor variations. 
   If the user gives an explicit -gstabs or -gcoff option, then do not
   try to add an implicit one, as this will fail. 
   -mic* options make characters signed by default.  */
#undef CC1_SPEC
#define CC1_SPEC "%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:%{!mcc:%{!mcf:%{!mja:%{!mjd:%{!mjf:%{!mrp:-mka}}}}}}}}}}}}"

/* Specs for the assembler, to handle processor variations.
   For compatibility with Intel's gnu960 tool chain, pass -A options to
   the assembler.  */
#undef ASM_SPEC
#define ASM_SPEC \
	"%{mka:-AKA}%{mkb:-AKB}%{msa:-ASA}%{msb:-ASB}\
	%{mmc:-AMC}%{mca:-ACA}%{mcc:-ACC}%{mcf:-ACF}\
        %{mja:-AJX}%{mjd:-AJX}%{mjf:-AJX}%{mrp:-AJX}\
	%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:%{!mcc:%{!mcf:%{!mja:%{!mjd:%{!mjf:%{!mrp:-AKB}}}}}}}}}}}}"

/* Specs for the linker, to handle processor variations.
   For compatibility with Intel's gnu960 tool chain, pass -F and -A options
   to the linker.  */
#undef LINK_SPEC
#define LINK_SPEC \
	"%{mka:-AKA}%{mkb:-AKB}%{msa:-ASA}%{msb:-ASB}\
	%{mmc:-AMC}%{mca:-ACA}%{mcc:-ACC}%{mcf:-ACF}\
        %{mja:-AJX}%{mjd:-AJX}%{mjf:-AJX}%{mrp:-AJX}"

/* Generate DBX debugging information.  */
#define DBX_DEBUGGING_INFO 1

/* Generate DBX_DEBUGGING_INFO by default.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Redefine this to print in hex.  No value adjustment is necessary
   anymore.  */
#define PUT_SDB_TYPE(A) \
  fprintf (asm_out_file, "\t.type\t0x%x;", A)

/* Handle pragmas for compatibility with Intel's compilers.  */

extern int i960_maxbitalignment;
extern int i960_last_maxbitalignment;

#define REGISTER_TARGET_PRAGMAS() do {			\
  c_register_pragma (0, "align", i960_pr_align);	\
  c_register_pragma (0, "noalign", i960_pr_noalign);	\
} while (0)


/* Don't enable anything by default.  The user is expected to supply a -mARCH
   option.  If none is given, then -mka is added by CC1_SPEC.  */
#define TARGET_DEFAULT 0

/* Target machine storage layout.  */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.
   The i960 case be either big endian or little endian.  We only support
   little endian, which is the most common.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 0

/* Bitfields cannot cross word boundaries.  */
#define BITFIELD_NBYTES_LIMITED 1

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a long double.  */
#define LONG_DOUBLE_TYPE_SIZE 128
/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 128

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 128

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* This makes zero-length anonymous fields lay the next field
   at a word boundary.  It also makes the whole struct have
   at least word alignment if there are any bitfields at all.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* No data type wants to be aligned rounder than this.
   Extended precision floats gets 4-word alignment.  */
#define BIGGEST_ALIGNMENT 128

/* Define this if move instructions will actually fail to work
   when given unaligned data.
   80960 will work even with unaligned data, but it is slow.  */
/// @todo should we support unaligned data? I'm thinking no!
#define STRICT_ALIGNMENT TARGET_STRICT_ALIGN

/* Macros to determine size of aggregates (structures and unions
   in C).  Normally, these may be defined to simply return the maximum
   alignment and simple rounded-up size, but on some machines (like
   the i960), the total size of a structure is based on a non-trivial
   rounding method.  */

#define ROUND_TYPE_ALIGN(TYPE, COMPUTED, SPECIFIED) \
  i960_round_align (MAX ((COMPUTED), (SPECIFIED)), TYPE)

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
   for the register allocator.  On 80960, this includes the frame pointer
   (g15), the previous FP (r0), the stack pointer (r1), the return
   instruction pointer (r2), and the argument pointer (g14).  */
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 1, 1, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 1, 1}


/* On the 80960, note that:
	g0..g3 are used for return values,
	g0..g7 may always be used for parameters,
	g8..g11 may be used for parameters, but are preserved if they aren't,
	g12 is the static chain if needed, otherwise is preserved
	g13 is the struct return pointer, otherwise is usable (so always usable)
	g14 is the leaf return ptr, zero, or arg pointer (must be reset to zero before returning if used)
	g15 is the frame pointer,
	r0 is the previous FP,
	r1 is the stack pointer,
	r2 is the return instruction pointer,
	r3-r15 are always available,
	r3 is clobbered by calls in functions that use the arg pointer
	r4-r11 may be clobbered by the mcount call when profiling
	r4-r15 if otherwise unused may be used for preserving global registers
	fp0..fp3 are never available.  */

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_REALLY_USED_REGISTERS \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1}

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* 80960 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 17
/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 15
/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 14
/* Register in which static-chain is passed to a function.
   On i960, we use g12.  We can't use any local register, because we need
   a register that can be set before a call or before a jump.  */
#define STATIC_CHAIN_REGNUM 12

#define I960_STRUCT_VALUE_REGNUM 13

/* Actual top-of-stack address is same as
   the contents of the stack pointer register.  */
#define STACK_POINTER_OFFSET (-crtl->outgoing_args_size)


/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference..  */

#define ELIMINABLE_REGS	 {\
    {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM} \
}

/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.

   Since the stack grows upward on the i960, this must be a negative number.
   This includes the 64 byte hardware register save area and the size of
   the frame.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
    (OFFSET) = i960_compute_initial_elimination_offset((FROM), (TO))


 
/* The order in which to allocate registers.  */

#define	REG_ALLOC_ORDER	\
{  4, 5, 6, 7, 0, 1, 2, 3, 13, 	 /* g4, g5, g6, g7, g0, g1, g2, g3, g13 */ \
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
   
/* The 80960 has four kinds of registers, global, local, floating point,
   and condition code.  The cc register is never allocated, so no class
   needs to be defined for it.  */

enum reg_class { NO_REGS, GLOBAL_REGS, LOCAL_REGS, LOCAL_OR_GLOBAL_REGS,
  FP_REGS, ALL_REGS, LIM_REG_CLASSES };

/* 'r' includes floating point registers if TARGET_NUMERICS.  'd' never
   does.  */
#define	GENERAL_REGS	((TARGET_NUMERICS) ? ALL_REGS : LOCAL_OR_GLOBAL_REGS)

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES							\
{ "NO_REGS", "GLOBAL_REGS", "LOCAL_REGS", "LOCAL_OR_GLOBAL_REGS",	\
  "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS	\
{ {0, 0}, {0x0ffff, 0}, {0xffff0000, 0}, {-1,0}, {0, -1}, {-1,-1}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)	\
  ((REGNO) < 16 ? GLOBAL_REGS	\
   : (REGNO) < 32 ? LOCAL_REGS	\
   : (REGNO) < 36 ? FP_REGS	\
   : NO_REGS)

/* The class value for index registers, and the one for base regs.
   There is currently no difference between base and index registers on the
   i960, but this distinction may one day be useful.  */
#define INDEX_REG_CLASS LOCAL_OR_GLOBAL_REGS
#define BASE_REG_CLASS LOCAL_OR_GLOBAL_REGS
/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

/* On 960, can't load constant into floating-point reg except
   0.0 or 1.0.

   Any hard reg is ok as a src operand of a reload insn.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  (GET_CODE (X) == REG && REGNO (X) < FIRST_PSEUDO_REGISTER	\
   ? (CLASS)							\
   : ((CLASS) == FP_REGS && CONSTANT_P (X)			\
      && (X) != CONST0_RTX (DFmode) && (X) != CONST1_RTX (DFmode)\
      && (X) != CONST0_RTX (SFmode) && (X) != CONST1_RTX (SFmode)\
      ? NO_REGS							\
      : (CLASS) == ALL_REGS ? LOCAL_OR_GLOBAL_REGS : (CLASS)))

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  i960_secondary_reload_class (CLASS, MODE, IN)


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 0

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 0

#define ARGS_GROW_DOWNWARD 0
/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On 80960, don't define this because there are no push insns.  */
/* #define PUSH_ROUNDING(BYTES) BYTES */

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0
/* When a parameter is passed in a register, no stack space is
   allocated for it.  However, when args are passed in the
   stack, space is allocated for every register parameter.  */
#define REG_PARM_STACK_SPACE(DECL) i960_reg_parm_stack_space (DECL)
/* Define to a non zero value if it is the responsibility of the caller to
 * allocate the area reserved for argument passed in registers. Yes! gcc 3.4.6
 * has a slightly different design which made it hard to understand
 */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

/* Keep the stack pointer constant throughout the function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define this macro if the targe tmachine has register windows. This C
 expression returns true if the register is call-saved but is in the
 register-window */
 
#define LOCAL_REGNO(REGNO) \
    ((REGNO) >= 16 && (REGNO) < 32)


/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) gen_rtx_REG ((MODE), 0)

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On 80960, returns are in g0..g3 */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* 1 if N is a possible register number for function argument passing.
   On 80960, parameters are passed in g0..g11 */

#define FUNCTION_ARG_REGNO_P(N) ((N) < 12)


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On 80960, this is two integers, which count the number of register
   parameters and the number of stack parameters seen so far.  */

struct i960_cumulative_args_t { int ca_nregparms; int ca_nstackparms; };

#define CUMULATIVE_ARGS struct i960_cumulative_args_t

/* Define the number of registers that can hold parameters.
   This macro is used only in macro definitions below and/or i960.c.  */
#define NPARM_REGS 12

/* Define how to round to the next parameter boundary.
   This macro is used only in macro definitions below and/or i960.c.  */
#define ROUND_PARM(X, MULTIPLE_OF)	\
  ((((X) + (MULTIPLE_OF) - 1) / (MULTIPLE_OF)) * MULTIPLE_OF)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On 80960, the offset always starts at 0; the first parm reg is g0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM).ca_nregparms = 0, (CUM).ca_nstackparms = 0)
/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(TYPE, FUNC) \
  gen_rtx_REG (TYPE_MODE (TYPE), 0)

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
/* Output the label for a function definition.
  This handles leaf functions and a few other things for the i960.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	i960_function_name_declare (FILE, NAME, DECL)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)	i960_output_function_profiler ((FILE), (LABELNO));

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define	EXIT_IGNORE_STACK 1

/* Addressing modes, and classification of registers for them.  */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_FP_P(REGNO) \
  ((REGNO) < 36 || (unsigned) reg_renumber[REGNO] < 36)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the 960, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REGNO (X) >= 32 && REGNO (X) < 36)

/* Maximum number of registers that can appear in a valid memory address.  */
#define	MAX_REGS_PER_ADDRESS 2

#if 0
#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)
#endif

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_INDEX_P_STRICT(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_P_STRICT(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

	On 80960, legitimate addresses are:
		base				ld	(g0),r0
		disp	(12 or 32 bit)		ld	foo,r0
		base + index			ld	(g0)[g1*1],r0
		base + displ			ld	0xf00(g0),r0
		base + index*scale + displ	ld	0xf00(g0)[g1*4],r0
		index*scale + base		ld	(g0)[g1*4],r0
		index*scale + displ		ld	0xf00[g1*4],r0
		index*scale			ld	[g1*4],r0
		index + base + displ		ld	0xf00(g0)[g1*1],r0

	In each case, scale can be 1, 2, 4, 8, or 16.  */



/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 16

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is no faster than for words.
   Value changed to 1 after reports of poor bit-field code with g++.
   Indications are that code is usually as good, sometimes better.  */   

#define SLOW_BYTE_ACCESS 1

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 0

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* Specify the widest mode that BLKmode objects can be promoted to */
#define	MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TImode)

/* These global variables are used to pass information between
   cc setter and cc user at insn emit time.  */

//extern struct rtx_def *i960_compare_op0, *i960_compare_op1;

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  CC_NOOVmode should be used when the first operand is a
   PLUS, MINUS, or NEG.  CCmode should be used when no special processing is
   needed.  */
#define SELECT_CC_MODE(OP,X,Y) i960_select_cc_mode (OP, X)

/* A function address in a call instruction is a byte address
   (for indexing purposes) so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE 1

/* Control the assembler format that we output.  */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES {						\
	"g0", "g1", "g2",  "g3",  "g4",  "g5",  "g6",  "g7",		\
	"g8", "g9", "g10", "g11", "g12", "g13", "g14", "fp",		\
	"pfp","sp", "rip", "r3",  "r4",  "r5",  "r6",  "r7",		\
	"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
	"fp0","fp1","fp2", "fp3", "cc", "fake" }

/* How to renumber registers for dbx and gdb.
   In the 960 encoding, g0..g15 are registers 16..31.  */

#define DBX_REGISTER_NUMBER(REGNO)					\
  (((REGNO) < 16) ? (REGNO) + 16					\
   : (((REGNO) > 31) ? (REGNO) : (REGNO) - 16))

/* Don't emit dbx records longer than this.  This is an arbitrary value.  */
#define DBX_CONTIN_LENGTH 1500


/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

/* The prefix to add to user-visible assembler symbols.  */

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX	""
#endif

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX ".L"
#endif

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%s%lu", LOCAL_LABEL_PREFIX, PREFIX, (unsigned long)(NUM))

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tst\t%s,(sp)\n\taddo\t4,sp,sp\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tsubo\t4,sp,sp\n\tld\t(sp),%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.word %s%d\n", LOCAL_LABEL_PREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.word %s%d-%s%d\n", LOCAL_LABEL_PREFIX, VALUE, LOCAL_LABEL_PREFIX, REL)

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (int)(SIZE))

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 20

/* Generate RTL to flush the register windows so as to make arbitrary frames
   available.  */
#define SETUP_FRAME_ADDRESSES()		\
  emit_insn (gen_flush_register_windows ())

#endif
