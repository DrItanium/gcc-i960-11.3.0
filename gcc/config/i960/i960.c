/* Subroutines used for code generation on intel 80960.
   Copyright (C) 1992, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "cgraph.h"
#include "c-family/c-common.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "conditions.h"
#include "insn-attr.h"
#include "reload.h"
#include "varasm.h"
#include "calls.h"
#include "stor-layout.h"
#include "output.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "cfgrtl.h"
#include "builtins.h"
#include "context.h"
#include "tree-pass.h"
#include "print-rtl.h"
#include "rtl-iter.h"
#include "diagnostic-core.h"
// include last
#include "target-def.h"

#define current_function_args_size (crtl->args.size.to_constant())
#define current_function_args_info (crtl->args.info)
#define current_function_stdarg (cfun->stdarg)
#define compat_STARTING_FRAME_OFFSET 64
#ifdef compat_CONST_OK_FOR_LETTER_P 
#warning "CONST_OK_FOR_LETTER_P needs to be reimplemented in gcc11 terms"
#endif
#ifdef compat_HARD_REGNO_MODE_OK
#warning "HARD_REGNO_MODE_OK needs to be reimplemented in gcc11 terms"
#endif
static unsigned int i960_function_arg_boundary (machine_mode, const_tree);
static void i960_output_function_prologue (FILE * /*, HOST_WIDE_INT*/);
static void i960_output_function_epilogue (FILE * /*, HOST_WIDE_INT*/);
static void i960_output_mi_thunk (FILE *, tree, HOST_WIDE_INT,
				  HOST_WIDE_INT, tree);
static bool i960_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static int i960_address_cost (rtx, machine_mode, addr_space_t, bool);
static tree i960_build_builtin_va_list (void);
static void i960_option_override (void);
static struct rtx_def *i960_function_arg (cumulative_args_t, const class function_arg_info&);
static void i960_function_arg_advance (cumulative_args_t, const class function_arg_info&);
static void i960_setup_incoming_varargs (cumulative_args_t, const class function_arg_info&, int *, int);
static void i960_conditional_register_usage(void);
static bool i960_frame_pointer_required(void);
/* Per-function machine data.  */
struct GTY(()) machine_function
{

  /* Number of bytes saved on the stack for outgoing/sub-function args.  */
  HOST_WIDE_INT args_size;

};

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx i960_compare_op0, i960_compare_op1;

/* Used to implement #pragma align/noalign.  Initialized by OVERRIDE_OPTIONS
   macro in i960.h.  */

int i960_maxbitalignment;
int i960_last_maxbitalignment;

/* Used to implement switching between MEM and ALU insn types, for better
   C series performance.  */

enum insn_types i960_last_insn_type;

/* The leaf-procedure return register.  Set only if this is a leaf routine.  */

static int i960_leaf_ret_reg;

/* True if replacing tail calls with jumps is OK.  */

static int tail_call_ok;

/* A string containing a list of insns to emit in the epilogue so as to
   restore all registers saved by the prologue.  Created by the prologue
   code as it saves registers away.  */

char epilogue_string[1000];

/* A unique number (per function) for return labels.  */

static int ret_label = 0;

/* This is true if FNDECL is either a varargs or a stdarg function.
   This is used to help identify functions that use an argument block.  */

#define VARARGS_STDARG_FUNCTION(FNDECL)	\
(TYPE_ARG_TYPES (TREE_TYPE (FNDECL)) != 0				\
  && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (FNDECL)))))	\
      != void_type_node)

/* Initialize the GCC target structure.  */

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
i960_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}
void
i960_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = i960_init_machine_status;
}
/* Override conflicting target switch options.
   Doesn't actually detect if more than one -mARCH option is given, but
   does handle the case of two blatantly conflicting -mARCH options.

   Also initialize variables before compiling any files.  */

void
i960_initialize ()
{
    //if (TARGET_K_SERIES && TARGET_C_SERIES) {
    //    warning (0, "conflicting architectures defined - using C series");
    //    target_flags &= ~TARGET_FLAG_K_SERIES;
    //}
    //if (TARGET_K_SERIES && TARGET_MC) {
    //    warning (0, "conflicting architectures defined - using K series");
    //    target_flags &= ~TARGET_FLAG_MC;
    //}
    //if (TARGET_C_SERIES && TARGET_MC) {
    //    warning (0, "conflicting architectures defined - using C series");
    //    target_flags &= ~TARGET_FLAG_MC;
    //}
    //if (TARGET_IC_COMPAT3_0) {
    //    flag_short_enums = 1;
    //    flag_signed_char = 1;
    //    target_flags |= TARGET_FLAG_CLEAN_LINKAGE;
    //    if (TARGET_IC_COMPAT2_0) {
    //        warning (0, "iC2.0 and iC3.0 are incompatible - using iC3.0");
    //        target_flags &= ~TARGET_FLAG_IC_COMPAT2_0;
    //    }
    //}
    //if (TARGET_IC_COMPAT2_0) {
    //    flag_signed_char = 1;
    //    target_flags |= TARGET_FLAG_CLEAN_LINKAGE;
    //}

    //if (TARGET_IC_COMPAT2_0) {
    //    i960_maxbitalignment = 8;
    //    i960_last_maxbitalignment = 128;
    //} else {
        i960_maxbitalignment = 128;
        i960_last_maxbitalignment = 8;
    //}
}

static void
i960_conditional_register_usage(void)
{
/* If no fp unit, make all of the fp registers fixed so that they can't
   be used.  */
    if (! TARGET_NUMERICS) {
        fixed_regs[32] = 1;
        fixed_regs[33] = 1;
        fixed_regs[34] = 1;
        fixed_regs[35] = 1;
    }
}
static bool
i960_frame_pointer_required(void)
{
/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
/* ??? It isn't clear to me why this is here.  Perhaps because of a bug (since
   fixed) in the definition of INITIAL_FRAME_POINTER_OFFSET which would have
   caused this to fail.  */
/* ??? Must check current_function_has_nonlocal_goto, otherwise frame pointer
  elimination messes up nonlocal goto sequences.  I think this works for other
  targets because they use indirect jumps for the return which disables fp
  elimination.  */
#if 0
    return (! leaf_function_p () || current_function_has_nonlocal_goto);
#else
    return true;
#endif
}
/* Return true if OP can be used as the source of an fp move insn.  */

/* Return truth value of whether OP can be used as an operands in a three
   address arithmetic insn (such as add %o1,7,%l2) of mode MODE.  */

int
i960_arith_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode) || literal (op, mode));
}

/* Return truth value of whether OP can be used as an operands in a three
   address logic insn, possibly complementing OP, of mode MODE.  */

int
i960_logic_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && INTVAL(op) >= -32 && INTVAL(op) < 32));
}

/* Return true if OP is a register or a valid floating point literal.  */

int
i960_fp_arith_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode) || i960_fp_literal (op, mode));
}

/* Return true if OP is a register or a valid signed integer literal.  */

int
i960_signed_arith_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode) || signed_literal (op, mode));
}

/* Return truth value of whether OP is an integer which fits the
   range constraining immediate operands in three-address insns.  */

int
i960_literal (rtx op, enum machine_mode )
{
  return ((GET_CODE (op) == CONST_INT) && INTVAL(op) >= 0 && INTVAL(op) < 32);
}

/* Return true if OP is a float constant of 1.  */

int
i960_fp_literal_one (rtx op, enum machine_mode mode)
{
  return (TARGET_NUMERICS && mode == GET_MODE (op) && op == CONST1_RTX (mode));
}

/* Return true if OP is a float constant of 0.  */

int
i960_fp_literal_zero (rtx op, enum machine_mode mode)
{
  return (TARGET_NUMERICS && mode == GET_MODE (op) && op == CONST0_RTX (mode));
}

/* Return true if OP is a valid floating point literal.  */

int
i960_fp_literal(rtx op, enum machine_mode mode)
{
  return i960_fp_literal_zero (op, mode) || fp_literal_one (op, mode);
}

/* Return true if OP is a valid signed immediate constant.  */

int
i960_signed_literal(rtx op, enum machine_mode )
{
  return ((GET_CODE (op) == CONST_INT) && INTVAL(op) > -32 && INTVAL(op) < 32);
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
i960_symbolic_memory_operand (rtx op, enum machine_mode)
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return truth value of whether OP is EQ or NE.  */

int
i960_eq_or_neq (rtx op, enum machine_mode)
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* OP is an integer register or a constant.  */

int
i960_arith32_operand (rtx op, enum machine_mode mode)
{
  if (register_operand (op, mode))
    return 1;
  return (CONSTANT_P (op));
}

/* Return true if OP is an integer constant which is a power of 2.  */

int
i960_power2_operand (rtx op,enum machine_mode)
{
  if (GET_CODE (op) != CONST_INT)
    return 0;

  return exact_log2 (INTVAL (op)) >= 0;
}

/* Return true if OP is an integer constant which is the complement of a
   power of 2.  */

int
i960_cmplpower2_operand (rtx op, enum machine_mode)
{
  if (GET_CODE (op) != CONST_INT)
    return 0;

  return exact_log2 (~ INTVAL (op)) >= 0;
}

/* If VAL has only one bit set, return the index of that bit.  Otherwise
   return -1.  */

int
i960_bitpos (unsigned int val)
{
  int i;

  for (i = 0; val != 0; i++, val >>= 1)
    {
      if (val & 1)
	{
	  if (val != 1)
	    return -1;
	  return i;
	}
    }
  return -1;
}

/* Return nonzero if OP is a mask, i.e. all one bits are consecutive.
   The return value indicates how many consecutive nonzero bits exist
   if this is a mask.  This is the same as the next function, except that
   it does not indicate what the start and stop bit positions are.  */

int
i960_is_mask (unsigned int val)
{
    int start = -1;
    int end = 0;
    for (int i = 0; val != 0; val >>= 1, ++i) {
        if (val & 1) {
            if (start < 0) {
                start = i;
            }
            end = i;
            continue;
        }
        /* Still looking for the first bit.  */
        if (start < 0) {
            continue;
        }

        /* We've seen the start of a bit sequence, and now a zero.  There
       must be more one bits, otherwise we would have exited the loop.
       Therefore, it is not a mask.  */
        if (val) {
            return 0;
        }
    }

    /* The bit string has ones from START to END bit positions only.  */
    return end - start + 1;
}

/* If VAL is a mask, then return nonzero, with S set to the starting bit
   position and E set to the ending bit position of the mask.  The return
   value indicates how many consecutive bits exist in the mask.  This is
   the same as the previous function, except that it also indicates the
   start and end bit positions of the mask.  */

int
i960_bitstr (unsigned int val, int* s, int* e)
{

    int start = -1;
    int end = -1;
    for (int i = 0; val != 0; val >>= 1, ++i) {
        if (val & 1) {
            if (start < 0) {
                start = i;
            }
            end = i;
            continue;
        }

        /* Still looking for the first bit.  */
        if (start < 0) {
            continue;
        }

        /* We've seen the start of a bit sequence, and now a zero.  There
       must be more one bits, otherwise we would have exited the loop.
       Therefor, it is not a mask.  */
        if (val) {
            start = -1;
            end = -1;
            break;
        }
    }

    /* The bit string has ones from START to END bit positions only.  */
    *s = start;
    *e = end;
    return ((start < 0) ? 0 : end - start + 1);
}

/* Return the machine mode to use for a comparison.  */

enum machine_mode
i960_select_cc_mode (RTX_CODE op, rtx x)
{
  if (op == GTU || op == LTU || op == GEU || op == LEU)
    return CC_UNSmode;
  return CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 36 in the proper mode.  */

rtx
i960_gen_compare_reg (enum rtx_code code, rtx x, rtx y)
{
  rtx cc_reg;
  enum machine_mode ccmode = SELECT_CC_MODE (code, x, y);
  enum machine_mode mode
    = GET_MODE (x) == VOIDmode ? GET_MODE (y) : GET_MODE (x);

    if (mode == SImode) {
        if (! arith_operand (x, mode)) {
            x = force_reg(SImode, x);
        }
        if (! arith_operand (y, mode)) {
            y = force_reg(SImode, y);
        }
    }

  cc_reg = gen_rtx_REG (ccmode, 36);
  emit_insn (gen_rtx_SET (cc_reg,
			  gen_rtx_COMPARE (ccmode, x, y)));

  return cc_reg;
}

/* For the i960, REG is cost 1, REG+immed CONST is cost 2, REG+REG is cost 2,
   REG+nonimmed CONST is cost 4.  REG+SYMBOL_REF, SYMBOL_REF, and similar
   are 4.  Indexed addresses are cost 6.  */

/* ??? Try using just RTX_COST, i.e. not defining ADDRESS_COST.  */

static int 
i960_address_cost (rtx x, machine_mode, addr_space_t, bool)
{
  if (GET_CODE (x) == REG) {
      return 1;
  }

  /* This is a MEMA operand -- it's free.  */
  if (GET_CODE (x) == CONST_INT
      && INTVAL (x) >= 0
      && INTVAL (x) < 4096) {
      return 0;
  }

  if (GET_CODE (x) == PLUS) {
      rtx base = XEXP (x, 0);
      rtx offset = XEXP (x, 1);

      if (GET_CODE (base) == SUBREG) {
          base = SUBREG_REG (base);
      }
      if (GET_CODE (offset) == SUBREG) {
          offset = SUBREG_REG (offset);
      }

      if (GET_CODE (base) == REG) {
          if (GET_CODE (offset) == REG) {
              return 2;
          }
          if (GET_CODE (offset) == CONST_INT) {
              if ((unsigned)INTVAL (offset) < 2047) {
                  return 2;
              }
              return 4;
          }
          if (CONSTANT_P (offset)) {
              return 4;
          }
      }
      if (GET_CODE (base) == PLUS || GET_CODE (base) == MULT) {
          return 6;
      }

      /* This is an invalid address.  The return value doesn't matter, but
	 for convenience we make this more expensive than anything else.  */
      return 12;
    }
  if (GET_CODE (x) == MULT) {
      return 6;
  }

  /* Symbol_refs and other unrecognized addresses are cost 4.  */
  return 4;
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
i960_emit_move_sequence (rtx* operands, enum machine_mode mode)
{
  /* We can only store registers to memory.  */
  
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) != REG
      && (operands[1] != const0_rtx || current_function_args_size
	  || current_function_stdarg
	  || currently_expanding_to_rtl))
    /* Here we use the same test as movsi+1 pattern -- see i960.md.  */
    operands[1] = force_reg (mode, operands[1]);

  /* Storing multi-word values in unaligned hard registers to memory may
     require a scratch since we have to store them a register at a time and
     adding 4 to the memory address may not yield a valid insn.  */
  /* ??? We don't always need the scratch, but that would complicate things.
     Maybe later.  */
  /* ??? We must also handle stores to pseudos here, because the pseudo may be
     replaced with a MEM later.  This would be cleaner if we didn't have
     a separate pattern for unaligned DImode/TImode stores.  */
  if (GET_MODE_SIZE(mode) > UNITS_PER_WORD
      && (GET_CODE (operands[0]) == MEM
	  || (GET_CODE (operands[0]) == REG
	      && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER))
      && GET_CODE (operands[1]) == REG
      && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
      && ! TARGET_HARD_REGNO_MODE_OK (REGNO (operands[1]), mode))
    {
      emit_insn (gen_rtx_PARALLEL
		 (VOIDmode,
		  gen_rtvec (2,
			     gen_rtx_SET (operands[0], operands[1]),
			     gen_rtx_CLOBBER (VOIDmode,
					      gen_rtx_SCRATCH (Pmode)))));
      return 1;
    }

  return 0;
}

/* Get reg_class from a letter such as appears in the machine description.
   'f' is a floating point register (fp0..fp3)
   'l' is a local register (r0-r15)
   'b' is a global register (g0-g15)
   'd' is any local or global register
   'r' or 'g' are pre-defined to the class GENERAL_REGS.  */
/* 'l' and 'b' are probably never used.  Note that 'd' and 'r' are *not*
   the same thing, since 'r' may include the fp registers.  */
// #define REG_CLASS_FROM_LETTER(C) (((C) == 'f') && (TARGET_NUMERICS) ? FP_REGS : ((C) == 'l' ? LOCAL_REGS : (C) == 'b' ? GLOBAL_REGS : ((C) == 'd' ? LOCAL_OR_GLOBAL_REGS : NO_REGS)))
/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For 80960:
	'I' is used for literal values 0..31
   	'J' means literal 0
	'K' means 0..-31.  */

#define TARGET_CONST_OK_FOR_LETTER_P(VALUE, C)  				\
  ((C) == 'I' ? (((unsigned) (VALUE)) <= 31)				\
   : (C) == 'J' ? ((VALUE) == 0)					\
   : (C) == 'K' ? ((VALUE) >= -31 && (VALUE) <= 0)			\
   : (C) == 'M' ? ((VALUE) >= -32 && (VALUE) <= 0)			\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.
   For the 80960, G is 0.0 and H is 1.0.  */

#define TARGET_CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((TARGET_NUMERICS) &&							\
   (((C) == 'G' && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))		\
    || ((C) == 'H' && ((VALUE) == CONST1_RTX (GET_MODE (VALUE))))))
/* Output assembler to move a double word value.  */

const char *
i960_output_move_double (rtx dst, rtx src) {
    rtx operands[5];
    /// @todo cleanup the if conditionals?
    if (GET_CODE (dst) == REG && GET_CODE (src) == REG) {
        if ((REGNO (src) & 1) || (REGNO (dst) & 1)) {
            /* We normally copy the low-numbered register first.  However, if
               the second source register is the same as the first destination
               register, we must copy in the opposite order.  */
            if (REGNO (src) + 1 == REGNO (dst)) {
                return "mov	%D1,%D0\n\tmov	%1,%0";
            } else {
                return "mov	%1,%0\n\tmov	%D1,%D0";
            }
        } else {
            return "movl	%1,%0 # m4";
        }
    } else if (GET_CODE (dst) == REG
            && GET_CODE (src) == CONST_INT
            && TARGET_CONST_OK_FOR_LETTER_P(INTVAL (src), 'I')) {
        if (REGNO (dst) & 1) {
            return "mov	%1,%0\n\tmov	0,%D0 #m5.0";
        } else {
            return "movl	%1,%0 #m5.1";
        }
    } else if (GET_CODE (dst) == REG
            && GET_CODE (src) == MEM) {
        if (REGNO (dst) & 1) {
            /* One can optimize a few cases here, but you have to be
               careful of clobbering registers used in the address and
               edge conditions.  */
            operands[0] = dst;
            operands[1] = src;
            operands[2] = gen_rtx_REG (Pmode, REGNO (dst) + 1);
            operands[3] = gen_rtx_MEM (word_mode, operands[2]);
            operands[4] = adjust_address (operands[3], word_mode,
                    UNITS_PER_WORD);
            output_asm_insn
                ("lda	%1,%2 #lda 1\n\tld	%3,%0\n\tld	%4,%D0", operands);
            return "";
        } else {
            return "ldl	%1,%0";
        }
    } else if (GET_CODE (dst) == MEM && GET_CODE (src) == REG) {
        if (REGNO (src) & 1) {
            operands[0] = dst;
            operands[1] = adjust_address (dst, word_mode, UNITS_PER_WORD);
            if (! memory_address_p (word_mode, XEXP (operands[1], 0))) {
                abort ();
            }
            operands[2] = src;
            output_asm_insn ("st	%2,%0\n\tst	%D2,%1", operands);
            return "";
        }
        return "stl	%1,%0";
    } else {
        abort ();
    }
    return "";
}

/* Output assembler to move a double word zero.  */

const char *
i960_output_move_double_zero (rtx dst)
{
    /// @todo improve this code, while it is register efficient, it is very execution poor. It can really hammer the bus for very little benefit

    // this code is also compatible with all of the different variants of the i960
    // it is also important to remember that g14 needs to stay zero while not being used so this function takes advantage of that fact
  rtx operands[2];

  operands[0] = dst;
    {
      operands[1] = adjust_address (dst, word_mode, 4);
      output_asm_insn ("st	g14,%0\n\tst	g14,%1", operands);
    }
  return "";
}

/* Output assembler to move a quad word value.  */

const char *
i960_output_move_quad (rtx dst, rtx src)
{
  rtx operands[7];
    /// @todo Can we emit two long word ops when we are not aligned to quad register boundaries?
  if (GET_CODE (dst) == REG && GET_CODE (src) == REG) {
      if (!isQuadRegisterAligned(REGNO(src)) || !isQuadRegisterAligned(REGNO(dst))) {
          /* We normally copy starting with the low numbered register.
             However, if there is an overlap such that the first dest reg
             is <= the last source reg but not < the first source reg, we
             must copy in the opposite order.  */
          if (REGNO (dst) <= REGNO (src) + 3
                  && REGNO (dst) >= REGNO (src)) {
              return "mov	%F1,%F0\n\tmov	%E1,%E0\n\tmov	%D1,%D0\n\tmov	%1,%0";
          } else {
              return "mov	%1,%0\n\tmov	%D1,%D0\n\tmov	%E1,%E0\n\tmov	%F1,%F0";
          }
      } else {
          return "movq	%1,%0";
      }
  } else if (GET_CODE (dst) == REG
          && GET_CODE (src) == CONST_INT
          && TARGET_CONST_OK_FOR_LETTER_P (INTVAL (src), 'I')) {
      if (!isQuadRegisterAligned(REGNO(dst))) {
          return "mov	%1,%0\n\tmov	0,%D0\n\tmov	0,%E0\n\tmov	0,%F0";
      } else {
          return "movq	%1,%0";
      }
  } else if (GET_CODE (dst) == REG && GET_CODE (src) == MEM) {
      if (!isQuadRegisterAligned(REGNO(dst))) {
          /* One can optimize a few cases here, but you have to be
             careful of clobbering registers used in the address and
             edge conditions.  */
          operands[0] = dst;
          operands[1] = src;
          operands[2] = gen_rtx_REG (Pmode, REGNO (dst) + 3);
          operands[3] = gen_rtx_MEM (word_mode, operands[2]);
          operands[4]
              = adjust_address (operands[3], word_mode, UNITS_PER_WORD);
          operands[5]
              = adjust_address (operands[4], word_mode, UNITS_PER_WORD);
          operands[6]
              = adjust_address (operands[5], word_mode, UNITS_PER_WORD);
          output_asm_insn ("lda	%1,%2 #lda2\n\tld	%3,%0\n\tld	%4,%D0\n\tld	%5,%E0\n\tld	%6,%F0", operands);
          return "";
      } else {
          return "ldq	%1,%0";
      }
  } else if (GET_CODE (dst) == MEM
          && GET_CODE (src) == REG) {
      if (REGNO (src) & 3) {
          operands[0] = dst;
          operands[1] = adjust_address (dst, word_mode, UNITS_PER_WORD);
          operands[2] = adjust_address (dst, word_mode, 2 * UNITS_PER_WORD);
          operands[3] = adjust_address (dst, word_mode, 3 * UNITS_PER_WORD);
          if (! memory_address_p (word_mode, XEXP (operands[3], 0))) {
              abort ();
          }
          operands[4] = src;
          output_asm_insn ("st	%4,%0\n\tst	%D4,%1\n\tst	%E4,%2\n\tst	%F4,%3", operands);
          return "";
      }
      return "stq	%1,%0";
  } else {
      abort ();
  }
  return "";
}

/* Output assembler to move a quad word zero.  */

const char *
i960_output_move_quad_zero (rtx dst)
{
    // Oof, I see why having the four store instructions but oof! It will nuke the memory bursting feature of the i960... but if the quad zero is not aligned to 16-byte boundaries then there is nothing to be done
    // It can also cause a bunch of instruction cache line thrashing if not careful! The Sx/Kx processors support unaligned memory burst transactions.
    // These processors break the transaction into two bus requests. The Sx/Kx processors access memory on 16-byte groups and provide an offset into that group.
    //
    // If the i960 Sx/Kx had a data cache then this does not actually matter much since we could "smooth" out the bus accesses with the data cache.
    /// @todo Fix this code to better take advantage of burst mode of the i960 processor if alignment can be determined at this point.
    // Using four separate store operations for move a quad zero to memory is the safest way to support all of the different i960 variants.
    // Newer versions do not support unaligned accesses so I get it.

  rtx operands[4];

  operands[0] = dst;
    {
      operands[1] = adjust_address (dst, word_mode, 4);
      operands[2] = adjust_address (dst, word_mode, 8);
      operands[3] = adjust_address (dst, word_mode, 12);
      output_asm_insn ("st	g14,%0\n\tst	g14,%1\n\tst	g14,%2\n\tst	g14,%3", operands);
    }
  return "";
}


/* Emit insns to load a constant to non-floating point registers.
   Uses several strategies to try to use as few insns as possible.  */

const char *
i960_output_ldconst (rtx dst, rtx src)
{
  int rsrc1 = 0;
  unsigned int rsrc2 = 0;
  enum machine_mode mode = GET_MODE (dst);
  rtx operands[4] { dst, src, dst, src };

  /* Anything that isn't a compile time constant, such as a SYMBOL_REF,
     must be a ldconst insn.  */

  if (GET_CODE (src) != CONST_INT && GET_CODE (src) != CONST_DOUBLE)
    {
      output_asm_insn ("ldconst	%1,%0 # ldconst 10", operands);
      return "";
    }
  else if (mode == TFmode)
    {
      long value_long[3];
      int i;

      if (i960_fp_literal_zero (src, TFmode))
          return "movt	0,%0 #ldconst 11";

      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*CONST_DOUBLE_REAL_VALUE(src), value_long);

      output_asm_insn ("# ldconst	%1,%0 # ldconst 12",operands);

      for (i = 0; i < 3; i++)
	{
	  operands[0] = gen_rtx_REG (SImode, REGNO (dst) + i);
	  operands[1] = GEN_INT (value_long[i]);
	  output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
			   operands);
	}

      return ""; 
   }
  else if (mode == DFmode)
    {
      rtx first, second;

      if (i960_fp_literal_zero (src, DFmode))
	return "movl	0,%0 #m6";

      split_double (src, &first, &second);

      output_asm_insn ("# ldconst	%1,%0 # ldconst13",operands);

      operands[0] = gen_rtx_REG (SImode, REGNO (dst));
      operands[1] = first;
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      operands[0] = gen_rtx_REG (SImode, REGNO (dst) + 1);
      operands[1] = second;
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      return "";
    }
  else if (mode == SFmode)
    {
      //REAL_VALUE_TYPE d;
      long value;

      REAL_VALUE_TO_TARGET_SINGLE(*CONST_DOUBLE_REAL_VALUE(src), value);

      output_asm_insn ("# ldconst	%1,%0 # ldconst14",operands);
      operands[0] = gen_rtx_REG (SImode, REGNO (dst));
      operands[1] = GEN_INT (value);
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      return "";
    }
  else if (mode == TImode)
    {
      /* ??? This is currently not handled at all.  */
      abort ();

      /* Note: lowest order word goes in lowest numbered reg.  */
      rsrc1 = INTVAL (src);
      if (rsrc1 >= 0 && rsrc1 < 32)
	return "movq	%1,%0";
      else
	output_asm_insn ("movq\t0,%0\t# ldconstq %1,%0",operands);
      /* Go pick up the low-order word.  */
    }
  else if (mode == DImode) {
      rtx upperhalf, lowerhalf, xoperands[2];

      if (GET_CODE (src) == CONST_DOUBLE || GET_CODE (src) == CONST_INT) {
          split_double (src, &lowerhalf, &upperhalf);
      } else {
          abort ();
      }

      /* Note: lowest order word goes in lowest numbered reg.  */
      /* Numbers from 0 to 31 can be handled with a single insn.  */
      rsrc1 = INTVAL (lowerhalf);
      if (upperhalf == const0_rtx && rsrc1 >= 0 && rsrc1 < 32) {
          return "movl	%1,%0 #m7";
      }

      /* emit the lower half with a recursive call (we don't want to fight with this ever!) */
      xoperands[0] = gen_rtx_REG(SImode, REGNO(dst));
      xoperands[1] = lowerhalf;
      //return "mov	%D1,%0 # ldconst 20";
      output_asm_insn (i960_output_ldconst(xoperands[0], xoperands[1]), xoperands);
      /* Output the upper half with a recursive call.  */
      xoperands[0] = gen_rtx_REG (SImode, REGNO (dst) + 1);
      xoperands[1] = upperhalf;
      output_asm_insn (i960_output_ldconst (xoperands[0], xoperands[1]),
		       xoperands);
      return "";
    } else {
      rsrc1 = INTVAL (src);
      if (mode == QImode) {
          if (rsrc1 > 0xff) {
              rsrc1 &= 0xff;
          }
      } else if (mode == HImode) {
          if (rsrc1 > 0xffff) {
              rsrc1 &= 0xffff;
          }
      }
  }

  if (rsrc1 >= 0) {
      /* ldconst	0..31,X		-> 	mov	0..31,X  */
      if (rsrc1 < 32) {
#if 0
	  if (i960_last_insn_type == I_TYPE_REG && TARGET_C_SERIES)
	    return "lda	%1,%0 #lda3";
#endif
	  return "mov	%1,%0 # ldconst 20";
	}

      /* ldconst	32..63,X	->	add	31,nn,X  */
      if (rsrc1 < 63) {
#if 0
	  if (i960_last_insn_type == I_TYPE_REG && TARGET_C_SERIES)
	    return "lda	%1,%0";
#endif
	  operands[1] = GEN_INT (rsrc1 - 31);
	  output_asm_insn ("addo\t31,%1,%0\t# ldconst %3,%0 # ldconst 30", operands);
	  return "";
	}
    }
  else if (rsrc1 < 0)
    {
      /* ldconst	-1..-31		->	sub	0,0..31,X  */
      if (rsrc1 >= -31)
	{
	  /* return 'sub -(%1),0,%0' */
	  operands[1] = GEN_INT (- rsrc1);
	  output_asm_insn ("subo\t%1,0,%0\t# ldconst %3,%0 # ldconst 40", operands);
	  return "";
	}
      
      /* ldconst	-32		->	not	31,X  */
      if (rsrc1 == -32)
	{
	  operands[1] = GEN_INT (~rsrc1);
	  output_asm_insn ("not\t%1,%0	# ldconst %3,%0 # ldconst 50", operands);
	  return "";
	}
    }

  /* If const is a single bit.  */
  if (i960_bitpos (rsrc1) >= 0)
    {
      operands[1] = GEN_INT (i960_bitpos (rsrc1));
      output_asm_insn ("setbit\t%1,0,%0\t# ldconst %3,%0 # ldconst 60", operands);
      return "";
    }

  /* If const is a bit string of less than 6 bits (1..31 shifted).  */
  if (i960_is_mask (rsrc1))
    {
      int s, e;

      if (i960_bitstr (rsrc1, &s, &e) < 6)
	{
	  rsrc2 = ((unsigned int) rsrc1) >> s;
	  operands[1] = GEN_INT (rsrc2);
	  operands[2] = GEN_INT (s);
	  output_asm_insn ("shlo\t%2,%1,%0\t# ldconst %3,%0 # ldconst 70", operands);
	  return "";
	}
    }
  // the following comment is kind of nonsense but perhaps we will expand on it at some point in the future
  /* Unimplemented cases:
     const is in range 0..31 but rotated around end of word:
     ror	31,3,g0	-> ldconst 0xe0000003,g0
   
     and any 2 instruction cases that might be worthwhile  */
    return "ldconst %1, %0 #ldconst 80";
  //output_asm_insn ("ldconst	%1,%0 # ldconst 80", operands);
  //return "";
}

/* Determine if there is an opportunity for a bypass optimization.
   Bypass succeeds on the 960K* if the destination of the previous
   instruction is the second operand of the current instruction.
   Bypass always succeeds on the C*.
 
   Return 1 if the pattern should interchange the operands.

   CMPBR_FLAG is true if this is for a compare-and-branch insn.
   OP1 and OP2 are the two source operands of a 3 operand insn.  */

bool
i960_bypass (rtx_insn* insn, rtx op1, rtx op2, int cmpbr_flag)
{
  rtx prev_insn, prev_dest;
#if 0
  if (TARGET_C_SERIES)
    return false;
#endif

  /* Can't do this if op1 isn't a register.  */
  if (! REG_P (op1))
    return false;

  /* Can't do this for a compare-and-branch if both ops aren't regs.  */
  if (cmpbr_flag && ! REG_P (op2))
    return false;

  prev_insn = prev_real_insn (insn);

  if (prev_insn && GET_CODE (prev_insn) == INSN
      && GET_CODE (PATTERN (prev_insn)) == SET)
    {
      prev_dest = SET_DEST (PATTERN (prev_insn));
      if ((GET_CODE (prev_dest) == REG && REGNO (prev_dest) == REGNO (op1))
	  || (GET_CODE (prev_dest) == SUBREG
	      && GET_CODE (SUBREG_REG (prev_dest)) == REG
	      && REGNO (SUBREG_REG (prev_dest)) == REGNO (op1)))
	return true;
    }
  return false;
}

/* Output the code which declares the function name.  This also handles
   leaf routines, which have special requirements, and initializes some
   global variables.  */

void
i960_function_name_declare (FILE* file, const char* name, tree fndecl)
{
  int i, j;
  int leaf_proc_ok;
  rtx_insn* insn;

  /* Increment global return label.  */

  ret_label++;

  /* Compute whether tail calls and leaf routine optimizations can be performed
     for this function.  */

  if (TARGET_TAILCALL)
    tail_call_ok = 1;
  else
    tail_call_ok = 0;

  if (TARGET_LEAFPROC)
    leaf_proc_ok = 1;
  else
    leaf_proc_ok = 0;

  /* Even if nobody uses extra parms, can't have leafproc or tail calls if
     argblock, because argblock uses g14 implicitly.  */

  if (current_function_args_size != 0 || VARARGS_STDARG_FUNCTION (fndecl))
    {
      tail_call_ok = 0;
      leaf_proc_ok = 0;
    }
      
  /* See if caller passes in an address to return value.  */

  if (aggregate_value_p (DECL_RESULT (fndecl), fndecl))
    {
      tail_call_ok = 0;
      leaf_proc_ok = 0;
    }

  /* Can not use tail calls or make this a leaf routine if there is a non
     zero frame size.  */

  if (get_frame_size().to_constant() != 0)
    leaf_proc_ok = 0;

  /* I don't understand this condition, and do not think that it is correct.
     Apparently this is just checking whether the frame pointer is used, and
     we can't trust regs_ever_live[fp] since it is (almost?) always set.  */

  if (tail_call_ok)
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == INSN
	  && reg_mentioned_p (frame_pointer_rtx, insn))
	{
	  tail_call_ok = 0;
	  break;
	}

  /* Check for CALL insns.  Can not be a leaf routine if there are any.  */

  if (leaf_proc_ok)
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == CALL_INSN)
	{
	  leaf_proc_ok = 0;
	  break;
	}

  /* Can not be a leaf routine if any non-call clobbered registers are
     used in this function.  */

  if (leaf_proc_ok)
    for (i = 0, j = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (df_regs_ever_live_p(i)
	  && ((! call_used_regs[i]) || (i > 7 && i < 12)))
	{
	  /* Global registers.  */
	  if (i < 16 && i > 7 && i != 13)
	    leaf_proc_ok = 0;
	  /* Local registers.  */
	  else if (i < 32)
	    leaf_proc_ok = 0;
	}

  /* Now choose a leaf return register, if we can find one, and if it is
     OK for this to be a leaf routine.  */

  i960_leaf_ret_reg = -1;

  if (optimize && leaf_proc_ok)
    {
      for (i960_leaf_ret_reg = -1, i = 0; i < 8; i++)
	if (!df_regs_ever_live_p(i))
	  {
	    i960_leaf_ret_reg = i;
        df_set_regs_ever_live (i, true);
	    break;
	  }
    }

  /* Do this after choosing the leaf return register, so it will be listed
     if one was chosen.  */

  fprintf (file, "\t#  Function '%s'\n", (name[0] == '*' ? &name[1] : name));
  fprintf (file, "\t#  Registers used: ");

  for (i = 0, j = 0; i < FIRST_PSEUDO_REGISTER; i++)
  {
      if (df_regs_ever_live_p(i))
      {
          fprintf (file, "%s%s ", reg_names[i], call_used_regs[i] ? "" : "*");

          if (i > 15 && j == 0)
          {
              fprintf (file,"\n\t#\t\t   ");
              j++;
          }
      }
  }

  fprintf (file, "\n");

  if (i960_leaf_ret_reg >= 0)
    {
      /* Make it a leaf procedure.  */

      if (TREE_PUBLIC (fndecl))
	fprintf (file,"\t.globl\t%s.lf\n", (name[0] == '*' ? &name[1] : name));

      fprintf (file, "\t.leafproc\t");
      assemble_name (file, name);
      fprintf (file, ",%s.lf\n", (name[0] == '*' ? &name[1] : name));
      ASM_OUTPUT_LABEL (file, name);
      fprintf (file, "\tlda    .Li960R%d,g14 #lda4\n", ret_label);
      fprintf (file, "%s.lf:\n", (name[0] == '*' ? &name[1] : name));
      fprintf (file, "\tmov    g14,g%d\n", i960_leaf_ret_reg);
#if 0
      if (TARGET_C_SERIES)
	{
	  fprintf (file, "\tlda    0,g14\n");
	  i960_last_insn_type = I_TYPE_MEM;
	}
      else
	{
#endif
	  fprintf (file, "\tmov    0,g14\n");
	  i960_last_insn_type = I_TYPE_REG;
#if 0
	}
#endif

    }
  else
    {
      ASM_OUTPUT_LABEL (file, name);
      i960_last_insn_type = I_TYPE_CTRL; 
    }
}

/* Compute and return the frame size.  */

int
i960_compute_frame_size (poly_int64 size)
{
  int actual_fsize;
  int outgoing_args_size = crtl->outgoing_args_size;

  /* The STARTING_FRAME_OFFSET is totally hidden to us as far
     as size is concerned.  */
  actual_fsize = (size + 15) & -16;
  actual_fsize += (outgoing_args_size + 15) & -16;

  return actual_fsize;
}

/* Here register group is range of registers which can be moved by
   one i960 instruction.  */

struct reg_group
{
  char start_reg;
  char length;
};

static int i960_form_reg_groups (int, int, int *, int, struct reg_group *);
static int i960_reg_group_compare (const void *, const void *);
static int i960_split_reg_group (struct reg_group *, int, int);
static void i960_arg_size_and_align (enum machine_mode, tree, int *, int *);

/* The following functions forms the biggest as possible register
   groups with registers in STATE.  REGS contain states of the
   registers in range [start, finish_reg).  The function returns the
   number of groups formed.  */
static int
i960_form_reg_groups (int start_reg, int finish_reg, int* regs, int state, struct reg_group* reg_groups)
{
  int i;
  int nw = 0;

  for (i = start_reg; i < finish_reg; )
    {
      if (regs [i] != state)
	{
	  i++;
	  continue;
	}
      else if (i % 2 != 0 || regs [i + 1] != state)
	reg_groups [nw].length = 1;
      else if (i % 4 != 0 || regs [i + 2] != state)
	reg_groups [nw].length = 2;
      else if (regs [i + 3] != state)
	reg_groups [nw].length = 3;
      else
	reg_groups [nw].length = 4;
      reg_groups [nw].start_reg = i;
      i += reg_groups [nw].length;
      nw++;
    }
  return nw;
}

/* We sort register winodws in descending order by length.  */
static int
i960_reg_group_compare (const void* group1, const void* group2)
{
  const struct reg_group *w1 = reinterpret_cast<const struct reg_group*>( group1);
  const struct reg_group *w2 = reinterpret_cast<const struct reg_group*>( group2);

  if (w1->length > w2->length)
    return -1;
  else if (w1->length < w2->length)
    return 1;
  else
    return 0;
}

/* Split the first register group in REG_GROUPS on subgroups one of
   which will contain SUBGROUP_LENGTH registers.  The function
   returns new number of winodws.  */
static int
i960_split_reg_group (reg_group* reg_groups, int nw, int subgroup_length)
{
  if (subgroup_length < reg_groups->length - subgroup_length)
    /* This guarantees correct alignments of the two subgroups for
       i960 (see spliting for the group length 2, 3, 4).  More
       generalized algorithm would require splitting the group more
       two subgroups.  */
    subgroup_length = reg_groups->length - subgroup_length;
  /* More generalized algorithm would require to try merging
     subgroups here.  But in case i960 it always results in failure
     because of register group alignment.  */
  reg_groups[nw].length = reg_groups->length - subgroup_length;
  reg_groups[nw].start_reg = reg_groups->start_reg + subgroup_length;
  nw++;
  reg_groups->length = subgroup_length;
  qsort (reg_groups, nw, sizeof (struct reg_group), i960_reg_group_compare);
  return nw;
}

/* Output code for the function prologue.  */

static void
i960_output_function_prologue (FILE* file/*, HOST_WIDE_INT size*/)
{
  int i, j, nr;
  int n_saved_regs = 0;
  int n_remaining_saved_regs;
  HOST_WIDE_INT lvar_size;
  HOST_WIDE_INT actual_fsize, offset;
  int gnw, lnw;
  struct reg_group *g, *l;
  char tmpstr[1000];
  /* -1 if reg must be saved on proc entry, 0 if available, 1 if saved
     somewhere.  */
  int regs[FIRST_PSEUDO_REGISTER];
  /* All global registers (which must be saved) divided by groups.  */
  struct reg_group global_reg_groups [16];
  /* All local registers (which are available) divided by groups.  */
  struct reg_group local_reg_groups [16];


  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++) {
      if (df_regs_ever_live_p(i) && ((!call_used_regs[i]) || (i > 7 && i < 12))
          /* No need to save the static chain pointer.  */
          && !(i == STATIC_CHAIN_REGNUM && cfun->static_chain_decl)) {
          regs[i] = -1;
          /* Count global registers that need saving.  */
          if (i < 16)
              n_saved_regs++;
      } else {
          regs[i] = 0;
      }
  }


  n_remaining_saved_regs = n_saved_regs;

  epilogue_string[0] = '\0';

  if (crtl->profile)
    {
      /* When profiling, we may use registers 20 to 27 to save arguments, so
	 they can't be used here for saving globals.  J is the number of
	 argument registers the mcount call will save.  */
      for (j = 7; j >= 0 && ! df_regs_ever_live_p(j); j--)
	;

      for (i = 20; i <= j + 20; i++)
	regs[i] = -1;
    }

  gnw = i960_form_reg_groups (0, 16, regs, -1, global_reg_groups);
  lnw = i960_form_reg_groups (19, 32, regs, 0, local_reg_groups);
  qsort (global_reg_groups, gnw, sizeof (struct reg_group),
	 i960_reg_group_compare);
  qsort (local_reg_groups, lnw, sizeof (struct reg_group),
	 i960_reg_group_compare);
    for (g = global_reg_groups, l = local_reg_groups; lnw != 0 && gnw != 0;) {
        if (g->length == l->length) {
            fprintf (file, "\tmov%s	%s,%s\n",
                     ((g->length == 4) ? "q" :
                      (g->length == 3) ? "t" :
                      (g->length == 2) ? "l" : ""),
                     reg_names[(unsigned char) g->start_reg],
                     reg_names[(unsigned char) l->start_reg]);
            sprintf (tmpstr, "\tmov%s	%s,%s\n",
                     ((g->length == 4) ? "q" :
                      (g->length == 3) ? "t" :
                      (g->length == 2) ? "l" : ""),
                     reg_names[(unsigned char) l->start_reg],
                     reg_names[(unsigned char) g->start_reg]);
            strcat (epilogue_string, tmpstr);
            n_remaining_saved_regs -= g->length;
            for (i = 0; i < g->length; i++) {
                regs [i + g->start_reg] = 1;
                regs [i + l->start_reg] = -1;
                df_set_regs_ever_live (i + l->start_reg, true);
            }
            g++;
            l++;
            gnw--;
            lnw--;
        } else if (g->length > l->length) {
            gnw = i960_split_reg_group(g, gnw, l->length);
        } else {
            lnw = i960_split_reg_group(l, lnw, g->length);
        }
    }

  actual_fsize = i960_compute_frame_size (get_frame_size()) + 4 * n_remaining_saved_regs;
#if 0
  /* ??? The 1.2.1 compiler does this also.  This is meant to round the frame
     size up to the nearest multiple of 16.  I don't know whether this is
     necessary, or even desirable.

     The frame pointer must be aligned, but the call instruction takes care of
     that.  If we leave the stack pointer unaligned, we may save a little on
     dynamic stack allocation.  And we don't lose, at least according to the
     i960CA manual.  */
  actual_fsize = (actual_fsize + 15) & ~0xF;
#endif

  /* Check stack limit if necessary.  */
    if (crtl->limit_stack) {
        rtx min_stack = stack_limit_rtx;
        if (actual_fsize != 0)
            min_stack = plus_constant (Pmode, stack_limit_rtx, -actual_fsize);

        /* Now, emulate a little bit of reload.  We want to turn 'min_stack'
       into an arith_operand.  Use register 20 as the temporary.  */
        if (i960_legitimate_address_p (Pmode, min_stack, 1)
            && !i960_arith_operand (min_stack, Pmode)) {
            rtx tmp = gen_rtx_MEM (Pmode, min_stack);
            fputs ("\tlda\t", file);
            i960_print_operand (file, tmp, 0);
            fputs (",r4 #lda5\n", file);
            min_stack = gen_rtx_REG (Pmode, 20);
        }
        if (arith_operand (min_stack, Pmode)) {
            fputs ("\tcmpo\tsp,", file);
            i960_print_operand (file, min_stack, 0);
            fputs ("\n\tfaultge.f\n", file);
        } else {
            warning (0, "stack limit expression is not supported");
        }
    }

  /* Allocate space for register save and locals.  */
    if (actual_fsize > 0) {
        if (actual_fsize < 32) {
            fprintf (file, "\taddo	" HOST_WIDE_INT_PRINT_DEC ",sp,sp\n", actual_fsize);
        } else {
            fprintf(file, "\tlda\t" HOST_WIDE_INT_PRINT_DEC "(sp),sp #lda6\n", actual_fsize);
        }
    }

  /* Take hardware register save area created by the call instruction
     into account, but store them before the argument block area.  */
  lvar_size = actual_fsize - i960_compute_frame_size (0) - n_remaining_saved_regs * 4;
  offset = compat_STARTING_FRAME_OFFSET + lvar_size;
  /* Save registers on stack if needed.  */
  /* ??? Is it worth to use the same algorithm as one for saving
     global registers in local registers? */
  for (i = 0, j = n_remaining_saved_regs; j > 0 && i < 16; i++) {
      if (regs[i] != -1) continue;
      nr = 1;
      if (i <= 14 && i % 2 == 0 && regs[i+1] == -1 && offset % 2 == 0) nr = 2;
      if (nr == 2 && i <= 12 && i % 4 == 0 && regs[i+2] == -1 && offset % 4 == 0) nr = 3;
      if (nr == 3 && regs[i+3] == -1) nr = 4;

      fprintf (file,"\tst%s	%s," HOST_WIDE_INT_PRINT_DEC "(fp)\n", ((nr == 4) ? "q" : (nr == 3) ? "t" : (nr == 2) ? "l" : ""), reg_names[i], offset);
      sprintf (tmpstr,"\tld%s	" HOST_WIDE_INT_PRINT_DEC "(fp),%s\n", ((nr == 4) ? "q" : (nr == 3) ? "t" : (nr == 2) ? "l" : ""), offset, reg_names[i]);
      strcat (epilogue_string, tmpstr);
      i += nr-1;
      j -= nr;
      offset += nr * 4;
    }

  if (actual_fsize == 0) {
      return;
  }

  fprintf (file, "\t#Prologue stats:\n");
  fprintf (file, "\t#  Total Frame Size: " HOST_WIDE_INT_PRINT_DEC " bytes\n",
	   actual_fsize);

  if (lvar_size)
    fprintf (file, "\t#  Local Variable Size: " HOST_WIDE_INT_PRINT_DEC
	     " bytes\n", lvar_size);
  if (n_saved_regs)
    fprintf (file, "\t#  Register Save Size: %d regs, %d bytes\n",
	     n_saved_regs, n_saved_regs * 4);
  fprintf (file, "\t#End Prologue#\n");
}

/* Output code for the function profiler.  */

void
i960_output_function_profiler (FILE* file, int labelno)
{
  /* The last used parameter register.  */
  int last_parm_reg;
  int i, j, increment;
  int varargs_stdarg_function
    = VARARGS_STDARG_FUNCTION (current_function_decl);

  /* Figure out the last used parameter register.  The proper thing to do
     is to walk incoming args of the function.  A function might have live
     parameter registers even if it has no incoming args.  Note that we
     don't have to save parameter registers g8 to g11 because they are
     call preserved.  */

  /* See also output_function_prologue, which tries to use local registers
     for preserved call-saved global registers.  */

  for (last_parm_reg = 7;
       last_parm_reg >= 0 && ! df_regs_ever_live_p(last_parm_reg);
       last_parm_reg--)
    ;

  /* Save parameter registers in regs r4 (20) to r11 (27).  */

  for (i = 0, j = 4; i <= last_parm_reg; i += increment, j += increment)
    {
      if (i % 4 == 0 && (last_parm_reg - i) >= 3)
	increment = 4;
      else if (i % 4 == 0 && (last_parm_reg - i) >= 2)
	increment = 3;
      else if (i % 2 == 0 && (last_parm_reg - i) >= 1)
	increment = 2;
      else
	increment = 1;

      fprintf (file, "\tmov%s	g%d,r%d\n",
	       (increment == 4 ? "q" : increment == 3 ? "t"
		: increment == 2 ? "l": ""), i, j);
      }

  /* If this function uses the arg pointer, then save it in r3 and then
     set it to zero.  */

  if (current_function_args_size != 0 || varargs_stdarg_function)
    fprintf (file, "\tmov	g14,r3\n\tmov	0,g14\n");

  /* Load location address into g0 and call mcount.  */

  fprintf (file, "\tlda\tLP%d,g0 #lda7\n\tcallx\tmcount\n", labelno);

  /* If this function uses the arg pointer, restore it.  */

  if (current_function_args_size != 0 || varargs_stdarg_function)
    fprintf (file, "\tmov	r3,g14\n");

  /* Restore parameter registers.  */

  for (i = 0, j = 4; i <= last_parm_reg; i += increment, j += increment)
    {
      if (i % 4 == 0 && (last_parm_reg - i) >= 3)
	increment = 4;
      else if (i % 4 == 0 && (last_parm_reg - i) >= 2)
	increment = 3;
      else if (i % 2 == 0 && (last_parm_reg - i) >= 1)
	increment = 2;
      else
	increment = 1;

      fprintf (file, "\tmov%s	r%d,g%d\n",
	       (increment == 4 ? "q" : increment == 3 ? "t"
		: increment == 2 ? "l": ""), j, i);
    }
}

/* Output code for the function epilogue.  */

static void
i960_output_function_epilogue (FILE* file/*, HOST_WIDE_INT size*/)
{
  if (i960_leaf_ret_reg >= 0)
    {
      fprintf (file, ".Li960R%d:	ret\n", ret_label);
      return;
    }

  if (*epilogue_string == 0)
    {
      //rtx tmp;
	
      /* Emit a return insn, but only if control can fall through to here.  */

      rtx_insn* tmp = get_last_insn ();
      while (tmp)
	{
	  if (GET_CODE (tmp) == BARRIER)
	    return;
	  if (GET_CODE (tmp) == CODE_LABEL)
	    break;
	  if (GET_CODE (tmp) == JUMP_INSN)
	    {
	      if (GET_CODE (PATTERN (tmp)) == RETURN)
		return;
	      break;
	    }
	  if (GET_CODE (tmp) == NOTE)
	    {
	      tmp = PREV_INSN (tmp);
	      continue;
	    }
	  break;
	}
      fprintf (file, ".Li960R%d:	ret\n", ret_label);
      return;
    }

  fprintf (file, ".Li960R%d:\n", ret_label);

  fprintf (file, "\t#EPILOGUE#\n");

  /* Output the string created by the prologue which will restore all
     registers saved by the prologue.  */

  if (epilogue_string[0] != '\0')
    fprintf (file, "%s", epilogue_string);

  /* Must clear g14 on return if this function set it.
     Only varargs/stdarg functions modify g14.  */

  if (VARARGS_STDARG_FUNCTION (current_function_decl))
    fprintf (file, "\tmov	0,g14\n");

  fprintf (file, "\tret\n");
  fprintf (file, "\t#End Epilogue#\n");
}

/* Output code for a call insn.  */

const char *
i960_output_call_insn (rtx target, rtx argsize_rtx, rtx arg_pointer, rtx_insn* insn)
{
  int argsize = INTVAL (argsize_rtx);
  auto nexti = next_real_insn (insn);
  rtx operands[2];
  int varargs_stdarg_function
    = VARARGS_STDARG_FUNCTION (current_function_decl);

  operands[0] = target;
  operands[1] = arg_pointer;

  if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	g14,r3", operands);

  if (argsize > 48)
    output_asm_insn ("lda	%a1,g14 #lda8", operands);
  else if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	0,g14", operands);

  /* The code used to assume that calls to SYMBOL_REFs could not be more
     than 24 bits away (b vs bx, callj vs callx).  This is not true.  This
     feature is now implemented by relaxing in the GNU linker.  It can convert
     bx to b if in range, and callx to calls/call/balx/bal as appropriate.  */

  /* Nexti could be zero if the called routine is volatile.  */
  if (optimize && (*epilogue_string == 0) && argsize == 0 && tail_call_ok 
      && (nexti == 0 || GET_CODE (PATTERN (nexti)) == RETURN))
    {
      /* Delete following return insn.  */
      if (nexti && no_labels_between_p (insn, nexti))
	delete_insn (nexti);
      output_asm_insn ("bx	%0", operands);
      return "# notreached";
    }

  output_asm_insn ("callx	%0", operands);

  /* If the caller sets g14 to the address of the argblock, then the caller
     must clear it after the return.  */

  if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	r3,g14", operands);
  else if (argsize > 48)
    output_asm_insn ("mov	0,g14", operands);

  return "";
}

/* Output code for a return insn.  */

const char *
i960_output_ret_insn (rtx_insn* insn)
{
  static char lbuf[20];
  
  if (*epilogue_string != 0)
    {
      if (! TARGET_CODE_ALIGN && next_real_insn (insn) == 0)
	return "";

      sprintf (lbuf, "b	.Li960R%d", ret_label);
      return lbuf;
    }

  /* Must clear g14 on return if this function set it.
     Only varargs/stdarg functions modify g14.  */

  if (VARARGS_STDARG_FUNCTION (current_function_decl))
    output_asm_insn ("mov	0,g14", 0);

  if (i960_leaf_ret_reg >= 0)
    {
      sprintf (lbuf, "bx	(%s)", reg_names[i960_leaf_ret_reg]);
      return lbuf;
    }
  return "ret";
}

/* Print the operand represented by rtx X formatted by code CODE.  */

void
i960_print_operand (FILE* file, rtx x, int code)
{
  enum rtx_code rtxcode = GET_CODE (x);

  if (rtxcode == REG)
    {
      switch (code)
	{
	case 'D':
	  /* Second reg of a double or quad.  */
	  fprintf (file, "%s", reg_names[REGNO (x)+1]);
	  break;

	case 'E':
	  /* Third reg of a quad.  */
	  fprintf (file, "%s", reg_names[REGNO (x)+2]);
	  break;

	case 'F':
	  /* Fourth reg of a quad.  */
	  fprintf (file, "%s", reg_names[REGNO (x)+3]);
	  break;

	case 0:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  break;

	default:
	  abort ();
	}
      return;
    }
  else if (rtxcode == MEM)
    {
        /// @todo is VOIDmode right?
      output_address (VOIDmode, XEXP (x, 0));
      return;
    }
  else if (rtxcode == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (x);
      if (code == 'C')
	val = ~val;
      if (val > 9999 || val < -999)
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, val);
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, val);
      return;
    }
  else if (rtxcode == CONST_DOUBLE)
    {
      char dstr[30];

      if (x == CONST0_RTX (GET_MODE (x)))
	{
	  fprintf (file, "0f0.0");
	  return;
	}
      else if (x == CONST1_RTX (GET_MODE (x)))
	{
	  fprintf (file, "0f1.0");
	  return;
	}

      real_to_decimal (dstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (dstr), 0, 1);
      fprintf (file, "0f%s", dstr);
      return;
    }

  switch(code)
    {
    case 'B':
      /* Branch or jump, depending on assembler.  */
#if 0
      if (TARGET_ASM_COMPAT)
	fputs ("j", file);
      else
#endif
          // only support binutils
	fputs ("b", file);
      break;

    case 'S':
      /* Sign of condition.  */
      if ((rtxcode == EQ) || (rtxcode == NE) || (rtxcode == GTU)
	  || (rtxcode == LTU) || (rtxcode == GEU) || (rtxcode == LEU))
	fputs ("o", file);
      else if ((rtxcode == GT) || (rtxcode == LT)
	  || (rtxcode == GE) || (rtxcode == LE))
	fputs ("i", file);
      else
	abort();
      break;

    case 'I':
      /* Inverted condition.  */
      rtxcode = reverse_condition (rtxcode);
      goto normal;

    case 'X':
      /* Inverted condition w/ reversed operands.  */
      rtxcode = reverse_condition (rtxcode);
      /* Fallthrough.  */

    case 'R':
      /* Reversed operand condition.  */
      rtxcode = swap_condition (rtxcode);
      /* Fallthrough.  */

    case 'C':
      /* Normal condition.  */
    normal:
      if (rtxcode == EQ)  { fputs ("e", file); return; }
      else if (rtxcode == NE)  { fputs ("ne", file); return; }
      else if (rtxcode == GT)  { fputs ("g", file); return; }
      else if (rtxcode == GTU) { fputs ("g", file); return; }
      else if (rtxcode == LT)  { fputs ("l", file); return; }
      else if (rtxcode == LTU) { fputs ("l", file); return; }
      else if (rtxcode == GE)  { fputs ("ge", file); return; }
      else if (rtxcode == GEU) { fputs ("ge", file); return; }
      else if (rtxcode == LE)  { fputs ("le", file); return; }
      else if (rtxcode == LEU) { fputs ("le", file); return; }
      else abort ();
      break;

      /// @todo support branch prediction hints
#if 0
    case '+':
      /* For conditional branches, substitute ".t" or ".f".  */
      if (TARGET_BRANCH_PREDICT)
	{
	  x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	  if (x)
	    {
	      int pred_val = INTVAL (XEXP (x, 0));
	      fputs ((pred_val < REG_BR_PROB_BASE / 2 ? ".f" : ".t"), file);
	    }
	}
      break;
#endif

    case 0:
      output_addr_const (file, x);
      break;

    default:
      abort ();
    }

  return;
}

/* Print a memory address as an operand to reference that memory location.

   This is exactly the same as legitimate_address_p, except that it the prints
   addresses instead of recognizing them.  */

void
i960_print_operand_addr (FILE* file, rtx addr)
{
  rtx breg, ireg;
  rtx scale, offset;

  ireg = 0;
  breg = 0;
  offset = 0;
  scale = const1_rtx;

  if (GET_CODE (addr) == REG)
    breg = addr;
  else if (CONSTANT_P (addr))
    offset = addr;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0, op1;

      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);

      if (GET_CODE (op0) == REG)
	{
	  breg = op0;
	  if (GET_CODE (op1) == REG)
	    ireg = op1;
	  else if (CONSTANT_P (op1))
	    offset = op1;
	  else
	    abort ();
	}
      else if (GET_CODE (op0) == PLUS)
	{
	  if (GET_CODE (XEXP (op0, 0)) == MULT)
	    {
	      ireg = XEXP (XEXP (op0, 0), 0);
	      scale = XEXP (XEXP (op0, 0), 1);
	      if (GET_CODE (XEXP (op0, 1)) == REG)
		{
		  breg = XEXP (op0, 1);
		  offset = op1;
		}
	      else
		abort ();
	    }
	  else if (GET_CODE (XEXP (op0, 0)) == REG)
	    {
	      breg = XEXP (op0, 0);
	      if (GET_CODE (XEXP (op0, 1)) == REG)
		{
		  ireg = XEXP (op0, 1);
		  offset = op1;
		}
	      else
		abort ();
	    }
	  else
	    abort ();
	}
      else if (GET_CODE (op0) == MULT)
	{
	  ireg = XEXP (op0, 0);
	  scale = XEXP (op0, 1);
	  if (GET_CODE (op1) == REG)
	    breg = op1;
	  else if (CONSTANT_P (op1))
	    offset = op1;
	  else
	    abort ();
	}
      else
	abort ();
    }
  else if (GET_CODE (addr) == MULT)
    {
      ireg = XEXP (addr, 0);
      scale = XEXP (addr, 1);
    }
  else
    abort ();

  if (offset)
    output_addr_const (file, offset);
  if (breg)
    fprintf (file, "(%s)", reg_names[REGNO (breg)]);
  if (ireg)
    fprintf (file, "[%s*" HOST_WIDE_INT_PRINT_DEC "]",
	     reg_names[REGNO (ireg)], INTVAL (scale));
}

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

/* This is exactly the same as i960_print_operand_addr, except that
   it recognizes addresses instead of printing them.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

/* These two macros allow us to accept either a REG or a SUBREG anyplace
   where a register is valid.  */

#define RTX_OK_FOR_BASE_P(X, STRICT)					\
  ((GET_CODE (X) == REG							\
    && (STRICT ? REG_OK_FOR_BASE_P_STRICT (X) : REG_OK_FOR_BASE_P (X)))	\
   || (GET_CODE (X) == SUBREG						\
       && GET_CODE (SUBREG_REG (X)) == REG				\
       && (STRICT ? REG_OK_FOR_BASE_P_STRICT (SUBREG_REG (X))		\
	   : REG_OK_FOR_BASE_P (SUBREG_REG (X)))))

#define RTX_OK_FOR_INDEX_P(X, STRICT)					\
  ((GET_CODE (X) == REG							\
    && (STRICT ? REG_OK_FOR_INDEX_P_STRICT (X) : REG_OK_FOR_INDEX_P (X)))\
   || (GET_CODE (X) == SUBREG						\
       && GET_CODE (SUBREG_REG (X)) == REG				\
       && (STRICT ? REG_OK_FOR_INDEX_P_STRICT (SUBREG_REG (X))		\
	   : REG_OK_FOR_INDEX_P (SUBREG_REG (X)))))

int
i960_legitimate_address_p (enum machine_mode mode, rtx addr, int strict)
{
  if (RTX_OK_FOR_BASE_P (addr, strict))
    return 1;
  else if (CONSTANT_P (addr))
    return 1;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0, op1;

      if (! TARGET_COMPLEX_ADDR && ! reload_completed)
	return 0;

      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);

      if (RTX_OK_FOR_BASE_P (op0, strict))
	{
	  if (RTX_OK_FOR_INDEX_P (op1, strict))
	    return 1;
	  else if (CONSTANT_P (op1))
	    return 1;
	  else
	    return 0;
	}
      else if (GET_CODE (op0) == PLUS)
	{
	  if (GET_CODE (XEXP (op0, 0)) == MULT)
	    {
	      if (! (RTX_OK_FOR_INDEX_P (XEXP (XEXP (op0, 0), 0), strict)
		     && SCALE_TERM_P (XEXP (XEXP (op0, 0), 1))))
		return 0;

	      if (RTX_OK_FOR_BASE_P (XEXP (op0, 1), strict)
		  && CONSTANT_P (op1))
		return 1;
	      else
		return 0;
	    }
	  else if (RTX_OK_FOR_BASE_P (XEXP (op0, 0), strict))
	    {
	      if (RTX_OK_FOR_INDEX_P (XEXP (op0, 1), strict)
		  && CONSTANT_P (op1))
		return 1;
	      else
		return 0;
	    }
	  else
	    return 0;
	}
      else if (GET_CODE (op0) == MULT)
	{
	  if (! (RTX_OK_FOR_INDEX_P (XEXP (op0, 0), strict)
		 && SCALE_TERM_P (XEXP (op0, 1))))
	    return 0;

	  if (RTX_OK_FOR_BASE_P (op1, strict))
	    return 1;
	  else if (CONSTANT_P (op1))
	    return 1;
	  else
	    return 0;
	}
      else
	return 0;
    }
  else if (GET_CODE (addr) == MULT)
    {
      if (! TARGET_COMPLEX_ADDR && ! reload_completed)
	return 0;

      return (RTX_OK_FOR_INDEX_P (XEXP (addr, 0), strict)
	      && SCALE_TERM_P (XEXP (addr, 1)));
    }
  else
    return 0;
}



/* Return the minimum alignment of an expression rtx X in bytes.  This takes
   advantage of machine specific facts, such as knowing that the frame pointer
   is always 16 byte aligned.  */

int
i960_expr_alignment (rtx x, int size)
{
  int align = 1;

  if (x == 0)
    return 1;

  switch (GET_CODE(x))
    {
    case CONST_INT:
      align = INTVAL(x);

      if ((align & 0xf) == 0)
	align = 16;
      else if ((align & 0x7) == 0)
	align = 8;
      else if ((align & 0x3) == 0)
	align = 4;
      else if ((align & 0x1) == 0)
	align = 2;
      else
	align = 1;
      break;

    case PLUS:
      align = MIN (i960_expr_alignment (XEXP (x, 0), size),
		   i960_expr_alignment (XEXP (x, 1), size));
      break;

    case SYMBOL_REF:
      /* If this is a valid program, objects are guaranteed to be
	 correctly aligned for whatever size the reference actually is.  */
      align = i960_object_bytes_bitalign (size) / BITS_PER_UNIT;
      break;

    case REG:
      if (REGNO (x) == FRAME_POINTER_REGNUM)
	align = 16;
      break;

    case ASHIFT:
      align = i960_expr_alignment (XEXP (x, 0), size);

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  align = align << INTVAL (XEXP (x, 1));
	  align = MIN (align, 16);
	}
      break;

    case MULT:
      align = (i960_expr_alignment (XEXP (x, 0), size) *
	       i960_expr_alignment (XEXP (x, 1), size));

      align = MIN (align, 16);
      break;
    default:
      break;
    }

  return align;
}

/* Return true if it is possible to reference both BASE and OFFSET, which
   have alignment at least as great as 4 byte, as if they had alignment valid
   for an object of size SIZE.  */

int
i960_improve_align (rtx base, rtx offset, int size)
{
  int i;

  /* We have at least a word reference to the object, so we know it has to
     be aligned at least to 4 bytes.  */

  i = MIN (i960_expr_alignment (base, 4),
	   i960_expr_alignment (offset, 4));
  i = MAX (i, 4);
  return (i >= size);
}

/* Return true if it is possible to access BASE and OFFSET, which have 4 byte
   (SImode) alignment as if they had 16 byte (TImode) alignment.  */

int
i960_si_ti (rtx base, rtx offset)
{
  return i960_improve_align (base, offset, 16);
}

/* Return true if it is possible to access BASE and OFFSET, which have 4 byte
   (SImode) alignment as if they had 8 byte (DImode) alignment.  */

int
i960_si_di (rtx base, rtx offset)
{
  return i960_improve_align (base, offset, 8);
}

/* Return raw values of size and alignment (in words) for the data
   type being accessed.  These values will be rounded by the caller.  */

static void 
i960_arg_size_and_align (enum machine_mode mode, tree type, int* size_out, int* align_out)
{
  int size, align;

  /* Use formal alignment requirements of type being passed, except make
     it at least a word.  If we don't have a type, this is a library call,
     and the parm has to be of scalar type.  In this case, consider its
     formal alignment requirement to be its size in words.  */

  if (mode == BLKmode) {
      size = ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
  } else if (mode == VOIDmode) {
      /* End of parm list.  */
      if (type == 0 || TYPE_MODE (type) != VOIDmode) {
          abort ();
      }
      size = 1;
  } else {
      size = ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
  }

  if (type == 0) {
      align = size;
  } else if (TYPE_ALIGN (type) >= BITS_PER_WORD) {
      align = TYPE_ALIGN (type) / BITS_PER_WORD;
  } else {
      align = 1;
  }

  *size_out  = size;
  *align_out = align;
}

/* On the 80960 the first 12 args are in registers and the rest are pushed.
   Any arg that is bigger than 4 words is placed on the stack and all
   subsequent arguments are placed on the stack.

   Additionally, parameters with an alignment requirement stronger than
   a word must be aligned appropriately.  Note that this means that a
   64 bit object with a 32 bit alignment is not 64 bit aligned and may be
   passed in an odd/even register pair.  */

/* Update CUM to advance past an argument described by MODE and TYPE.  */

void 
i960_function_arg_advance (cumulative_args_t cat, const class function_arg_info& info)
{
  int size, align;
  i960_arg_size_and_align (info.mode, info.type, &size, &align);

CUMULATIVE_ARGS *cum = get_cumulative_args (cat);
if (size > 4 || cum->ca_nstackparms != 0
      || (size + ROUND_PARM (cum->ca_nregparms, align)) > NPARM_REGS
      || TARGET_MUST_PASS_IN_STACK (info))
    {
      /* Indicate that all the registers are in use, even if all are not,
	 so va_start will compute the right value.  */
      cum->ca_nregparms = NPARM_REGS;
      cum->ca_nstackparms = ROUND_PARM (cum->ca_nstackparms, align) + size;
    }
  else
    cum->ca_nregparms = ROUND_PARM (cum->ca_nregparms, align) + size;
}

/* Return the register that the argument described by MODE and TYPE is
   passed in, or else return 0 if it is passed on the stack.  */

rtx
i960_function_arg (cumulative_args_t cat,/* enum machine_mode mode, tree type, int named*/ const function_arg_info& info)
{
  rtx ret;
  int size, align;

  if (info.mode == VOIDmode)
    return 0;

  i960_arg_size_and_align (info.mode, info.type, &size, &align);

CUMULATIVE_ARGS *cum = get_cumulative_args (cat);
  if (size > 4 || cum->ca_nstackparms != 0
      || (size + ROUND_PARM (cum->ca_nregparms, align)) > NPARM_REGS
      || TARGET_MUST_PASS_IN_STACK (info))
    {
      cum->ca_nstackparms = ROUND_PARM (cum->ca_nstackparms, align);
      ret = 0;
    }
  else
    {
      cum->ca_nregparms = ROUND_PARM (cum->ca_nregparms, align);
      ret = gen_rtx_REG (info.mode, cum->ca_nregparms);
    }

  return ret;
}

/* Return the number of bits that an object of size N bytes is aligned to.  */

int
i960_object_bytes_bitalign (int n)
{
  if (n > 8)      n = 128;
  else if (n > 4) n = 64;
  else if (n > 2) n = 32;
  else if (n > 1) n = 16;
  else            n = 8;

  return n;
}

/* Compute the alignment for an aggregate type TSIZE.
   Alignment is MAX (greatest member alignment,
                     MIN (pragma align, structure size alignment)).  */

int
i960_round_align (int align, tree type)
{
  //if (TARGET_OLD_ALIGN || TYPE_PACKED (type))
  //  return align;
  if (TREE_CODE (type) != RECORD_TYPE)
    return align;
  tree tsize = TYPE_SIZE (type);

  if (! tsize || TREE_CODE (tsize) != INTEGER_CST)
    return align;

  int new_align = i960_object_bytes_bitalign (TREE_INT_CST_LOW (tsize)
					  / BITS_PER_UNIT);
  /* Handle #pragma align.  */
  if (new_align > i960_maxbitalignment)
    new_align = i960_maxbitalignment;

  if (align < new_align)
    align = new_align;

  return align;
}

/* Do any needed setup for a varargs function.  For the i960, we must
   create a register parameter block if one doesn't exist, and then copy
   all register parameters to memory.  */

void 
i960_setup_incoming_varargs (cumulative_args_t cat, const class function_arg_info& info, int * pretend_size, int no_rtl)
{
  /* Note: for a varargs fn with only a va_alist argument, this is 0.  */
  CUMULATIVE_ARGS *cum = get_cumulative_args (cat);
  int first_reg = cum->ca_nregparms;

  /* Copy only unnamed register arguments to memory.  If there are
     any stack parms, there are no unnamed arguments in registers, and
     an argument block was already allocated by the caller.
     Remember that any arg bigger than 4 words is passed on the stack as
     are all subsequent args.

     If there are no stack arguments but there are exactly NPARM_REGS
     registers, either there were no extra arguments or the caller
     allocated an argument block.  */

  if (cum->ca_nstackparms == 0 && first_reg < NPARM_REGS && !no_rtl)
    {
      rtx label = gen_label_rtx ();
      rtx regblock, fake_arg_pointer_rtx;

      /* Use a different rtx than arg_pointer_rtx so that cse and friends
	 can go on believing that the argument pointer can never be zero.  */
      fake_arg_pointer_rtx = gen_raw_REG (Pmode, ARG_POINTER_REGNUM);

      /* If the argument pointer is 0, no arguments were passed on the stack
	 and we need to allocate a chunk to save the registers (if any
	 arguments were passed on the stack the caller would allocate the
	 48 bytes as well).  We must allocate all 48 bytes (12*4) because
	 va_start assumes it.  */
      // do a cmpsi of g14 with 0
      emit_insn (gen_cmpsi (fake_arg_pointer_rtx, const0_rtx));
      // bne to target label
      emit_jump_insn (gen_bne (label));
      // set g14 to sp, it allows it be passed to another function as needed
      emit_insn (gen_rtx_SET (fake_arg_pointer_rtx,
			      stack_pointer_rtx));
      // set stack pointer to 48 + sp
      emit_insn (gen_rtx_SET (stack_pointer_rtx,
			      memory_address (SImode,
					      plus_constant (info.mode,
                                         stack_pointer_rtx, 48))));
      // emit the label
      emit_label (label);

      /* ??? Note that we unnecessarily store one extra register for stdarg
	 fns.  We could optimize this, but it's kept as for now.  */
      regblock = gen_rtx_MEM (BLKmode,
			      plus_constant (info.mode, arg_pointer_rtx, first_reg * 4));
      set_mem_alias_set (regblock, get_varargs_alias_set ());
      set_mem_align (regblock, BITS_PER_WORD);
      move_block_from_reg (first_reg, regblock,
			   NPARM_REGS - first_reg);
    }
}
/* Define the `__builtin_va_list' type for the ABI.  */

static tree
i960_build_builtin_va_list ()
{
    // generate an array that can accept up to one item
  return build_array_type (unsigned_type_node,
			   build_index_type (size_one_node));
}
/*
 * According to the compiler documentation, g14 is defined as:
 *
 * If the function requires an argument block, this register contains a pointer to the argument block;
 * otherwise it contains zero. If g14 contains zero upon entry, then it must contain zero upon exit.
 * If g14 contains a pointer to an argument block upon function entry, then g14 is considered a call-scratch register.
 *
 * g14 may also be used to hold the return address when a function is called using a BAL instruction. In this case,
 * g14 must contain zero upon return from the function. This dual usage of g14 means that a function that requires an
 * argument block cannot be entered using a bal instruction.
 *
 *
 * -------
 * My commentary (2024/12/8):
 *
 * I believe that this dual usage of g14 leads to massive amounts of pain. I realize that Intel has designed this calling
 * convention so that you can pass up to 12 parameters through registers. The problem with this concept is that g14 becomes
 * a bottleneck in the design.
 *
 * However, the design seems to be relatively straightforward so I will keep using it.
 *
 */
void
i960_va_start (tree valist, rtx nextarg)
{
    tree t, base, num;
    rtx fake_arg_pointer_rtx;
    // so va_start is defined as void va_start(va_list ap, parm_n)
    // where ap is the va_list to populate with the contents of the name provided by parm_n
    /* The array type always decays to a pointer before we get here, so we
       can't use ARRAY_REF.  */
    // *valist = g14;
    // construct an expression tree that points to the va_list itself indirectly?
    base = build1 (INDIRECT_REF, unsigned_type_node, valist);
    // construct an expression which is the valist plus the unit size of the valist itself
    num = build1 (INDIRECT_REF, unsigned_type_node,
                  build2 (PLUS_EXPR, unsigned_type_node, valist, TYPE_SIZE_UNIT (TREE_TYPE (valist))));
    // Because of the way that g14 is used, we can assume that g14 has been cleaned up via incoming_varargs
    /* Use a different rtx than arg_pointer_rtx so that cse and friends
       can go on believing that the argument pointer can never be zero.  */
    fake_arg_pointer_rtx = gen_raw_REG (Pmode, ARG_POINTER_REGNUM);
    // make a tree out of this argument block pointer (g14)
    tree s = make_tree(unsigned_type_node, fake_arg_pointer_rtx);
    // now we want to modify base to be what was in g14 previously (the argument block pointer)
    t = build2 (MODIFY_EXPR, unsigned_type_node, base, s);
    TREE_SIDE_EFFECTS (t) = 1;
    expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    // *(valist + ?) = (ca_nregparms + ca_nstackparms) * 4
    t = build2 (MODIFY_EXPR, unsigned_type_node, num,
                  build_int_cst (integer_type_node, (current_function_args_info.ca_nregparms
                                                         + current_function_args_info.ca_nstackparms) * 4));
    TREE_SIDE_EFFECTS (t) = 1;
    expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Calculate the final size of the reg parm stack space for the current
   function, based on how many bytes would be allocated on the stack.  */

int
i960_final_reg_parm_stack_space (int const_size, tree var_size)
{
  if (var_size || const_size > 48)
    return 48;
  else
    return 0;
}

/* Calculate the size of the reg parm stack space.  This is a bit complicated
   on the i960.  */

int
i960_reg_parm_stack_space (tree fndecl)
{
  /* In this case, we are called from emit_library_call, and we don't need
     to pretend we have more space for parameters than what's apparent.  */
  if (fndecl == 0)
    return 0;

  /* In this case, we are called from locate_and_pad_parms when we're
     not IN_REGS, so we have an arg block.  */
  if (fndecl != current_function_decl)
    return 48;

  /* Otherwise, we have an arg block if the current function has more than
     48 bytes of parameters.  */
  if (current_function_args_size != 0 || VARARGS_STDARG_FUNCTION (fndecl))
    return 48;

  return 0;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
i960_secondary_reload_class (enum reg_class theClass, enum machine_mode mode, rtx in)
{
  int regno = -1;

  if (GET_CODE (in) == REG || GET_CODE (in) == SUBREG)
    regno = true_regnum (in);

  /* We can place anything into LOCAL_OR_GLOBAL_REGS and can put
     LOCAL_OR_GLOBAL_REGS into anything.  */
  if (theClass == LOCAL_OR_GLOBAL_REGS || theClass == LOCAL_REGS
      || theClass == GLOBAL_REGS || (regno >= 0 && regno < 32))
    return NO_REGS;

  /* We can place any hard register, 0.0, and 1.0 into FP_REGS.  */
  if (theClass == FP_REGS
      && ((regno >= 0 && regno < FIRST_PSEUDO_REGISTER)
	  || in == CONST0_RTX (mode) || in == CONST1_RTX (mode)))
    return NO_REGS;

  return LOCAL_OR_GLOBAL_REGS;
}

/* Look at the opcode P, and set i96_last_insn_type to indicate which
   function unit it executed on.  */

/* ??? This would make more sense as an attribute.  */

void
i960_scan_opcode (const char* p)
{
  switch (*p)
    {
    case 'a':
    case 'd':
    case 'e':
    case 'm':
    case 'n':
    case 'o':
    case 'r':
      /* Ret is not actually of type REG, but it won't matter, because no
	 insn will ever follow it.  */
    case 'u':
    case 'x':
      i960_last_insn_type = I_TYPE_REG;
      break;

    case 'b':
      if (p[1] == 'x' || p[3] == 'x')
        i960_last_insn_type = I_TYPE_MEM;
      i960_last_insn_type = I_TYPE_CTRL;
      break;

    case 'f':
    case 't':
      i960_last_insn_type = I_TYPE_CTRL;
      break;

    case 'c':
      if (p[1] == 'a')
	{
	  if (p[4] == 'x')
	    i960_last_insn_type = I_TYPE_MEM;
	  else
	    i960_last_insn_type = I_TYPE_CTRL;
	}
      else if (p[1] == 'm')
	{
	  if (p[3] == 'd')
	    i960_last_insn_type = I_TYPE_REG;
	  else if (p[4] == 'b' || p[4] == 'j')
	    i960_last_insn_type = I_TYPE_CTRL;
	  else
	    i960_last_insn_type = I_TYPE_REG;
	}
      else
        i960_last_insn_type = I_TYPE_REG;
      break;

    case 'l':
      i960_last_insn_type = I_TYPE_MEM;
      break;

    case 's':
      if (p[1] == 't')
        i960_last_insn_type = I_TYPE_MEM;
      else
        i960_last_insn_type = I_TYPE_REG;
      break;
    }
}

static void
i960_output_mi_thunk (FILE* file, tree /*thunk*/, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, tree function)
{
    int d = delta;
    if (d < 0 && d > -32) {
        fprintf (file, "\tsubo %d,g0,g0\n", -d);
    } else if (d > 0 && d < 32) {
        fprintf (file, "\taddo %d,g0,g0\n", d);
    } else {
        fprintf (file, "\tldconst %d,r5\n", d);
        fprintf (file, "\taddo r5,g0,g0\n");
    } 
    fprintf (file, "\tbx ");
    assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));	
    fprintf (file, "\n");
}

static bool
i960_rtx_costs (rtx x, machine_mode, int code, int outer_code, int* total, bool)
{
  switch (code)
    {
      /* Constants that can be (non-ldconst) insn operands are cost 0.
	 Constants that can be non-ldconst operands in rare cases are cost 1.
         Other constants have higher costs.

         Must check for OUTER_CODE of SET for power2_operand, because
         reload_cse_move2add calls us with OUTER_CODE of PLUS to decide
	 when to replace set with add.  */

    case CONST_INT:
      if ((INTVAL (x) >= 0 && INTVAL (x) < 32)
	  || (outer_code == SET && power2_operand (x, VOIDmode)))
	{
	  *total = 0;
	  return true;
	}
      else if (INTVAL (x) >= -31 && INTVAL (x) < 0)
	{
	  *total = 1;
	  return true;
	}
      /* FALLTHRU */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      //*total = (TARGET_C_SERIES ? 6 : 8);
      *total = 8;
      return true;

    case CONST_DOUBLE:
      if (x == CONST0_RTX (DFmode) || x == CONST0_RTX (SFmode)
	  || x == CONST1_RTX (DFmode) || x == CONST1_RTX (SFmode))
	*total = 1;
      else
	*total = 12;
      return true;

    default:
      return false;
    }
}
namespace {
    constexpr bool isHardRegister(unsigned int index) noexcept {
        return index < 32;
    }
    constexpr bool isFloatingPointRegister(unsigned int index) noexcept {
        return index >= 32 && index < 36;
    }
}
static unsigned int
i960_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On 80960, ordinary registers hold 32 bits worth, but can be ganged
   together to hold double or extended precision floating point numbers,
   and the floating point registers hold any size floating point number */
    if (isHardRegister(regno)) {
        if (mode == VOIDmode) {
            return 1;
        } else {
            return ((GET_MODE_SIZE(mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
        }
    } else {
        if (regno < FIRST_PSEUDO_REGISTER) {
            return 1;
        } else {
            return 0;
        }
    }
}
bool i960_modes_tieable_p			(machine_mode mode1, machine_mode mode2) {
/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
    return (mode1 == mode2) || GET_MODE_CLASS(mode1) == GET_MODE_CLASS(mode2);
}
static bool
i960_hard_regno_mode_ok (unsigned int regno, machine_mode mode) {
/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On 80960, the cpu registers can hold any mode but the float registers
   can only hold SFmode, DFmode, or TFmode.  */
    if (isHardRegister(regno)) {
        switch (mode) {
            case CCmode:
            case CC_UNSmode:
            case CC_CHKmode:
                return false;

            case DImode:
            case DFmode:
                return (regno & 1) == 0;

            case TImode:
            case TFmode:
                return (regno & 3) == 0;

            default:
                return true;
        }
    } else if (isFloatingPointRegister(regno)) {
        switch (mode) {
            case SFmode: 
            case DFmode: 
            case TFmode:
            case SCmode: 
            case DCmode:
                return true;
            default:
                return false;
        }
    } else if (regno == 36) {
        switch (mode) {
            case CCmode: 
            case CC_UNSmode: 
            case CC_CHKmode:
                return true;

            default:
                return false;
        }
    } else if (regno == 37) {
        return false;
    }
    return false;
}
static rtx
i960_legitimize_address (rtx x,
			rtx oldx,
			machine_mode mode)
{
    (void)oldx;
    (void)mode;
/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

/* On 80960, convert non-canonical addresses to canonical form.  */
//#define TARGET_LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)	
//{ rtx orig_x = (X);				
//  (X) = legitimize_address (X, OLDX, MODE);	
//  if ((X) != orig_x && memory_address_p (MODE, X)) 
//    goto WIN; }

  if (GET_CODE (x) == SYMBOL_REF)
    {
      abort ();
      x = copy_to_reg (x);
    }

  if (! TARGET_COMPLEX_ADDR && ! reload_completed)
    return x;

  /* Canonicalize (plus (mult (reg) (const)) (plus (reg) (const)))
     into (plus (plus (mult (reg) (const)) (reg)) (const)).  This can be
     created by virtual register instantiation, register elimination, and
     similar optimizations.  */
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT
      && GET_CODE (XEXP (x, 1)) == PLUS)
    x = gen_rtx_PLUS (Pmode,
		      gen_rtx_PLUS (Pmode, XEXP (x, 0), XEXP (XEXP (x, 1), 0)),
		      XEXP (XEXP (x, 1), 1));

  /* Canonicalize (plus (plus (mult (reg) (const)) (plus (reg) (const))) const)
     into (plus (plus (mult (reg) (const)) (reg)) (const)).  */
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS
           && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
           && GET_CODE (XEXP (XEXP (x, 0), 1)) == PLUS
           && CONSTANT_P (XEXP (x, 1)))
  {
      rtx constant, other;

      if (GET_CODE (XEXP (x, 1)) == CONST_INT) {
          constant = XEXP (x, 1);
          other = XEXP (XEXP (XEXP (x, 0), 1), 1);
      } else if (GET_CODE (XEXP (XEXP (XEXP (x, 0), 1), 1)) == CONST_INT) {
          constant = XEXP (XEXP (XEXP (x, 0), 1), 1);
          other = XEXP (x, 1);
      } else {
          constant = 0;
          other = 0;
      }

      if (constant) {
          x = gen_rtx_PLUS(Pmode,
                           gen_rtx_PLUS(Pmode, XEXP (XEXP(x, 0), 0),
                                        XEXP (XEXP(XEXP(x, 0), 1), 0)),
                           plus_constant(Pmode, other, INTVAL (constant)));
      }
  }

  return x;
}
bool
i960_truly_noop_truncation (poly_uint64 outprec, poly_uint64 inprec)
{
    (void)outprec;
    (void)inprec;
/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
    return true;
}
bool
i960_legitimate_constant_p (machine_mode, rtx x)
{
/* LEGITIMATE_CONSTANT_P is nonzero if the constant value X
   is a legitimate general operand.
   It is given that X satisfies CONSTANT_P.

   Anything but a CONST_DOUBLE can be made to work, excepting 0.0 and 1.0.

   ??? This probably should be defined to 1.  */
    return (GET_CODE(x) != CONST_DOUBLE) || i960_fp_literal(x, GET_MODE(x));
}
static HOST_WIDE_INT
i960_constant_alignment (const_tree exp, HOST_WIDE_INT basic_align)
{
//#define TARGET_CONSTANT_ALIGNMENT(EXP, ALIGN) \
//  (TREE_CODE (EXP) == STRING_CST	\
//   && i960_object_bytes_bitalign (int_size_in_bytes (TREE_TYPE (EXP))) > (int)(ALIGN) \
//   ? i960_object_bytes_bitalign (int_size_in_bytes (TREE_TYPE (EXP)))	    \
//   : (int)(ALIGN))
/* Specify alignment for string literals (which might be higher than the
   base type's minimal alignment requirement.  This allows strings to be
   aligned on word boundaries, and optimizes calls to the str* and mem*
   library functions.  */
    if (TREE_CODE(exp) == STRING_CST && (i960_object_bytes_bitalign(int_size_in_bytes(TREE_TYPE(exp))) > basic_align)) {
        return i960_object_bytes_bitalign(int_size_in_bytes(TREE_TYPE(exp)));
    } else {
        return basic_align;
    }
}
static unsigned int 
i960_function_arg_boundary (machine_mode mode, const_tree type) {
/* Indicate the alignment boundary for an argument of the specified mode and
   type.  */
    /// @todo fix this to be normal code
#define X(MODE, TYPE)				\
  (((TYPE) != 0)							\
   ? ((TYPE_ALIGN (TYPE) <= PARM_BOUNDARY)				\
      ? PARM_BOUNDARY							\
      : TYPE_ALIGN (TYPE))						\
   : ((GET_MODE_ALIGNMENT (MODE) <= PARM_BOUNDARY)			\
      ? PARM_BOUNDARY							\
      : GET_MODE_ALIGNMENT (MODE)))
    return X(mode, type);
#undef X
}
/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On 80960, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
//#define TARGET_CLASS_MAX_NREGS(CLASS, MODE)					\
//  ((CLASS) == FP_REGS ? 1 : TARGET_HARD_REGNO_NREGS (0, (MODE)))
//#define TARGET_STARTING_FRAME_OFFSET 64
static HOST_WIDE_INT i960_starting_frame_offset(void) { return 64; }

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE i960_option_override
#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS i960_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK i960_hard_regno_mode_ok
#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P i960_modes_tieable_p
#undef TARGET_LEGITIMIZE_ADDRESS 
#define TARGET_LEGITIMIZE_ADDRESS i960_legitimize_address
#undef TARGET_TRULY_NOOP_TRUNCATION 
#define TARGET_TRULY_NOOP_TRUNCATION i960_truly_noop_truncation
#undef TARGET_LEGITIMATE_CONSTANT_P 
#define TARGET_LEGITIMATE_CONSTANT_P i960_legitimate_constant_p
#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT i960_constant_alignment
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS i960_setup_incoming_varargs
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE i960_function_arg_advance
#undef TARGET_FUNCTION_ARG 
#define TARGET_FUNCTION_ARG i960_function_arg
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY i960_function_arg_boundary
/// @todo reactivate
#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START i960_va_start
#undef TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET i960_starting_frame_offset
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE i960_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE i960_output_function_epilogue
#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK i960_output_mi_thunk
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS i960_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST i960_address_cost
#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST i960_build_builtin_va_list
// still use the old condition code stuff in the .md file so disable LRA
#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false
#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE i960_conditional_register_usage

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED i960_frame_pointer_required


struct gcc_target targetm = TARGET_INITIALIZER;
#include "gt-i960.h"

/*
 * Some important terminology from the i960's CTOOLS manual
 *
 * From the manual itself:
 * Argument Block:
 *      An argument block is used to pass parameters when the parameters cannot be passed in registers. This can occur either
 *      because there are not enough registers left to pass the parameter, or when the parameter is too large
 *      (greater than 4 words) to pass in registers. As soon as a parameter is passed in an argument block, all further parameters get passed in the argument
 *      block. The calling function is responsible for the creation of an argument block if one is needed. When an argument block is created it must contain enough
 *      space at the beginning to store all possible parameter registers g0-g11. Thus the first 48 bytes of an argument block are reserved for
 *      storing these registers. The first parameter passed in the argument block starts at an address 48 bytes above the base of the argument block.
 */
