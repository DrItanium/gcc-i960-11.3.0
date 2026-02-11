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
#include "print-tree.h"
#include "gimple.h"
#include "gimplify.h"
// include last
#include "target-def.h"

static HOST_WIDE_INT i960_get_current_function_args_size(void);
#define current_function_args_size (i960_get_current_function_args_size())
#define current_function_args_info (crtl->args.info)
#define current_function_stdarg (cfun->stdarg)
#define compat_STARTING_FRAME_OFFSET 64
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

/* Used to implement #pragma align/noalign.  Initialized by OVERRIDE_OPTIONS
   macro in i960.h.  */

int i960_maxbitalignment;
int i960_last_maxbitalignment;


/* A string containing a list of insns to emit in the epilogue so as to
   restore all registers saved by the prologue.  Created by the prologue
   code as it saves registers away.  */

char epilogue_string[1000];

/* A unique number (per function) for return labels.  */

static int ret_label = 0;

/* This is true if FNDECL is either a varargs or a stdarg function.
   This is used to help identify functions that use an argument block.  */

#define VARARGS_STDARG_FUNCTION(FNDECL)	(stdarg_p(TREE_TYPE(FNDECL)))


int i960_has_numerics = false;
int i960_has_protected = false;
int i960_has_complex_addr = false;
int i960_has_branch_predict = false;
// C series and later is superscalar but I'm not supporting that right now

struct i960_processor_info {
    i960_processor_type arch;
    const char* name;
    const char* description;
    unsigned int features;
};

static const struct i960_processor_info i960_processors[] = {
    { ARCH_KA, "ka", "i960 KA", I960_FEATURE_COMPLEX_ADDRESSING },
    { ARCH_KB, "kb", "i960 KB", I960_FEATURE_COMPLEX_ADDRESSING | I960_FEATURE_NUMERICS },
    { ARCH_KC, "kc", "i960 KC", I960_FEATURE_COMPLEX_ADDRESSING | I960_FEATURE_NUMERICS | I960_FEATURE_PROTECTED },
    { ARCH_SA, "sa", "i960 SA", I960_FEATURE_COMPLEX_ADDRESSING },
    { ARCH_SB, "sb", "i960 SB", I960_FEATURE_COMPLEX_ADDRESSING | I960_FEATURE_NUMERICS },
    { ARCH_SC, "sc", "i960 SC", I960_FEATURE_COMPLEX_ADDRESSING | I960_FEATURE_NUMERICS | I960_FEATURE_PROTECTED },
    { ARCH_MC, "mc", "i960 MC", I960_FEATURE_COMPLEX_ADDRESSING | I960_FEATURE_NUMERICS | I960_FEATURE_PROTECTED },
    { (i960_processor_type) 0, nullptr, nullptr, 0 },
};
static const i960_processor_info&
i960_get_processor_info() 
{
    return i960_processors[i960_arch];
}
static bool
i960_processor_has_feature(unsigned int feature) 
{
    const i960_processor_info& proc = i960_get_processor_info();
    return (proc.features & feature) != 0;
}

static bool
i960_resolve_option(int optionValue, unsigned int cpuFeature, const char* featureName, bool defaultIfCapable) 
{
    bool cpuHasFeature = i960_processor_has_feature(cpuFeature);
    if (optionValue == I960_OPTION_ENABLED) {
        if (!cpuHasFeature) {
            const auto& proc = i960_get_processor_info();
            error ("processor %qs does not support %s", proc.name, featureName);
            return false;
        }
        return true;
    } else if (optionValue == I960_OPTION_DISABLED) {
        return false;
    } else {
        return cpuHasFeature && defaultIfCapable;
    }
}

/* Initialize the GCC target structure.  */

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
i960_init_machine_status ()
{
  return ggc_cleared_alloc<machine_function> ();
}
void
i960_option_override ()
{
  /* Set the per-function-data initializer.  */
  init_machine_status = i960_init_machine_status;
  const auto& proc = i960_get_processor_info();
  // Protected architecture is a superset of the numerics architecture, if you
  // enable protected then also enable numerics.
  if (i960_protected_option == I960_OPTION_ENABLED) {
      if (!i960_processor_has_feature(I960_FEATURE_PROTECTED)) {
          error ("processor %qs does not support protected extensions", proc.name);
          i960_protected_option = I960_OPTION_DISABLED;
      } else {
          if (i960_float_abi == FLOAT_ABI_SOFT) {
              // protected implies that we have an FPU
              error("%<-mprotected%> requires %<-mhard-float%> (numerics extensions)");
          }
          i960_float_abi = FLOAT_ABI_HARD;
      }
  }
  // resolve numerics based on floating point ABI
  if (TARGET_HARD_FLOAT) {
    // user wants hardware FPU -- check and see if it is legal
    if (!i960_processor_has_feature(I960_FEATURE_NUMERICS)) {
        if (global_options_set.x_i960_float_abi) {
            error("processor %qs does not support hardware floating-point", proc.name);
        }
        // fall back to soft float
        i960_float_abi = FLOAT_ABI_SOFT;
        i960_has_numerics = false;
    } else {
        i960_has_numerics = true;
    }
  } else {
    // target soft_float
    i960_has_numerics = false;
  }
  // resolve protected extensions
  i960_has_protected = i960_resolve_option( i960_protected_option, I960_FEATURE_PROTECTED, "protected extensions", true);
  if (i960_has_protected && !i960_has_numerics) {
      error("protected extensions require numerics extensions (hardware floating-point)");
      i960_has_protected = false;
  }

  i960_has_complex_addr = i960_resolve_option( i960_complex_addr_option, I960_FEATURE_COMPLEX_ADDRESSING, "complex addressing modes", true);
  i960_has_branch_predict = i960_resolve_option( i960_branch_predict_option, I960_FEATURE_BRANCH_PREDICT, "branch prediction", true);
  if (!global_options_set.x_i960_float_abi) {
      if (i960_processor_has_feature(I960_FEATURE_NUMERICS)) {
          i960_float_abi = FLOAT_ABI_HARD;
      } else {
          i960_float_abi = FLOAT_ABI_SOFT;
      }
  }
}
/* Override conflicting target switch options.
   Doesn't actually detect if more than one -mARCH option is given, but
   does handle the case of two blatantly conflicting -mARCH options.

   Also initialize variables before compiling any files.  */

void
i960_initialize ()
{
    i960_maxbitalignment = 128;
    i960_last_maxbitalignment = 8;
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
    return true;
}
/* Return true if OP can be used as the source of an fp move insn.  */

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

bool
i960_symbolic_memory_operand (rtx op, enum machine_mode)
{
    rtx tmp = op;
    if (GET_CODE(tmp) == SUBREG) {
        tmp = SUBREG_REG(tmp);
    }
    if (GET_CODE(tmp) != MEM) {
        return false;
    }
    tmp = XEXP(tmp, 0);
    switch (GET_CODE(tmp)) {
        case SYMBOL_REF:
        case CONST:
        case HIGH:
        case LABEL_REF:
            return true;
        default:
            return false;
    }
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
i960_bitpos (unsigned int val) {
    // we actually have a bounded set of elements to check against
    switch (val) {
#define X(index) case (1 << index) : return index
        X(0); X(1); X(2); X(3); X(4); X(5); X(6); X(7);
        X(8); X(9); X(10); X(11); X(12); X(13); X(14); X(15);
        X(16); X(17); X(18); X(19); X(20); X(21); X(22); X(23);
        X(24); X(25); X(26); X(27); X(28); X(29); X(30); X(31);
#undef X
        default:
            return -1;
    }
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
    switch (op) {
        case GTU:
        case LTU:
        case GEU:
        case LEU:
            return CC_UNSmode;
        default:
            return CCmode;
    }
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 36 in the proper mode.  */

rtx
i960_gen_compare_reg (enum rtx_code code, rtx x, rtx y)
{
  machine_mode ccmode = SELECT_CC_MODE (code, x, y);
  machine_mode mode
    = GET_MODE (x) == VOIDmode ? GET_MODE (y) : GET_MODE (x);

    if (mode == SImode) {
        // if x is not an arithmetic operand then force x to be in a
        // hardware register
        if (! arith_operand (x, mode)) {
            x = force_reg(SImode, x);
        }
        // if y is not an arithmetic operand then force y to be in a
        // hardware register
        if (! arith_operand (y, mode)) {
            y = force_reg(SImode, y);
        }
    }

  rtx cc_reg = gen_rtx_REG (ccmode, 36);
  // emit the compare instruction itself
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
  if (GET_CODE (x) == CONST_INT && INTVAL (x) >= 0 && INTVAL (x) < 4096) {
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
              unsigned uOffsetValue = INTVAL(offset);
              if (uOffsetValue < 2047) {
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

   Return true if we have written out everything that needs to be done to
   do the move.  Otherwise, return false and the caller will emit the move
   normally.  */
namespace {
constexpr bool
isMultiWordSequence(unsigned int value) noexcept {
    return value > UNITS_PER_WORD;
}
constexpr bool isNonPseudoRegister(unsigned int value) noexcept {
    return value < FIRST_PSEUDO_REGISTER;
}
constexpr bool isPseudoRegister(unsigned int value) noexcept {
    return value >= FIRST_PSEUDO_REGISTER;
}

template<typename T>
bool isOperandType(T value, decltype(MEM) kind) noexcept {
    return GET_CODE(value) == kind;
}
template<typename T> bool isMemoryOperand(T value) noexcept { return isOperandType<T>(value, MEM); }
template<typename T> bool isRegisterOperand(T value) noexcept { return isOperandType<T>(value, REG); }
template<typename T>
bool isNonPseudoRegister(T value) noexcept {
    return isNonPseudoRegister(REGNO(value));
}
template<typename T>
bool isPseudoRegister(T value) noexcept {
    return isPseudoRegister(REGNO(value));
}
constexpr bool isMultiWordSequence(machine_mode mode) noexcept {
    return isMultiWordSequence(GET_MODE_SIZE(mode));
}
}
bool
i960_emit_move_sequence (rtx* operands, machine_mode mode)
{
  /* We can only store registers to memory.  */
  
    if (isMemoryOperand(operands[0]) && !isRegisterOperand(operands[1]) &&
        (operands[1] != const0_rtx 
                || current_function_args_size
                || current_function_stdarg
                || currently_expanding_to_rtl)) {
        /* Here we use the same test as movsi+1 pattern -- see i960.md.  */
        operands[1] = force_reg (mode, operands[1]);
    }

  /* Storing multi-word values in unaligned hard registers to memory may
     require a scratch since we have to store them a register at a time and
     adding 4 to the memory address may not yield a valid insn.  */
  // the above design is a good idea to be maximally compatible.
  /* ??? We don't always need the scratch, but that would complicate things.
     Maybe later.  */
  /* ??? We must also handle stores to pseudos here, because the pseudo may be
     replaced with a MEM later.  This would be cleaner if we didn't have
     a separate pattern for unaligned DImode/TImode stores.  */
  if (isMultiWordSequence(mode)
      && (isMemoryOperand(operands[0]) 
	  || (isRegisterOperand(operands[0])
          && isPseudoRegister(REGNO(operands[0]))))
      && isRegisterOperand(operands[1])
      && isNonPseudoRegister(operands[1])
      && ! TARGET_HARD_REGNO_MODE_OK (REGNO (operands[1]), mode))
    {
        // what the hell is this?
        emit_insn (gen_rtx_PARALLEL
                (VOIDmode,
                 gen_rtvec (2,
                     gen_rtx_SET (operands[0], operands[1]),
                     gen_rtx_CLOBBER (VOIDmode,
                         gen_rtx_SCRATCH (Pmode)))));
        return true;
    }


  return false;
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
constexpr bool pairMatches(rtx_code a, rtx_code aExpect, rtx_code b, rtx_code bExpect) noexcept {
    return a == aExpect && b == bExpect;
}
bool pairMatches(rtx a, rtx_code aExpect, rtx b, rtx_code bExpect) noexcept {
    return pairMatches(GET_CODE(a), aExpect, GET_CODE(b), bExpect);
}
/* Output assembler to move a double word value.  */
const char *
i960_output_move_double (rtx dst, rtx src) {
    rtx operands[5];
    /// @todo cleanup the if conditionals?
    if (pairMatches(dst, REG, src, REG)) {
        if ((REGNO (src) & 1) || (REGNO (dst) & 1)) {
            /* We normally copy the low-numbered register first.  However, if
               the second source register is the same as the first destination
               register, we must copy in the opposite order.  */
            if (REGNO (src) + 1 == REGNO (dst)) {
                return "mov\t%D1,%D0\n\tmov	%1,%0";
            } else {
                return "mov\t%1,%0\n\tmov\t%D1,%D0";
            }
        } else {
            return "movl\t%1,%0";
        }
    } else if (pairMatches(dst, REG, src, CONST_INT)
            && TARGET_CONST_OK_FOR_LETTER_P(INTVAL (src), 'I')) {
        if (REGNO (dst) & 1) {
            return "mov	%1,%0\n\tmov\t0,%D0";
        } else {
            return "movl\t%1,%0";
        }
    } else if (pairMatches(dst, REG, src, MEM)) {
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
                ("lda\t%1,%2\n\tld\t%3,%0\n\tld\t%4,%D0", operands);
            return "";
        } else {
            return "ldl	%1,%0";
        }
    } else if (pairMatches(dst, MEM, src, REG)) {
        if (REGNO (src) & 1) {
            operands[0] = dst;
            operands[1] = adjust_address (dst, word_mode, UNITS_PER_WORD);
            if (! memory_address_p (word_mode, XEXP (operands[1], 0))) {
                abort ();
            }
            operands[2] = src;
            output_asm_insn ("st\t%2,%0\n\tst\t%D2,%1", operands);
            return "";
        }
        return "stl\t%1,%0";
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
          output_asm_insn ("lda	%1,%2\n\tld	%3,%0\n\tld	%4,%D0\n\tld	%5,%E0\n\tld	%6,%F0", operands);
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
  operands[1] = adjust_address (dst, word_mode, 4);
  operands[2] = adjust_address (dst, word_mode, 8);
  operands[3] = adjust_address (dst, word_mode, 12);
  output_asm_insn("st\tg14, %0;st\tg14, %1;st\tg14, %2;st\tg14, %3", operands);
  return "";
}


/* Emit insns to load a constant to non-floating point registers.
   Uses several strategies to try to use as few insns as possible.  */

const char *
i960_output_ldconst (rtx dst, rtx src)
{
    // we should be using ldconst as much as possible
    // if it turns out that the assembler isn't up to the challenge then the
    // assembler should be retrofitted to support the other constant modes
    //
    // new assembler mnemonics should come along as well
    //
    // For example, I do not know if ldconst 0.0f, fp0 will do the right thing
    // but we could introduce some extra stuff to assist.
    //
    // ldconstl 0, g0 => movl 0, g0
    // ldconstl 0x89'ABCD'EF01, g0 => ldconst 0xABCDEF01, g0 ; ldconst 0x89, g1
    // ldconstt 0, g0 => movt 0, g0
    // ldconstt 0xABCD'EF01'2345'6789'ABCD'EF01, g0 => 
    //      ldconst 0xABCD'EF01, g0
    //      ldconst 0x2345'6789, g1
    //      ldconst 0xABCD'EF01, g2
    //
    // and so on...
    //
    // This attempt to outsmart the assembler just introduces more places for
    // this frankly fragile code to get even more fragile.
  int rsrc1 = 0;
  unsigned int rsrc2 = 0;
  enum machine_mode mode = GET_MODE (dst);
  rtx operands[4] { dst, src, dst, src };

  /* Anything that isn't a compile time constant, such as a SYMBOL_REF,
     must be a ldconst insn.  */

  if (GET_CODE (src) != CONST_INT && GET_CODE (src) != CONST_DOUBLE)
    {
      output_asm_insn ("ldconst	%1,%0", operands);
      return "";
    }
  else if (mode == TFmode)
    {
      long value_long[3];
      int i;

      if (i960_fp_literal_zero (src, TFmode))
          return "movt	0,%0";

      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*CONST_DOUBLE_REAL_VALUE(src), value_long);
      
      output_asm_insn ("# ldconst	%1,%0",operands);

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
	return "movl	0,%0";

      split_double (src, &first, &second);

      output_asm_insn ("# ldconst	%1,%0",operands);

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

      output_asm_insn ("# ldconst	%1,%0",operands);
      operands[0] = gen_rtx_REG (SImode, REGNO (dst));
      operands[1] = GEN_INT (value);
      output_asm_insn (i960_output_ldconst (operands[0], operands[1]),
		      operands);
      return "";
    }
  else if (mode == TImode)
    {
      rtx low, high;
      /* Note: lowest order word goes in lowest numbered reg.  */
      auto tisrc1 = INTVAL(src);

      if (tisrc1 >= 0 && tisrc1 < 32)
          return "movq	%1,%0";
      else {
          if (GET_CODE(src) == CONST_INT) {
              // otherwise we need to break this up into multiple subwords
              // this is a hack but a good starting point
              split_double(src, &low, &high);
              operands[0] = gen_rtx_REG(SImode, REGNO(dst));
              operands[1] = low;
              output_asm_insn(i960_output_ldconst(operands[0], operands[1]), operands);
              operands[0] = gen_rtx_REG(SImode, REGNO(dst) + 1);
              operands[1] = high;
              output_asm_insn(i960_output_ldconst(operands[0], operands[1]), operands);
              // since we got a CONST_INT it means it can fit within the
              // confines of a single 64-bit number so just emit zeroes after
              // that!
              operands[0] = gen_rtx_REG(DImode, REGNO(dst) + 2);
              operands[1] = const0_rtx; 
              output_asm_insn(i960_output_ldconst(operands[0], operands[1]), operands);
          } else {
              // something beyond a CONST_INT was provided!
            abort();
          }
          return "";
      }
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
          return "movl	%1,%0";
      }

      /* emit the lower half with a recursive call (we don't want to fight with this ever!) */
      xoperands[0] = gen_rtx_REG(SImode, REGNO(dst));
      xoperands[1] = lowerhalf;
      //return "mov	%D1,%0";
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
      // we could just let the assembler handle this instead and not try to
      // outsmart it!
      if (rsrc1 < 32) {
          return "mov	%1,%0";
      }

      /* ldconst	32..63,X	->	add	31,nn,X  */
      // look... once again just emit ldconst and let the assembler handle
      // this...
      if (rsrc1 < 63) {
	  operands[1] = GEN_INT (rsrc1 - 31);
	  output_asm_insn ("addo\t31,%1,%0\t# ldconst %3,%0", operands);
	  return "";
	}
    }
  else if (rsrc1 < 0)
    {
        // again... ldconst in the assembler handles this and also make the
        // code easier to reason with when I do -S
      /* ldconst	-1..-31		->	sub	0,0..31,X  */
      if (rsrc1 >= -31)
	{
	  /* return 'sub -(%1),0,%0' */
	  operands[1] = GEN_INT (- rsrc1);
	  output_asm_insn ("subo\t%1,0,%0\t# ldconst %3,%0", operands);
	  return "";
	}
      
      /* ldconst	-32		->	not	31,X  */
      if (rsrc1 == -32)
	{
	  operands[1] = GEN_INT (~rsrc1);
	  output_asm_insn ("not\t%1,%0	# ldconst %3,%0", operands);
	  return "";
	}
    }
    // once again ldconst should be retrofitted with this if the assembler
    // doesn't emit this sequence...
  /* If const is a single bit.  */
  if (i960_bitpos (rsrc1) >= 0)
    {
      operands[1] = GEN_INT (i960_bitpos (rsrc1));
      output_asm_insn ("setbit\t%1,0,%0\t# ldconst %3,%0", operands);
      return "";
    }

  /* If const is a bit string of less than 6 bits (1..31 shifted).  */
  // I guess at one point the assembler wasn't as good at generating effective
  // ldconst sequences...
  if (i960_is_mask (rsrc1))
    {
      int s, e;

      if (i960_bitstr (rsrc1, &s, &e) < 6)
	{
	  rsrc2 = ((unsigned int) rsrc1) >> s;
	  operands[1] = GEN_INT (rsrc2);
	  operands[2] = GEN_INT (s);
	  output_asm_insn ("shlo\t%2,%1,%0\t# ldconst %3,%0", operands);
	  return "";
	}
    }
  // the following comment is kind of nonsense but perhaps we will expand on it at some point in the future
  /* Unimplemented cases:
     const is in range 0..31 but rotated around end of word:
     ror	31,3,g0	-> ldconst 0xe0000003,g0
   
     and any 2 instruction cases that might be worthwhile  */
    // need to convert a 64-bit constant out to a 32-bit constant
    return "ldconst %a1, %0";
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
  /* Increment global return label.  */

  ret_label++;


  /* Do this after choosing the leaf return register, so it will be listed
     if one was chosen.  */

  fprintf (file, "\t#  Function '%s'\n", (name[0] == '*' ? &name[1] : name));
  fprintf (file, "\t#  Registers used: ");

  for (int i = 0, j = 0; i < FIRST_PSEUDO_REGISTER; i++)
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
  ASM_OUTPUT_LABEL (file, name);
}

namespace {
poly_int64 tryBumpFrameSize(poly_int64 value) noexcept { return (value + 15) & (-16); }

}
/* Compute and return the frame size.  */
int
i960_compute_frame_size (poly_int64 size)
{
  poly_int64 actual_fsize = 0;

  /* The STARTING_FRAME_OFFSET is totally hidden to us as far
     as size is concerned.  */
  actual_fsize = tryBumpFrameSize(size);
  actual_fsize += tryBumpFrameSize(crtl->outgoing_args_size);
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
  for (i = start_reg; i < finish_reg; ) {
      if (regs [i] != state) {
          i++;
          continue;
      } else if (i % 2 != 0 || regs [i + 1] != state) {
          reg_groups [nw].length = 1;
      } else if (i % 4 != 0 || regs [i + 2] != state) {
          reg_groups [nw].length = 2;
      } else if (regs [i + 3] != state) {
          reg_groups [nw].length = 3;
      } else {
          reg_groups [nw].length = 4;
      }
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
    // This code is responsible for generating scaffolding prior to actual code
    // execution of a given function. As it stands right now, the body of the
    // function has not executed anything yet. 
    //
    // We want the compiler to save globals to locals and anything else to the
    // stack.
    //
    // The problem with this code currently is that it operates solely on
    // strings instead of something more concrete which makes life far more
    // painful.
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


  // figure out which of the global registers that are less than 12 which are
  // unused need to be saved.
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++) {
      if (df_regs_ever_live_p(i) && ((!call_used_regs[i]) || (i > 7 && i < 12))
          /* No need to save the static chain pointer.  */
          && !(i == STATIC_CHAIN_REGNUM && cfun->static_chain_decl)) {
          regs[i] = -1;
          /* Count global registers that need saving.  */
          if (i < 16) {
              n_saved_regs++;
          }
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
      for (j = 7; j >= 0 && ! df_regs_ever_live_p(j); j--); 

      for (i = 20; i <= j + 20; i++) {
          regs[i] = -1;
      }
    }
  // I think this code is trying to construct a safe move table to allow
  // register to register saves. However, it is very hard to track
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

  actual_fsize = i960_compute_frame_size (get_frame_size()) + (4 * n_remaining_saved_regs);

  /* Check stack limit if necessary.  */
    if (crtl->limit_stack) {
            warning (0, "stack limit expression is not supported");
    }

  /* Allocate space for register save and locals.  */
    if (actual_fsize > 0) {
        if (actual_fsize < 32) {
            fprintf (file, "\taddo	" HOST_WIDE_INT_PRINT_DEC ",sp,sp\n", actual_fsize);
        } else {
            fprintf(file, "\tlda\t" HOST_WIDE_INT_PRINT_DEC "(sp),sp\n", actual_fsize);
        }
    }

  /* Take hardware register save area created by the call instruction
     into account, but store them before the argument block area.  */
  lvar_size = actual_fsize - i960_compute_frame_size (0) - (n_remaining_saved_regs * 4);
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

  fprintf (file, "\tlda\tLP%d,g0\n\tcallx\tmcount\n", labelno);

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
    output_asm_insn ("lda	%a1,g14", operands);
  else if (current_function_args_size != 0 || varargs_stdarg_function)
    output_asm_insn ("mov	0,g14", operands);

  /* The code used to assume that calls to SYMBOL_REFs could not be more
     than 24 bits away (b vs bx, callj vs callx).  This is not true.  This
     feature is now implemented by relaxing in the GNU linker.  It can convert
     bx to b if in range, and callx to calls/call/balx/bal as appropriate.  */


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

  return "ret";
}

/* Print the operand represented by rtx X formatted by code CODE.  */
rtx_code
i960_reverse_condition(rtx_code code) 
{
    switch (code) {
        // these are not mapped by reverse_condition_maybe_unordered
        case GTU:
        case GEU:
        case LTU:
        case LEU:
            return reverse_condition(code);
        default:
            return reverse_condition_maybe_unordered(code);
    }
}
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
        // TODO fix this so that we don't output a 64-bit number!
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
          gcc_unreachable();
      break;

    case 'I':
      /* Inverted condition.  */
      rtxcode = i960_reverse_condition(rtxcode);
      goto normal; 

    case 'X':
      /* Inverted condition w/ reversed operands.  */
      rtxcode = i960_reverse_condition(rtxcode);
      /* Fallthrough.  */
    case 'R':
      /* Reversed operand condition.  */
      rtxcode = swap_condition (rtxcode);
      /* Fallthrough.  */

    case 'C':
      /* Normal condition.  */
normal: // this is not good if I am seeing a label...
      switch (rtxcode) {
          case EQ:
          case UNEQ:
              fputs("e", file);
              return;
          case NE:
          case LTGT: // this is rad, it means != or less or greater than!
              fputs("ne", file);
              return;
          case GT:
          case GTU:
          case UNGT:
              fputs("g", file);
              return;
          case LT:
          case LTU:
          case UNLT:
              fputs("l", file);
              return;
          case GE:
          case GEU:
          case UNGE:
              fputs("ge", file);
              return;
          case LE:
          case LEU:
          case UNLE:
              fputs("le", file);
              return;
          case UNORDERED:
              fputs("no", file);
              return;
          case ORDERED:
              fputs("o", file);
              return;
          default:
              gcc_unreachable();
      }
      break;

      // We do not support branch prediction hints
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
i960_print_operand_addr (FILE* file, machine_mode mode, rtx addr)
{
    rtx breg, ireg;
    rtx scale, offset;

    ireg = 0;
    breg = 0;
    offset = 0;
    scale = const1_rtx;

    if (GET_CODE (addr) == REG) {
        breg = addr;
    } else if (CONSTANT_P (addr)) {
        offset = addr;
    } else if (GET_CODE (addr) == PLUS) {
        rtx op0 = XEXP (addr, 0);
        rtx op1 = XEXP (addr, 1);

        if (GET_CODE (op0) == REG) {
            breg = op0;
            if (GET_CODE (op1) == REG) {
                ireg = op1;
            } else if (CONSTANT_P (op1)) {
                offset = op1;
            } else {
                abort ();
            }
        } else if (GET_CODE (op0) == PLUS) {
            if (GET_CODE (XEXP (op0, 0)) == MULT) {
                ireg = XEXP (XEXP (op0, 0), 0);
                scale = XEXP (XEXP (op0, 0), 1);
                if (GET_CODE (XEXP (op0, 1)) == REG) {
                    breg = XEXP (op0, 1);
                    offset = op1;
                } else {
                    abort ();
                }
            } else if (GET_CODE (XEXP (op0, 0)) == REG) {
                breg = XEXP (op0, 0);
                if (GET_CODE (XEXP (op0, 1)) == REG) {
                    ireg = XEXP (op0, 1);
                    offset = op1;
                } else {
                    abort ();
                }
            } else {
                abort ();
            }
        } else if (GET_CODE (op0) == MULT) {
            ireg = XEXP (op0, 0);
            scale = XEXP (op0, 1);
            if (GET_CODE (op1) == REG) {
                breg = op1;
            } else if (CONSTANT_P (op1)) {
                offset = op1;
            } else {
                abort ();
            }
        } else {
            abort ();
        }
    } else if (GET_CODE (addr) == MULT) {
        ireg = XEXP (addr, 0);
        scale = XEXP (addr, 1);
    } else {
        abort ();
    }

    if (offset) {
        output_addr_const (file, offset);
    }
    if (breg) {
        fprintf (file, "(%s)", reg_names[REGNO (breg)]);
    }
    if (ireg) {
        fprintf (file, "[%s*" HOST_WIDE_INT_PRINT_DEC "]",
                reg_names[REGNO (ireg)], INTVAL (scale));
    }
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

/* Returns 1 if the scale factor of an index term is valid.  */
#define SCALE_TERM_P(X)							\
  (GET_CODE (X) == CONST_INT						\
   && (INTVAL (X) == 1 || INTVAL (X) == 2 || INTVAL (X) == 4 		\
       || INTVAL(X) == 8 || INTVAL (X) == 16))

bool
i960_legitimate_address_p (machine_mode mode, rtx addr, bool strict)
{
    if (RTX_OK_FOR_BASE_P (addr, strict)) {
        return true;
    } else if (CONSTANT_P (addr)) {
        return true;
    } else if (GET_CODE (addr) == PLUS) {
        rtx op0, op1;
        if (! TARGET_COMPLEX_ADDR && ! reload_completed) return false;

        op0 = XEXP (addr, 0);
        op1 = XEXP (addr, 1);
        if (RTX_OK_FOR_BASE_P (op0, strict))
        {
            if (RTX_OK_FOR_INDEX_P (op1, strict)) {
                return true;
            } else if (CONSTANT_P (op1)) {
                return true;
            } else {
                return false;
            }
        } else if (GET_CODE (op0) == PLUS) {
            if (GET_CODE (XEXP (op0, 0)) == MULT) {
                if (! (RTX_OK_FOR_INDEX_P (XEXP (XEXP (op0, 0), 0), strict) && SCALE_TERM_P (XEXP (XEXP (op0, 0), 1)))) {
                    return false;
                }

                return (RTX_OK_FOR_BASE_P (XEXP (op0, 1), strict) && CONSTANT_P (op1));
            } else if (RTX_OK_FOR_BASE_P (XEXP (op0, 0), strict)) {
                return (RTX_OK_FOR_INDEX_P (XEXP (op0, 1), strict) && CONSTANT_P (op1));
            } else {
                return false;
            }
        } else if (GET_CODE (op0) == MULT) {
            if (! (RTX_OK_FOR_INDEX_P (XEXP (op0, 0), strict) && SCALE_TERM_P (XEXP (op0, 1))))
                return false;
            if (RTX_OK_FOR_BASE_P (op1, strict))
                return true;
            else if (CONSTANT_P (op1))
                return true;
            else
                return false;
        } else {
            return false;
        }
    } else if (GET_CODE (addr) == MULT) {
        if (! TARGET_COMPLEX_ADDR && ! reload_completed) return false;

        return (RTX_OK_FOR_INDEX_P (XEXP (addr, 0), strict) && SCALE_TERM_P (XEXP (addr, 1)));
    } else {
        return false;
    }
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
i960_function_arg_advance (cumulative_args_t cat, const function_arg_info& info)
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
   all register parameters to memory.  Use the function arg info block to
   figure out information about the register set!
*/

void 
i960_setup_incoming_varargs (cumulative_args_t cat, const function_arg_info& arg, int * pretend_size, int no_rtl)
{
    // Okay so the idea is to create a register parameter block which is used
    // to store the 12 registers on the stack. This is relatively simple but
    // also very strange to me because we check g14 to see if it already is non
    // zero (which is _very_ dangerous). I think instead we should just
    // allocate the register parameter block on the stack and just leave it
    // there to be used.
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

  if (cum->ca_nstackparms == 0 && first_reg < NPARM_REGS && !no_rtl) {
	  rtx_code_label *label = gen_label_rtx ();
      rtx regblock, fake_arg_pointer_rtx;
      /* Use a different rtx than arg_pointer_rtx so that cse and friends
	 can go on believing that the argument pointer can never be zero.  */
      fake_arg_pointer_rtx = gen_raw_REG (Pmode, ARG_POINTER_REGNUM);
      /* If the argument pointer is 0, no arguments were passed on the stack
	 and we need to allocate a chunk to save the registers (if any
	 arguments were passed on the stack the caller would allocate the
	 48 bytes as well).  We must allocate all 48 bytes (12*4) because
	 va_start assumes it.  */
      // this is actually just a cbranchsi4
      // do a cmpsi of g14 with 0
      // bne to target label
      emit_jump_insn(gen_cbranchsi4(
                  gen_rtx_NE(VOIDmode, fake_arg_pointer_rtx, const0_rtx),
                  fake_arg_pointer_rtx,
                  const0_rtx,
                  label));
      // set g14 to sp, it allows it be passed to another function as needed
      emit_move_insn(fake_arg_pointer_rtx, stack_pointer_rtx);
      // set stack pointer to 48 + sp
      emit_move_insn(stack_pointer_rtx,
			      memory_address (SImode,
					      plus_constant (Pmode, /* info.mode previously */
                                         stack_pointer_rtx, 48)));
      // emit the label
      emit_label (label);

      /* ??? Note that we unnecessarily store one extra register for stdarg
	 fns.  We could optimize this, but it's kept as for now.  */
      regblock = gen_rtx_MEM (BLKmode,
			      plus_constant (Pmode/* info.mode */, fake_arg_pointer_rtx, first_reg * 4));
      set_mem_alias_set (regblock, get_varargs_alias_set ());
      set_mem_align (regblock, BITS_PER_WORD);
      move_block_from_reg (first_reg, regblock, NPARM_REGS - first_reg);
    }
}
/* Define the `__builtin_va_list' type for the ABI.  */

static tree
i960_build_builtin_va_list ()
{
    // construct an argument block in memory handled by incoming varargs
    // a va list is defined like this:
    // typedef struct __gnuc_va_list {
    //    void* addr; // address of the first argument
    //    unsigned int bytesSkipped; // number of bytes skipped past so far
    // } va_list;
    tree record = (*lang_hooks.types.make_type)(RECORD_TYPE);
    tree typeDecl = build_decl(BUILTINS_LOCATION,
            TYPE_DECL,
            get_identifier("__va_list_tag"),
            record);
    tree addrField = build_decl(BUILTINS_LOCATION,
            FIELD_DECL,
            get_identifier("__va_addr"),
            ptr_type_node );
    tree bytesSkippedField = build_decl(BUILTINS_LOCATION,
            FIELD_DECL,
            get_identifier("__va_bytes_skipped"),
            unsigned_type_node);
    DECL_FIELD_CONTEXT(addrField) = record;
    DECL_FIELD_CONTEXT(bytesSkippedField) = record;
    TYPE_STUB_DECL(record) = typeDecl;
    TYPE_NAME(record) = typeDecl;
    TYPE_FIELDS(record) = addrField;
    DECL_CHAIN(addrField) = bytesSkippedField;
    layout_type(record);

    return record;
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
 * ------
 * 2025/12/27
 *
 * This idea that you can determine the kind of g14 usage is really annoying
 * since it introduces some problems with the register allocator in gcc 11. It
 * seems to believe that g14 is the same as sp but I have no way of convincing
 * the compiler in any other way that this is false... 
 *
 * The g13 register used to be used for the struct return register but gcc has
 * eliminated that concept entirely and so will I. So now we have a free
 * register for whatever we want. I now prefer that this register be used as a
 * temporary for setting up an argument block. 
 *
 * Also, the idea that if g14/g13 is non zero then there is no need to
 * reallocate space is absolute nonsense. What if g13/g14 got trashed by accident or we missed 
 * something in the code generator? It would be almost impossible to determine
 * this. It also is a major security hole as well, I could just have internal
 * kernel structures get their data stashed to somewhere in user space since
 * g14/g13 was already set before the interrupt/system call/etc... not a good
 * idea... 
 *
 * If we drop the uses of g14 to just zero and link register then life becomes
 * much easier (ideally, I would like it so that we don't even waste time) to
 * statically determine lifetimes and such.
 *
 * In fact, we need to rewrite the stack frame tracking data.
 * ------
 * 2025/12/29
 *
 * After further examination, the argument pointer is being properly setup so I
 * have reverted to using g14. Even the i960 blue book talks about this as
 * well. I am also just leaving g13 alone for now as the "struct pointer" which
 * is no longer a thing in modern gcc but that's fine. What is an actual
 * problem is that the frame size is broken, fixing this will be the next step
 * that I need to work with.
 *
 */
void
i960_va_start (tree valist, rtx nextarg)
{
      tree t;
      // st g14, 64(fp)  # arg0 / base
      // mov 4, g4
      // st g4, 68(fp)   # arg1 / count / skip 
      auto structureSize = int_size_in_bytes(TYPE_SIZE_UNIT(TREE_TYPE(valist)));
      tree f_base = TYPE_FIELDS(va_list_type_node);
      tree f_count = DECL_CHAIN(f_base);

      tree base = build3(COMPONENT_REF, TREE_TYPE(f_base), valist, f_base, NULL_TREE);
      tree count = build3(COMPONENT_REF, TREE_TYPE(f_count), valist, f_count, NULL_TREE);
      // setup the base operation
      /* Use a different rtx than arg_pointer_rtx so that cse and friends can go
       * on believing that the argument pointer can never be zero. */
      auto fakeArgPointer = gen_raw_REG(Pmode, ARG_POINTER_REGNUM);
      t = make_tree(TREE_TYPE(base), fakeArgPointer);
      t = build2(MODIFY_EXPR, TREE_TYPE(base), base, t);
      TREE_SIDE_EFFECTS(t) = 1;
      expand_expr(t, const0_rtx, VOIDmode, EXPAND_NORMAL);
      // setup the count store operation
      auto computedCount = ((current_function_args_info.ca_nregparms + current_function_args_info.ca_nstackparms) * UNITS_PER_WORD);
      t = build2(MODIFY_EXPR, TREE_TYPE(count), count, build_int_cst(NULL_TREE, computedCount));
      TREE_SIDE_EFFECTS(t) = 1;
      expand_expr(t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

static tree
i960_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p, gimple_seq *post_p ) {
    auto int48 = size_int(48);
    tree f_base = TYPE_FIELDS(va_list_type_node);
    tree f_count = DECL_CHAIN(f_base);
    auto base = build3(COMPONENT_REF, TREE_TYPE(f_base), valist, f_base, NULL_TREE);
    auto count = build3(COMPONENT_REF, TREE_TYPE(f_count), valist, f_count, NULL_TREE);
    auto addr = create_tmp_var(ptr_type_node);
    // just gimplify this existing work to start to see how well I can generate the corresponding code
    // round up sizeof(type) to a word
#define SIGN0 -
#define SIGN1 -
    auto size = (int_size_in_bytes(type) + UNITS_PER_WORD - 1) & (SIGN0 UNITS_PER_WORD);
    // round up alignment to a word
    auto ali = TYPE_ALIGN(type);
    if (ali < BITS_PER_WORD) {
        ali = BITS_PER_WORD;
    }
    ali /= BITS_PER_UNIT;
    // align count appropriate for the argument
    auto pad = fold_build2(PLUS_EXPR, TREE_TYPE(count), count, size_int(ali - 1)); // count + (ali - 1)
    pad = fold_build2(BIT_AND_EXPR, TREE_TYPE(pad), pad, size_int(SIGN1 ali)); // (count + (ali - 1)) & (-ali)
    pad = save_expr(pad); // turn it into a reusable code component
    // increment vpad past this argument
    tree next = fold_build2(PLUS_EXPR, TREE_TYPE(pad), pad, size_int(size)); // ((count + (ali - 1)) & (-ali)) + size
    next = save_expr(next); // turn it into a reusable code component

    // if size > 16 then we just and with 1 later on
    auto t2 = size > 16 ? integer_one_node : fold_build2(GT_EXPR, TREE_TYPE(next), next, int48); // next > 48
    auto t1 = fold_build2(LE_EXPR, boolean_type_node, count, int48) ; // count < 48
    t1 = fold_build2(TRUTH_AND_EXPR, boolean_type_node, t1, t2); // ((count < 48) & (next > 48)
    auto _this = fold_build3(COND_EXPR, unsigned_type_node, t1,  int48, pad); // ((count < 48) & (next > 48)) ? 48 : (count + (ali - 1)) & (-ali)
    // find the address for the current argument
    t1 = fold_build_pointer_plus(base, _this); // A[0] + ((count < 48) & (next > 48)) ? 48 : (count + (ali - 1)) & (-ali)
    gimplify_assign(addr, t1, pre_p); // temporary = (A[0] + ((count < 48) & (next > 48)) ? 48 : (count + (ali - 1)) & (-ali))
    //auto addr_rtx = expand_expr(t1, NULL_RTX, Pmode, EXPAND_NORMAL);
    // increment count
    gimplify_assign(count, next, pre_p); // update the next pointer
    addr = fold_convert(build_pointer_type(type), addr); // turn temporary into a pointer
    return build_va_arg_indirect_ref(addr); // return pointer(A[0] + ((count < 48) & (next > 48)) ? 48 : (count + (ali - 1)) & (-ali))
}


/* Calculate the size of the reg parm stack space.  This is a bit complicated
   on the i960.  */
int
i960_reg_parm_stack_space (tree fndecl)
{
    /* In this case, we are called from emit_library_call, and we don't need
       to pretend we have more space for parameters than what's apparent.  */
    if (!fndecl) {
        return 0;
    }

    /* In this case, we are called from locate_and_pad_parms when we're
       not IN_REGS, so we have an arg block.  */
    if (fndecl != current_function_decl) {
        return 48;
    }

    /* Otherwise, we have an arg block if the current function has more than
       48 bytes of parameters.  */
    if (current_function_args_size != 0 || stdarg_p(TREE_TYPE(fndecl))) {
        return 48;
    }
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


static void
i960_output_mi_thunk (FILE* file, tree /*thunk*/, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, tree function)
{
    // TODO rewrite to not emit a fixed block of assembly instructions
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
    if (isGPR(regno)) {
        return ((GET_MODE_SIZE(mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
    } else if (isFloatingPointRegister(regno)) {
        // the fp registers always hold a single 80-bit value
        return 1;
    } else if (regno < FIRST_PSEUDO_REGISTER) {
        return 1;
    } else {
        return 0;
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
    if (isGPR(regno)) {
        switch (mode) {
            case CCmode:
            case CC_UNSmode:
            case CC_CHKmode:
                return false;

            case DImode:
            case DFmode:
                return isLongRegisterAligned(regno);

            case TImode:
            case TFmode:
                return isQuadRegisterAligned(regno);

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
#if 0
#define TARGET_CONSTANT_ALIGNMENT(EXP, ALIGN) \
  (TREE_CODE (EXP) == STRING_CST	\
   && i960_object_bytes_bitalign (int_size_in_bytes (TREE_TYPE (EXP))) > (int)(ALIGN) \
   ? i960_object_bytes_bitalign (int_size_in_bytes (TREE_TYPE (EXP)))	    \
   : (int)(ALIGN))
#endif
/* Specify alignment for string literals (which might be higher than the
   base type's minimal alignment requirement.  This allows strings to be
   aligned on word boundaries, and optimizes calls to the str* and mem*
   library functions.  */
    HOST_WIDE_INT result = 0;
    if (TREE_CODE(exp) == STRING_CST && (i960_object_bytes_bitalign(int_size_in_bytes(TREE_TYPE(exp))) > basic_align)) {
        result = i960_object_bytes_bitalign(int_size_in_bytes(TREE_TYPE(exp)));
    } else {
        result = basic_align;
    }
    return result;
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
static bool
i960_print_operand_punct_valid_p (unsigned char code)
{
  return code == '+';
}
/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On 80960, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#if 0
#define TARGET_CLASS_MAX_NREGS(CLASS, MODE)					\
  ((CLASS) == FP_REGS ? 1 : TARGET_HARD_REGNO_NREGS (0, (MODE)))
#define TARGET_STARTING_FRAME_OFFSET 64
#endif
static HOST_WIDE_INT i960_starting_frame_offset() { return 64; }

HOST_WIDE_INT
i960_compute_initial_elimination_offset(unsigned int from, unsigned int to) {
    // initial implementation (well I took out the - since it was causing problems)
    // This implementation sucks!
    switch (from) {
        case FRAME_POINTER_REGNUM:
            switch (to) {
                case STACK_POINTER_REGNUM:
                    return -(64 + i960_compute_frame_size(get_frame_size()));
                default:
                    gcc_unreachable();
            }
            gcc_unreachable();
        default:
            // anything else doesn't make sense right now
            gcc_unreachable();
    }
    gcc_unreachable();
    //return -(64 + i960_compute_frame_size(get_frame_size()));
}
  //do { (OFFSET) = -(64 + i960_compute_frame_size (get_frame_size ())); } while (0)

static bool
i960_enable_lra() {
    // @todo reactivate lra at some point
    return false;
}

static bool
i960_return_in_memory(const_tree type, const_tree fntype) {
/* Force aggregates and objects larger than 16 bytes to be returned in memory,
   since we only have 4 registers available for return values.  */
  return (TYPE_MODE (type) == BLKmode || int_size_in_bytes (type) > 16);
}

static rtx
i960_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
		       int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, I960_STRUCT_VALUE_REGNUM);
}

static inline HOST_WIDE_INT
i960_get_current_function_args_size(void) 
{
    // this was recommended by Copilot but manually written by the author
    // I want to understand what it is doing and why this is being selected
    if (crtl->args.size.is_constant()) {
        return crtl->args.size.to_constant();
    } else {
        // apparently this is a sentinel value meaning "variable"
        // return a conservative amount of stack space needed
        return 48;
    }
}

bool
i960_can_use_g14_for_zero_store() 
{
    // copilot helped me demystify what the hell the old statement was actually
    // doing
    //
    // I have written this manually based on the recommendations from it

    // the goal of this expression is to actually check and see if we can use
    // g14 as zero. This saves instructions loading constant zeros and wasting
    // registers
    //
    // We can use g14 as zero when:
    // 1) The function has no arguments (args.size == 0)
    // 2) The function is not varargs/stdarg
    // 3) The RTL expansion phase has already come and gone
    
    // VLAs disable the ability to use g14 as zero
    if (!crtl->args.size.is_constant()) {
        return false;
    }
    // these are the conditions for the three different cases
    return crtl->args.size.to_constant() == 0
        && !cfun->stdarg
        && !currently_expanding_to_rtl;
}

bool
i960_cannot_use_g14_for_zero_store()
{
    // if we have VLAs then g14 is already in use
    if (!crtl->args.size.is_constant()) {
        return true;
    }

    // if we are still expanding, have arguments, or varargs then we cannot use
    // g14 either
    return currently_expanding_to_rtl 
        || crtl->args.size.to_constant() != 0
        || cfun->stdarg;
}
bool isFloatingPointRegister(rtx x) { return isFloatingPointRegister(REGNO(x)); }
bool isGPR(rtx x) { return isGPR(REGNO(x)); }

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
// still use the old condition code stuff in the .md file so disable LRA for
// now
#undef TARGET_LRA_P
#define TARGET_LRA_P i960_enable_lra
#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE i960_conditional_register_usage

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED i960_frame_pointer_required
#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR i960_gimplify_va_arg_expr
#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P i960_legitimate_address_p

#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P i960_print_operand_punct_valid_p

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND i960_print_operand

#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS i960_print_operand_addr

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY i960_return_in_memory

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX i960_struct_value_rtx

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

// Things to fix/implement
// nested functions
// TARGET_REGISTER_MOVE_COST
// TARGET_MEMORY_MOVE_COST
// BRANCH_COST
// SLOW_BRANCH_ACCESS
// TARGET_SLOW_UNALIGNED_ACCESS (?)
// MOVE_RATIO
// TARGET_INSN_COST
//
