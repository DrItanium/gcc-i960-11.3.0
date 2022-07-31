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

struct GTY(()) machine_function
{

  /* Number of bytes saved on the stack for outgoing/sub-function args.  */
  HOST_WIDE_INT args_size;

};

static struct machine_function*
i960_init_machine_status(void) 
{
    return ggc_cleared_alloc<machine_function> ();
}

static void
i960_option_override (void) {
    init_machine_status = i960_init_machine_status;
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

HOST_WIDE_INT 
i960_initial_elimination_offset(int from, int to) {
    return -(64 + i960_compute_frame_size(get_frame_size()));
}

void
i960_output_function_profiler(FILE* file, int labelNo) {
    /// @todo implement the full function profiler at some point, 
    fprintf(file, "/* profiler %d */", labelNo);
}

static rtx
i960_function_value(const_tree valtype, const_tree, bool) {
    /// @todo is zero right?
    return gen_rtx_REG(TYPE_MODE(valtype), 0);
}

#undef TARGET_FUNCTION_VALUE 
#define TARGET_FUNCTION_VALUE i960_function_value

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE i960_option_override

struct gcc_target targetm = TARGET_INITIALIZER;
#include "gt-i960.h"
