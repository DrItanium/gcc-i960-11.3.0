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

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

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
#endif
