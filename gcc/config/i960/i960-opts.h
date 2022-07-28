/* Definitions for option handling for i960
   Copyright (C) 2022 Joshua Scoggins

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef I960_OPTS_H
#define I960_OPTS_H
#if 0
/* Run-time compilation parameters selecting different hardware subsets.  */

/* 960 architecture with floating-point.  */
#define TARGET_FLAG_NUMERICS	0x01
//#define TARGET_NUMERICS		(target_flags & TARGET_FLAG_NUMERICS)

/* 960 architecture with memory management.  */
/* ??? Not used currently.  */
#define	TARGET_FLAG_PROTECTED	0x02
//#define	TARGET_PROTECTED	(target_flags & TARGET_FLAG_PROTECTED)

/* The following three are mainly used to provide a little sanity checking
   against the -mARCH flags given. The Jx series, for the purposes of
   gcc, is a Kx with a data cache.  */

/* Nonzero if we should generate code for the KA and similar processors.
   No FPU, no microcode instructions.  */
#define TARGET_FLAG_K_SERIES	0x04
//#define TARGET_K_SERIES		(target_flags & TARGET_FLAG_K_SERIES)

/* Nonzero if we should generate code for the MC processor.
   Not really different from KB for our purposes.  */
#define	TARGET_FLAG_MC		0x08
//#define TARGET_MC 		(target_flags & TARGET_FLAG_MC)

/* Nonzero if we should generate code for the CA processor.
   Enables different optimization strategies.  */
#define	TARGET_FLAG_C_SERIES	0x10
#define	TARGET_C_SERIES 	(target_flags & TARGET_FLAG_C_SERIES)

/* Nonzero if we should generate leaf-procedures when we find them.
   You may not want to do this because leaf-proc entries are
   slower when not entered via BAL - this would be true when
   a linker not supporting the optimization is used.  */
#define	TARGET_FLAG_LEAFPROC	0x20
//#define	TARGET_LEAFPROC		(target_flags & TARGET_FLAG_LEAFPROC)

/* Nonzero if we should perform tail-call optimizations when we find them.
   You may not want to do this because the detection of cases where
   this is not valid is not totally complete.  */
#define	TARGET_FLAG_TAILCALL	0x40
//#define	TARGET_TAILCALL		(target_flags & TARGET_FLAG_TAILCALL)

/* Nonzero if use of a complex addressing mode is a win on this implementation.
   Complex addressing modes are probably not worthwhile on the K-series,
   but they definitely are on the C-series.  */
#define	TARGET_FLAG_COMPLEX_ADDR 0x80
//#define	TARGET_COMPLEX_ADDR	(target_flags & TARGET_FLAG_COMPLEX_ADDR)

/* Align code to 8 byte boundaries for faster fetching.  */
#define	TARGET_FLAG_CODE_ALIGN	0x100
//#define	TARGET_CODE_ALIGN	(target_flags  & TARGET_FLAG_CODE_ALIGN)

/* Append branch prediction suffixes to branch opcodes.  */
/* ??? Not used currently.  */
#define	TARGET_FLAG_BRANCH_PREDICT 0x200
//#define	TARGET_BRANCH_PREDICT	(target_flags  & TARGET_FLAG_BRANCH_PREDICT)

/* Forces prototype and return promotions.  */
/* ??? This does not work.  */
#define	TARGET_FLAG_CLEAN_LINKAGE 0x400
//#define	TARGET_CLEAN_LINKAGE	(target_flags & TARGET_FLAG_CLEAN_LINKAGE)

/* For compatibility with iC960 v3.0.  */
#define	TARGET_FLAG_IC_COMPAT3_0 0x800 
//#define	TARGET_IC_COMPAT3_0	(target_flags & TARGET_FLAG_IC_COMPAT3_0)

/* For compatibility with iC960 v2.0.  */
#define	TARGET_FLAG_IC_COMPAT2_0 0x1000
//#define	TARGET_IC_COMPAT2_0	(target_flags & TARGET_FLAG_IC_COMPAT2_0)

/* If no unaligned accesses are to be permitted.  */
#define	TARGET_FLAG_STRICT_ALIGN 0x2000
#define	TARGET_STRICT_ALIGN	(target_flags & TARGET_FLAG_STRICT_ALIGN)

/* For compatibility with iC960 assembler.  */
#define	TARGET_FLAG_ASM_COMPAT	0x4000
#define	TARGET_ASM_COMPAT	(target_flags & TARGET_FLAG_ASM_COMPAT)

/* For compatibility with the gcc960 v1.2 compiler.  Use the old structure
   alignment rules.  Also, turns on STRICT_ALIGNMENT.  */
#define TARGET_FLAG_OLD_ALIGN	0x8000
#define TARGET_OLD_ALIGN	(target_flags & TARGET_FLAG_OLD_ALIGN)

/* Nonzero if long doubles are to be 64 bits.  Useful for soft-float targets
   if 80 bit long double support is missing.  */
#define TARGET_FLAG_LONG_DOUBLE_64	0x10000
#define TARGET_LONG_DOUBLE_64	(target_flags & TARGET_FLAG_LONG_DOUBLE_64)


#endif
#endif
