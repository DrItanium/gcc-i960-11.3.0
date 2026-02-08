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
// floating point abi types
enum i960_float_abi_type {
    FLOAT_ABI_SOFT,
    FLOAT_ABI_HARD
};

enum i960_processor_type {
    ARCH_KA, // K-series, no FPU, no MMU, 32-bit data bus (although FPU and MMU are there)
    ARCH_KB, // K-series, FPU, no MMU, 32-bit data bus (although MMU is there)
    ARCH_KC, // K-series, FPU, MMU, 32-bit data bus (theoretical)
    ARCH_SA, // S-series, no FPU, no MMU, 16-bit data bus
    ARCH_SB, // S-series, FPU, no MMU, 16-bit data bus (although MMU is in there)
    ARCH_SC, // S-series, FPU, MMU, 16-bit data bus
    ARCH_MC, // M-series, FPU, MMU, 32-bit data bus
};

// support for the numerics architecture (FPU)
#define I960_FEATURE_NUMERICS (1 << 0) 
// support for the protected architecture (MMU + OS and string instructions)
#define I960_FEATURE_PROTECTED (1 << 1)
// use complex addressing modes
#define I960_FEATURE_COMPLEX_ADDRESSING (1 << 2)
#define I960_FEATURE_CODE_ALIGN (1 << 3)
#define I960_FEATURE_BRANCH_PREDICT (1 << 4)

#define I960_OPTION_UNSET (-1)
#define I960_OPTION_DISABLED (0)
#define I960_OPTION_ENABLED (1)
#endif
