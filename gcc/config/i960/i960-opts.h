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
    // these cores are based off of the original design (with the exception of
    // the i960SA processor) so really they are the same core and accept the
    // same set of options. The problem though is that the extended
    // architecture cannot be used on the S series because of a lack of tag
    // pin.
    //
    // The K and M series probably has the tag pin via omission of
    // documentation. It is important to note that this does not really help us
    // at all!
    ARCH_KA, // K-series, no FPU, no MMU, 32-bit data bus (although FPU and MMU are there)
    ARCH_KB, // K-series, FPU, no MMU, 32-bit data bus (although MMU is there)
    ARCH_KC, // K-series, FPU, MMU, 32-bit data bus (theoretical)
    ARCH_SA, // S-series, no FPU, no MMU, 16-bit data bus
    ARCH_SB, // S-series, FPU, no MMU, 16-bit data bus (although MMU is in there)
    ARCH_SC, // S-series, FPU, MMU, 16-bit data bus (theoretical and never
             // released)
    ARCH_MC, // M-series, FPU, MMU, 32-bit data bus
    ARCH_XA, // X-series, FPU, MMU, Extended ISA 32-bit data bus (not supported but referenced here)
    // C-series processors. These processors are superscalar i960s with a new
    // core that does not support FPU or MMU operations. Does introduce a DMA
    // instructions.
    ARCH_CA, // C-series, DMA, Core
    ARCH_CF, // C-series
    // J-Series processors take the C-series core and improve on it by reducing
    // power consumption. Various different models, does not support DMA but
    // does have new core instructions that are of the vein of cmpob and cmpib
    // but with moves (conditional select), adds (conditional add), and
    // conditional subtraction. These processors are meant to be used as
    // microcontrollers. Branch predicition hints are supported
    ARCH_JA, // 
    ARCH_JC, // J-series (no MMU)
    ARCH_JD, // J-series
    ARCH_JF,
    ARCH_JS,
    ARCH_JT,
    // H-Series Processors
    // A further evolution of the J series with improved performance and some
    // builtin features like timers. 
    ARCH_HA,
    ARCH_HD,
    ARCH_HT,
    // RX I/O processor, not much known about it but it is basically an i960
    // with a PCI peripheral attached to it. Not really supported. Both this
    // and the V series have mentions in the intel compiler manual that the
    // processor has gone hardcore risc and dropped duplicate instructions.
    ARCH_RX,
    // V series, not much known about it but it seems to have gone hardcore
    // risc. 
    ARCH_VH,
    // @todo newer cpu targets like the H, J, R, and V series go here
};

// support for the numerics architecture (FPU)
#define I960_FEATURE_NUMERICS (1 << 0) 
// support for the protected architecture (MMU + OS and string instructions)
#define I960_FEATURE_PROTECTED (1 << 1)
// support tagged memory and ADA features (only here for completeness, no way to test this out)
#define I960_FEATURE_EXTENDED (1 << 2)
// use complex addressing modes
#define I960_FEATURE_COMPLEX (1 << 3)
// use code alignment
#define I960_FEATURE_CODE_ALIGNMENT (1 << 4)
// use strict alignment
#define I960_FEATURE_STRICT_ALIGNMENT (1 << 5)
// new core instructions
#define I960_FEATURE_NEW_CORE (1 << 6)
// allow branch prediction hints to be specified (unsupported but here for completeness)
#define I960_FEATURE_BRANCH_PREDICTION_HINTS (1 << 7)

#endif
