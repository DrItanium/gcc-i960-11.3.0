#ifndef GCC_I960_ELF_H
#define GCC_I960_ELF_H
/* Definitions for embedded i960-elf target.

Copyright (C) 2000-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* A C string constant that tells the GCC driver program options to pass to
   the linker.  It can also specify how to translate options you give to GCC
   into options for GCC to pass to the linker.  */

/* elfos.h does not link with crti.o/crtn.o.  We override elfos.h so
   that we can use the standard ELF Unix method.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* elfos.h disables the use of leading underscores... this causes problems
 * when you name a function sp or any of the registers of the i960 */
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"
/* End of elf.h */
#endif
