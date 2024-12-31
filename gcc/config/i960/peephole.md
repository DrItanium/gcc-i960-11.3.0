;;- Machine description for Intel 80960 chip for GNU C compiler
;;   Copyright (C) 1992, 1995, 1998, 2001 Free Software Foundation, Inc.
;;   Contributed by Steven McGeady, Intel Corp.
;;   Additional work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
;;   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.
;;   Updated to gcc 11.3.0 by Joshua Scoggins (2022-2024)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;; Various peephole optimizations for multiple-word moves, loads, and stores.
;; Multiple register moves.

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(match_operand:SI 3 "register_operand" "r"))
   (set (match_operand:SI 4 "register_operand" "=r")
	(match_operand:SI 5 "register_operand" "r"))
   (set (match_operand:SI 6 "register_operand" "=r")
	(match_operand:SI 7 "register_operand" "r"))]
  "isQuadRegisterAligned(REGNO(operands[0]))
   && isQuadRegisterAligned(REGNO(operands[1]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[4]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[5]))
   && (REGNO (operands[0]) + 3 == REGNO (operands[6]))
   && (REGNO (operands[1]) + 3 == REGNO (operands[7]))"
  "movq	%1,%0")

;; Matched 4/17/92
(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "register_operand" "r"))
   (set (match_operand:DI 2 "register_operand" "=r")
	(match_operand:DI 3 "register_operand" "r"))]
  "isQuadRegisterAligned(REGNO(operands[0]))
   && isQuadRegisterAligned(REGNO(operands[1]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[3]))"
  "movq	%1,%0")

;; Matched 4/17/92
(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(match_operand:SI 3 "register_operand" "r"))
   (set (match_operand:SI 4 "register_operand" "=r")
	(match_operand:SI 5 "register_operand" "r"))]
  "isQuadRegisterAligned(REGNO(operands[0]))
   && isQuadRegisterAligned(REGNO(operands[1]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[3]))
   && (REGNO (operands[0]) + 3 == REGNO (operands[4]))
   && (REGNO (operands[1]) + 3 == REGNO (operands[5]))"
  "movq	%1,%0")

;; Matched 4/17/92
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(match_operand:SI 3 "register_operand" "r"))
   (set (match_operand:DI 4 "register_operand" "=r")
	(match_operand:DI 5 "register_operand" "r"))]
  "((REGNO (operands[0]) & 3) == 0)
   && ((REGNO (operands[1]) & 3) == 0)
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[4]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[5]))"
  "movq	%1,%0")

;; Matched 4/17/92
(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(match_operand:SI 3 "register_operand" "r"))]
  "((REGNO (operands[0]) & 3) == 0)
   && ((REGNO (operands[1]) & 3) == 0)
   && (REGNO (operands[0]) + 2 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[3]))"
  "movt	%1,%0")

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(match_operand:SI 3 "register_operand" "r"))
   (set (match_operand:SI 4 "register_operand" "=r")
	(match_operand:SI 5 "register_operand" "r"))]
  "((REGNO (operands[0]) & 3) == 0)
   && ((REGNO (operands[1]) & 3) == 0)
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[4]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[5]))"
  "movt	%1,%0")

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(match_operand:SI 3 "register_operand" "r"))]
  "((REGNO (operands[0]) & 1) == 0)
   && ((REGNO (operands[1]) & 1) == 0)
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))"
  "movl	%1,%0")

; Multiple register loads.

;; Matched 6/15/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "immediate_operand" "n"))))
   (set (match_operand:SI 3 "register_operand" "=r")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 4 "immediate_operand" "n"))))
   (set (match_operand:SI 5 "register_operand" "=r")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 6 "immediate_operand" "n"))))
   (set (match_operand:SI 7 "register_operand" "=r")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 8 "immediate_operand" "n"))))]
  "(i960_si_ti (operands[1], operands[2]) && ((REGNO (operands[0]) & 3) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[1]) != REGNO (operands[3]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[5]))
   && (REGNO (operands[1]) != REGNO (operands[5]))
   && (REGNO (operands[0]) + 3 == REGNO (operands[7]))
   && (INTVAL (operands[2]) + 4 == INTVAL (operands[4]))
   && (INTVAL (operands[2]) + 8 == INTVAL (operands[6]))
   && (INTVAL (operands[2]) + 12 == INTVAL (operands[8])))"
  "ldq	%2(%1),%0")

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:DF 0 "register_operand" "=d")
	(mem:DF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "immediate_operand" "n"))))
   (set (match_operand:DF 3 "register_operand" "=d")
	(mem:DF (plus:SI (match_dup 1)
			 (match_operand:SI 4 "immediate_operand" "n"))))]
  "(i960_si_ti (operands[1], operands[2]) && ((REGNO (operands[0]) & 3) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[3]))
   && (REGNO (operands[1]) != REGNO (operands[3]))
   && (INTVAL (operands[2]) + 8 == INTVAL (operands[4])))"
  "ldq	%2(%1),%0")

;; Matched 1/24/92
(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mem:DI (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "immediate_operand" "n"))))
   (set (match_operand:DI 3 "register_operand" "=d")
	(mem:DI (plus:SI (match_dup 1)
			 (match_operand:SI 4 "immediate_operand" "n"))))]
  "(i960_si_ti (operands[1], operands[2]) && ((REGNO (operands[0]) & 3) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[3]))
   && (REGNO (operands[1]) != REGNO (operands[3]))
   && (INTVAL (operands[2]) + 8 == INTVAL (operands[4])))"
  "ldq	%2(%1),%0")

;; Matched 4/17/92
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mem:SI (match_operand:SI 1 "register_operand" "d")))
   (set (match_operand:SI 2 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 3 "immediate_operand" "n"))))
   (set (match_operand:SI 4 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 5 "immediate_operand" "n"))))
   (set (match_operand:SI 6 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 7 "immediate_operand" "n"))))]
  "(i960_si_ti (operands[1], 0) && ((REGNO (operands[0]) & 3) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (REGNO (operands[1]) != REGNO (operands[2]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[4]))
   && (REGNO (operands[1]) != REGNO (operands[4]))
   && (REGNO (operands[0]) + 3 == REGNO (operands[6]))
   && (INTVAL (operands[3]) == 4)
   && (INTVAL (operands[5]) == 8)
   && (INTVAL (operands[7]) == 12))"
  "ldq	(%1),%0")

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "immediate_operand" "n"))))
   (set (match_operand:SI 3 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 4 "immediate_operand" "n"))))
   (set (match_operand:SI 5 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 6 "immediate_operand" "n"))))]
  "(i960_si_ti (operands[1], operands[2]) && ((REGNO (operands[0]) & 3) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[1]) != REGNO (operands[3]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[5]))
   && (INTVAL (operands[2]) + 4 == INTVAL (operands[4]))
   && (INTVAL (operands[2]) + 8 == INTVAL (operands[6])))"
  "ldt	%2(%1),%0")

;; Matched 6/15/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mem:SI (match_operand:SI 1 "register_operand" "d")))
   (set (match_operand:SI 2 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 3 "immediate_operand" "n"))))
   (set (match_operand:SI 4 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 5 "immediate_operand" "n"))))]
  "(i960_si_ti (operands[1], 0) && ((REGNO (operands[0]) & 3) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (REGNO (operands[1]) != REGNO (operands[2]))
   && (REGNO (operands[0]) + 2 == REGNO (operands[4]))
   && (INTVAL (operands[3]) == 4)
   && (INTVAL (operands[5]) == 8))"
  "ldt	(%1),%0")

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "immediate_operand" "n"))))
   (set (match_operand:SI 3 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 4 "immediate_operand" "n"))))]
  "(i960_si_di (operands[1], operands[2]) && ((REGNO (operands[0]) & 1) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[3]))
   && (INTVAL (operands[2]) + 4 == INTVAL (operands[4])))"
  "ldl	%2(%1),%0")

;; Matched 5/28/91
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mem:SI (match_operand:SI 1 "register_operand" "d")))
   (set (match_operand:SI 2 "register_operand" "=d")
	(mem:SI (plus:SI (match_dup 1)
			 (match_operand:SI 3 "immediate_operand" "n"))))]
  "(i960_si_di (operands[1], 0) && ((REGNO (operands[0]) & 1) == 0)
   && (REGNO (operands[1]) != REGNO (operands[0]))
   && (REGNO (operands[0]) + 1 == REGNO (operands[2]))
   && (INTVAL (operands[3]) == 4))"
  "ldl	(%1),%0")

; Multiple register stores.

;; Matched 5/28/91
(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 0 "register_operand" "d")
			 (match_operand:SI 1 "immediate_operand" "n")))
	(match_operand:SI 2 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 3 "immediate_operand" "n")))
	(match_operand:SI 4 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 5 "immediate_operand" "n")))
	(match_operand:SI 6 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 7 "immediate_operand" "n")))
	(match_operand:SI 8 "register_operand" "d"))]
  "(i960_si_ti (operands[0], operands[1]) && ((REGNO (operands[2]) & 3) == 0)
   && (REGNO (operands[2]) + 1 == REGNO (operands[4]))
   && (REGNO (operands[2]) + 2 == REGNO (operands[6]))
   && (REGNO (operands[2]) + 3 == REGNO (operands[8]))
   && (INTVAL (operands[1]) + 4 == INTVAL (operands[3]))
   && (INTVAL (operands[1]) + 8 == INTVAL (operands[5]))
   && (INTVAL (operands[1]) + 12 == INTVAL (operands[7])))"
  "stq	%2,%1(%0)")

;; Matched 6/16/91
(define_peephole
  [(set (mem:DF (plus:SI (match_operand:SI 0 "register_operand" "d")
			 (match_operand:SI 1 "immediate_operand" "n")))
	(match_operand:DF 2 "register_operand" "d"))
   (set (mem:DF (plus:SI (match_dup 0)
			 (match_operand:SI 3 "immediate_operand" "n")))
	(match_operand:DF 4 "register_operand" "d"))]
  "(i960_si_ti (operands[0], operands[1]) && ((REGNO (operands[2]) & 3) == 0)
   && (REGNO (operands[2]) + 2 == REGNO (operands[4]))
   && (INTVAL (operands[1]) + 8 == INTVAL (operands[3])))"
  "stq	%2,%1(%0)")

;; Matched 4/17/92
(define_peephole
  [(set (mem:DI (plus:SI (match_operand:SI 0 "register_operand" "d")
			 (match_operand:SI 1 "immediate_operand" "n")))
	(match_operand:DI 2 "register_operand" "d"))
   (set (mem:DI (plus:SI (match_dup 0)
			 (match_operand:SI 3 "immediate_operand" "n")))
	(match_operand:DI 4 "register_operand" "d"))]
  "(i960_si_ti (operands[0], operands[1]) && ((REGNO (operands[2]) & 3) == 0)
   && (REGNO (operands[2]) + 2 == REGNO (operands[4]))
   && (INTVAL (operands[1]) + 8 == INTVAL (operands[3])))"
  "stq	%2,%1(%0)")

;; Matched 1/23/92
(define_peephole
  [(set (mem:SI (match_operand:SI 0 "register_operand" "d"))
	(match_operand:SI 1 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 2 "immediate_operand" "n")))
	(match_operand:SI 3 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 4 "immediate_operand" "n")))
	(match_operand:SI 5 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 6 "immediate_operand" "n")))
	(match_operand:SI 7 "register_operand" "d"))]
  "(i960_si_ti (operands[0], 0) && ((REGNO (operands[1]) & 3) == 0)
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[5]))
   && (REGNO (operands[1]) + 3 == REGNO (operands[7]))
   && (INTVAL (operands[2]) == 4)
   && (INTVAL (operands[4]) == 8)
   && (INTVAL (operands[6]) == 12))"
  "stq	%1,(%0)")

;; Matched 5/29/91
(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 0 "register_operand" "d")
			 (match_operand:SI 1 "immediate_operand" "n")))
	(match_operand:SI 2 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 3 "immediate_operand" "n")))
	(match_operand:SI 4 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 5 "immediate_operand" "n")))
	(match_operand:SI 6 "register_operand" "d"))]
  "(i960_si_ti (operands[0], operands[1]) && ((REGNO (operands[2]) & 3) == 0)
   && (REGNO (operands[2]) + 1 == REGNO (operands[4]))
   && (REGNO (operands[2]) + 2 == REGNO (operands[6]))
   && (INTVAL (operands[1]) + 4 == INTVAL (operands[3]))
   && (INTVAL (operands[1]) + 8 == INTVAL (operands[5])))"
  "stt	%2,%1(%0)")

;; Matched 5/29/91
(define_peephole
  [(set (mem:SI (match_operand:SI 0 "register_operand" "d"))
	(match_operand:SI 1 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 2 "immediate_operand" "n")))
	(match_operand:SI 3 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 4 "immediate_operand" "n")))
	(match_operand:SI 5 "register_operand" "d"))]
  "(i960_si_ti (operands[0], 0) && ((REGNO (operands[1]) & 3) == 0)
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))
   && (REGNO (operands[1]) + 2 == REGNO (operands[5]))
   && (INTVAL (operands[2]) == 4)
   && (INTVAL (operands[4]) == 8))"
  "stt	%1,(%0)")

;; Matched 5/28/91
(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 0 "register_operand" "d")
			 (match_operand:SI 1 "immediate_operand" "n")))
	(match_operand:SI 2 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 3 "immediate_operand" "n")))
	(match_operand:SI 4 "register_operand" "d"))]
  "(i960_si_di (operands[0], operands[1]) && ((REGNO (operands[2]) & 1) == 0)
   && (REGNO (operands[2]) + 1 == REGNO (operands[4]))
   && (INTVAL (operands[1]) + 4 == INTVAL (operands[3])))"
  "stl	%2,%1(%0)")

;; Matched 5/28/91
(define_peephole
  [(set (mem:SI (match_operand:SI 0 "register_operand" "d"))
	(match_operand:SI 1 "register_operand" "d"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 2 "immediate_operand" "n")))
	(match_operand:SI 3 "register_operand" "d"))]
  "(i960_si_di (operands[0], 0) && ((REGNO (operands[1]) & 1) == 0)
   && (REGNO (operands[1]) + 1 == REGNO (operands[3]))
   && (INTVAL (operands[2]) == 4))"
  "stl	%1,(%0)")

;; nand and nor operations 32 and 64-bits wide
;; Otherwise, I cannot get the matcher to work correctly!
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
    (and:SI (match_operand:SI 1 "register_operand" "%d")
            (match_operand:SI 2 "register_operand" "d")))
   (set (match_dup 0)
        (not:SI (match_dup 0)))]
  ""
  "nand %1,%2,%0"
  )

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
    (and:SI (match_operand:SI 1 "register_operand" "%d")
                    (match_operand:SI 2 "register_operand" "d")))
  (set (match_operand:SI 3 "register_operand" "=d")
    (and:SI (match_operand:SI 4 "register_operand" "%d")
                    (match_operand:SI 5 "register_operand" "d")))
   (set (match_dup 0)
        (not:SI (match_dup 0)))
   (set (match_dup 3)
        (not:SI (match_dup 3)))]
  ""
  "nand %1,%2,%0\;nand %4,%5,%3"
  )


(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
    (ior:SI (match_operand:SI 1 "register_operand" "%d")
                    (match_operand:SI 2 "register_operand" "d")))
   (set (match_dup 0)
        (not:SI (match_dup 0)))]
  ""
  "nor %1,%2,%0" )
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
    (ior:SI (match_operand:SI 1 "register_operand" "%d")
                    (match_operand:SI 2 "register_operand" "d")))
  (set (match_operand:SI 3 "register_operand" "=d")
    (ior:SI (match_operand:SI 4 "register_operand" "%d")
                    (match_operand:SI 5 "register_operand" "d")))
   (set (match_dup 0)
        (not:SI (match_dup 0)))
   (set (match_dup 3)
        (not:SI (match_dup 3)))]
  ""
  "nor %1,%2,%0\;nor %4,%5,%3"
  )
