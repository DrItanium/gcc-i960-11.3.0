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

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d")
	(match_operator:SI 1 "comparison_operator" [(reg:CC CC_REG) (const_int 0)]))]
  ""
  "test%C1	%0 # test v0"
  [(set_attr "type" "compare")])


(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d")
	(match_operator:SI 1 "comparison_operator" [(reg:CC_UNS CC_REG) (const_int 0)]))]
  ""
  "test%C1	%0 #test v1"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 0 "comparison_operator"
			 [(match_operand:SI 1 "arith_operand" "d")
			  (match_operand:SI 2 "arith_operand" "dI")])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  ""
  ;; "cmp%S0%B0%R0%+	%2,%1,%l3"
  "cmp%S0%B0%R0\t%2,%1,%l3 # if ?cond then label else pc"
  [(set_attr "type" "branch")])

(define_expand "cbranchsi4"
    [(set (pc)
    (if_then_else (match_operator 0 "comparison_operator"
                    [(match_operand:SI 1 "" "")
                     (match_operand:SI 2 "" "")])
                     (label_ref (match_operand 3 "" ""))
                     (pc)))]
                     "" )

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 0 "comparison_operator"
			 [(match_operand:SI 1 "arith_operand" "d")
			  (match_operand:SI 2 "arith_operand" "dI")])
	 (pc)
	 (label_ref (match_operand 3 "" ""))))]
  ""
  ;;"cmp%S0%B0%X0%+	%2,%1,%l3"
  "cmp%S0%B0%X0\t%2,%1,%l3 # if ?cond then pc else label"
  [(set_attr "type" "branch")])

;; Now the trap instructions.  The i960 appears to only have conditional
;; traps...

(define_insn ("trap")
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "cmpo g0,g0 ; faulte.t")

(define_expand "conditional_trap"
  [(trap_if (match_operator 0 "comparison_operator"
	     [(match_dup 2) (const_int 0)])
	    (match_operand 1 "const_int_operand" "i"))]
  ""
  "
{
  operands[2] = i960_gen_compare_reg (GET_CODE (operands[0]),
				 i960_compare_op0, i960_compare_op1);
}")

(define_insn ""
  [(trap_if (match_operator 0 "comparison_operator"
	     [(reg:CC CC_REG) (const_int 0)])
	    (match_operand 1 "const_int_operand" "i"))]
  ""
  "fault%C0.f")

(define_insn ""
  [(trap_if (match_operator 0 "comparison_operator"
	     [(reg:CC_UNS CC_REG) (const_int 0)])
	    (match_operand 1 "const_int_operand" "i"))]
  ""
  "fault%C0.f")
