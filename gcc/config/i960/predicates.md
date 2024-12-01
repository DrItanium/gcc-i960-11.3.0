;;- Predicate description for Intel 80960 chip for GNU C compiler
;;   Copyright (C) 2022-2024 Joshua Scoggins
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_predicate "fpmove_src_operand"
    (ior (match_code "const_double")
         (match_operand 0 "general_operand")))
(define_predicate "literal"
    (and (match_code "const_int")
         (match_test "INTVAL(op) >= 0 && INTVAL(op) < 32")))
(define_predicate "fp_literal_one"
    (and (match_test "TARGET_NUMERICS")
         (and (match_code "const_double")
              (match_test "op == CONST1_RTX(mode)"))))

(define_predicate "fp_literal_zero"
    (and (match_test "TARGET_NUMERICS")
         (and (match_code "const_double")
              (match_test "op == CONST0_RTX(mode)"))))
(define_predicate "fp_literal"
    (ior (match_operand 0 "fp_literal_zero")
         (match_operand 0 "fp_literal_one")))
 (define_predicate "signed_literal"
    (and (match_code "const_int")
         (match_test "INTVAL(op) > -32 && INTVAL(op) < 32")))
(define_predicate "arith_operand"
    (ior (match_operand 0 "register_operand")
         (match_operand 0 "literal")))
(define_predicate "logic_operand"
    (ior (match_operand 0 "register_operand")
         (and (match_code "const_int")
              (match_test "INTVAL(op) >= -32 && INTVAL(op) < 32"))))
(define_predicate "fp_arith_operand"
    (ior (match_operand 0 "register_operand")
         (match_operand 0 "fp_literal")))
(define_predicate "signed_arith_operand"
    (ior (match_operand 0 "register_operand")
         (match_operand 0 "signed_literal")))
(define_predicate "power2_operand"
    (and (match_code "const_int")
         (match_test "exact_log2(INTVAL(op)) >= 0")))

(define_predicate "cmplpower2_operand"
    (and (match_code "const_int")
         (match_test "(~exact_log2(INTVAL(op))) >= 0")))

(define_predicate "arith32_operand"
     (match_code "subreg,reg,label_ref,symbol_ref,const_int,const_double,const"))