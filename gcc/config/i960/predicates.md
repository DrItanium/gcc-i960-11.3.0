;;- Predicate description for Intel 80960 chip for GNU C compiler
;;   Copyright (C) 2022 Joshua Scoggins
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

(define_predicate "arith_operand" (match_code "const_int,subreg,reg")))
(define_predicate "logic_operand" (match_code "const_int,subreg,reg")))
(define_predicate "signed_arith_operand" (match_code "const_int,subreg,reg")))
(define_predicate "fp_arith_operand" (match_code "subreg,reg,const_double")))
(define_predicate "literal" (match_code "const_int"))
(define_predicate "power2_operand" (match_code "const_int"))
(define_predicate "cmplpower2_operand" (match_code "const_int"))
(define_predicate "fp_literal" (match_code "const_double"))
(define_predicate "fp_literal_double" (match_code "const_double"))
(define_predicate "fp_literal_one" (match_code "const_double"))
(define_predicate "signed_literal" (match_code "const_int"))
