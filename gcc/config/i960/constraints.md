;;- Constraint description for Intel 80960 chip for GNU C compiler
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


(define_register_constraint "f" "TARGET_NUMERICS ? FP_REGS : NO_REGS"
 "Floating point registers provided by the numerics extension")

(define_register_constraint "l" "LOCAL_REGS" "Local registers (r0-r15)")
(define_register_constraint "b" "GLOBAL_REGS" "Global registers (g0-g15)")
(define_register_constraint "d" "LOCAL_OR_GLOBAL_REGS" "Any local or global register")
(define_constraint "I" 
 "Literal values [0, 31]"
 (and (match_code "const_int")
      (match_test "IN_RANGE (ival, 0, 31)")))
