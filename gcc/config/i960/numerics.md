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
;; Conversions between float and double.

(define_insn "extendsfdf2"
[(set (match_operand:DF 0 "register_operand" "=*f,d")
(float_extend:DF (match_operand:SF 1 "fp_arith_operand" "dGH,fGH")))]
"TARGET_NUMERICS"
"@
movr	%1,%0
movrl	%1,%0"
[(set_attr "type" "fpmove")])

(define_insn "truncdfsf2"
[(set (match_operand:SF 0 "register_operand" "=d")
(float_truncate:SF
(match_operand:DF 1 "fp_arith_operand" "fGH")))]
"TARGET_NUMERICS"
"movr	%1,%0"
[(set_attr "type" "fpmove")])

;; Conversion between fixed point and floating point.

(define_insn "floatsidf2"
[(set (match_operand:DF 0 "register_operand" "=f")
(float:DF (match_operand:SI 1 "register_operand" "d")))]
"TARGET_NUMERICS"
"cvtir	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "floatsisf2"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(float:SF (match_operand:SI 1 "register_operand" "d")))]
"TARGET_NUMERICS"
"cvtir	%1,%0"
[(set_attr "type" "fpcvt")])

;; convert long integer to real :)
(define_insn "floatdisf2"
[(set (match_operand:SF 0 "register_operand" "=f")
(float:SF (match_operand:DI 1 "register_operand" "d")))]
"TARGET_NUMERICS"
"cvtilr	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "floatdidf2"
[(set (match_operand:DF 0 "register_operand" "=f")
(float:DF (match_operand:DI 1 "register_operand" "d")))]
"TARGET_NUMERICS"
"cvtilr	%1,%0"
[(set_attr "type" "fpcvt")])

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.
;; The i960 requires conversion from DFmode to DImode to make
;; unsigned conversions work properly.

(define_insn "fixuns_truncdfdi2"
[(set (match_operand:DI 0 "register_operand" "=d")
(unsigned_fix:DI (fix:DF (match_operand:DF 1 "fp_arith_operand" "fGH"))))]
"TARGET_NUMERICS"
"cvtzril	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "fixuns_truncsfdi2"
[(set (match_operand:DI 0 "register_operand" "=d")
(unsigned_fix:DI (fix:SF (match_operand:SF 1 "fp_arith_operand" "fGH"))))]
"TARGET_NUMERICS"
"cvtzril	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "fix_truncdfsi2"
[(set (match_operand:SI 0 "register_operand" "=d")
(fix:SI (fix:DF (match_operand:DF 1 "fp_arith_operand" "fGH"))))]
"TARGET_NUMERICS"
"cvtzri	%1,%0"
[(set_attr "type" "fpcvt")])

(define_expand "fixuns_truncdfsi2"
[(set (match_operand:SI 0 "register_operand" "")
(unsigned_fix:SI (fix:DF (match_operand:DF 1 "fp_arith_operand" ""))))]
"TARGET_NUMERICS"
"
{
rtx temp = gen_reg_rtx (DImode);
emit_insn (gen_rtx_SET (temp, gen_rtx_UNSIGNED_FIX (DImode, gen_rtx_FIX (DFmode, operands[1]))));
emit_insn (gen_rtx_SET (operands[0], gen_rtx_SUBREG (SImode, temp, 0)));
DONE;
}")

(define_insn "fix_truncsfsi2"
[(set (match_operand:SI 0 "register_operand" "=d")
(fix:SI (fix:SF (match_operand:SF 1 "fp_arith_operand" "dfGH"))))]
"TARGET_NUMERICS"
"cvtzri	%1,%0"
[(set_attr "type" "fpcvt")])

(define_expand "fixuns_truncsfsi2"
[(set (match_operand:SI 0 "register_operand" "")
(unsigned_fix:SI (fix:SF (match_operand:SF 1 "fp_arith_operand" ""))))]
"TARGET_NUMERICS"
"
{
rtx temp = gen_reg_rtx (DImode);
emit_insn (gen_rtx_SET (temp, gen_rtx_UNSIGNED_FIX (DImode, gen_rtx_FIX (SFmode, operands[1]))));
emit_insn (gen_rtx_SET (operands[0], gen_rtx_SUBREG (SImode, temp, 0)));
DONE;
}")

;; Floating point arithmetic instructions.

(define_insn "adddf3"
[(set (match_operand:DF 0 "register_operand" "=d*f")
(plus:DF (match_operand:DF 1 "fp_arith_operand" "%rGH")
(match_operand:DF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"addrl	%1,%2,%0"
[(set_attr "type" "fpadd")])

(define_insn "addsf3"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(plus:SF (match_operand:SF 1 "fp_arith_operand" "%rGH")
(match_operand:SF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"addr	%1,%2,%0"
[(set_attr "type" "fpadd")])


(define_insn "subdf3"
[(set (match_operand:DF 0 "register_operand" "=d*f")
(minus:DF (match_operand:DF 1 "fp_arith_operand" "rGH")
(match_operand:DF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"subrl	%2,%1,%0"
[(set_attr "type" "fpadd")])

(define_insn "subsf3"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(minus:SF (match_operand:SF 1 "fp_arith_operand" "rGH")
(match_operand:SF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"subr	%2,%1,%0"
[(set_attr "type" "fpadd")])


(define_insn "muldf3"
[(set (match_operand:DF 0 "register_operand" "=d*f")
(mult:DF (match_operand:DF 1 "fp_arith_operand" "%rGH")
(match_operand:DF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"mulrl	%1,%2,%0"
[(set_attr "type" "fpmul")])

(define_insn "mulsf3"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(mult:SF (match_operand:SF 1 "fp_arith_operand" "%rGH")
(match_operand:SF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"mulr	%1,%2,%0"
[(set_attr "type" "fpmul")])


(define_insn "divdf3"
[(set (match_operand:DF 0 "register_operand" "=d*f")
(div:DF (match_operand:DF 1 "fp_arith_operand" "rGH")
(match_operand:DF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"divrl	%2,%1,%0"
[(set_attr "type" "fpdiv")])

(define_insn "divsf3"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(div:SF (match_operand:SF 1 "fp_arith_operand" "rGH")
(match_operand:SF 2 "fp_arith_operand" "rGH")))]
"TARGET_NUMERICS"
"divr	%2,%1,%0"
[(set_attr "type" "fpdiv")])

(define_insn "negdf2"
[(set (match_operand:DF 0 "register_operand" "=d,d*f")
(neg:DF (match_operand:DF 1 "register_operand" "d,r")))]
""
"*
{
if (which_alternative == 0)
{
if (REGNO (operands[0]) == REGNO (operands[1]))
return \"notbit	31,%D1,%D0\";
return \"mov	%1,%0\;notbit	31,%D1,%D0\";
}
return \"subrl	%1,0f0.0,%0\";
}"
[(set_attr "type" "fpadd")])

(define_insn "negsf2"
[(set (match_operand:SF 0 "register_operand" "=d,d*f")
(neg:SF (match_operand:SF 1 "register_operand" "d,r")))]
""
"@
notbit	31,%1,%0
subr	%1,0f0.0,%0"
[(set_attr "type" "fpadd")])

;;; The abs patterns also work even if the target machine doesn't have
;;; floating point, because in that case dstreg and srcreg will always be
;;; less than 32.

(define_insn "absdf2"
[(set (match_operand:DF 0 "register_operand" "=d*f")
(abs:DF (match_operand:DF 1 "register_operand" "df")))]
""
"*
{
int dstreg = REGNO (operands[0]);
int srcreg = REGNO (operands[1]);

if (dstreg < 32)
{
if (srcreg < 32)
{
if (dstreg != srcreg)
output_asm_insn (\"mov	%1,%0\", operands);
return \"clrbit	31,%D1,%D0\";
}
/* Src is an fp reg.  */
return \"movrl	%1,%0\;clrbit	31,%D1,%D0\";
}
if (srcreg >= 32)
return \"cpysre	%1,0f0.0,%0\";
return \"movrl	%1,%0\;cpysre	%0,0f0.0,%0\";
}"
[(set_attr "type" "multi")])

(define_insn "abssf2"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(abs:SF (match_operand:SF 1 "register_operand" "df")))]
""
"*
{
int dstreg = REGNO (operands[0]);
int srcreg = REGNO (operands[1]);

if (dstreg < 32 && srcreg < 32)
return \"clrbit	31,%1,%0\";

if (dstreg >= 32 && srcreg >= 32)
return \"cpysre	%1,0f0.0,%0\";

if (dstreg < 32)
return \"movr	%1,%0\;clrbit	31,%0,%0\";

return \"movr	%1,%0\;cpysre	%0,0f0.0,%0\";
}"
[(set_attr "type" "multi")])

;; Tetra (16 byte) float support.

(define_expand "cmptf"
[(set (reg:CC CC_REG)
(compare:CC (match_operand:TF 0 "register_operand" "")
(match_operand:TF 1 "nonmemory_operand" "")))]
"TARGET_NUMERICS"
"
{
i960_compare_op0 = operands[0];
i960_compare_op1 = operands[1];
DONE;
}")

(define_insn ""
[(set (reg:CC CC_REG)
(compare:CC (match_operand:TF 0 "register_operand" "f")
(match_operand:TF 1 "nonmemory_operand" "fGH")))]
"TARGET_NUMERICS"
"cmpr %0,%1"
[(set_attr "type" "fpcc")])

(define_expand "movtf"
[(set (match_operand:TF 0 "general_operand" "")
(match_operand:TF 1 "fpmove_src_operand" ""))]
""
"
{
if (i960_emit_move_sequence (operands, TFmode))
DONE;
}")

(define_insn "*fpmove_transfer0"
[(set (match_operand:TF 0 "nonimmediate_operand"  "=r,f,d,d,m")
(match_operand:TF 1 "fpmove_src_operand"    "r,GH,F,m,d"))]
"(register_operand (operands[0], TFmode) || register_operand (operands[1], TFmode))"
"* {
switch (which_alternative)
{
case 0:
if (FP_REG_P (operands[0]) || FP_REG_P (operands[1])) {
return \"movre	%1,%0\";
} else {
return \"movt	%1,%0\";
}
case 1:
return \"movre	%1,%0\";
case 2:
return i960_output_ldconst (operands[0], operands[1]);
case 3:
return \"ldt	%1,%0\";
case 4:
return \"stt	%1,%0\";
default:
abort();
}
}"
[(set_attr "type" "move,move,load,fpload,fpstore")])

(define_insn "extendsftf2"
[(set (match_operand:TF 0 "register_operand" "=f,d")
(float_extend:TF
(match_operand:SF 1 "register_operand" "d,f")))]
"TARGET_NUMERICS"
"@
movr	%1,%0
movre	%1,%0"
[(set_attr "type" "fpmove")])

(define_insn "extenddftf2"
[(set (match_operand:TF 0 "register_operand" "=f,d")
(float_extend:TF
(match_operand:DF 1 "register_operand" "d,f")))]
"TARGET_NUMERICS"
"@
movrl	%1,%0
movre	%1,%0"
[(set_attr "type" "fpmove")])

(define_insn "trunctfdf2"
[(set (match_operand:DF 0 "register_operand" "=d")
(float_truncate:DF
(match_operand:TF 1 "register_operand" "f")))]
"TARGET_NUMERICS"
"movrl	%1,%0"
[(set_attr "type" "fpmove")])

(define_insn "trunctfsf2"
[(set (match_operand:SF 0 "register_operand" "=d")
(float_truncate:SF
(match_operand:TF 1 "register_operand" "f")))]
"TARGET_NUMERICS"
"movr	%1,%0"
[(set_attr "type" "fpmove")])

(define_insn "floatsitf2"
[(set (match_operand:TF 0 "register_operand" "=f")
(float:TF (match_operand:SI 1 "register_operand" "d")))]
"TARGET_NUMERICS"
"cvtir	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "fix_trunctfsi2"
[(set (match_operand:SI 0 "register_operand" "=d")
(fix:SI (fix:TF (match_operand:TF 1 "register_operand" "f"))))]
"TARGET_NUMERICS"
"cvtzri	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "fixuns_trunctfsi2"
[(set (match_operand:SI 0 "register_operand" "=d")
(unsigned_fix:SI (fix:TF (match_operand:TF 1 "register_operand" "f"))))]
"TARGET_NUMERICS"
"cvtzri	%1,%0"
[(set_attr "type" "fpcvt")])

(define_insn "addtf3"
[(set (match_operand:TF 0 "register_operand" "=f")
(plus:TF (match_operand:TF 1 "nonmemory_operand" "%fGH")
(match_operand:TF 2 "nonmemory_operand" "fGH")))]
"TARGET_NUMERICS"
"addr	%1,%2,%0"
[(set_attr "type" "fpadd")])

(define_insn "subtf3"
[(set (match_operand:TF 0 "register_operand" "=f")
(minus:TF (match_operand:TF 1 "nonmemory_operand" "fGH")
(match_operand:TF 2 "nonmemory_operand" "fGH")))]
"TARGET_NUMERICS"
"subr	%2,%1,%0"
[(set_attr "type" "fpadd")])

(define_insn "multf3"
[(set (match_operand:TF 0 "register_operand" "=f")
(mult:TF (match_operand:TF 1 "nonmemory_operand" "%fGH")
(match_operand:TF 2 "nonmemory_operand" "fGH")))]
"TARGET_NUMERICS"
"mulr	%1,%2,%0"
[(set_attr "type" "fpmul")])

(define_insn "divtf3"
[(set (match_operand:TF 0 "register_operand" "=f")
(div:TF (match_operand:TF 1 "nonmemory_operand" "fGH")
(match_operand:TF 2 "nonmemory_operand" "fGH")))]
"TARGET_NUMERICS"
"divr	%2,%1,%0"
[(set_attr "type" "fpdiv")])

(define_insn "negtf2"
[(set (match_operand:TF 0 "register_operand" "=f")
(neg:TF (match_operand:TF 1 "register_operand" "f")))]
"TARGET_NUMERICS"
"subr	%1,0f0.0,%0"
[(set_attr "type" "fpadd")])

(define_insn "abstf2"
[(set (match_operand:TF 0 "register_operand" "=f")
(abs:TF (match_operand:TF 1 "register_operand" "f")))]
"(TARGET_NUMERICS)"
"cpysre	%1,0f0.0,%0"
[(set_attr "type" "fpmove")])
(define_insn "i960_sqrtr"
[(set (match_operand:SF 0 "register_operand" "=d*f")
(sqrt:SF (match_operand:SF 1 "fp_arith_operand" "%rGH")))]
"TARGET_NUMERICS"
"sqrtr %1,%0")
(define_insn "i960_sqrtrl"
[(set (match_operand:DF 0 "register_operand" "=d*f")
(sqrt:DF (match_operand:DF 1 "fp_arith_operand" "%rGH")))]
"TARGET_NUMERICS"
"sqrtrl %1,%0")
