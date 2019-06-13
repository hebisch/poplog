/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Floating point assembly routines for ARM
   Author:  Waldek Hebisch
 */

#_<

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Float_qrem
        ;

weak constant
    _pfcopy
    _pfzero
    _pfneg
    _pfeq
    _pfsgreq
    _pfsgr
    _pfabs
;
    

lconstant macro (

        ;;; User stack pointer

        USP     = "r10",
        LR      = "lr",

        ;;; Pop ddecimal structure fields:
        ;;; _DD_1 = MS part, _DD_2 = LS part

        _DD_1   = @@DD_1,
        _DD_2   = @@DD_2,

);

>_#
    .arch armv5
    .fpu vfp
    .file   "afloat.s"
    .text

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

/* Calling conventions:
   - double floats are represented by their addresess (pointers),
     addresess point to raw memory (without a key)
   - ddecimals are represented by pointers to Pop structures
   - decimals and single floats are passed by value
*/

DEF_C_LAB (_pfcopy)
    ldr     r3, [USP, #4]
    ldr     r2, [USP], #8
    fldd    d7, [r2]
    fstd    d7, [r3]
    bx LR

;;; Convertions

;;; Pop decimal to single float, just drop pop tag bit
DEF_C_LAB (_pf_sfloat_dec)
    b C_LAB (_pf_sfloat_dec)
    ldr   r3, [USP]
    sub   r3, r3, #1
    str r3, [USP]
    bx LR

;;; Used to read floats
;;; argument on top of stack is storage for result, second one
;;; is machine integer
DEF_C_LAB (_pf_dfloat_int)
    flds    s13, [USP, #4]
    ldr     r2, [USP], #8
    ;;; fmsr    s13, r3
    fsitod  d7, s13
    fstd    d7, [r2, #0]
    bx LR

DEF_C_LAB (_pf_dfloat_uint)
    flds    s13, [USP, #4]
    ldr     r2, [USP], #8
    fuitod  d7, s13
    fstd    d7, [r2, #0]
    bx LR

;;; Pop decimal to double float, pointer to result is on top
;;; of user stack
DEF_C_LAB (_pf_dfloat_dec)
    ldr   r3, [USP, #4]
    ldr   r2, [USP], #8
    sub   r3, r3, #1
    fmsr  s13, r3
    fcvtds  d7, s13
    fstd    d7, [r2, #0]
    bx LR

;;; Pop ddecimal to double float, pointer to result is on top
;;; of user stack
DEF_C_LAB (_pf_dfloat_ddec)
    ldr   r3, [USP, #4]
    ldr   r2, [USP], #8
    ldr   r1, [r3, #_DD_1]
    str   r1, [r2, #4]
    ldr   r1, [r3, #_DD_2]
    str   r1, [r2]
    bx LR

;;; Double float do Pop decimal, returns false on overflow
DEF_C_LAB (_pf_cvt_to_dec)
    ldr   r3, [USP]
    ;;; Rounding: single float has 8 bit exponent, while double
    ;;; float has 11 bit exponent, so when converting double
    ;;; to single 3 lowest bits of single come from low
    ;;; word of double.  So rounding depends only on low word
    ;;; of double.  We round towards zero when we have
    ;;;   *0*...
    ;;; or
    ;;;   010...0
    ;;; Otherwise we round outwards.  To get correct rounding
    ;;; we set 3 bits in double to 1, computing OR with
    ;;; 0x38000000.  In *0*... case convertion
    ;;; from double to single gives correct 30 upper bits and
    ;;; masking 2 lowest bits gives correctly rounded decimal.
    ;;; In *1*... case we have set 3 bits to get *1111*... After
    ;;; that convertion from double to single rounds outwards
    ;;; and also gives correct 30 upper bits, except for
    ;;; 010...0 case.  In this last case we just use double as-is.
    ldr   r1, [r3]
    cmp   r1, #0x40000000
    ldr   r2, [r3, #4]
    orrne r1, #0x38000000
    fmdrr d7, r1, r2
    fcvtsd  s13, d7
    fmrs   r2, s13
    bic    r2, r2, #3
    orr    r2, #1
    mov    r1, r2, asl #1
    mov    r1, r1, lsr #24
    cmp    r1, #0
    moveq  r2, #1
    cmp    r1, #0xff
    ldreq  r2, L.false
    str    r2, [USP]
    bx LR

;;; Double float do Pop ddecimal,
DEF_C_LAB (_pf_cvt_to_ddec)
    ldr   r3, [USP, #4]
    ldr   r2, [USP], #8
    ldr   r1, [r3, #4]
    str   r1, [r2, #_DD_1]
    ldr   r1, [r3]
    str   r1, [r2, #_DD_2]
    bx LR

DEF_C_LAB (_pf_round_d_to_s)
    ldr     r3, [USP]
    fldd    d7, [r3]
    fcvtsd  s13, d7
    fmrs    r2, s13
    mov     r1, r2, asl #1
    mov     r1, r1, lsr #24
    cmp     r1, #0xff
    beq     L.ret_false
    fsts    s13, [r3]
    bx    LR

DEF_C_LAB (_pf_extend_s_to_d)
    ldr   r3, [USP]
    ldr   r2, [r3]
    mov   r1, r2, asl #1
    mov   r1, r1, lsr #24
    cmp   r1, #0xff
    beq   L.ret_false
    fmsr  s13, r2
    fcvtds  d7, s13
    fstd    d7, [r3]
    bx    LR

L.expmask:
    .word 0x7ff00000

DEF_C_LAB(_pf_check_d)
    ldr   r3, [USP]
    ldr   r2, [r3, #4]
    ldr   r1, L.expmask
    and   r2, r1
    cmp   r2, r1
L.ret_false:
    ldreq r3, L.false
    streq r3, [USP]
    bx LR
    
DEF_C_LAB (_pf_intof)
    ldr r3, [USP]
    fldd    d7, [r3]
    fldd    d6, L.maxint
    fcmped  d7, d6
    fmstat
    bpl     L.out_of_range
    fldd    d6, L.minint
    fcmped  d7, d6
    fmstat
    ble     L.out_of_range
    ftosizd s13, d7
    fsts    s13, [USP]
    ldr   r0, L.true
    str  r0, [USP, #-4]!
    bx LR
L.out_of_range:
    ldr   r0, L.false
    str  r0, [USP]
    bx LR
    .align  3
L.maxint:
    .word   0
    .word   1105199104
L.minint:
    .word   2097152
    .word   -1042284544
L.maxuint:
    .word   0 
    .word   1106247680  

DEF_C_LAB (_pf_uintof)
    ldr r3, [USP]
    fldd    d7, [r3]
    fldd    d6, L.maxuint
    fcmped  d7, d6
    fmstat
    bpl     L.out_of_range
    fcmpezd d7
    fmstat
    bmi     L.out_of_range
    ftouizd s13, d7
    fsts    s13, [USP]
    ldr   r0, L.true
    str  r0, [USP, #-4]!
    bx LR

DEF_C_LAB (_pf_expof)
    ldr     r0, [USP]
    ldr     r2, [r0, #4]
    mov     r3, r2, asl #1
    mov     r3, r3, lsr #21
    sub     r3, r3, #1020
    sub     r3, r3, #2
    str     r3, [USP]
    bx      LR

DEF_C_LAB(-> _pf_expof)
    ldr     r0, [USP]
    ldr     r1, [USP, #4]!
    add     r1, r1, #1020
    ldr     r2, [r0, #4]
    ldr     r3, L.expmask
    add     r1, r1, #2
    bic     r2, r3
    cmp     r1, #2048
    bcs     L.exp_too_big
    mov     r1, r1, asl #20
    orr     r2, r1
    str     r2, [r0, #4]
    ldr     r3, L.true
    str     r3, [USP]
    bx LR
L.exp_too_big:
    ldr     r3, L.false
    str     r3, [USP]
    bx LR

DEF_C_LAB (_pfmodf)
    ldr     r1, [USP, #4]
    ldr     r0, [USP], #8
    ldr     r2, [r0, #4]
    mov     r3, r2, asl #1
    mov     r3, r3, lsr #21
    sub     r3, r3, #1020
    subs    r3, r3, #3
    bmi     L.negexp
    cmp     r3, #51
    bgt     L.bigexp
    ;;; Normal case, store argument in d7
    fldd    d7, [r0]
    cmp     r3, #20
    ldrgt   r2, [r0]
    mvn     r12, #0
    rsbgt   r3, r3, #52
    rsble   r3, r3, #20
    and     r12, r2, r12, asl r3
    movle   r2, #0
    stmleia r0, {r2, r12}
    strgt   r12, [r0]
    ;;; Load integer part from memory, subtract from argument giving
    ;;; fractional part, store result
    fldd    d6, [r0]
    fsubd   d7, d7, d6
    fstd    d7, [r1]
    bx      lr

L.bigexp:
    mov     r3, #0
    str     r3, [r1]
    str     r3, [r1, #4]
    bx      lr
L.negexp:
    ldr     r2, [r0]
    mov     r3, #0
    str     r2, [r1]
    ldr     r2, [r0, #4]
    str     r2, [r1, #4]
    str     r3, [r0]
    str     r3, [r0, #4]
    bx      lr

DEF_C_LAB (_pfzero)
    ldr r3, [USP]
    fldd    d7, [r3]
    fcmpzd  d7
    fmstat
    ldrne   r0, L.false
    ldreq   r0, L.true
    str  r0, [USP]
    bx LR

DEF_C_LAB (_pfneg)
    ldr r3, [USP]
    fldd    d7, [r3]
    fcmpezd d7
    fmstat
    ldrpl   r0, L.false
    ldrmi   r0, L.true
    str  r0, [USP]
    bx LR

DEF_C_LAB (_pfeq)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d7, [r3]
    fldd    d6, [r2]
    ;;; d6 <= d7 ?
    fcmpd  d6, d7
    fmstat
    ldrne   r0, L.false
    ldreq   r0, L.true
    str  r0, [USP]
    bx LR

DEF_C_LAB (_pfsgreq)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d7, [r3]
    fldd    d6, [r2]
    ;;; d6 <= d7 ?
    fcmped  d6, d7
    fmstat
    ldrhi   r0, L.false
    ldrls   r0, L.true
    str  r0, [USP]
    bx LR

DEF_C_LAB (_pfsgr)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d7, [r3]
    fldd    d6, [r2]
    ;;; d6 < d7 ?
    fcmped  d6, d7
    fmstat
    ldrpl   r0, L.false
    ldrmi   r0, L.true
    str  r0, [USP]
    bx LR

DEF_C_LAB (_pfabs)
    ldr r3, [USP], #4
    fldd    d7, [r3]
    fabsd   d7, d7
    fstd    d7, [r3]
    bx      LR

DEF_C_LAB (_pfnegate)
    ldr r3, [USP], #4
    fldd    d7, [r3]
    fnegd   d7, d7
    fstd    d7, [r3]
    bx      LR

;;; address of first argument is on top of stack, the second is also
;;; the result
DEF_C_LAB (_pfadd)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d7, [r3]
    fldd    d6, [r2]
    faddd   d7, d7, d6
    fstd    d7, [r3]
    bx      LR

DEF_C_LAB (_pfsub)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d7, [r3]
    fldd    d6, [r2]
    fsubd   d7, d7, d6
    fstd    d7, [r3]
    bx      LR

DEF_C_LAB (_pfmult)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d7, [r3]
    fldd    d6, [r2]
    fmuld   d7, d7, d6
    fstd    d7, [r3]
    bx      LR

DEF_C_LAB (_pfdiv)
    ldr r3, [USP, #4]
    ldr r2, [USP], #4
    fldd    d6, [r2]
    fldd    d7, [r3]
    fdivd   d7, d7, d6
    fstd    d7, [r3]
    bx LR

L.true:
    .word C_LAB(true)
L.false:
    .word C_LAB(false)

DEF_C_LAB (_pfqrem)
    b XC_LAB(Sys$-Float_qrem)

    .align  3

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
