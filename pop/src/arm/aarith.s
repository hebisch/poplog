/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Assembler arithmetic routines for ARM
   Author:  Waldek Hebisch
*/

#_<

#_INCLUDE 'declare.ph'

lconstant macro (

    USP   = "r10",
    LR    = "lr", 
    PB    = "r11",

    _PD_EXECUTE             = @@PD_EXECUTE,
    _PD_ARRAY_TABLE         = @@PD_ARRAY_TABLE,
    _PD_ARRAY_VECTOR        = @@PD_ARRAY_VECTOR,
    _PD_ARRAY_SUBSCR_PDR    = @@PD_ARRAY_SUBSCR_PDR,

);

>_#

    .arch armv5
    .file "aarith.s"

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

;;; Bit operations
DEF_C_LAB 4 (_biset)
    ldr r1, [USP], #4
    ldr r0, [USP]
    orr r1, r0, r1
    str r1, [USP]
    bx LR

DEF_C_LAB 4 (_biclear)
    ldr r1, [USP], #4
    ldr r0, [USP]
    bic r1, r0, r1
    str r1, [USP]
    bx LR

DEF_C_LAB 4 (_bimask)
    ldr r1, [USP], #4
    ldr r0, [USP]
    and r1, r0, r1
    str r1, [USP]
    bx LR

DEF_C_LAB 4 (_bixor)
    ldr r1, [USP], #4
    ldr r0, [USP]
    eor r1, r0, r1
    str r1, [USP]
    bx LR

;;; Machine and POP arithmetic
DEF_C_LAB 4 (_mult)
    ldr r1, [USP], #4
    ldr r0, [USP]
    mul r1, r0, r1
    str r1, [USP]
    bx LR

DEF_C_LAB 4 (_pmult)
    ldr r1, [USP], #4
    mov r1, r1, asr #2
    ldr r0, [USP]
    sub r0, r0, #3
    mul r1, r0, r1
    add r1, r1, #3
    str r1, [USP]
    bx LR

DEF_C_LAB 4 (_div)
    stmfd   sp!, {r0, lr}
    ldr r0, [USP, #4]
    ldr r1, [USP]
    bl do_div
    ldr r1, [USP]
    ldr r3, [USP, #4]
    str r0, [USP]
    mul r2, r1, r0
    sub r3, r3, r2
    str r3, [USP, #4]
    ldmfd   sp!, {r0, pc}

DEF_C_LAB 2 (_divq)
    stmfd   sp!, {r0, lr}
    ldr r1, [USP], #4
    ldr r0, [USP]
    bl do_div
    str r0, [USP]
    ldmfd   sp!, {r0, pc}

DEF_C_LAB 4 (_pdiv)
    stmfd   sp!, {r0, lr}
    ldr r0, [USP, #4]
    mov r0, r0, asr #2
    ldr r1, [USP]
    mov r1, r1, asr #2
    bl do_div
    ldr r1, [USP]
    mov r1, r1, asr #2
    ldr r3, [USP, #4]
    mov r3, r3, asr #2
    mul r2, r1, r0
    mov r0, r0, asl #2
    orr r0, r0, #3
    str r0, [USP]
    sub r3, r3, r2
    mov r3, r3, asl #2
    orr r3, r3, #3
    str r3, [USP, #4]
    ldmfd   sp!, {r0, pc}

DEF_C_LAB (_pmult_testovf)
    ldr r0, [USP]
    ldr r1, [USP, #4]
    mov r0, r0, asr #2
    sub r1, r1, #3
    smulls r0, r3, r1, r0
    cmp r0, #0
    orr r0, #3
    str r0, [USP, #4]
    mov r1, #0
    movmi r1, #-1
    cmp r3, r1
    ldreq r0, L.true
    ldrne r0, L.false
    str r0, [USP]
    bx LR

DEF_C_LAB (_pint_testovf)
    ldr r0, [USP]
    mov r1, r0, asl #2
    mov r2, r1, asr #2
    orr r1, #3
    cmp r2, r0
    streq r1, [USP], #-4
    ldreq r0, L.true
    ldrne r0, L.false
    str r0, [USP]
    bx LR

DEF_C_LAB (_pshift_testovf)
    ldr r0, [USP, #4]
    subs r0, r0, #3
    beq L.pshift_done
    ldr r1, [USP]
    cmp r1, #30
    bcs L.pshift_ovf
    mov r2, r0, asl r1
    mov r3, r2, asr r1
    cmp r0, r3
    bne L.pshift_ovf
    orr r2, #3
    str r2, [USP, #4]
L.pshift_done:
    ldr r0, L.true
    str r0, [USP]
    bx LR
L.pshift_ovf:
    ldr r0, L.false
    ;;; pop one arg and replace the other
    str r0, [USP, #4]!
    bx LR

L.false:
    .word C_LAB(false)
L.true:
    .word C_LAB(true)

;;; Helper for random number generator
DEF_C_LAB (_posword_mul_high)
    ldr r0, [USP, #4]
    ldr r2, [USP], #4
    mov r3, r0, asl #1
    umull   r0, r1, r3, r2
    str r1, [USP]
    bx      lr

;;; Bignum routines
DEF_C_LAB (_bgi_add)
    ;;; Push last argument and link register to the control stack
    ldr r0, [USP, #16]
    stmfd sp!, {r0, lr}
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #20
    bl do_bgi_add
    ;;; Clean control stack and return
    ldmfd sp!, {r0, pc}

DEF_C_LAB (_bgi_sub)
    ;;; Push last argument and link register to the control stack
    ldr r0, [USP, #16]
    stmfd sp!, {r0, lr}
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #20
    bl do_bgi_sub
    ;;; Clean control stack and return
    ldmfd sp!, {r0, pc}

DEF_C_LAB (_bgi_negate)
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r0, [USP], #12
    ;;; transfer control to C code
    b do_bgi_negate

DEF_C_LAB (_bgi_negate_no_ov)
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r0, [USP], #12
    ;;; transfer control to C code
    b do_bgi_negate_no_ov

;;; left shift
;;; Arguments:
;;;   destination is on top of the user stack
;;;   next is shift amount
;;;   next is lenght of biginit argument
;;;   next is biginit argument
DEF_C_LAB (_bgi_lshift)
    stmfd sp!, {r0, lr}
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #12
    bl do_bgi_lshift
    str r0, [USP]
    ldmfd sp!, {r0, pc}

;;; logical right shift
DEF_C_LAB (_bgi_rshiftl)
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #16
    b do_bgi_rshiftl

;;; Multiply unsigned biginit by unsigned machine integer (slice)
;;; Arguments:
;;;   destination is on top of the user stack
;;;   next is length of biginit argument
;;;   next is biginit argument
;;;   next is machine integer
DEF_C_LAB (_bgi_mult)
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #16
    ;;; transfer control to C code
    b do_bgi_mult

DEF_C_LAB (_bgi_mult_add)
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #16
    ;;; transfer control to C code
    b do_bgi_mult_add

;;; Subtruct product from destination
DEF_C_LAB (_bgi_sub_mult)
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #16
    ;;; transfer control to C code
    b do_bgi_sub_mult

DEF_C_LAB (_bgi_div)
    stmfd sp!, {r0, lr}
    ;;; Load arguments to registers
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r3, [USP, #12]
    ldr r0, [USP], #12
    ;;; call C code
    bl do_bgi_div
    str r0, [USP]
    ldmfd sp!, {r0, pc}

DEF_C_LAB (_quotient_estimate_init)
    stmfd sp!, {r0, lr}
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r0, [USP], #8
    ;;; call C code
    bl do_quotient_estimate_init
    str r0, [USP]
    ldmfd sp!, {r0, pc}

DEF_C_LAB (_quotient_estimate)
    stmfd sp!, {r0, lr}
    ldr r1, [USP, #4]
    ldr r2, [USP, #8]
    ldr r0, [USP], #8
    ;;; call C code
    bl do_quotient_estimate
    str r0, [USP]
    ldmfd sp!, {r0, pc}

;;; Array indexing
    ;;; Register usage
    ;;; r2   - running pointer in array descriptor
    ;;; r12  - running total offset
DEF_C_LAB (_array_sub)
    ;;; initialize pointer to array descriptor
    add r2, PB, #_PD_ARRAY_TABLE
    ;;; get inital offset
    ldr r12, [r2], #4
    ;;; load dimension, check if end
    ldr r1, [r2]
    tst r1, r1
    beq .Lfin
.Larg_loop:
    ldr r3, [USP], #4
    tst r3, #2
    beq .Lsub_error
    ldr r0, [r2, #4]
    sub r3, r3, r0
    cmp r3, r1
    bcs .Lsub_error
    ldr r0, [r2, #8]
    tst r0, r0
    ;;; Zero means multiplication by 1
    ;;; FIXME: avoid this
    beq .Lmul
    mul r3, r0, r3
.Lmul:
    ;;; load next dimension
    ldr r1, [r2, #12]!
    ;;; update total
    add r12, r12, r3
    ;;; check if end
    tst r1, r1
    bne .Larg_loop
.Lfin:
    str r12, [USP, #-4]!
    ldr r0, [PB, #_PD_ARRAY_VECTOR]
    str r0, [USP, #-4]!
    ldr r0, [PB, #_PD_ARRAY_SUBSCR_PDR]
    ldr pc, [r0, #_PD_EXECUTE]
.Lsub_error:
    sub USP, USP, #4
    b XC_LAB(weakref Sys$-Array$-Sub_error)
    bl XC_LAB(setpop)

;;; Shifts

DEF_C_LAB (_rshift)
    ldr r1, [USP], #4
    ldr r0, [USP]
    mov r1, r1, asr #2
    mov r0, r0, asr #2
    cmp r1, #0
    movge r0, r0, asr r1
    rsblt r1, r1, #0
    movlt r0, r0, asl r1
    mov r0, r0, asl #2
    orr r0, r0, #3
    str r0, [USP]
    bx LR

;;; DEF_C_LAB (_shift)
    ldr r1, [USP], #4
    ldr r0, [USP]
    mov r1, r1, asr #2
    mov r0, r0, asr #2
    cmp r1, #0
    movge r0, r0, asl r1
    rsblt r1, r1, #0
    movlt r0, r0, asr r1
    mov r0, r0, asl #2
    orr r0, r0, #3
    str r0, [USP]
    bx LR

    .align  3

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
