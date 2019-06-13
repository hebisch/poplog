/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Lisp support assembly routines for ARM
*/

#_<

#_INCLUDE 'declare.ph'

lconstant macro (

        ;;; User stack pointer

        USP     = "r10",
        LR      = "lr",
);

>_#

    .arch armv5
    .file "alisp.s"
    .text

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

L0._userhi:
    .word I_LAB(_userhi)
nil.lab:
    .word C_LAB(nil)

DEF_C_LAB (_setstklen)
    ldr   r3, [USP, #4]
    ldr   r2, [USP], #8
    add   r2, r2, r3
    sub   r2, r2, #6
    ldr   r3, L0._userhi
    ldr   r3, [r3]
    sub   r2, r3, r2
    cmp   USP, r2
    bxeq  LR
    ;;; fall trough
DEF_C_LAB (_setstklen_diff)
    ;;; if too short set and return
    movcc USP, r2
    bxcc  LR
str.loop:
    ldr   r3, nil.lab
    str   r3, [USP, #-4]!
    cmp   USP, r2
    bne   str.loop
    bx    LR

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
