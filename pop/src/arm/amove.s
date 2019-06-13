/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Bitfield/compare/copy/move assembly routines for ARM
   Author:  Waldek Hebisch
*/

#_<

#_INCLUDE 'declare.ph'

lconstant macro (
    USP = "r10",
    LR  = "lr",
);

>_#

    .arch armv5
    .file   "amove.s"
;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

DEF_C_LAB (_bcmp)
DEF_C_LAB (_scmp)
DEF_C_LAB (_cmp)
DEF_C_LAB (_icmp)
    str LR, [sp, #-4]!
    sub sp, sp, #4
    ldr r0, [USP], #4
    ldr r1, [USP], #4
    ldr r2, [USP]
    bl memcmp
    cmp r0, #0
    ldreq r0, L.true
    ldrne r0, L.false
    str r0, [USP]
    add sp, sp, #4
    ldr pc, [sp], #4

L.false:
    .word C_LAB(false)
L.true:
    .word C_LAB(true)

DEF_C_LAB (_moveq)
    ;;; version below still works
DEF_C_LAB (_bmove)
DEF_C_LAB (_smove)
DEF_C_LAB (_imove)
DEF_C_LAB (_dmove)
DEF_C_LAB (_move)
    ldr r0, [USP], #4
    ldr r1, [USP], #4
    ldr r2, [USP]
    add r3, r0, r2
    str r3, [USP]
    b memmove

DEF_C_LAB (_bfill)
    ldr r0, [USP], #4
    ldr r2, [USP], #4
    ldr r1, [USP], #4
    b memset

DEF_C_LAB (_ifill)
DEF_C_LAB (_fill)
    ;;; dst
    ldr r0, [USP], #4
    ;;; length
    ldr r2, [USP], #4
    ;;; value
    ldr r1, [USP], #4
    cmp r2, #0
    bxle LR
fill.loop:
    str r1, [r0], #4
    subs r2, #4
    bgt fill.loop
    bx LR

L10.1:
    .word I_LAB(_userhi)
DEF_C_LAB (_move_userstack)
    ldr r3, L10.1
    ldr r0, [USP], #4
    ldr r2, [r3]
    add r1, r2, r0
    str r1, [r3]
    add r0, USP
    sub r2, r2, USP
    mov r1, USP
    mov USP, r0
    b memmove

DEF_C_LAB (_move_callstack)
    ldr r0, [USP], #4
    ldr r1, [USP], #4
    sub r2, r0, sp
    ;;; Note: due to signed overflow direction may be opposite
    ;;; to indicated by labels.  But since address 0 is illegal
    ;;; in case of overflow source area does not intersect
    ;;; destination area, so direction does not matter.
    cmp r1, #0
    bgt move.up
    blt move.down
    bx LR
    ;;; Stack moves up, so copy goes down
move.up:
    ;;; New limit in r12
    add r12, r0, r1
up.loop:
    ldr r3, [r0, #-4]!
    str r3, [r12, #-4]!
    subs r2, #4
    bgt up.loop
    ;;; finally adjust sp
    add sp, sp, r1
    bx LR
    ;;; Stack moves down, so copy goes up
move.down:
    ;;; copy, so we keep sp valid
    mov r12, sp
    ;;; adjust
    add sp, sp, r1
    ;;; again copy sp
    mov r0, sp
down.loop:
    ldr r3, [r12], #4
    str r3, [r0], #4
    subs r2, #4
    bgt down.loop
    bx LR

DEF_C_LAB(_bfield)
    and     r12, r1, #31
    mov     r1, r1, asr #5
    add     r3, r12, r0
    cmp     r3, #32
    ldr     r3, [r2, r1, asl #2]
    add     r1, r1, #1
    ldrhi   r1, [r2, r1, asl #2]
    mov     r3, r3, lsr r12
    rsbhi   r12, r12, #32
    orrhi   r3, r3, r1, asl r12
    rsb     r2, r0, #32
    mov     r0, r3, asl r2
    mov     r0, r0, lsr r2
    bx      LR

DEF_C_LAB(_sbfield)
    and     r12, r1, #31
    mov     r1, r1, asr #5
    add     r3, r12, r0
    cmp     r3, #32
    ldr     r3, [r2, r1, asl #2]
    add     r1, r1, #1
    ldrhi   r1, [r2, r1, asl #2]
    mov     r3, r3, lsr r12
    rsbhi   r12, r12, #32
    orrhi   r3, r3, r1, asl r12
    rsb     r2, r0, #32
    mov     r0, r3, asl r2
    mov     r0, r0, asr r2
    bx      LR

DEF_C_LAB (_ubfield)
    mvn     r4, #0
    mvn     r4, r4, asl r0
    mov     r3, r1, asr #5
    add     r2, r2, r3, asl #2
    ldr     r12, [r2]
    ldr     r3, [USP], #4
    and     r1, r1, #31
    bic     r12, r12, r4, asl r1
    and     r3, r3, r4
    add     r0, r1, r0
    orr     r12, r12, r3, asl r1
    cmp     r0, #32
    str     r12, [r2]
    ldrhi   r0, [r2, #4]
    rsbhi   r1, r1, #32
    bichi   r4, r0, r4, lsr r1
    orrhi   r1, r4, r3, lsr r1
    strhi   r1, [r2, #4]
    bx      LR

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
