/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Assembly routines supporting calling external
            functions for ARM
   Author:  Waldek Hebisch
*/

#_<

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'
#_INCLUDE 'numbers.ph'

lconstant macro (
    USP   = "r10",
    LR    = "lr",
    PB    = "r11",
    SP    = "sp",

    _K_EXTERN_TYPE  = @@K_EXTERN_TYPE,
    _EFC_FUNC   = @@EFC_FUNC,
    _EFC_ARG    = @@EFC_ARG,
    _EFC_ARG_DEST   = @@EFC_ARG_DEST,
);

if _pint(_K_EXTERN_TYPE) /== (12*4 + 2) then
    mishap(_pint(_K_EXTERN_TYPE), (12*4 + 2), 2,
           '_K_EXTERN_TYPE must agree with ext_arm.c');
endif;

>_#

    .arch armv5
    .fpu vfp
    .file   "aextern.s"
    .text

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

saved_sp.lab:
    .word I_LAB(Sys$-Extern$- _saved_sp)

saved_usp.lab:
    .word I_LAB(Sys$-Extern$- _saved_usp)

pop_in_extern.lab:
    .word EXTERN_NAME(__pop_in_user_extern)

ext_result.lab:
    .word C_LAB(Sys$-Extern$-result_struct)

;;; _call_external(nargs, routine, fltsingle)
DEF_C_LAB (_call_external)
    ;;; Save caller's stack pointer for interrupt/callback
    ldr r0, saved_sp.lab
    str SP, [r0]
    
    ;;; save registers.  Caller should save Pop registers
    ;;; to avoid problems with garbage collection, so
    ;;; we do not store them
    ;;;
    ;;; ATM we do not support register variables, but we store some
    stmfd SP!, {r4, r5, r9, lr}

    ;;; Load fixed arguments

    ldr    r5, [USP], #4    ;;; fltsingle
    ldr    r8, [USP], #4    ;;; routine
    ldr    r0, [USP], #4    ;;; nargs

    ;;; allocate buffer for register arguments
    ;;; 4*4 for integer arguments, 8*8 for floating point ones
    ;;; together 80 = 4*4 + 8*8
    sub    SP, SP, #80
    mov    r7, SP

    ;;; allocate space for stack arguments
    ;;; at least 4 arguments are passed in registers, if there
    ;;; are only integer arguments on the stack, then (narg - 4) words
    ;;; on the stack is enough.  If there are double arguments on
    ;;; the stack, then at least 8 arguments is passed in registers
    ;;; and 2*(narg - 8) words on the stack is enough.  The second
    ;;; is bigger when narg >= 12,

    ;;; narg <= 4 => need no stack space
    cmp r0, #4
    bls do_args
    cmp r0, #12
    bcc alloc8
    ;;; narg >= 12 => allocate 2*(narg - 8) words
    sub r2, r0, #8
    sub SP, SP, r2, asl #3
    b do_args
alloc8:
    ;;; 4 < narg < 12 => just allocate 8 words
    sub SP, SP, #32

do_args:
    mov r2, SP
    sub SP, SP, #4
    str r5, [SP, #-4]!
    mov r3, r7
    mov r1, USP
    mov r5, r0
    ;;; copy_external_arguments(int n, int * ap, void * sp, reg_buff * rp,
    ;;;                 int fltsingle) {
    bl copy_external_arguments
    add SP, SP, #8
    add USP, USP, r5, asl #2
    ;;; load buffered arguments to registers
    ldmfd r7!, {r0, r1, r2, r3}
    fldmiad r7, {d0-d7}
    ldr r5, saved_usp.lab
    str USP, [r5]
    ldr r5, pop_in_extern.lab
    str SP, [r5]
    blx r8
    ldr r5, pop_in_extern.lab
    mov r3, #0
    str r3, [r5]
    ldr r5, saved_usp.lab
    ldr USP, [r5]
    ;;; store result
    ldr r3, ext_result.lab
    str r0, [r3, #8]
    fstd    d0, [r3]

    ;;; restore SP point to saved registers
    ldr r0, saved_sp.lab
    ldr r0, [r0]
    sub SP, r0, #16

    ;;; Restore registers
    ldmfd SP!, {r4, r5, r9, lr}

    ;;; restore PB
    ldr PB, [SP]
    ;;; Zero registers
    mov r8, #3
    mov r7, r8
    mov r6, r8

    ;;; Zero saved SP to indicates external call over
    mov r1, #0
    str r1, [r0]
    bx LR

DEF_C_LAB(Sys$- _exfunc_clos_action)
    stmfd SP!, {r0, r1}
    ldr r0, [r12, #_EFC_ARG_DEST]
    ldr r1, [r12, #_EFC_ARG]
    str r1, [r0]
    ldmfd SP!, {r0, r1}
    ldr r12, [r12, #_EFC_FUNC]
    ldr pc, [r12]

.globl  EXTERN_NAME(_pop_external_callback)
EXTERN_NAME(_pop_external_callback):
DEF_C_LAB(Sys$- _external_callback_func)
    ;;; Save call-preserved C registers and return address
    stmfd SP!, {r4, r5, r6, r7, r8, r9, r10, r11, lr}

    ;;; Zero pop registers
    mov r4, #0
    mov r5, r4
    mov r6, r4

    ;;; Save __pop_in_user_extern
    ldr r8, pop_in_extern.lab
    ldr r3, [r8]
    str r3, [SP, #-4]!

    ;;; Create dummy stack frame, 4 words to keep alignment
    sub SP, SP, #16

    ;;; Zero __pop_in_user_extern
    str r4, [r8]

    ;;; Restore old PB
    ldr r8, saved_sp.lab
    ldr r3, [r8]
    ldr PB, [r3]

    ;;; Restore USP
    ldr r8, saved_usp.lab
    ldr USP, [r8]

    ;;; Pass argp
    str r0, [USP, #-4]!

    ;;; Dummy for change in break addr
    str r4, [USP, #-4]!

    bl XC_LAB(Sys$-Extern$-Callback)

    ;;; Handle return value
    ldr r0, [USP], #4

    ;;; Save USP
    ldr r8, saved_usp.lab
    str USP, [r8]

    ;;; Remove dummy stack frame
    add SP, SP, #16

    ;;; Restore __pop_in_user_extern
    ldr r8, pop_in_extern.lab
    ldr r3, [SP], #4
    str r3, [r8]

    ;;; Restore C registers and return
    ldmfd SP!, {r4, r5, r6, r7, r8, r9, r10, r11, pc}

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
