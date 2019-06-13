/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Poplog entry point for ARM
   Author:  Waldek Hebisch
*/


#_<

#_INCLUDE 'declare.ph'

lconstant macro (
    USP         = "r10",
    SAVED_USP   = [I_LAB(Sys$-Extern$- _saved_usp)],
);

section $-Sys;

vars
    _init_args,
    ;

endsection;

>_#

    .arch armv5
    .file   "amain.s"

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:


;;; _MAIN:
;;; Entry point to Poplog; called from C/system startup.

;;; Call:
;;; main(argc, argv, envp)

L1.0:   .word EXTERN_NAME(__pop_in_user_extern)
L1.1:   .word I_LAB(Sys$- _init_args)
L1.2:   .word SAVED_USP
DEF_C_LAB (Sys$- _entry_point)
    .globl  EXTERN_NAME(main)
EXTERN_NAME(main):
    ;;; To keep stack aligned need to store even number of
    ;;; registers
    stmfd sp!, {r4, r9, r11, lr}
        
    ;;; Save pointer to argument vector (argv)
    ldr r3, L1.1
    str r1, [r3]

    ;;; Clear __pop_in_user_extern

    ldr  r3, L1.0
    mov  r0, #0
    str  r0, [r3]

#_IF DEF LINUX

    ;;; set personality

    bl linux_setper

#_ENDIF

    ;;; Initialise the floating-point unit

    ;;; call    fpu_init

    ;;; Set up a temporary user stack pointer

    ldr USP, L1.2
    ldr USP, [USP]

    ;;; clear Pop registers
    ;;; mov    $3, %r13
    ;;; mov    $3, %r14
    ;;; mov    $3, %r15
        
    ;;; Start the system
    bl  XC_LAB(setpop)

    ;;; Exit with 0

    mov r0, #0
    bl  EXTERN_NAME(_exit)

    .align  3

;;; End wrapper: set sizes
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start

