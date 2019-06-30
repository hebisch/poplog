/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/amain.s
 * Purpose:     Poplog entry point for Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

lconstant macro (
    USP     = "rbx",
    SAVED_USP   = [I_LAB(Sys$-Extern$- _saved_usp)],
);

section $-Sys;

vars
    _init_args,
    ;

endsection;

>_#

    .file   "amain.s"

/************************* wrapping structures ************************/

    .text
    .quad   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .quad   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text


;;; _MAIN:
;;; Entry point to POPLOG; called from _START in /lib/crt0.

;;; Call:
;;; main(argc, argv, envp)

DEF_C_LAB (Sys$- _entry_point)
    .globl  EXTERN_NAME(main)
EXTERN_NAME(main):

    ;;; Align stack

    pushq   $0

    ;;; Clear __pop_in_user_extern

    movq    $0, EXTERN_NAME(__pop_in_user_extern)

    ;;; Save pointer to argument vector (argv)

    movq    %rsi,  I_LAB(Sys$- _init_args)

#_IF DEF LINUX

    ;;; set personality

    call linux_setper

#_ENDIF

    ;;; Initialise the floating-point unit

    call    fpu_init

    ;;; Set up a temporary user stack pointer

    movq    SAVED_USP, %USP

    ;;; clear Pop registers
    movq    $3, %r13
    movq    $3, %r14
    movq    $3, %r15

    ;;; Start the system
    call    XC_LAB(setpop)

    ;;; Exit with 0

    movl    $0, %edi
    call    EXTERN_NAME(_exit)

    .align  16


/***************** end labels for wrapping structures *****************/

    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
    .data
Ldata_end:
    .set Ldata_size, Ldata_end-Ldata_start

/**********************************************************************/

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO
--- Poplog System, Jan 18 1995
    Mods for Linux & S*CO (may not need S*CO-specific stuff).
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
 */
