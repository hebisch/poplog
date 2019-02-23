/* --- Copyright University of Sussex, University of Birmingham 2008. All rights reserved. ----------
 * File:        S.pcunix/src/amain.s
 * Purpose:     Poplog entry point for Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

lconstant macro (
    USP     = "ebx",
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
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
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

    ;;; Clear __pop_in_user_extern

    movl    $0, EXTERN_NAME(__pop_in_user_extern)

    ;;; Save pointer to argument vector (argv)

    movl    8(%esp), %eax
    movl    %eax, I_LAB(Sys$- _init_args)


#_IF DEF LINUX

        ;;; set personality (AS, from Waldek Hebisch. 2 Dec 2008)
        movl 12(%esp), %edx
        movl  4(%esp), %ecx
        push %edx
        push %eax
        push %ecx
        call linux_setper
        addl $12, %esp

#_ENDIF

    ;;; Initialise the floating-point unit

    call    fpu_init

    ;;; Set up a temporary user stack pointer

    movl    SAVED_USP, %USP

    ;;; Start the system

    call    XC_LAB(setpop)

    ;;; Exit with 0

    pushl   $0
    call    EXTERN_NAME(_exit)

    .align  4


/***************** end labels for wrapping structures *****************/

    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
    .data
Ldata_end:
    .set Ldata_size, Ldata_end-Ldata_start

/**********************************************************************/

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 2 Dec 2008
    Installed call of linux_setper, using code provided by
    Waldek Hebisch, to make running with 'setarch' unnecessary.
--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO
--- Poplog System, Jan 18 1995
    Mods for Linux & S*CO (may not need S*CO-specific stuff).
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
 */
