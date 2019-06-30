/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/asignals.s
 * Purpose: Signal handler for Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

section $-Sys;

constant procedure (
    Callstack_reset,
    Error_signal,
    Vfork_parent,
);

vars
    Extern$- _saved_sp,
;

endsection;

lconstant macro (

    USP = "rbx",    ;;; User stack pointer
    PB  = "rbp",    ;;; Procedure base register

    SAVED_SP = [I_LAB(Sys$-Extern$- _saved_sp)],

);

>_#

    .file   "asignals.s"

/************************* wrapping structures ************************/

    .text
    .quad   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .quad   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text

;;; -- ROUTINES TO CLEAN UP AND CALL A POP ERROR HANDLER ------------------

;;; RESET_POP_ENVIRON:
;;; Restore a safe environment for Poplog

reset_pop_environ:

    ;;; Save return address, as stack may be adjusted later

    popq    %rax

    ;;; Restore environment

    movq    $0, EXTERN_NAME(__pop_in_user_extern)   ;;; just in case
    movq    I_LAB(_userhi), %USP            ;;; reset user stack
    movq    $0, EXTERN_NAME(__pop_fpe_handler)  ;;; clear FPE handler

    ;;; If saved stack pointer is non-zero, we're in an external call

    cmpq    $0, SAVED_SP
    je  L1.1
    movq    SAVED_SP, %rsp          ;;; reset stack pointer
    movq    $0, SAVED_SP            ;;; clear saved stack pointer
    movq    (%rsp), %PB         ;;; restore procedure base

L1.1:   jmp *%rax               ;;; return to caller

    .align  16


;;; __POP_ERRSIG:
;;; Called by error signals (e.g SEGV)

    .globl  EXTERN_NAME(__pop_errsig)
EXTERN_NAME(__pop_errsig):

    call    reset_pop_environ
    call    XC_LAB(Sys$-Error_signal)   ;;; never returns

    .align  16


;;; __POP_ERROR_RETURN:
;;;     called from a signal handler to reset the return address in the
;;;     signal context

    .globl  EXTERN_NAME(__pop_error_return)
EXTERN_NAME(__pop_error_return):

    movq    8(%rsp), %rax
    movq    %rax, 68(%rbp)
    ret

    .align  16


;;; -- SYSTEM CALL INTERFACE ----------------------------------------------

#_IF DEF BSD_VFORK

;;; _DO_VFORK:
;;; Interface to VFORK system call

DEF_C_LAB (Sys$- _do_vfork)

    call    _vfork
    testq   %rax, %rax
    js  L2.1
    jz  L1.2

    ;;; Parent process:

    movq    $0, SAVED_SP        ;;; in case child in extern call
    subq    $16, %USP
    movq    %eax, 8(%USP)           ;;; child PID
    movq    $C_LAB(weakref Sys$-Vfork_parent), (%USP)
    call    XC_LAB(Sys$-Callstack_reset)    ;;; never returns

L1.2:   ;;; Child process:

    subq    $8, %USP
    movq    $0, (%USP)
    ret

L2.1:   ;;; Vfork failed:

    subq    $8, %USP
    movq    $(-1), (%USP)
    ret

    .align  16

#_ENDIF

;;; _CALL_SYS:
;;; Make a system call, transferring arguments from the user stack to
;;; the system stack. For system use only.

;;; Call:
;;; _call_sys(_NARGS, _FNC_ADDR) -> _RES

;;; Register usage:
;;; EAX work; result of the system call
;;; ECX number of arguments to the system call
;;; EDX address of the system call routine

;;; Memory usage:
;;; SAVED_SP
;;;     global variable which holds a pointer to the stack frame
;;;     of the calling procedure; used by _SIGNAL_HANDLER above.

;;; DEF_C_LAB (_call_sys)
do_call_sys:

    ;;; Save pointer to caller's stack frame in case of interrupts

;;; leaq    8(%rsp), %rax
;;; movq    %rax, SAVED_SP
    pushq   %r8
    pushq   %r9
    pushq   %r10
    pushq   %r11
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15

    ;;; Get the system call address in R10 and the argument count in RAX

    movq    (%USP), %r10
    movq    8(%USP), %rax
    addq    $16, %USP

    ;;; Copy any arguments to the system stack

    cmpq    $0, %rax
    je      L2.2
    movq    %rax, %rcx
    subq    $6, %rcx
    jbe     L1.4
    movq    $6, %rax
    ;;; If number of stack arguments is odd we need to
    ;;; push dummy to align the control stack
    testq   $1, %rcx
    jz      L1.3
    pushq   $0

L1.3:   pushq   (%USP)
    addq    $8, %USP
    decq    %rcx
    jnz     L1.3

L1.4:
    leaq    (%USP, %rax, 8), %USP
    cmpq    $0, %rax
    je  L2.2
    movq    -8(%USP), %rdi
    decq    %rax

    cmpq    $0, %rax
    je      L2.2
    movq    -16(%USP), %rsi
    decq    %rax

    cmpq    $0, %rax
    je      L2.2
    movq    -24(%USP), %rdx
    decq    %rax

    cmpq    $0, %rax
    je      L2.2
    movq    -32(%USP), %rcx
    decq    %rax

    cmpq    $0, %rax
    je      L2.2
    movq    -40(%USP), %r8
    decq    %rax

    cmpq    $0, %rax
    je      L2.2
    movq    -48(%USP), %r9
    decq    %rax

L2.2:   ;;; Make the call:
    ;;; clear the direction flag first, as Sequent's library routines
    ;;; seem to expect it like that!

    cld
    call    *%r10

    ;;; Stack the result (from EAX)

    subq    $8, %USP
    movq    %rax, (%USP)

    ;;; Restore the stack pointer and return

    movq    SAVED_SP, %rax
    movq    -24(%rax), %r8
    movq    -32(%rax), %r9
    movq    -40(%rax), %r10
    movq    -48(%rax), %r11
    movq    -56(%rax), %r12
    movq    -64(%rax), %r13
    movq    -72(%rax), %r14
    movq    -80(%rax), %r15
    leaq    -16(%rax), %rsp
;;; movq    $0, SAVED_SP        ;;; indicates external call over
    ret

    .align  16

DEF_C_LAB (_call_sys)

    ;;; Save pointer to caller's stack frame in case of interrupts

    leaq    8(%rsp), %rax
    movq    %rax, SAVED_SP
    call do_call_sys
    movq    $0, SAVED_SP
    ret

DEF_C_LAB (_call_sys_se)

    ;;; Save pointer to caller's stack frame in case of interrupts

    leaq    8(%rsp), %rax
    movq    %rax, SAVED_SP
    call do_call_sys
    movq    (%USP), %rax
    movslq  %eax, %rax
    movq    %rax, (%USP)
    movq    $0, SAVED_SP
    ret

;;; --- C-ACCESSIBLE VARIABLES, ETC -------------------------------------

    .data
    .globl  EXTERN_NAME(__pop_fpe_handler)
EXTERN_NAME(__pop_fpe_handler):
    .quad   0


/***************** end labels for wrapping structures *****************/

    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
    .data
Ldata_end:
    .set Ldata_size, Ldata_end-Ldata_start

/**********************************************************************/


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 25 1996
    Added __pop_error_return for setting the return address after an
    error signal on systems without a proper signal context
--- John Gibson, Mar 14 1995
    Removed _pop_m*alloc_exhausted (no longer needed)
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Minor mod for Linux (use "(" ")" for assembler expressions).
--- John Gibson, Oct 25 1994
    Removed the C pointers to pop vars (now set up in initial.p)
--- Robert John Duncan, May 24 1994
    No longer needs to set _sys*error
--- Robert John Duncan, Apr 14 1994
    Reorganised to be a bit less Unix-specific
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct); fixed __pop_fpe_handler to be
    .long and only defined _do_vfork if BSD_VFORK is set.
--- John Gibson, Mar 14 1991
    Added _pop_in_X_call
--- Robert John Duncan, Jan 29 1991
    Actual signal handlers rewritten in C and moved to c_core.c (this
    file now contains only routines gone to on return from a signal).
    Added clearing of _pop_in_user_extern in reset_pop_environ.
    __pop_fpe_handler now declared here
--- Robert John Duncan, Dec  4 1990
    Replaced _pop_a*dd_sig with _pop_add_ast taking ast type as 1st arg
--- Roger Evans, Nov 22 1990
    Added __pop_xt_dummy_fd
--- John Gibson, Nov 19 1990
    Grouped all C-accessible pointers and added
    __WEAK_pop_external_callback etc
--- Robert John Duncan, Nov 13 1990
    Replaced _m*alloc_use_external by _external_flags and
    _pop_malloc_use_external by pop_external_flags, etc.
--- Simon Nichols, Aug 29 1990
    Signal queue routines rewritten in C and put into malloc.c.
    These access the pop variable _trap via the constant pointer
    _pop_signals_pending.
--- Rob Duncan, Jun 14 1990
    Added missing "_" to __pop_m*alloc_exhausted
--- Rob Duncan, May 16 1990
    addsig now sets _trap to 1, and _remsig sets it to 0 when signal
    queue is empty. _remsig also now returns signal on stack.
--- John Gibson, May 13 1990
    Added __pop_malloc_use_external to enable malloc to get at
    Sys$-_malloc_use_external.
    Changed name of routine called by malloc when internal dynamic
    memory exhausted to __pop_m*alloc_exhausted, and error pop procedure
    called to Sys$-M*alloc_exhausted.
--- Rob Duncan, May  4 1990
    Replaced _extn_saved_sp with SAVED_SP and made it point to the
    caller's stack frame.
    Moved in -call_sys- from "aextern.s" and changed it to use SAVED_SP.
    Replaced explicit definitions of signal numbers with those from
    "signals.ph".
    Changed -reset_pop_environ- to clear the user stack.
--- John Gibson, Mar 23 1990
    Removed call of reset_pop_environ from do_vfork (not needed)
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- Rob Duncan, Jan 24 1989
    Changed -signal_handler- and -addsig- to communicate the signal
    number in a register rather than the -sigsavnum- variable, for
    extra security when handling multiple signals. Tidied up.
--- Rob Duncan, Oct 18 1988
    Made -reset_pop_environ- clear the FPE handler
 */
