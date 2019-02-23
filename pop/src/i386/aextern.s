/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/aextern.s
 * Purpose:     External function calls for Intel 80x86 (Unix assembler)
 * Author:      Generated automatically from S.symmetry/src/aextern.s (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'
#_INCLUDE 'numbers.ph'

lconstant macro (

    USP     = "ebx",    ;;; User stack pointer
    PB      = "ebp",    ;;; Procedure base register

    SAVED_SP    = [I_LAB(Sys$-Extern$- _saved_sp)],
    SAVED_USP   = [I_LAB(Sys$-Extern$- _saved_usp)],

    _BGI_LENGTH = @@BGI_LENGTH,
    _BGI_SLICES = @@BGI_SLICES,
    _DD_1       = @@DD_1,   ;;; MS half of ddecimal
    _DD_2       = @@DD_2,   ;;; LS half of ddecimal
    _EFC_FUNC   = @@EFC_FUNC,
    _EFC_ARG    = @@EFC_ARG,
    _EFC_ARG_DEST   = @@EFC_ARG_DEST,
    _KEY        = @@KEY,
    _K_EXTERN_TYPE  = @@K_EXTERN_TYPE,
    _XP_PTR     = @@XP_PTR,

    CHECK_BREAK = [DEF UNIX and not(DEF BSD_MPROTECT) and not(DEF SCO)],

);

>_#

    .file   "aextern.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text

;;; _call_external(nargs, routine, fltsingle)
;;; User interface to external routines: moves nargs from the user
;;; stack to the call stack, converting where necessary.
;;; Bit N set in the fltsingle arg means pass the (N+1)'th arg as a
;;; single float (if it's (d)decimal), otherwise decimals are passed
;;; as doubles. Bit 31 of fltsingle covers all args from 32 onwards.
;;; Results are returned in the three-word structure result_struct
;;; double float result first, followed by a single word result.
;;; Must be capable of dealing with callback.

;;; Register usage:
;;; EAX argument conversion; result from the external call
;;; ECX number of arguments (nargs);
;;;     address of result array
;;; EDX fltsingle bit mask
;;; ESI external type of compound arguments
;;; EDI address of the external routine (routine)

;;; Memory usage:
;;; SAVED_SP
;;;     pointer to the stack frame of the calling procedure;
;;;     used by the interrupt handler defined in "asignals.s"
;;; SAVED_USP
;;;     value of USP after clearing the external arguments;
;;;     used in callback
;;; result_struct
;;;     holds both single- and double-length results
;;; save_curbrk
;;;             (on systems without mprotect) remembers the current
;;;             break in case the external routine allocates extra store

DEF_C_LAB (_call_external)

#_IF CHECK_BREAK

    ;;; Save the current break in case the external routine allocates
    ;;; its own dynamic memory.

    movl    EXTERN_NAME(___brk_addr), %eax
    movl    %eax, save_curbrk

#_ENDIF

    ;;; Save caller's stack pointer for interrupt/callback

    leal    4(%esp), %eax
    movl    %eax, SAVED_SP

    ;;; Load fixed arguments

    movl    (%USP), %edx    ;;; fltsingle
    movl    4(%USP), %edi   ;;; routine
    movl    8(%USP), %ecx   ;;; nargs
    addl    $12, %USP

    testl   %ecx, %ecx
    jz  do_call

    ;;; Rotate fltsingle to get bit for last arg at the top

    cmpl    $32, %ecx
    jae argloop
    rorl    %cl, %edx

    ;;; Transfer (and convert) any arguments

argloop:

    movl    (%USP), %eax
    addl    $4, %USP

    testl   $1, %eax    ;;; issimple
    jz  L2.1
    testl   $2, %eax    ;;; isinteger
    jz  L1.1

    ;;; Pop integer: convert to m/c integer and push

    sarl    $2, %eax
    pushl   %eax
    jmp nextarg

L1.1:   ;;; Pop decimal: convert to m/c single float and push

    subl    $1, %eax
    pushl   %eax

    ;;; If the sign bit of fltsingle is not set, convert to double

    testl   %edx, %edx
    js  nextarg
    flds    (%esp)      ;;; Load from stack as single
    subl    $4, %esp
    fstpl   (%esp)      ;;; Store back as double
    jmp nextarg

L2.1:   ;;; Pop structure: get extern type from key into ESI

    movl    _KEY(%eax), %esi
    movzbl  _K_EXTERN_TYPE(%esi), %esi

    testl   %esi, %esi
    jnz L3.1

    ;;; EXTERN_TYPE_NORMAL (0): push unchanged

    pushl   %eax
    jmp nextarg

L3.1:   cmpl    $_:EXTERN_TYPE_DEREF, %esi
    jne L4.1

    ;;; External pointer: push dereferenced

    pushl   _XP_PTR(%eax)
    jmp nextarg

L4.1:   cmpl    $_:EXTERN_TYPE_DDEC, %esi
    jne L5.1

    ;;; Ddecimal: push the two halves

    pushl   _DD_1(%eax)
    pushl   _DD_2(%eax)

    ;;; If the sign bit of fltsingle is set, convert to single float

    testl   %edx, %edx
    jns nextarg
    fldl    (%esp)      ;;; Load from stack as double
    addl    $4, %esp
    fstps   (%esp)      ;;; Store back as single
    jmp nextarg

L5.1:   ;;; Must be biginteger: load first slice to ESI

    movl    _BGI_SLICES(%eax), %esi

    ;;; If there's more than one slice, pull in the bottom bit of the
    ;;; second

    cmpl    $1, _BGI_LENGTH(%eax)
    je  L6.1
    movl    _BGI_SLICES+4(%eax), %eax
    shll    $1, %esi
    shrdl   $1, %eax, %esi

L6.1:   ;;; Push the result

    pushl   %esi

nextarg:

    subl    $1, %ecx
    jz  do_call

    ;;; Shift up fltsingle

    cmpl    $32, %ecx
    jae argloop
    shll    $1, %edx
    jmp argloop

do_call:

    cld             ;;; clear direction flag
    movl    %USP, SAVED_USP ;;; save USP for callback
    movl    %esp, EXTERN_NAME(__pop_in_user_extern)
                    ;;; enable async callback
    call    *%edi
    movl    $0, EXTERN_NAME(__pop_in_user_extern)
                    ;;; disable async callback
    movl    SAVED_USP, %USP ;;; restore USP

    ;;; Copy possible results into result_struct:
    ;;; double result from ST(0) first, then word result from EAX

    leal    C_LAB(Sys$-Extern$-result_struct), %ecx

    movl    %eax, 8(%ecx)       ;;; word result

    ;;; See if there's a double result (i.e something in ST(0)):
    ;;; use FXAM, and copy the resulting status word to EAX.
    ;;; Mask out everything except bits C0 and C3; if these are both
    ;;; set, then the register was empty and there's no result

    fxam
    fstsw   %ax
    andw    $0x4100, %ax
    cmpw    $0x4100, %ax
    je  L7.1

    fstpl   (%ecx)          ;;; double result

L7.1:   ;;; Reset stack pointer

    movl    SAVED_SP, %eax
    leal    -4(%eax), %esp
    movl    $0, SAVED_SP        ;;; Indicates external call over

#_IF CHECK_BREAK

    ;;; Check for change in the break

    movl    save_curbrk, %eax
    cmpl    %eax, EXTERN_NAME(___brk_addr)
    je  L0.1
    jmp XC_LAB(Sys$-Mem_break_changed)

#_ENDIF

L0.1:   ret


    .align  4


;;; _EXFUNC_CLOS_ACTION:
;;; called from the code in an exfunc_closure (see asmout.p), with
;;; the top of stack pointing to (exfunc_clos address)+8

DEF_C_LAB(Sys$- _exfunc_clos_action)
    ;;; Load address of external closure record to eax

    popl    %eax
    leal    -8(%eax), %eax

    ;;; Store frozen argument to destination

    movl    _EFC_ARG_DEST(%eax), %edx
    movl    _EFC_ARG(%eax), %ecx
    movl    %ecx, (%edx)

    ;;; Chain function via external ptr

    movl    _EFC_FUNC(%eax), %eax
    jmp *(%eax)

    .align  4


;;; _POP_EXTERNAL_CALLBACK:
;;; interface routine for external callback

;;; C Synopsis:
;;; int _pop_external_callback(unsigned argp[])

;;; Arguments:
;;; argp[0] is the function code for -Callback-

.globl  EXTERN_NAME(_pop_external_callback)
EXTERN_NAME(_pop_external_callback):
    ;;; for indirect weak reference
DEF_C_LAB(Sys$- _external_callback_func)

    ;;; Save user registers

    pushl   %ebx
    pushl   %ebp
    pushl   %esi
    pushl   %edi

    ;;; Save __pop_in_user_extern

    pushl   EXTERN_NAME(__pop_in_user_extern)

    ;;; Create 3 word dummy stack frame for SF_NEXT_SEG_SP
    ;;; and SF_NEXT_SEG_HI

    leal    -12(%esp), %esp

    ;;; Disable async callback

    movl    $0, EXTERN_NAME(__pop_in_user_extern)

    ;;; Restore procedure base register and saved USP

    movl    SAVED_SP, %eax
    movl    (%eax), %PB
    movl    SAVED_USP, %USP

    ;;; Push argp

    subl    $8, %USP
    movl    36(%esp), %eax
    movl    %eax, 4(%USP)

#_IF CHECK_BREAK

    ;;; Push any change in the break

    movl    save_curbrk, %eax
    subl    EXTERN_NAME(___brk_addr), %eax
    movl    %eax, (%USP)

#_ELSE

    ;;; No need to worry about the break: push a dummy argument

    movl    $0, (%USP)

#_ENDIF

    ;;; Do the callback

    call    XC_LAB(Sys$-Extern$-Callback)

#_IF CHECK_BREAK

    ;;; Resave current break

    movl    EXTERN_NAME(___brk_addr), %eax
    movl    %eax, save_curbrk

#_ENDIF

    ;;; Return status code

    movl    (%USP), %eax
    addl    $4, %USP

    ;;; Resave USP

    movl    %USP, SAVED_USP

    ;;; Remove stack frame

    leal    12(%esp), %esp

    ;;; Re-enable async callback (restoring saved __pop_in_user_extern)

    popl    EXTERN_NAME(__pop_in_user_extern)

    ;;; Restore registers and return

    cld
    popl    %edi
    popl    %esi
    popl    %ebp
    popl    %ebx

    ret

    .align  4


    .data

#_IF CHECK_BREAK

;;; SAVE_CURBRK:
;;; Saves the break value across an external call

save_curbrk:
    .long   0

#_ENDIF

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
--- Robert Duncan, Apr 25 1996
    Added missing EXTERN_NAME wrappers round references to ___brk_addr
--- Robert Duncan, Mar  1 1996
    Added missing CLD to return from callback: DF must be clear when
    executing external code
--- John Gibson, Feb 17 1995
    Moved definition of Extern$- _r*esult_struct to extern_ptr.p as
    Extern$-result_struct
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Changes for Linux and SCO (note that Linux uses ___brk_addr
    instead of __currbrk for current break).
--- Robert John Duncan, Dec  7 1994
    Fix to argument offset in _pop_external_callback
--- Robert John Duncan, Jul  7 1994
    Fix to register usage in _exfunc_clos_action
--- Robert John Duncan, Apr 14 1994
    Changed tests of the memory break to be Unix-specific, to improve
    portability to non-Unix systems.
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct). Removed code for Symmetry bug
    and replaced * with \* (may not be necessary).
--- Simon Nichols, Nov 10 1993
    Made _pop_external_callback set __pop_in_user_extern back to the same
    value it had on entry.
--- Robert John Duncan, May 20 1993
    Template part of exfunc closures now generated by POPC, which
    jumps to the _exfunc_clos_action code in this file.
--- John Gibson, Dec 18 1992
    Moved pop_ex*func_arg to c_core.c (otherwise it's undefined if this
    file is not extracted)
--- Robert John Duncan, Aug 26 1992
    Changed _call_external to take fltsingle arg whose Nth bit
    specifies the treatment of (d)decimals for the (N+1)'th arg.
    Also made more compatible with the Sun386/System V version by using
    EXTERN_NAME, changing some instructions etc.
--- Robert John Duncan, Jan 29 1991
    Added setting and clearing of ___pop_in_user_extern (tested nonzero
    by signal handler in c_core.c to determine whether asynchronous
    callback is allowed)
--- John Gibson, Nov 19 1990
    Added pop label for _pop_external_callback.
    Removed functions for getting interrupt/disable flag info
    (replaced by pointer constants in asignals.s)
--- Robert John Duncan, Nov 13 1990
    _call_external now doesn't return the result structure on the stack;
    the structure is exported instead as Sys$-Extern$- _r*esult_struct.
    _pop_external_callback now receives and passes on a status return
    from Callback.
--- Simon Nichols, Sep  6 1990
    Added exfunc closure stuff
--- Roger Evans, Jul  3 1990
    added pop_interrupt_disabled
--- Rob Duncan, May 16 1990
    _call_external now returns pointer to 3-word array, first
    2 words of which are possible single/double float result, 3rd word
    is possible result of any other kind.
--- Rob Duncan, May  4 1990
    Moved out -call_sys- to "asignals.s".
    Added callback interface and rewrote -call_external- appropriately.
    Added -pop_interrupt_pending-.
--- Rob Duncan, Mar 15 1990
    Changed -call_external- to use new K_EXTERNAL_TYPE field in the key
    and to cope with bigintegers
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- John Gibson, Jul  5 1989
    Replaced _M_K_EXTERNAL with _:M_K_EXTERNAL
--- Rob Duncan, Oct 11 1988
    Renumbered labels in -call_external-
--- Roger Evans, Oct 10 1988
    External data classes now dereference the E_PTR field, other
    structures pass E_DATA field (third longword).
    Single decimals: bottom bit now cleared before passing out
 */
