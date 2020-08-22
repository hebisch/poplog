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

    USP     = "rbx",    ;;; User stack pointer
    PB      = "rbp",    ;;; Procedure base register

    SAVED_SP    = [I_LAB(Sys$-Extern$- _saved_sp)],
    SAVED_USP   = [I_LAB(Sys$-Extern$- _saved_usp)],

    _BGI_LENGTH = @@BGI_LENGTH,
    _BGI_SLICES = @@BGI_SLICES,
    _DD_1       = @@DD_1,   ;;; MS half of ddecimal
;;; _DD_2       = @@DD_2,   ;;; LS half of ddecimal
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
;;;
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

;;;    Registers: 
;;;       %rdi, %rsi, %rdx, %rcx, %r8, %r9   -- integer arguments
;;;       %xmm0 to %xmm5                     -- floating point arguments
;;;
;;;       %rax   --  work register, external type of compound arguments
;;;                  number of floating point arguments in registers
;;;                  at call
;;;       %rbp   --  points to work variables on the machine stack
;;;       %r10   --  work register2, external type of compound arguments
;;;                  during conversion
;;;                  routne at call
;;;       %r11   --  floating point argument store routine
;;;       %r12   --  fltsingle
;;;       %r13   --  integer argument store routine
;;;       %r14   --  displacement on machine (control) stack
;;;       %r15   --  displacement on Poplog (user) stack
;;;
;;;    Work variables on the machine stack:
;;;       (%rbp)     -- saved rountine
;;;       -8(%rbp)   -- work variable for memory conversion
;;;       -16(%rbp)  -- number of floating point arguments in registers
;;;                     during conversion
;;;       -24(%rbp)  -- stored size of arguments on user stack

  .text

;;; Functions to store arguments, use macros to save on repetition
#_<
lvars ci = 0;
define lconstant macro gen_storei;
    lvars x y;
    itemread() -> x;
    itemread() -> y;
'int_arg' >< ci >< ':\n';
    ci + 1 -> ci;
    '\tmovq %rax', ",", '%' >< y, '\n';
    '\tmovq $int_arg' >< ci, ', %r13\n';
    '\tjmp next_arg\n\n';
enddefine;
define lconstant macro gen_storesf;
    lvars cf = 0, cfn;
    while cf < 6 do
        cf + 1 -> cfn;
        'float_arg', >< cf, >< ':\n';
        '\incl   -16(%rbp)\n';
        '\tmovq  $float_arg', >< cfn, ',%r11\n';
        '\ttestl   $1, %r12d\n';
        '\tjnz single_', >< cf >< '\n';

;;; just load the value

        '\tmovlpd  -8(%rbp), %xmm' >< cf >< '\n';
        '\tjmp next_arg\n\n';

;;; convert to single

    'single_', >< cf >< ':\n';
        '\tcvtsd2ss   -8(%rbp), %xmm' >< cf >< '\n';
        '\tjmp next_arg\n\n';
        cfn -> cf;
    endwhile
enddefine;
>_#

gen_storesf

gen_storei rdi
gen_storei rsi
gen_storei rdx
gen_storei rcx
gen_storei r8
gen_storei r9

int_arg6:
    movq %rax, (%rsp, %r14)
    addq    $8, %r14
    jmp next_arg

float_arg6:
    testl   $1, %r12d
    jnz single_6
    movq -8(%rbp), %rax
    movq %rax, (%rsp, %r14)
    addq    $8, %r14
    jmp next_arg

single_6:
    cvtsd2ss  -8(%rbp), %xmm7
    movss   %xmm7, (%rsp, %r14)
    addq    $8, %r14
    jmp next_arg


DEF_C_LAB (_call_external)
#_IF CHECK_BREAK

    ;;; Save the current break in case the external routine allocates
    ;;; its own dynamic memory.

    movq    EXTERN_NAME(___brk_addr), %rax
    movq    %rax, save_curbrk

#_ENDIF

    ;;; Save caller's stack pointer for interrupt/callback

    leaq    8(%rsp), %rbp
    movq    %rbp, SAVED_SP

    ;;; save registers.  Caller should save Pop registers
    ;;; to avoid problems with garbage collection, so
    ;;; we do not store them
    push    %r8
    push    %r9
    push    %r10
    push    %r11
    push    %r12

    ;;; Load fixed arguments

    movq    (%USP), %r12    ;;; fltsingle
    movq    8(%USP), %r10   ;;; routine
    movq    16(%USP), %r15  ;;; nargs
    shlq    $3, %r15        ;;; convert to byte offset;
    leaq    24(%USP), %USP  ;;; adjust user stack
    xorl    %eax,%eax
    testq   %r15, %r15
    jz      do_call1

;;;     We need 4 words on stack as a workplace     
;;;     In the worst case, when we have one argument (which always
;;;     fits into a register this gives 3+nargs, push allocate 1
;;;     so we put another two into alignment code

    push    %r10
    movq    %rsp, %rbp
    xorq    %r14, %r14
    subq    %r15, %rsp       ;;; allocate space for arguments
    subq    $0x16, %rsp        ;;; 
    andq    $-0x10, %rsp       ;;; align the stack
    movl    $0, -16(%rbp)
    movq   %r15, -24(%rbp)

    movq   $int_arg0,   %r13
    movq   $float_arg0, %r11  

    ;;; Transfer (and convert) any arguments


argloop:

    movq    -8(%USP, %r15), %rax

    testq   $1, %rax    ;;; issimple
    jz  L2.1
    testq   $2, %rax    ;;; isinteger
    jz      L1.1

    ;;; Pop integer: convert to m/c integer and call store
    ;;; routine

    sarq    $2, %rax
    jmp     *%r13

L1.1:
    ;;; Pop decimal: store in memory for conversion to m/c single
    ;;; float and push

    subq    $1, %rax
    movq    %rax, -8(%rbp)
    ;;;  call conversion & store
    jmp     *%r11        

L2.1:   ;;; Pop structure: get extern type from key into ESI

    movq    _KEY(%rax), %r10
    movzbl  _K_EXTERN_TYPE(%r10), %r10d

    testl   %r10d, %r10d
    jnz L3.1

    ;;; EXTERN_TYPE_NORMAL (0): push unchanged

    jmp     *%r13

L3.1:
    cmpl    $_:EXTERN_TYPE_DEREF, %r10d
    jne L4.1

    ;;; External pointer: push dereferenced

    movq    _XP_PTR(%rax), %rax
    jmp     *%r13

L4.1:   cmpl    $_:EXTERN_TYPE_DDEC, %r10d
    jne L5.1

    ;;; Ddecimal: call double store routine

    movq    _DD_1(%rax), %r10
    movq    %r10, -8(%rbp)
    jmp     *%r11

L5.1:
    ;;; Must be biginteger

    ;;; If there's more than two slices, pull in the high bits first

    cmpl    $3, _BGI_LENGTH(%rax)
    jb      L5.2
    movl    _BGI_SLICES+8(%rax), %r10d
    shl     $62, %r10
    ;;; spill to the stack
    movq    %r10, -8(%rbp)
    movl    _BGI_SLICES+4(%rax),%r10d
    shlq    $31, %r10
    movl    _BGI_SLICES(%rax), %eax
    orq     %r10, %rax
    orq     -8(%rbp), %rax
    jmp     *%r13

    ;;; At most two slices, pull in the lowest part first
L5.2:
    movl    _BGI_SLICES(%rax), %r10d
    cmpl    $1, _BGI_LENGTH(%rax)
    je  L6.1

    movslq  _BGI_SLICES+4(%rax), %rax
    shrq    $1, %rax
    orq     %r10, %rax
    jmp     *%r13

L6.1:   ;;; Push the result
    movslq  %r10d, %rax
    jmp     *%r13

next_arg:
    sarl    $1, %r12d
    subq    $8, %r15
    jnz     argloop

    addq    -24(%rbp), %USP
    movq    (%rbp), %r10
    movb    -16(%rbp), %al

do_call1:
    cld             ;;; clear direction flag
    movq    %USP, SAVED_USP ;;; save USP for callback
    ;;; enable async callback
    movq    %rsp, EXTERN_NAME(__pop_in_user_extern)
    call    *%r10
    ;;; disable async callback
    movq    $0, EXTERN_NAME(__pop_in_user_extern)
    movq    SAVED_USP, %USP ;;; restore USP

    ;;; Copy possible results into result_struct:
    ;;; double result first, then word result from RAX

    leaq    C_LAB(Sys$-Extern$-result_struct), %rcx

    movq    %rax, 8(%rcx)       ;;; word result
    movq    %xmm0, (%rcx)       ;;; double result

L7.1:   ;;; Reset stack pointer

    movq    SAVED_SP, %rax
    movq    (%rax), %rbp
    movq    -16(%rax), %r8
    movq    -24(%rax), %r9
    movq    -32(%rax), %r10
    movq    -40(%rax), %r11
    movq    -48(%rax), %r12
    ;;; zero Pop registers
    movq    $3, %r13
    movq    $3, %r14
    movq    $3, %r15
    leaq    -8(%rax), %rsp
    movq    $0, SAVED_SP        ;;; Indicates external call over

#_IF CHECK_BREAK

    ;;; Check for change in the break

    movq    save_curbrk, %rax
    cmpq    %rax, EXTERN_NAME(___brk_addr)
    je  L0.1
    jmp XC_LAB(Sys$-Mem_break_changed)

#_ENDIF

L0.1:   ret


    .align  16


;;; _EXFUNC_CLOS_ACTION:
;;; called from the code in an exfunc_closure (see asmout.p), with
;;; the top of stack pointing to (exfunc_clos address)+10

DEF_C_LAB(Sys$- _exfunc_clos_action)
    movq    (%rsp), %r11
    leaq    -11(%r11), %r11

    ;;; Store frozen argument to destination

    movq    _EFC_ARG_DEST(%r11), %r10
    movq    _EFC_ARG(%r11), %r11
    movq    %r11, (%r10)

    ;;; Chain function via external ptr

    popq    %r11
    movq    _EFC_FUNC-11(%r11), %r11
    jmp *(%r11)

    .align  16


;;; _POP_EXTERNAL_CALLBACK:
;;; interface routine for external callback

;;; C Synopsis:
;;; int _pop_external_callback(unsigned argp[])

;;; Arguments:
;;; argp[0] is the function code for -Callback-

;;; .globl  EXTERN_NAME(debug_pop)


.globl  EXTERN_NAME(_pop_external_callback)
EXTERN_NAME(_pop_external_callback):
    ;;; for indirect weak reference
DEF_C_LAB(Sys$- _external_callback_func)
    ;;; Save call-preserved C registers      
    pushq   %rbx
    pushq   %rbp
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15

    ;;; Zero Pop registers
    movq $3, %r13
    movq $3, %r14
    movq $3, %r15

    ;;; Save __pop_in_user_extern

    pushq   EXTERN_NAME(__pop_in_user_extern)

    ;;; Create 3 word dummy stack frame for SF_NEXT_SEG_SP
    ;;; and SF_NEXT_SEG_HI
    ;;; We allocate 4 words to preserve alignment

    leaq    -32(%rsp), %rsp

    ;;; Disable async callback

    movq    $0, EXTERN_NAME(__pop_in_user_extern)

    ;;; Restore procedure base register and saved USP

    movq    SAVED_SP, %rax
    movq    (%rax), %PB
    movq    SAVED_USP, %USP

    ;;; Push argp

    subq    $16, %USP
    movq    %rdi, 8(%USP)

#_IF CHECK_BREAK

    ;;; Push any change in the break

    movq    save_curbrk, %rax
    subq    EXTERN_NAME(___brk_addr), %rax
    movq    %rax, (%USP)

#_ELSE

    ;;; No need to worry about the break: push a dummy argument

    movq    $0, (%USP)

#_ENDIF

    ;;; Do the callback

    call    XC_LAB(Sys$-Extern$-Callback)

#_IF CHECK_BREAK

    ;;; Resave current break

    movq    EXTERN_NAME(___brk_addr), %rax
    movq    %rax, save_curbrk

#_ENDIF

    ;;; Return status code

    movq    (%USP), %rax
    addq    $8, %USP

    ;;; Resave USP

    movq    %USP, SAVED_USP

    ;;; Remove stack frame

    leaq    32(%rsp), %rsp

    ;;; Re-enable async callback (restoring saved __pop_in_user_extern)

    popq    EXTERN_NAME(__pop_in_user_extern)

    ;;; Restore C registers and return

    cld
    popq    %r15
    popq    %r14
    popq    %r13
    popq    %r12
    popq    %rbp
    popq    %rbx
    ret

    .align  16


EXTERN_NAME(pop_print):
#_IF CHECK_BREAK

    ;;; Save the current break so that callback code has correct
    ;;; info 

    movq    EXTERN_NAME(___brk_addr), %rax
    movq    %rax, save_curbrk
    movl    $0, %eax
#_ENDIF
    movq %rdi, SAVED_SP
    movq %rsi, SAVED_USP
    pushq %rdx
    movq $C_LAB(sys_syspr), %rdi
    movq %rsp, %rsi
    call pop_call
    addq $8, %rsp
    ret
    .align  16

    .data

#_IF CHECK_BREAK

;;; SAVE_CURBRK:
;;; Saves the break value across an external call
    .align 8
save_curbrk:
    .quad   0

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
