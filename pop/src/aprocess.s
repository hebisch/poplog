/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/aprocess.s
 * Purpose:     Assembler support for processes on Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'
#_INCLUDE 'process.ph'

lconstant macro (

    USP                     = "rbx",
    PB                      = "rbp",
    CHAIN_REG               = "rdx",

    _ID_VALOF               = @@ID_VALOF,
    _PD_EXECUTE             = @@PD_EXECUTE,
    _PD_EXIT                = @@PD_EXIT,
    _PD_FLAGS               = @@PD_FLAGS,
    _PD_REGMASK             = @@PD_REGMASK,
    _PD_FRAME_LEN           = @@PD_FRAME_LEN,
    _PD_NLOCALS             = @@PD_NLOCALS,
    _PD_NUM_STK_VARS        = @@PD_NUM_STK_VARS,
    _PD_TABLE               = @@PD_TABLE,
    _PS_CALLSTACK_LIM       = @@PS_CALLSTACK_LIM,
    _PS_CALLSTACK_PARTIAL   = @@PS_CALLSTACK_PARTIAL,
    _PS_FLAGS               = @@PS_FLAGS,
    _PS_PARTIAL_RETURN      = @@PS_PARTIAL_RETURN,
    _PS_STATE               = @@PS_STATE,
);

lconstant macro MOVFL   = if DEF FRAME_LEN_16BIT then "movzwl"
                          elseif DEF FRAME_LEN_32BIT then "movl"
                          else "movzbl"
                          endif;

>_#

    .file   "aprocess.s"

/************************* wrapping structures ************************/

    .text
    .quad   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .quad   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


;;; === SWAPPING THE CALLSTACK ========================================

    .data

PROCESS:                ;;; Process record
    .quad   0
LIMIT:                  ;;; Limit of the callstack save area
    .quad   0
NEXT_FRAME:             ;;; Pointer to the next frame in the saved callstack
    .quad   0

    .text

    ;;; Size of a standard branch instruction (as placed by I_BRANCH_std)
.set    BRANCH_std, 9

;;; _SWAP_OUT_CALLSTACK:
;;;     Swap out the callstack for a process
;;;     (i.e. copy the callstack into the process record)

;;; Call:
;;;     _swap_out_callstack(PROCESS)

DEF_C_LAB (_swap_out_callstack)

    movq    (%USP), %rax
    addq    $8, %USP

    movq    %rax, PROCESS                   ;;; Save the process record
    movq    _PS_CALLSTACK_LIM(%rax), %rdi   ;;; Start of saved callstack
    movq    _PS_STATE(%rax), %rax
    movq    %rax, LIMIT                     ;;; End of saved callstack
    jmp     sotest

soloop:

    ;;; Copy out the next stack frame:
    ;;; save the return address in ECX, then set the procedure base
    ;;; register from the owner address on the stack top

    popq    %rcx
    movq    (%rsp), %PB

    ;;; Make the return address relative to the procedure base

    subq    %PB, %rcx

    ;;; Test if the procedure has dlocal expression code to run

    testb   $_:M_PD_PROC_DLEXPR_CODE, _PD_FLAGS(%PB)
    jnz     sobrk

socont:

    ;;; Continue here after running the dlocal expression code:

    ;;; EDI points to where the stack frame is to be saved in the process
    ;;; record; we compute the position of the next frame by subtracting
    ;;; the length of the current stack frame

    MOVFL   _PD_FRAME_LEN(%PB), %eax
    salq    $3, %rax
    subq    %rax, %rdi
    movq    %rdi, NEXT_FRAME

    ;;; Save relative return address and owner address in the process
    ;;; record

    movq    %rcx, (%rdi)
    movq    %PB, 8(%rdi)
    addq    $16, %rdi

    ;;; Copy any on-stack lvars into the process record

    cld
    MOVFL   _PD_NUM_STK_VARS(%PB), %ecx
    leaq    8(%rsp), %rsi
    rep
    smovq
    movq    %rsi, %rsp

    ;;; Test if there are any dynamic locals

    MOVFL   _PD_NLOCALS(%PB), %ecx
    cmpq    $0, %rcx
    je      L2.1

    ;;; If so, copy the current values of the locals into the
    ;;; process record, and restore their previous values from the
    ;;; stack.
    ;;; ESI is initialised to point to the last entry in the procedure's
    ;;; identifier table

    leaq    _PD_TABLE-8(%PB, %rcx, 8), %rsi

L1.1:                           ;;; Loop:
    movq    (%rsi), %rdx    ;;; ident to EDX
    movq    _ID_VALOF(%rdx), %rax   ;;; idval to EAX
    sstoq                   ;;; save idval in process record
    popq    _ID_VALOF(%rdx) ;;; restore previous idval from stack
    subq    $8, %rsi        ;;; decrement ESI
    loop    L1.1

L2.1:
    ;;; FIXME: save registers back to process struct
    ;;; restore values from stack
    ;;; rax, rcx, rdx and rsi is free here

    movzwl _PD_REGMASK(%PB), %ecx

    bt  $15, %rcx
    jnc L2.14
    movq %r15, %rax
    sstoq
    popq %r15
L2.14:
    bt  $14, %rcx
    jnc L2.13
    movq %r14, %rax
    sstoq
    popq %r14
L2.13:
    bt  $13, %rcx
    jnc L2.12
    movq %r13, %rax
    sstoq
    popq %r13
L2.12:
    bt  $12, %rcx
    jnc L2.11
    movq %r12, %rax
    sstoq
    popq %r12
L2.11:
    bt  $11, %rcx
    jnc L2.10
    movq %r11, %rax
    sstoq
    popq %r11
L2.10:
    bt  $10, %rcx
    jnc L2.9
    movq %r10, %rax
    sstoq
    popq %r10
L2.9:
    bt  $9, %rcx
    jnc L2.8
    movq %r9, %rax
    sstoq
    popq %r9
L2.8:
    bt  $8, %rcx
    jnc L2.7
    movq %r8, %rax
    sstoq
    popq %r8
L2.7:

    ;;; Stack frame saved -- restore EDI to point to where the next
    ;;; should go

    movq    NEXT_FRAME, %rdi

sotest:

    ;;; Test if there are any more stack frames to swap out

    cmpq    %rdi, LIMIT
    jb      soloop

sodone:

    ;;; Finished swapping out -- chain procedure on stack

    movq    PROCESS, %rax
    movq    $0, _PS_CALLSTACK_PARTIAL(%rax)
    movw    $0, _PS_FLAGS(%rax)             ;;; 0 flags = suspended
    movq    (%USP), %rax
    addq    $8, %USP
    jmp     *_PD_EXECUTE(%rax)              ;;; chain procedure

sobrk:

    ;;; Come here to run dlocal expression code, by jumping into the
    ;;; appropriate part of the exit code of the procedure.
    ;;; The process is passed in CHAIN_REG

    movq    PROCESS, %CHAIN_REG

    ;;; Save relative return address and callstack save pointer in the
    ;;; process record

    movq    %rcx, _PS_PARTIAL_RETURN(%CHAIN_REG)
    movq    %rdi, _PS_CALLSTACK_PARTIAL(%CHAIN_REG)

    ;;; Go to the procedure's suspend code

    movq    _PD_EXIT(%PB), %rax
    subq    $BRANCH_std << 1, %rax
    jmp     *%rax

    .align  16

DEF_C_LAB (_swap_out_continue)

    ;;; The suspend code returns here to resume the swapout loop
    ;;; The process will still be in CHAIN_REG

    movq    %CHAIN_REG, PROCESS

    ;;; Restore callstack save pointer, relative return address and
    ;;; save limit from the process record

    movq    _PS_CALLSTACK_PARTIAL(%CHAIN_REG), %rdi
    movq    _PS_PARTIAL_RETURN(%CHAIN_REG), %rcx
    movq    _PS_STATE(%CHAIN_REG), %rax
    movq    %rax, LIMIT

    ;;; Restore procedure base from the stack top

    movq    (%rsp), %PB

    ;;; Continue the swapout loop

    jmp     socont

    .align  16


;;; _SWAP_IN_CALLSTACK:
;;;     Swap in the callstack for a process
;;;     (i.e. copy stack frames from the process record onto the stack)

;;; Call:
;;;     _swap_in_callstack(PROCESS)

DEF_C_LAB (_swap_in_callstack)

    movq    (%USP), %rax
    addq    $8, %USP

    movq    %rax, PROCESS           ;;; Save the process record

    ;;; Get a pointer to the start of the saved callstack in ESI
    ;;; and the limit in LIMIT

    movq    _PS_STATE(%rax), %rsi
    movq    _PS_CALLSTACK_LIM(%rax), %rax
    movq    %rax, LIMIT
    jmp     sitest

siloop:

    ;;; ESI points to the saved stack frame in the process record.
    ;;; Restore the procedure base register from the saved owner address.

    movq    8(%rsi), %PB

    ;;; Compute the position of the next frame in the record by adding
    ;;; the length of the current frame

    MOVFL   _PD_FRAME_LEN(%PB), %eax
    leaq    (%rsi, %rax, 8), %rsi
    movq    %rsi, NEXT_FRAME
    subq    $8, %rsi

    std

    ;;; Store registers to stack and restore values from
    ;;; process record

    movzwl _PD_REGMASK(%PB), %ecx

    bt $8, %rcx
    jnc L1.9
    push %r8
    slodq
    movq %rax, %r8
L1.9:
    bt $9, %rcx
    jnc L1.10
    push %r9
    slodq
    movq %rax, %r9
L1.10:
    bt $10, %rcx
    jnc L1.11
    push %r10
    slodq
    movq %rax, %r10
L1.11:
    bt $11, %rcx
    jnc L1.12
    push %r11
    slodq
    movq %rax, %r11
L1.12:
    bt $12, %rcx
    jnc L1.13
    push %r12
    slodq
    movq %rax, %r12
L1.13:
    bt $13, %rcx
    jnc L1.14
    push %r13
    slodq
    movq %rax, %r13
L1.14:
    bt $14, %rcx
    jnc L1.15
    push %r14
    slodq
    movq %rax, %r14
L1.15:
    bt $15, %rcx
    jnc L1.16
    push %r15
    slodq
    movq %rax, %r15

L1.16:

    ;;; Check for any dynamic locals

    MOVFL   _PD_NLOCALS(%PB), %ecx
    cmpq    $0, %rcx
    je      L2.2

    ;;; If so, push the current values of the locals on the stack
    ;;; and get their new values from the process record.
    ;;; EDI is initialised to point to the first entry in the procedure's
    ;;; identifier table

    leaq    _PD_TABLE(%PB), %rdi

L1.2:                           ;;; Loop:
    movq    (%rdi), %rdx    ;;; ident to EDX
    pushq   _ID_VALOF(%rdx) ;;; push the idval
    slodq                   ;;; new idval from process record to EAX
    movq    %rax, _ID_VALOF(%rdx)   ;;; set new idval
    addq    $8, %rdi        ;;; increment EDI
    loop    L1.2

L2.2:   ;;; Copy any on-stack lvars from the saved frame,
    ;;; adjusting ESP first in case of interrupts

    MOVFL   _PD_NUM_STK_VARS(%PB), %ecx
    leaq    -8(%rsp), %rdi

    leaq    (, %rcx, 8), %rax
    subq    %rax, %rsp

    rep
    smovq

    ;;; Push the owner address from the procedure base register
    ;;; and set ECX to be the saved relative return address

    pushq   %PB
    movq    -8(%rsi), %rcx

    ;;; Set ESI to point to the next frame

    movq    NEXT_FRAME, %rsi

    ;;; Test for dlocal expression code to run

    testb   $_:M_PD_PROC_DLEXPR_CODE, _PD_FLAGS(%PB)
    jnz     sibrk

sicont:

    ;;; Continue here after running dlocal expression code:

    ;;; Make the relative return address in ECX absolute and push it

    addq    %PB, %rcx
    pushq   %rcx

sitest:

    ;;; Test if there are more frames to swap in

    cmpq    %rsi, LIMIT
    ja      siloop

sidone:

    ;;; Finished swapping in - chain procedure from stack

    movq    PROCESS, %rax
    movq    $0, _PS_CALLSTACK_PARTIAL(%rax)
    movq    (%USP), %rax
    addq    $8, %USP
    jmp     *_PD_EXECUTE(%rax)              ;;; chain procedure

sibrk:

    ;;; Come here to run dlocal expression code, by jumping into the
    ;;; appropriate part of the exit code of the procedure.
    ;;; The process is passed in CHAIN_REG

    movq    PROCESS, %CHAIN_REG

    ;;; Save relative return address and callstack save pointer in the
    ;;; process record

    movq    %rcx, _PS_PARTIAL_RETURN(%CHAIN_REG)
    movq    %rsi, _PS_CALLSTACK_PARTIAL(%CHAIN_REG)

    ;;; Go to the procedure's resume code

    movq    _PD_EXIT(%PB), %rax
    subq    $BRANCH_std, %rax
    jmp     *%rax

    .align  16

DEF_C_LAB (_swap_in_continue)

    ;;; The resume code returns here to continue the swapin loop
    ;;; The process will still be in CHAIN_REG

    movq    %CHAIN_REG, PROCESS

    ;;; Restore callstack save pointer, relative return address and
    ;;; save limit from the process record

    movq    _PS_CALLSTACK_PARTIAL(%CHAIN_REG), %rsi
    movq    _PS_PARTIAL_RETURN(%CHAIN_REG), %rcx
    movq    _PS_CALLSTACK_LIM(%CHAIN_REG), %rax
    movq    %rax, LIMIT

    ;;; Restore the procedure base from the stack top

    movq    (%rsp), %PB

    ;;; Continue the swapin loop

    jmp     sicont

    .align  16


;;; === SWAPPING THE USERSTACK ========================================

;;; _USSAVE:
;;;     Save and erase a number of bytes from the end of the userstack

;;; Call:
;;;     _ussave(_BYTE_LENGTH, _DST_ADDR);

DEF_C_LAB (_ussave)

    movq    (%USP), %rdi
    movq    8(%USP), %rcx
    addq    $16, %USP
    std

    ;;; Make ESI point to the first word on the stack,
    ;;; and EDI point to the end of the save area

    movq    I_LAB(_userhi), %rsi
    subq    $8, %rsi
    movq    %rsi, %rdx                      ;;; Save for later
    leaq    -8(%rdi, %rcx), %rdi

    ;;; Do the save

    sarq    $3, %rcx
    rep
    smovq

    ;;; Make EDI point to the stack end

    movq    %rdx, %rdi

    ;;; Compute the amount of stack left and shift it up

    leaq    8(%rsi), %rcx
    subq    %USP, %rcx
    sarq    $3, %rcx
    rep
    smovq

    ;;; Set the stack pointer and return

    leaq    8(%rdi), %USP
    ret

    .align  16

;;; _USRESTORE:
;;;     Restore a number of bytes at the end of the user stack

;;; Call:
;;;     _usrestore(_BYTE_LENGTH, _SRC_ADDR)

DEF_C_LAB (_usrestore)

    movq    (%USP), %rax
    movq    8(%USP), %rdx
    addq    $16, %USP
    cld

    ;;; Shift existing stack down, leaving room for restored stuff

    movq    I_LAB(_userhi), %rcx
    subq    %USP, %rcx
    sarq    $3, %rcx        ;;; Length of existing stack in words
    movq    %USP, %rsi      ;;; Source address is current USP
    subq    %rdx, %USP      ;;; New USP ...
    movq    %USP, %rdi      ;;; ... becomes destination address
    rep
    smovq

    ;;; Now copy the new stuff in

    movq    %rax, %rsi      ;;; Source of new stuff
    movq    I_LAB(_userhi), %rdi
    subq    %rdx, %rdi      ;;; Destination
    movq    %rdx, %rcx
    sarq    $3, %rcx        ;;; Length in words
    rep
    smovq

    ret

    .align  16

;;; _USERASUND:
;;;     Erase a number of bytes from the end of the user stack

;;; Call:
;;;     _userasund(_BYTE_LENGTH)

DEF_C_LAB (_userasund)

    movq    (%USP), %rax    ;;; Load length to erase to EAX
    addq    $8, %USP
    movq    I_LAB(_userhi), %rcx
    leaq    -8(%rcx), %rdi  ;;; Destination is first word on the stack
    addq    %rax, %USP      ;;; New stack pointer
    subq    %USP, %rcx      ;;; New stack length (= length of move)
    jz      L1.3            ;;; If zero, nothing to do
    movq    %rdi, %rsi      ;;; Source is destination ...
    subq    %rax, %rsi      ;;; ... offset by the amount being erased
    sarq    $3, %rcx        ;;; Length of move in words
    std
    rep
    smovq
L1.3:   ret

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
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Changes for Linux and S*CO (extra "\" for "*" operator).
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- Robert John Duncan, Jun 19 1991
    Made _swap_in_callstack chain a procedure off the stack when
    finished
--- Robert John Duncan, Oct  1 1990
    Fixed copying of on-stack lvars in _swap_in_callstack to adjust
    the stack pointer first in case of interrupts.
--- Rob Duncan, Jun 14 1990
    Added declaration for _PD_EXECUTE
--- John Gibson, Dec  6 1989
    Changes for new pop pointers (use explicit ID_VALOF offset)
--- John Gibson, Oct  5 1989
    _swap_out_callstack now clears the process' flags and chains the
    procedure on the stack when finished; the SWAP_BACK_IN flag is
    therefore unnecessary.
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- John Gibson, Jul  5 1989
    Replaced _M_PD_PROC_DLEXPR_CODE with _:M_PD_PROC_DLEXPR_CODE etc
 */
