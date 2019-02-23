/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/aprocess.s
 * Purpose:     Assembler support for processes on Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'
#_INCLUDE 'process.ph'

lconstant macro (

    USP         = "ebx",
    PB          = "ebp",
    CHAIN_REG       = "edx",

    _ID_VALOF       = @@ID_VALOF,
    _PD_EXECUTE     = @@PD_EXECUTE,
    _PD_EXIT        = @@PD_EXIT,
    _PD_FLAGS       = @@PD_FLAGS,
    _PD_FRAME_LEN       = @@PD_FRAME_LEN,
    _PD_NLOCALS     = @@PD_NLOCALS,
    _PD_NUM_STK_VARS    = @@PD_NUM_STK_VARS,
    _PD_TABLE       = @@PD_TABLE,
    _PS_CALLSTACK_LIM   = @@PS_CALLSTACK_LIM,
    _PS_CALLSTACK_PARTIAL   = @@PS_CALLSTACK_PARTIAL,
    _PS_FLAGS       = @@PS_FLAGS,
    _PS_PARTIAL_RETURN  = @@PS_PARTIAL_RETURN,
    _PS_STATE       = @@PS_STATE,
);

>_#

    .file   "aprocess.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


;;; === SWAPPING THE CALLSTACK ========================================

    .data

PROCESS:        ;;; Process record
    .long   0
LIMIT:          ;;; Limit of the callstack save area
    .long   0
NEXT_FRAME:     ;;; Pointer to the next frame in the saved callstack
    .long   0

    .text

.set    BRANCH_std, 7   ;;; Size of a standard branch instruction
            ;;; (as placed by I_BRANCH_std)

;;; _SWAP_OUT_CALLSTACK:
;;; Swap out the callstack for a process
;;; (i.e. copy the callstack into the process record)

;;; Call:
;;; _swap_out_callstack(PROCESS)

DEF_C_LAB (_swap_out_callstack)

    movl    (%USP), %eax
    addl    $4, %USP

    movl    %eax, PROCESS           ;;; Save the process record
    movl    _PS_CALLSTACK_LIM(%eax), %edi   ;;; Start of saved callstack
    movl    _PS_STATE(%eax), %eax
    movl    %eax, LIMIT         ;;; End of saved callstack
    jmp sotest

soloop:

    ;;; Copy out the next stack frame:
    ;;; save the return address in ECX, then set the procedure base
    ;;; register from the owner address on the stack top

    popl    %ecx
    movl    (%esp), %PB

    ;;; Make the return address relative to the procedure base

    subl    %PB, %ecx

    ;;; Test if the procedure has dlocal expression code to run

    testb   $_:M_PD_PROC_DLEXPR_CODE, _PD_FLAGS(%PB)
    jnz sobrk

socont:

    ;;; Continue here after running the dlocal expression code:

    ;;; EDI points to where the stack frame is to be saved in the process
    ;;; record; we compute the position of the next frame by subtracting
    ;;; the length of the current stack frame

    movzbl  _PD_FRAME_LEN(%PB), %eax
    sall    $2, %eax
    subl    %eax, %edi
    movl    %edi, NEXT_FRAME

    ;;; Save relative return address and owner address in the process
    ;;; record

    movl    %ecx, (%edi)
    movl    %PB, 4(%edi)
    addl    $8, %edi

    ;;; Copy any on-stack lvars into the process record

    cld
    movzbl  _PD_NUM_STK_VARS(%PB), %ecx
    leal    4(%esp), %esi
    rep
    smovl
    movl    %esi, %esp

    ;;; Test if there are any dynamic locals

    movzbl  _PD_NLOCALS(%PB), %ecx
    cmpl    $0, %ecx
    je  L2.1

    ;;; If so, copy the current values of the locals into the
    ;;; process record, and restore their previous values from the
    ;;; stack.
    ;;; ESI is initialised to point to the last entry in the procedure's
    ;;; identifier table

    leal    _PD_TABLE-4(%PB, %ecx, 4), %esi

L1.1:               ;;; Loop:
    movl    (%esi), %edx    ;;; ident to EDX
    movl    _ID_VALOF(%edx), %eax   ;;; idval to EAX
    sstol           ;;; save idval in process record
    popl    _ID_VALOF(%edx) ;;; restore previous idval from stack
    subl    $4, %esi    ;;; decrement ESI
    loop    L1.1

L2.1:   ;;; Stack frame saved -- restore EDI to point to where the next
    ;;; should go

    movl    NEXT_FRAME, %edi

sotest:

    ;;; Test if there are any more stack frames to swap out

    cmpl    %edi, LIMIT
    jb  soloop

sodone:

    ;;; Finished swapping out -- chain procedure on stack

    movl    PROCESS, %eax
    movl    $0, _PS_CALLSTACK_PARTIAL(%eax)
    movw    $0, _PS_FLAGS(%eax)     ;;; 0 flags = suspended
    movl    (%USP), %eax
    addl    $4, %USP
    jmp *_PD_EXECUTE(%eax)      ;;; chain procedure

sobrk:

    ;;; Come here to run dlocal expression code, by jumping into the
    ;;; appropriate part of the exit code of the procedure.
    ;;; The process is passed in CHAIN_REG

    movl    PROCESS, %CHAIN_REG

    ;;; Save relative return address and callstack save pointer in the
    ;;; process record

    movl    %ecx, _PS_PARTIAL_RETURN(%CHAIN_REG)
    movl    %edi, _PS_CALLSTACK_PARTIAL(%CHAIN_REG)

    ;;; Go to the procedure's suspend code

    movl    _PD_EXIT(%PB), %eax
    subl    $BRANCH_std << 1, %eax
    jmp *%eax

    .align  4

DEF_C_LAB (_swap_out_continue)

    ;;; The suspend code returns here to resume the swapout loop
    ;;; The process will still be in CHAIN_REG

    movl    %CHAIN_REG, PROCESS

    ;;; Restore callstack save pointer, relative return address and
    ;;; save limit from the process record

    movl    _PS_CALLSTACK_PARTIAL(%CHAIN_REG), %edi
    movl    _PS_PARTIAL_RETURN(%CHAIN_REG), %ecx
    movl    _PS_STATE(%CHAIN_REG), %eax
    movl    %eax, LIMIT

    ;;; Restore procedure base from the stack top

    movl    (%esp), %PB

    ;;; Continue the swapout loop

    jmp socont

    .align  4


;;; _SWAP_IN_CALLSTACK:
;;; Swap in the callstack for a process
;;; (i.e. copy stack frames from the process record onto the stack)

;;; Call:
;;; _swap_in_callstack(PROCESS)

DEF_C_LAB (_swap_in_callstack)

    movl    (%USP), %eax
    addl    $4, %USP

    movl    %eax, PROCESS       ;;; Save the process record

    ;;; Get a pointer to the start of the saved callstack in ESI
    ;;; and the limit in LIMIT

    movl    _PS_STATE(%eax), %esi
    movl    _PS_CALLSTACK_LIM(%eax), %eax
    movl    %eax, LIMIT
    jmp sitest

siloop:

    ;;; ESI points to the saved stack frame in the process record.
    ;;; Restore the procedure base register from the saved owner address.

    movl    4(%esi), %PB

    ;;; Compute the position of the next frame in the record by adding
    ;;; the length of the current frame

    movzbl  _PD_FRAME_LEN(%PB), %eax
    leal    (%esi, %eax, 4), %esi
    movl    %esi, NEXT_FRAME
    subl    $4, %esi

    ;;; Check for any dynamic locals

    std
    movzbl  _PD_NLOCALS(%PB), %ecx
    cmpl    $0, %ecx
    je  L2.2

    ;;; If so, push the current values of the locals on the stack
    ;;; and get their new values from the process record.
    ;;; EDI is initialised to point to the first entry in the procedure's
    ;;; identifier table

    leal    _PD_TABLE(%PB), %edi

L1.2:               ;;; Loop:
    movl    (%edi), %edx    ;;; ident to EDX
    pushl   _ID_VALOF(%edx) ;;; push the idval
    slodl           ;;; new idval from process record to EAX
    movl    %eax, _ID_VALOF(%edx)   ;;; set new idval
    addl    $4, %edi    ;;; increment EDI
    loop    L1.2

L2.2:   ;;; Copy any on-stack lvars from the saved frame,
    ;;; adjusting ESP first in case of interrupts

    movzbl  _PD_NUM_STK_VARS(%PB), %ecx
    leal    -4(%esp), %edi

    leal    (, %ecx, 4), %eax
    subl    %eax, %esp

    rep
    smovl

    ;;; Push the owner address from the procedure base register
    ;;; and set ECX to be the saved relative return address

    pushl   %PB
    movl    -4(%esi), %ecx

    ;;; Set ESI to point to the next frame

    movl    NEXT_FRAME, %esi

    ;;; Test for dlocal expression code to run

    testb   $_:M_PD_PROC_DLEXPR_CODE, _PD_FLAGS(%PB)
    jnz sibrk

sicont:

    ;;; Continue here after running dlocal expression code:

    ;;; Make the relative return address in ECX absolute and push it

    addl    %PB, %ecx
    pushl   %ecx

sitest:

    ;;; Test if there are more frames to swap in

    cmpl    %esi, LIMIT
    ja  siloop

sidone:

    ;;; Finished swapping in - chain procedure from stack

    movl    PROCESS, %eax
    movl    $0, _PS_CALLSTACK_PARTIAL(%eax)
    movl    (%USP), %eax
    addl    $4, %USP
    jmp *_PD_EXECUTE(%eax)      ;;; chain procedure

sibrk:

    ;;; Come here to run dlocal expression code, by jumping into the
    ;;; appropriate part of the exit code of the procedure.
    ;;; The process is passed in CHAIN_REG

    movl    PROCESS, %CHAIN_REG

    ;;; Save relative return address and callstack save pointer in the
    ;;; process record

    movl    %ecx, _PS_PARTIAL_RETURN(%CHAIN_REG)
    movl    %esi, _PS_CALLSTACK_PARTIAL(%CHAIN_REG)

    ;;; Go to the procedure's resume code

    movl    _PD_EXIT(%PB), %eax
    subl    $BRANCH_std, %eax
    jmp *%eax

    .align  4

DEF_C_LAB (_swap_in_continue)

    ;;; The resume code returns here to continue the swapin loop
    ;;; The process will still be in CHAIN_REG

    movl    %CHAIN_REG, PROCESS

    ;;; Restore callstack save pointer, relative return address and
    ;;; save limit from the process record

    movl    _PS_CALLSTACK_PARTIAL(%CHAIN_REG), %esi
    movl    _PS_PARTIAL_RETURN(%CHAIN_REG), %ecx
    movl    _PS_CALLSTACK_LIM(%CHAIN_REG), %eax
    movl    %eax, LIMIT

    ;;; Restore the procedure base from the stack top

    movl    (%esp), %PB

    ;;; Continue the swapin loop

    jmp sicont

    .align  4


;;; === SWAPPING THE USERSTACK ========================================

;;; _USSAVE:
;;; Save and erase a number of bytes from the end of the userstack

;;; Call:
;;; _ussave(_BYTE_LENGTH, _DST_ADDR);

DEF_C_LAB (_ussave)

    movl    (%USP), %edi
    movl    4(%USP), %ecx
    addl    $8, %USP
    std

    ;;; Make ESI point to the first word on the stack,
    ;;; and EDI point to the end of the save area

    movl    I_LAB(_userhi), %esi
    subl    $4, %esi
    movl    %esi, %edx          ;;; Save for later
    leal    -4(%edi, %ecx), %edi

    ;;; Do the save

    sarl    $2, %ecx
    rep
    smovl

    ;;; Make EDI point to the stack end

    movl    %edx, %edi

    ;;; Compute the amount of stack left and shift it up

    leal    4(%esi), %ecx
    subl    %USP, %ecx
    sarl    $2, %ecx
    rep
    smovl

    ;;; Set the stack pointer and return

    leal    4(%edi), %USP
    ret

    .align  4

;;; _USRESTORE:
;;; Restore a number of bytes at the end of the user stack

;;; Call:
;;; _usrestore(_BYTE_LENGTH, _SRC_ADDR)

DEF_C_LAB (_usrestore)

    movl    (%USP), %eax
    movl    4(%USP), %edx
    addl    $8, %USP
    cld

    ;;; Shift existing stack down, leaving room for restored stuff

    movl    I_LAB(_userhi), %ecx
    subl    %USP, %ecx
    sarl    $2, %ecx    ;;; Length of existing stack in words
    movl    %USP, %esi  ;;; Source address is current USP
    subl    %edx, %USP  ;;; New USP ...
    movl    %USP, %edi  ;;; ... becomes destination address
    rep
    smovl

    ;;; Now copy the new stuff in

    movl    %eax, %esi  ;;; Source of new stuff
    movl    I_LAB(_userhi), %edi
    subl    %edx, %edi  ;;; Destination
    movl    %edx, %ecx
    sarl    $2, %ecx    ;;; Length in words
    rep
    smovl

    ret

    .align  4

;;; _USERASUND:
;;; Erase a number of bytes from the end of the user stack

;;; Call:
;;; _userasund(_BYTE_LENGTH)

DEF_C_LAB (_userasund)

    movl    (%USP), %eax    ;;; Load length to erase to EAX
    addl    $4, %USP
    movl    I_LAB(_userhi), %ecx
    leal    -4(%ecx), %edi  ;;; Destination is first word on the stack
    addl    %eax, %USP  ;;; New stack pointer
    subl    %USP, %ecx  ;;; New stack length (= length of move)
    jz  L1.3        ;;; If zero, nothing to do
    movl    %edi, %esi  ;;; Source is destination ...
    subl    %eax, %esi  ;;; ... offset by the amount being erased
    sarl    $2, %ecx    ;;; Length of move in words
    std
    rep
    smovl
L1.3:   ret

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
