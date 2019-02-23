/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:        S.pcunix/src/amisc.s
 * Purpose:     Main assembler routines for Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

vars
    _call_stack_lim, _plog_trail_sp, _plog_trail_lim
    ;

section $-Sys;

constant
    procedure (Call_overflow, User_overflow, Callstack_reset,
    Conspair, Plog$-Area_overflow, Async_raise_signal)
    ;

endsection;

lconstant macro (

    USP         = "ebx",
    PB          = "ebp",
    CHAIN_REG       = "edx",

    _KEY            = @@KEY,
    _K_APPLY        = @@K_APPLY,
    _PD_EXECUTE     = @@PD_EXECUTE,
    _PD_EXIT        = @@PD_EXIT,
    _PD_FRAME_LEN       = @@PD_FRAME_LEN,
    _PD_UPDATER     = @@PD_UPDATER,
    _P_BACK         = @@P_BACK,
    _P_FRONT        = @@P_FRONT,
    _RF_CONT        = @@RF_CONT,
    _SF_OWNER       = @@SF_OWNER,
    _V_BYTES        = @@V_BYTES,

);

>_#

    .file   "amisc.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text


;;; === APPLYING A POP PROCEDURE ======================================

;;; _POPENTER:
;;; Standard checking entry to a pop procedure.

;;; Arguments:
;;; EAX object to be applied

;;; Results:
;;; none, but EAX will always be left containing the address of the
;;; procedure entered to allow for initialisation of the procedure base
;;; register

DEF_C_LAB (_popenter)

    ;;; Test object for simple or compound

    testl   $1, %eax
    jnz L1.1

    ;;; Object is compound: compare key with procedure key

    cmpl    $C_LAB(procedure_key), _KEY(%eax)
    jne L3.1

    ;;; Applying a procedure: jump to its execute address

    jmp *_PD_EXECUTE(%eax)

L1.1:   ;;; Applying a (simple) number:
    ;;; push the number and run the appropriate key apply procedure
    ;;; (integer or decimal)

    subl    $4, %USP
    movl    %eax, (%USP)
    testl   $2, %eax
    jz  L2.1

    ;;; Applying an integer

    movl    C_LAB(integer_key) + _K_APPLY, %eax
    movl    _RF_CONT(%eax), %eax
    jmp *_PD_EXECUTE(%eax)

L2.1:   ;;; Applying a decimal

    movl    C_LAB(weakref decimal_key) + _K_APPLY, %eax
    movl    _RF_CONT(%eax), %eax
    jmp *_PD_EXECUTE(%eax)

L3.1:   ;;; Applying a structure:
    ;;; push the object and run the key apply procedure

    subl    $4, %USP
    movl    %eax, (%USP)
    movl    _KEY(%eax), %eax
    movl    _K_APPLY(%eax), %eax
    movl    _RF_CONT(%eax), %eax
    jmp *_PD_EXECUTE(%eax)

    .align  4


;;; _POPUENTER:
;;; _POPUNCENTER:
;;; Apply the updater of a pop object. _POPUENTER is the standard entry
;;; point which deals with all objects; _POPUNCENTER assumes the object
;;; is a procedure and just checks that its updater exists.

;;; Arguments:
;;; EAX object to be updated

;;; Results:
;;; none, but the address of the procedure finally applied will be left
;;; in EAX to allow for initialisation of the procedure base register.

;;; Other registers used:
;;; EAX various addresses; ultimately the updater procedure address
;;; ECX a copy of the original object (because EAX gets overwritten)

DEF_C_LAB (_popuenter)

    ;;; Test object for simple or compound

    testl   $1, %eax
    jnz L1.2

    ;;; Object is compound: compare key with procedure key

    cmpl    $C_LAB(procedure_key), _KEY(%eax)
    jne L3.2

    ;;; Applying the updater of a procedure: fall through to the
    ;;; non-checking entry

DEF_C_LAB (_popuncenter)

    ;;; Object in EAX is known to be a procedure:
    ;;; copy it to ECX for possible use as an argument to the error
    ;;; routine, then get its updater

    movl    %eax, %ecx
    movl    _PD_UPDATER(%eax), %eax

    ;;; If the updater is <false> (non-existent) jump to the error case

    cmpl    $C_LAB(false), %eax
    je  no_updater

    ;;; Otherwise go to the updater's execute address

    jmp *_PD_EXECUTE(%eax)

L1.2:   ;;; Applying the updater of a number:
    ;;; copy the number to ECX (to be pushed as an argument later)
    ;;; then get the appropriate key apply procedure (integer or decimal)
    ;;; in EAX. Jump to check (and apply) its updater

    movl    %eax, %ecx
    testl   $2, %eax
    jz  L2.2

    ;;; Integer key

    movl    C_LAB(integer_key) + _K_APPLY, %eax
    jmp L4.1

L2.2:   ;;; Decimal key

    movl    C_LAB(weakref decimal_key) + _K_APPLY, %eax
    jmp L4.1

L3.2:   ;;; Applying the updater of a structure:
    ;;; copy the object to ECX (to be pushed as an argument later)
    ;;; then get its key apply procedure in EAX

    movl    %eax, %ecx
    movl    _KEY(%eax), %eax
    movl    _K_APPLY(%eax), %eax

L4.1:   ;;; Check the updater of the key apply procedure in EAX

    movl    _RF_CONT(%eax), %eax
    movl    _PD_UPDATER(%eax), %eax
    cmpl    $C_LAB(false), %eax
    je  no_updater

    ;;; Updater OK -- push the original object (now in ECX) as an
    ;;; argument to it, and jump to its execute address

    subl    $4, %USP
    movl    %ecx, (%USP)
    jmp *_PD_EXECUTE(%eax)

no_updater:

    ;;; Object in ECX has no updater -- raise an error

    subl    $4, %USP
    movl    %ecx, (%USP)
    jmp XC_LAB(-> Sys$-Exec_nonpd)

    .align  4


;;; === CALLSTACK MANIPULATION ========================================

;;; _ERASE_SP_1:
;;; Erases the longword below the return address on the callstack,
;;; then chains -Callstack_reset-. Used by -Callstack_reset- in
;;; cleaning up

DEF_C_LAB (_erase_sp_1)

    popl    %eax
    movl    %eax, (%esp)
    jmp XC_LAB(Sys$-Callstack_reset)

    .align  4


;;; _NEXTFRAME:
;;; Takes a pointer to a stack frame and returns a pointer to the next

;;; Call:
;;; _nextframe(_FRAME_POINTER) -> _NEXT_FRAME_POINTER

DEF_C_LAB (_nextframe)

    ;;; Load frame pointer to EAX

    movl    (%USP), %eax

    ;;; Owner address in ECX

    movl    _SF_OWNER(%eax), %ecx

    ;;; Find the length of the frame (in longwords) from the owner's
    ;;; procedure header

    movzbl  _PD_FRAME_LEN(%ecx), %ecx

    ;;; Add the frame length in bytes (ECX * 4) to the given frame
    ;;; pointer to get the next frame pointer.

    leal    (%eax, %ecx, 4), %eax
    movl    %eax, (%USP)
    ret

    .align  4


;;; _UNWIND_FRAME:
;;; Unwinds the previous stack frame by jumping into its owner's exit
;;; code. This subroutine will be called immediately after executing an
;;; M_UNWIND operation, so that the (unwanted) return address into the
;;; procedure being unwound will be all that remains of the current stack
;;; frame, i.e. :

;;;     previous frame starts here --->  |_______|
;;;      return address from caller -->  |_______|
;;;        return address from here -->  |_______|  <-- ESP

;;; The procedure's exit code will finish with a RET into its caller; we
;;; replace that return address with the return from here (to return
;;; control to our caller), but save it in CHAIN_REG from where it can
;;; be restored with an M_CALL_WITH_RETURN operation.

;;; The M_UNWIND operation should have restored the procedure base
;;; register (PB) to contain the address of the owner of the previous
;;; frame.

DEF_C_LAB (_unwind_frame)

    ;;; Extract the length of the previous stack frame (in longwords)
    ;;; from its owner procedure, whose address should be in PB

    movzbl  _PD_FRAME_LEN(%PB), %eax

    ;;; The last word in that frame will be the return address:
    ;;; copy that to CHAIN_REG, then replace it with our return

    leal    4(%esp, %eax, 4), %eax
    movl    (%eax), %CHAIN_REG
    popl    (%eax)

    ;;; Clear the unwanted return address from the stack and go into
    ;;; the procedure's exit code

    addl    $4, %esp
    jmp *_PD_EXIT(%PB)

    .align  4

;;; === CHAINING A POP PROCEDURE ======================================

;;; _SYSCHAIN:
;;; _SYSCHAIN_CALLER:
;;; _SYSNCCHAIN:
;;; _SYSNCCHAIN_CALLER:
;;; Chain a pop procedure, either directly or out of the current caller.
;;; These are executed as a result of replacing the return address in a
;;; stack frame with one of their addresses and then (after some
;;; arbitrary exit code) doing an RET. In the case of _SYS(NC)CHAIN,
;;; the displaced return address is saved in CHAIN_REG, as that has to
;;; become the return address of the chained procedure;
;;; _SYS(NC)CHAIN_CALLER sets CHAIN_REG to be the return address from
;;; the previous frame, as a result of calling _UNWIND_FRAME.
;;; All the subroutines expect the procedure to be chained on the
;;; user stack.

DEF_C_LAB (_syschain_caller)

    ;;; Set up a dummy return address for _UNWIND_FRAME and call it;
    ;;; this will leave the caller's return address in CHAIN_REG

    pushl   $0
    call    C_LAB(_unwind_frame)

    ;;; Fall through to direct chain

DEF_C_LAB (_syschain)

    ;;; Reinstate the return address from CHAIN_REG

    pushl   %CHAIN_REG

    ;;; Get the object to chain from the stack and run it

    movl    (%USP), %eax
    addl    $4, %USP
    jmp C_LAB(_popenter)

    .align  4

DEF_C_LAB (_sysncchain_caller)

    ;;; Set up a dummy return address for _UNWIND_FRAME and call it;
    ;;; this will leave the caller's return address in CHAIN_REG

    pushl   $0
    call    C_LAB(_unwind_frame)

    ;;; Fall through to direct chain

DEF_C_LAB (_sysncchain)

    ;;; Reinstate the return address from CHAIN_REG

    pushl   %CHAIN_REG

    ;;; Get the procedure to chain from the stack and go to its execute
    ;;; address

    movl    (%USP), %eax
    addl    $4, %USP
    jmp *_PD_EXECUTE(%eax)

    .align  4


;;; === PREDICATES ON POP OBJECTS =====================================

;;; _ISCOMPOUND:
;;; _ISSIMPLE:
;;; _ISINTEGER:
;;; _NEG:
;;; _ZERO:
;;; _NOT:
;;; Unary predicates: expect one argument on the user stack, and
;;; overwrite it with true or false.
;;; RETURN_TRUE provides a common exit point for the success case

DEF_C_LAB (_iscompound)

    ;;; Object is a pop pointer -- bottom bit clear

    testl   $1, (%USP)
    jz  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB (_issimple)

    ;;; Object is not a pop pointer -- bottom bit set

    testl   $1, (%USP)
    jnz return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB (_isinteger)

    ;;; Object is a pop integer -- second bit set

    testl   $2, (%USP)
    jnz return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB (_neg)

    ;;; Object is negative

    cmpl    $0, (%USP)
    jl  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB (_zero)

    ;;; Object is (machine) zero

    cmpl    $0, (%USP)
    je  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB (_not)

    ;;; Object is false

    cmpl    $C_LAB(false), (%USP)
    je  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

;;; _EQ:
;;; _NEQ:
;;; _GR:
;;; _GREQ:
;;; _LT:
;;; _LTEQ:
;;; _SGR:
;;; _SGREQ:
;;; _SLT:
;;; _SLTEQ:
;;; Binary comparisons. Expect two arguments on the user stack and return
;;; true or false.

DEF_C_LAB 7 (_eq)

    ;;; Identity (pop ==)

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    je  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 7 (_neq)

    ;;; Non-identity (pop /==)

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jne return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_gr)

    ;;; Unsigned greater than

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    ja  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_greq)

    ;;; Unsigned greater than or equal

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jae return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_lt)

    ;;; Unsigned less than

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jb  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_lteq)

    ;;; Unsigned less than or equal

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jbe return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_sgr)

    ;;; Signed greater than

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jg  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_sgreq)

    ;;; Signed greater than or equal

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jge return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_slt)

    ;;; Signed less than

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jl  return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

DEF_C_LAB 6 (_slteq)

    ;;; Signed less than or equal

    movl    (%USP), %eax
    addl    $4, %USP
    cmpl    %eax, (%USP)
    jle return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

;;; _BITST:
;;; Logical AND two items and test for non-zero (pop &&/=_0)

DEF_C_LAB 4 (_bitst)

    movl    (%USP), %eax
    addl    $4, %USP
    testl   %eax, (%USP)
    jnz return_true
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

;;; _HASKEY:
;;; tests whether the first item on stack is the key of the second item.
;;; It's assumed that the key is always that of a compound item, so that
;;; if the object tested is not compound, the result must be <false>
;;; immediately.

;;; Call:
;;; _haskey(ITEM, KEY) -> BOOL

;;; Register usage:
;;; EAX the object
;;; ECX the key

DEF_C_LAB (_haskey)

    ;;; Load key to ECX

    movl    (%USP), %ecx
    addl    $4, %USP

    ;;; Load item to EAX and test for compound:
    ;;; return <false> if not

    movl    (%USP), %eax
    testl   $1, %eax
    jnz L1.3

    ;;; Compare keys

    cmpl    %ecx, _KEY(%eax)
    je  return_true
L1.3:   movl    $C_LAB(false), (%USP)
    ret

    .align  4


return_true:

    movl    $C_LAB(true), (%USP)
    ret

    .align  4


;;; === OPTIMISED DATA ROUTINES =======================================

;;; _CONSPAIR:
;;; Optimised -conspair-.

;;; Call:
;;; _conspair(FRONT, BACK) -> PAIR

DEF_C_LAB (_conspair)

    ;;; Get the free pair list

    movl    I_LAB(Sys$- _free_pairs), %eax

    ;;; If its simple, there's nothing left -- chain the storage
    ;;; allocater

    testl   $1, %eax
    jnz XC_LAB(Sys$-Conspair)

    ;;; Otherwise, take the first pair from the free list

    movl    _P_BACK(%eax), %ecx
    movl    %ecx, I_LAB(Sys$- _free_pairs)

    ;;; Initialise the new pair with the values from the stack

    movl    (%USP), %ecx
    movl    %ecx, _P_BACK(%eax)
    movl    4(%USP), %ecx
    movl    %ecx, _P_FRONT(%eax)

    ;;; Return the pair

    addl    $4, %USP
    movl    %eax, (%USP)
    ret

    .align  4


;;; _DATAKEY:
;;; Optimised -datakey-: returns the key of any pop object.

;;; Call:
;;; _datakey(ITEM) -> KEY

DEF_C_LAB (_datakey)

    ;;; Load item to EAX and test for compound

    movl    (%USP), %eax
    testl   $1, %eax
    jnz L1.5

    ;;; Object is compound: extract key and return

    movl    _KEY(%eax), %eax
    movl    %eax, (%USP)
    ret

L1.5:   ;;; Object is a (simple) number:
    ;;; return integer or decimal key as appropriate

    testl   $2, %eax
    jz  L2.3

    ;;; Object is an integer

    movl    $C_LAB(integer_key), (%USP)
    ret

L2.3:   ;;; Object is a decimal

    movl    $C_LAB(weakref decimal_key), (%USP)
    ret

    .align  4


;;; === STRING HANDLING ===============================================

;;; _SUBSS:
;;; Fast pop string subscript (implements -fast_subscrs-).

;;; Call:
;;;     _subss(INT, STRING) -> INT

;;; Register usage:
;;; ESI string address
;;; ECX index as popint
;;; EAX the indexed byte

DEF_C_LAB (_subss)

    movl    (%USP), %esi
    movl    4(%USP), %ecx
    addl    $4, %USP

    ;;; Convert index to a machine integer and load the indexed byte
    ;;; (zero extended) into EAX

    sarl    $2, %ecx
    movzbl  _V_BYTES-1(%esi, %ecx), %eax

    ;;; Return character as a popint

    leal    3(, %eax, 4), %eax
    movl    %eax, (%USP)
    ret

    .align  4

;;; _U_SUBSS:
;;; Fast pop string update (implements updaterof -fast_subscrs-).

;;; Call:
;;; INT -> _u_subss(INT, STRING)

;;; Register usage:
;;; EDI string address
;;; ECX index as popint
;;; EAX the new character as a popint

DEF_C_LAB (-> _subss)
DEF_C_LAB (_u_subss)

    movl    (%USP), %edi
    movl    4(%USP), %ecx
    movl    8(%USP), %eax
    addl    $12, %USP

    ;;; Convert character and index to machine integers;
    ;;; insert the new byte and return

    sarl    $2, %eax
    sarl    $2, %ecx
    movb    %al, _V_BYTES-1(%edi, %ecx)
    ret

    .align  4


;;; _LOCC:
;;; Search a byte string for a given character. Return an offset to
;;; the matching byte or -1 if not found.

;;; Call:
;;; _locc(_SRCADDR, _BYTE_COUNT, _BYTE) -> _OFFSET
;;; _locc(_SRCADDR, _BYTE_COUNT, _BYTE) -> _-1


;;; Register usage:
;;; ESI source address + 1 (for computing the offset)
;;; EDI source pointer during scan
;;; EAX byte to search for
;;; ECX length of scan in bytes

DEF_C_LAB (_locc)

    movl    (%USP), %eax
    movl    4(%USP), %ecx
    addl    $8, %USP

    ;;; Test the byte-count in ECX for zero:
    ;;; if so, nothing to search, so return -1 immediately

    cmpl    $0, %ecx
    je  L1.6

    ;;; Load the string address into EDI, and copy into ESI offset by 1

    movl    (%USP), %edi
    leal    1(%edi), %esi

    ;;; Clear the direction flag to increment EDI during the scan

    cld

    ;;; Scan for byte AL in EDI for maximum length ECX

    repnz
    sscab

    ;;; If the last byte comparison was non-zero, it was never found:
    ;;; return -1

    jnz L1.6

    ;;; Otherwise, EDI points one beyond the matching byte:
    ;;; compute offset by subtracting ESI (already adjusted by 1)

    subl    %esi, %edi
    movl    %edi, (%USP)
    ret

L1.6:   ;;; Search failed -- return -1

    movl    $-1, (%USP)
    ret

    .align  4


;;; _SKPC:
;;; Search a byte string for the first byte not matching a given byte
;;; (i.e skip over matching bytes). Return an offset to the first
;;; different byte in the string, or -1 if none found.

;;; Call:
;;; _skpc(_SRCADDR, _BYTE_COUNT, _BYTE) -> _OFFSET
;;; _skpc(_SRCADDR, _BYTE_COUNT, _BYTE) -> _-1

;;; Register usage:
;;; ESI source address + 1 (for computing the offset)
;;; EDI source pointer during the scan
;;; EAX byte to skip
;;; ECX length of region in bytes

DEF_C_LAB (_skpc)

    movl    (%USP), %eax
    movl    4(%USP), %ecx
    addl    $8, %USP

    ;;; Test the byte-count in ECX for zero:
    ;;; if so, nothing to search, so return -1 immediately

    cmpl    $0, %ecx
    je  L1.7

    ;;; Load the string address into EDI and copy to ESI offset by 1

    movl    (%USP), %edi
    leal    1(%edi), %esi

    ;;; Clear the direction flag to increment EDI during the scan

    cld

    ;;; Scan for byte not matching AL in EDI for maximum length ECX

    repz
    sscab

    ;;; If the last byte comparison was zero, everything matched AL:
    ;;; return -1

    jz  L1.7

    ;;; Otherwise, EDI points one beyond the matching byte:
    ;;; compute offset by subtracting ESI (already adjusted by 1)

    subl    %esi, %edi
    movl    %edi, (%USP)
    ret

L1.7:   ;;; Search failed -- return -1

    movl    $-1, (%USP)
    ret

    .align  4


;;; === PROCEDURE ENTRY CHECKS ========================================

;;; _CHECKPLOGALL:
;;; Prolog procedure entry: check the Prolog stacks, then do normal
;;; _CHECKALL

DEF_C_LAB (_checkplogall)

    movl    $C_LAB(_special_var_block), %eax
    movl    _SVB_OFFS(_plog_trail_sp)(%eax), %ecx
    cmpl    %ecx, _SVB_OFFS(_plog_trail_lim)(%eax)
    ja  checkall
    jmp XC_LAB(weakref[prologvar_key] Sys$-Plog$-Area_overflow)

    .align  4

;;; _CHECKALL:
;;; Check normal stacks on procedure entry.

DEF_C_LAB (_checkall)

    movl    $C_LAB(_special_var_block), %eax

checkall:

    ;;; Check for callstack overflow

    cmpl    %esp, _SVB_OFFS(_call_stack_lim)(%eax)
    ja  L1.10

    ;;; Check for userstack overflow

    cmpl    %USP, _SVB_OFFS(_userlim)(%eax)
    ja  L2.6

L0.1:   ;;; Check interrupt trap bit

    testl   $1, _SVB_OFFS(_trap)(%eax)
    jnz L3.5

    ;;; Everything OK

    ret

L1.10:  ;;; Callstack overflow: if not disabled, jump to repair, otherwise
    ;;; go back to test for interrupts

    testl   $2, I_LAB(_disable)
    jnz L0.1
    jmp XC_LAB(Sys$-Call_overflow)

L2.6:   ;;; Userstack overflow: if not disabled, jump to repair, otherwise
    ;;; go back to test for interrupts

    testl   $2, I_LAB(_disable)
    jnz L0.1
    jmp XC_LAB(Sys$-User_overflow)

L3.5:   ;;; Interrupt: if not disabled, jump to handler, otherwise return

    testl   $1, I_LAB(_disable)
    jz  XC_LAB(Sys$-Async_raise_signal)
    ret


    .align  4


;;; _CHECKINTERRUPT:
;;; Check for intterupt flag set; if so, branch to _CHECKALL to handle it

DEF_C_LAB (_checkinterrupt)

    testl   $1, I_LAB(_trap)
    jnz C_LAB(_checkall)
    ret

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
--- John Gibson, Feb 10 1997
    Removed _mt*chc subroutine
--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Changes for Linux and S*CO.
--- John Gibson, Oct 18 1994
    free*pairs -> _free_pairs
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- Simon Nichols, Nov 10 1993
    Renamed amisc.s and moved the main routine to amain.s
--- John Gibson, Oct  2 1992
    Got rid of _c*mpc_order
--- Rob Duncan, May 16 1990
    Changed _checkall so that -Async_raise_signal- is not called
    when _disable interrupt bit set (also now doesn't clear _trap
    if it is called).
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- John Gibson, Jan 15 1989
    Replaced use of UC_LAB etc for updater with -> before pathname
 */
