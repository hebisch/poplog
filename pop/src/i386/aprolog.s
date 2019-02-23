/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/aprolog.s
 * Purpose:     Assembler support for Prolog on Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

constant
    procedure (Sys$-Plog$-Assign_pair, Sys$-Plog$-New_var),
    _checkplogall
    ;

vars
    _plog_trail_sp, _plog_next_var, _plog_contn_sp, _plog_contn_top,
    _plog_save_contn_sp, _plog_trail_barrier, _plog_trail_lim,
    _call_stack_lim
    ;

lconstant macro (

    ;;; User stack pointer

    USP         = "ebx",

    ;;; Structure offsets

    _KEY            = @@KEY,
    _PGT_FUNCTOR        = @@PGT_FUNCTOR,
    _PGT_LENGTH     = @@PGT_LENGTH,
    _PGV_CONT       = @@PGV_CONT,
    _PGV_SIZE       = @@(struct PLOGVAR)++,
    _P_BACK         = @@P_BACK,
    _P_FRONT        = @@P_FRONT,
    _SF_PLGSV_CONTN_TOP = @@SF_PLGSV_CONTN_TOP,
    _SF_PLGSV_NEXT_VAR  = @@SF_PLGSV_NEXT_VAR,
    _SF_PLGSV_TRAIL_SP  = @@SF_PLGSV_TRAIL_SP,

);

>_#

    .file   "aprolog.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text


;;; === SAVING AND RESTORING THE PROLOG STATE =========================

;;; _PROLOG_SAVE_CHECK:
;;; Saves the prolog continuation stack, trail and next_free_var pointers
;;; in their corresponding saved locations, then does stack overflow and
;;; interrupt checks.

;;; Call:
;;; _prolog_save_check();

;;; Register usage:
;;; ESI pointer to "special var block"
;;; EAX work
;;; ECX trail pointer

;;; Usage:
;;; does procedure entry checking for procedures using PLOG_SAVE and
;;; PLOG_RESTORE

DEF_C_LAB (_prolog_save_check)

    movl    $C_LAB(_special_var_block), %esi

    ;;; Save next_free_var pointer

    movl    _SVB_OFFS(_plog_next_var)(%esi), %eax
    movl    %eax, _SF_PLGSV_NEXT_VAR+4(%esp)

    ;;; Save trail end pointer, relative to trail barrier

    movl    _SVB_OFFS(_plog_trail_sp)(%esi), %eax
    movl    %eax, %ecx              ;;; wanted later
    subl    _SVB_OFFS(_plog_trail_barrier)(%esi), %eax
    movl    %eax, _SF_PLGSV_TRAIL_SP+4(%esp)

    ;;; Save continuation stack top and pointer

    movl    _SVB_OFFS(_plog_contn_sp)(%esi), %eax
    movl    %eax, _SVB_OFFS(_plog_save_contn_sp)(%esi)
    movl    _SVB_OFFS(_plog_contn_top)(%esi), %eax
    movl    %eax, _SF_PLGSV_CONTN_TOP+4(%esp)

    ;;; Check for trail overflow

    cmpl    %ecx, _SVB_OFFS(_plog_trail_lim)(%esi)
    jbe C_LAB(_checkplogall)

    ;;; Check system stack overflow

    cmpl    %esp, _SVB_OFFS(_call_stack_lim)(%esi)
    ja  C_LAB(_checkplogall)

    ;;; Check userstack overflow

    cmpl    %USP, _SVB_OFFS(_userlim)(%esi)
    ja  C_LAB(_checkplogall)

    ;;; Test interrupt bit

    testl   $1, _SVB_OFFS(_trap)(%esi)
    jnz C_LAB(_checkplogall)
    ret

    .align  4

;;; _PROLOG_RESTORE:
;;; restores prolog continuation stack, trail and next_free_var pointer
;;; from the values saved by the last -plog_save_check-, and unwinds
;;; the trail back to the restored position, resetting vars to undef.

;;; Call:
;;; _prolog_restore();

;;; Register usage:
;;; EDI pointer to "special var block"
;;; EAX work
;;; ECX/ESI trail pointers

DEF_C_LAB (_prolog_restore)

    movl    $C_LAB(_special_var_block), %edi

    ;;; Restore next_free_var pointer

    movl    _SF_PLGSV_NEXT_VAR+4(%esp), %eax
    movl    %eax, _SVB_OFFS(_plog_next_var)(%edi)

    ;;; Restore continuation stack top and pointer

    movl    _SVB_OFFS(_plog_save_contn_sp)(%edi), %eax
    movl    %eax, _SVB_OFFS(_plog_contn_sp)(%edi)
    movl    _SF_PLGSV_CONTN_TOP+4(%esp), %eax
    movl    %eax, _SVB_OFFS(_plog_contn_top)(%edi)

    ;;; Compare current trail end pointer with the saved value
    ;;; (saved relative to trail barrier)

    movl    _SF_PLGSV_TRAIL_SP+4(%esp), %esi
    addl    _SVB_OFFS(_plog_trail_barrier)(%edi), %esi
    movl    _SVB_OFFS(_plog_trail_sp)(%edi), %ecx
    cmpl    %ecx, %esi
    je  L2.1

    ;;; Restore the trail end pointer ...

    movl    %esi, _SVB_OFFS(_plog_trail_sp)(%edi)

    ;;; ... and uninstantiate the variables on it

    cld
L1.3:   slodl               ;;; plogvar in EAX
    movl    %eax, _PGV_CONT(%eax)   ;;; assigned to its own PGV_CONT
    cmpl    %ecx, %esi
    jne L1.3

L2.1:   ret

    .align  4


;;; === HEAD MATCHING =================================================

;;; _PROLOG_UNIFY_ATOM:
;;; unifies an argument against an atom.
;;; Sets flags to EQ if unification succeeds and NEQ if it fails.

;;; Call:
;;; _prolog_unify_atom();

;;; Arguments:
;;; EAX (arg_reg_0) the argument
;;; ECX (arg_reg_1) the atom

;;; Usage:
;;; from -sysPLOG_IFNOT_ATOM-: the subroutine call will be followed by
;;; an I_PLOG_IFNOT_ATOM to jump on the status flags.

DEF_C_LAB (_prolog_unify_atom)

L1.4:   ;;; Start of dereferencing loop:
    ;;; break out if item is simple

    testl   $1, %eax
    jnz L2.2

    ;;; Item is compound:
    ;;; break out unless item is plogvar

    cmpl    $C_LAB(prologvar_key), _KEY(%eax)
    jne L2.2

    ;;; Dereference one link of plogvar:
    ;;; loop unless (var!PGV_CONT) == var

    movl    %eax, %esi
    movl    _PGV_CONT(%eax), %eax
    cmpl    %eax, %esi
    jne L1.4

    ;;; Argument is an uninstantiated plogvar:
    ;;; assign the atom in ECX to it and push it on the trail

    movl    %ecx, _PGV_CONT(%eax)
    movl    I_LAB(_plog_trail_sp), %edi
    movl    %eax, (%edi)
    addl    $4, %edi
    movl    %edi, I_LAB(_plog_trail_sp)

    ;;; Set flags to equal to indicate success and return

    cmpl    %eax, %eax
    ret

L2.2:   ;;; Item in EAX is non-var: compare with atom in ECX and return

    cmpl    %eax, %ecx
    ret

    .align  4

;;; _PROLOG_PAIR_SWITCH:
;;; tests if an argument can be unified with a pair

;;; Call:
;;; _prolog_pair_switch();

;;; Arguments:
;;; EAX (arg_reg_0) the argument for testing

;;; Results:
;;; if argument is an uninstantaited prologvar, then the dereferenced
;;; var is left on the stack. Otherwise none.

;;; Flags:
;;; A   (UGT) if argument is an uninstantiated prologvar
;;; E   (EQ)  if argument is a pair
;;; B   (ULT) if argument is anything else

;;; Usage:
;;; from sysPLOG_TERM_SWITCH, where the term in question is a pair. The
;;; subroutine call will be followed by an I_PLOG_TERM_SWITCH to jump on
;;; the flag setting.

DEF_C_LAB (_prolog_pair_switch)

L1.5:   ;;; Dereferencing loop:
    ;;; break out if argument is simple (and go straight to failure)

    testl   $1, %eax
    jnz L3.1

    ;;; Argument is compound:
    ;;; break out if not a prologvar

    cmpl    $C_LAB(prologvar_key), _KEY(%eax)
    jne L2.3

    ;;; Argument is a prologvar:
    ;;; dereference one link, then test for the end of the chain
    ;;; (var!PGV_CONT == var); loop if not

    movl    %eax, %esi
    movl    _PGV_CONT(%eax), %eax
    cmpl    %eax, %esi
    jne L1.5

    ;;; Uninstantiated prologvar: push it on the stack

    subl    $4, %USP
    movl    %eax, (%USP)

    ;;; Set flags to UGT and return

    testl   %eax, %eax  ;;; address of prologvar must be > 0
    ret

L2.3:   ;;; Argument is compound: test for a pair and return with flags
    ;;; set to EQ if so

    cmpl    $C_LAB(pair_key), _KEY(%eax)
    jne L3.1
    ret

L3.1:   ;;; Argument is neither pair nor prologvar:
    ;;; set flags to ULT (i.e. set the carry flag) and return

    stc
    ret

    .align  4

;;; _PROLOG_TERM_SWITCH:
;;; tests if an argument can be unified with a term of particular functor
;;; and arity

;;; Call:
;;; _prolog_term_switch();

;;; Arguments:
;;; EAX (arg_reg_0) the argument for testing
;;; ECX (arg_reg_1) the functor of the term
;;; EDX (arg_reg_2) the length of the term (arity+1) as a pop integer

;;; Results:
;;; if argument is an uninstantiated prologvar, then the dereferenced
;;; var is left on the stack. Otherwise none.

;;; Flags:
;;; A   (UGT) if argument is an uninstantiated prologvar
;;; E   (EQ)  if argument is a matching term
;;; B   (ULT) if argument is anything else

;;; Usage:
;;; from sysPLOG_TERM_SWITCH, where the term in question is a prologterm.
;;; The subroutine call will be followed by an I_PLOG_TERM_SWITCH to
;;; jump on the flag setting.

DEF_C_LAB (_prolog_term_switch)

L1.6:   ;;; Dereferencing loop:
    ;;; break out if argument is simple (and go straight to failure)

    testl   $1, %eax
    jnz L3.2

    ;;; Argument is compound:
    ;;; break out if not a prologvar

    cmpl    $C_LAB(prologvar_key), _KEY(%eax)
    jne L2.4

    ;;; Argument is a prologvar:
    ;;; dereference one link, then test for the end of the chain
    ;;; (var!PGV_CONT == var); loop if not

    movl    %eax, %esi
    movl    _PGV_CONT(%eax), %eax
    cmpl    %eax, %esi
    jne L1.6

    ;;; Uninstantiated prologvar: push it on the stack

    subl    $4, %USP
    movl    %eax, (%USP)

    ;;; Set flags to UGT and return

    testl   %eax, %eax  ;;; address of prologvar must be > 0
    ret

L2.4:   ;;; Argument is compound:
    ;;; test for prologterm with same functor and arity, and return
    ;;; with flags set to EQ if so

    cmpl    $C_LAB(prologterm_key), _KEY(%eax)
    jne L3.2
    cmpl    %ecx, _PGT_FUNCTOR(%eax)
    jne L3.2
    sarl    $2, %edx            ;;; convert length to sysint
    cmpl    %edx, _PGT_LENGTH(%eax)
    jne L3.2
    ret

L3.2:   ;;; Argument doesn't match: set carry flag (i.e. ULT) and return

    stc
    ret

    .align  4


;;; === ASSIGNING TO PROLOG VARIABLES =================================

;;; _PROLOG_ASSIGN:
;;; assign to a prolog variable and push the variable on the trail.

;;; Call:
;;; _prolog_assign(PROLOGVAR, ITEM);

;;; Register usage:
;;; EAX ITEM
;;; ECX PROLOGVAR
;;; EDI trail pointer

DEF_C_LAB (_prolog_assign)

    movl    (%USP), %eax
    movl    4(%USP), %ecx
    addl    $8, %USP

    ;;; Assign item to var!PGV_CONT

    movl    %eax, _PGV_CONT(%ecx)

    ;;; Push var on the trail

    movl    I_LAB(_plog_trail_sp), %edi
    movl    %ecx, (%edi)
    addl    $4, %edi
    movl    %edi, I_LAB(_plog_trail_sp)
    ret

    .align  4

;;; _PROLOG_ASSIGN_PAIR:
;;; optimised version of _prolog_assign(conspair())

;;; Call:
;;; _prolog_assign_pair(PROLOGVAR, FRONT, BACK);

;;; Register usage:
;;; EAX the free pair list, then a new pair taken from it
;;; ECX the prologvar
;;; EDI trail pointer

DEF_C_LAB (_prolog_assign_pair)

    ;;; Load free pair list to EAX
    ;;; If simple, there are no free pairs left so chain
    ;;; -Assign_pair- to allocate more store

    movl    I_LAB(Sys$- _free_pairs), %eax
    testl   $1, %eax
    jnz XC_LAB(Sys$-Plog$-Assign_pair)

    ;;; Otherwise, take the first pair from the free list

    movl    _P_BACK(%eax), %ecx
    movl    %ecx, I_LAB(Sys$- _free_pairs)

    ;;; Initialise the new pair with the values from the stack

    movl    (%USP), %ecx
    movl    %ecx, _P_BACK(%eax)
    movl    4(%USP), %ecx
    movl    %ecx, _P_FRONT(%eax)

    ;;; Assign the new pair to the prologvar

    movl    8(%USP), %ecx
    addl    $12, %USP
    movl    %eax, _PGV_CONT(%ecx)

    ;;; and push the var on the trail

    movl    I_LAB(_plog_trail_sp), %edi
    movl    %ecx, (%edi)
    addl    $4, %edi
    movl    %edi, I_LAB(_plog_trail_sp)
    ret

    .align  4


;;; === OPTIONAL OPTIMISATIONS ========================================
;;; (Replacing definitions from "plogcore.p" and "plogterms.p")

;;; _PROLOG_DEREF:
;;; dereference a chain of prolog variables

;;; Call:
;;; _prolog_deref(ITEM) -> DEREF'ED_ITEM

;;; Register usage:
;;; EAX the item
;;; ECX work (for comparing a variable with its contents)

DEF_C_LAB (_prolog_deref)

    ;;; Load item to EAX

    movl    (%USP), %eax

L1.9:   ;;; Start of dereferencing loop: quit if item is simple

    testl   $1, %eax
    jnz L2.5

    ;;; Item is compound: quit if not prologvar

    cmpl    $C_LAB(prologvar_key), _KEY(%eax)
    jne L2.5

    ;;; Item is prologvar:
    ;;; dereference one link, then test for end of chain
    ;;; (var!PGV_CONT == var)

    movl    %eax, %ecx
    movl    _PGV_CONT(%eax), %eax
    cmpl    %eax, %ecx
    jne L1.9

L2.5:   ;;; Finished -- return the dereferenced item

    movl    %eax, (%USP)
    ret

    .align  4

;;; _PROLOG_NEWVAR:
;;; returns a new prologvar from the free block. If there are none left,
;;; chains to pop New_var which allocates more store.

;;; Call:
;;; _prolog_newvar() -> PROLOGVAR

;;; Register usage:
;;; EAX next var in the free block

DEF_C_LAB (_prolog_newvar)

    ;;; Load the next var in the block to EAX

    movl    I_LAB(_plog_next_var), %eax

    ;;; If it's the end-of-block ref, go to the storage allocator

    cmpl    $C_LAB(ref_key), _KEY(%eax)
    je  XC_LAB(Sys$-Plog$-New_var)

    ;;; Otherwise it's a new variable:
    ;;; make it undef, and push it on the stack

    movl    %eax, _PGV_CONT(%eax)
    subl    $4, %USP
    movl    %eax, (%USP)

    ;;; Increment the -next_var- pointer and return

    addl    $_PGV_SIZE, %eax    ;;;  == @@(struct PLOGVAR)[_1]
    movl    %eax, I_LAB(_plog_next_var)
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
--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Changes for Linux and S*CO.
--- John Gibson, Oct 18 1994
    free*pairs -> _free_pairs
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- John Gibson, Dec  7 1989
    Added _PGV_SIZE
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- John Gibson, Apr  9 1989
    Prolog_newvar, Prolog_assign_pair renamed into section Sys$-Plog
 */
