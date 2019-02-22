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

    USP         = "rbx",

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
    .quad   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .quad   Ldata_size, C_LAB(Sys$-objmod_pad_key)
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

    movq    $C_LAB(_special_var_block), %rsi

    ;;; Save next_free_var pointer

    movq    _SVB_OFFS(_plog_next_var)(%rsi), %rax
    movq    %rax, _SF_PLGSV_NEXT_VAR+8(%rsp)

    ;;; Save trail end pointer, relative to trail barrier

    movq    _SVB_OFFS(_plog_trail_sp)(%rsi), %rax
    movq    %rax, %rcx              ;;; wanted later
    subq    _SVB_OFFS(_plog_trail_barrier)(%rsi), %rax
    movq    %rax, _SF_PLGSV_TRAIL_SP+8(%rsp)

    ;;; Save continuation stack top and pointer

    movq    _SVB_OFFS(_plog_contn_sp)(%rsi), %rax
    movq    %rax, _SVB_OFFS(_plog_save_contn_sp)(%rsi)
    movq    _SVB_OFFS(_plog_contn_top)(%rsi), %rax
    movq    %rax, _SF_PLGSV_CONTN_TOP+8(%rsp)

    ;;; Check for trail overflow

    cmpq    %rcx, _SVB_OFFS(_plog_trail_lim)(%rsi)
    jbe C_LAB(_checkplogall)

    ;;; Check system stack overflow

    cmpq    %rsp, _SVB_OFFS(_call_stack_lim)(%rsi)
    ja  C_LAB(_checkplogall)

    ;;; Check userstack overflow

    cmpq    %USP, _SVB_OFFS(_userlim)(%rsi)
    ja  C_LAB(_checkplogall)

    ;;; Test interrupt bit

    testq   $1, _SVB_OFFS(_trap)(%rsi)
    jnz C_LAB(_checkplogall)
    ret

    .align  16

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

    movq    $C_LAB(_special_var_block), %rdi

    ;;; Restore next_free_var pointer

    movq    _SF_PLGSV_NEXT_VAR+8(%rsp), %rax
    movq    %rax, _SVB_OFFS(_plog_next_var)(%rdi)

    ;;; Restore continuation stack top and pointer

    movq    _SVB_OFFS(_plog_save_contn_sp)(%rdi), %rax
    movq    %rax, _SVB_OFFS(_plog_contn_sp)(%rdi)
    movq    _SF_PLGSV_CONTN_TOP+8(%rsp), %rax
    movq    %rax, _SVB_OFFS(_plog_contn_top)(%rdi)

    ;;; Compare current trail end pointer with the saved value
    ;;; (saved relative to trail barrier)

    movq    _SF_PLGSV_TRAIL_SP+8(%rsp), %rsi
    addq    _SVB_OFFS(_plog_trail_barrier)(%rdi), %rsi
    movq    _SVB_OFFS(_plog_trail_sp)(%rdi), %rcx
    cmpq    %rcx, %rsi
    je  L2.1

    ;;; Restore the trail end pointer ...

    movq    %rsi, _SVB_OFFS(_plog_trail_sp)(%rdi)

    ;;; ... and uninstantiate the variables on it

    cld
L1.3:   slodq               ;;; plogvar in EAX
    movq    %rax, _PGV_CONT(%rax)   ;;; assigned to its own PGV_CONT
    cmpq    %rcx, %rsi
    jne L1.3

L2.1:   ret

    .align  16


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

    testq   $1, %rax
    jnz L2.2

    ;;; Item is compound:
    ;;; break out unless item is plogvar

    cmpq    $C_LAB(prologvar_key), _KEY(%rax)
    jne L2.2

    ;;; Dereference one link of plogvar:
    ;;; loop unless (var!PGV_CONT) == var

    movq    %rax, %rsi
    movq    _PGV_CONT(%rax), %rax
    cmpq    %rax, %rsi
    jne L1.4

    ;;; Argument is an uninstantiated plogvar:
    ;;; assign the atom in ECX to it and push it on the trail

    movq    %rcx, _PGV_CONT(%rax)
    movq    I_LAB(_plog_trail_sp), %rdi
    movq    %rax, (%rdi)
    addq    $8, %rdi
    movq    %rdi, I_LAB(_plog_trail_sp)

    ;;; Set flags to equal to indicate success and return

    cmpq    %rax, %rax
    ret

L2.2:   ;;; Item in EAX is non-var: compare with atom in ECX and return

    cmpq    %rax, %rcx
    ret

    .align  16

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

    testq   $1, %rax
    jnz L3.1

    ;;; Argument is compound:
    ;;; break out if not a prologvar

    cmpq    $C_LAB(prologvar_key), _KEY(%rax)
    jne L2.3

    ;;; Argument is a prologvar:
    ;;; dereference one link, then test for the end of the chain
    ;;; (var!PGV_CONT == var); loop if not

    movq    %rax, %rsi
    movq    _PGV_CONT(%rax), %rax
    cmpq    %rax, %rsi
    jne L1.5

    ;;; Uninstantiated prologvar: push it on the stack

    subq    $8, %USP
    movq    %rax, (%USP)

    ;;; Set flags to UGT and return

    testq   %rax, %rax  ;;; address of prologvar must be > 0
    ret

L2.3:   ;;; Argument is compound: test for a pair and return with flags
    ;;; set to EQ if so

    cmpq    $C_LAB(pair_key), _KEY(%rax)
    jne L3.1
    ret

L3.1:   ;;; Argument is neither pair nor prologvar:
    ;;; set flags to ULT (i.e. set the carry flag) and return

    stc
    ret

    .align  16

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

    testq   $1, %rax
    jnz L3.2

    ;;; Argument is compound:
    ;;; break out if not a prologvar

    cmpq    $C_LAB(prologvar_key), _KEY(%rax)
    jne L2.4

    ;;; Argument is a prologvar:
    ;;; dereference one link, then test for the end of the chain
    ;;; (var!PGV_CONT == var); loop if not

    movq    %rax, %rsi
    movq    _PGV_CONT(%rax), %rax
    cmpq    %rax, %rsi
    jne L1.6

    ;;; Uninstantiated prologvar: push it on the stack

    subq    $8, %USP
    movq    %rax, (%USP)

    ;;; Set flags to UGT and return

    testq   %rax, %rax  ;;; address of prologvar must be > 0
    ret

L2.4:   ;;; Argument is compound:
    ;;; test for prologterm with same functor and arity, and return
    ;;; with flags set to EQ if so

    cmpq    $C_LAB(prologterm_key), _KEY(%rax)
    jne L3.2
    cmpq    %rcx, _PGT_FUNCTOR(%rax)
    jne L3.2
    sarq    $2, %rdx            ;;; convert length to sysint
    cmpq    %rdx, _PGT_LENGTH(%rax)
    jne L3.2
    ret

L3.2:   ;;; Argument doesn't match: set carry flag (i.e. ULT) and return

    stc
    ret

    .align  16


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

    movq    (%USP), %rax
    movq    8(%USP), %rcx
    addq    $16, %USP

    ;;; Assign item to var!PGV_CONT

    movq    %rax, _PGV_CONT(%rcx)

    ;;; Push var on the trail

    movq    I_LAB(_plog_trail_sp), %rdi
    movq    %rcx, (%rdi)
    addq    $8, %rdi
    movq    %rdi, I_LAB(_plog_trail_sp)
    ret

    .align  16

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

    movq    I_LAB(Sys$- _free_pairs), %rax
    testq   $1, %rax
    jnz XC_LAB(Sys$-Plog$-Assign_pair)

    ;;; Otherwise, take the first pair from the free list

    movq    _P_BACK(%rax), %rcx
    movq    %rcx, I_LAB(Sys$- _free_pairs)

    ;;; Initialise the new pair with the values from the stack

    movq    (%USP), %rcx
    movq    %rcx, _P_BACK(%rax)
    movq    8(%USP), %rcx
    movq    %rcx, _P_FRONT(%rax)

    ;;; Assign the new pair to the prologvar

    movq    16(%USP), %rcx
    addq    $24, %USP
    movq    %rax, _PGV_CONT(%rcx)

    ;;; and push the var on the trail

    movq    I_LAB(_plog_trail_sp), %rdi
    movq    %rcx, (%rdi)
    addq    $8, %rdi
    movq    %rdi, I_LAB(_plog_trail_sp)
    ret

    .align  16


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

    movq    (%USP), %rax

L1.9:   ;;; Start of dereferencing loop: quit if item is simple

    testq   $1, %rax
    jnz L2.5

    ;;; Item is compound: quit if not prologvar

    cmpq    $C_LAB(prologvar_key), _KEY(%rax)
    jne L2.5

    ;;; Item is prologvar:
    ;;; dereference one link, then test for end of chain
    ;;; (var!PGV_CONT == var)

    movq    %rax, %rcx
    movq    _PGV_CONT(%rax), %rax
    cmpq    %rax, %rcx
    jne L1.9

L2.5:   ;;; Finished -- return the dereferenced item

    movq    %rax, (%USP)
    ret

    .align  16

;;; _PROLOG_NEWVAR:
;;; returns a new prologvar from the free block. If there are none left,
;;; chains to pop New_var which allocates more store.

;;; Call:
;;; _prolog_newvar() -> PROLOGVAR

;;; Register usage:
;;; EAX next var in the free block

DEF_C_LAB (_prolog_newvar)

    ;;; Load the next var in the block to EAX

    movq    I_LAB(_plog_next_var), %rax

    ;;; If it's the end-of-block ref, go to the storage allocator

    cmpq    $C_LAB(ref_key), _KEY(%rax)
    je  XC_LAB(Sys$-Plog$-New_var)

    ;;; Otherwise it's a new variable:
    ;;; make it undef, and push it on the stack

    movq    %rax, _PGV_CONT(%rax)
    subq    $8, %USP
    movq    %rax, (%USP)

    ;;; Increment the -next_var- pointer and return

    addq    $_PGV_SIZE, %rax    ;;;  == @@(struct PLOGVAR)[_1]
    movq    %rax, I_LAB(_plog_next_var)
    ret

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
