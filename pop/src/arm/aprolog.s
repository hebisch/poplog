/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Prolog support assembly routines for ARM
   Author:  Waldek Hebisch
*/

#_<

#_INCLUDE 'declare.ph'

constant procedure (
    $-Sys$-Plog$-Assign_pair, Sys$-Plog$-New_var,
    ),
;

vars
    _plog_contn_sp, _plog_contn_top,  _plog_next_var, _plog_save_contn_sp,
     _plog_trail_barrier, _plog_trail_sp,
     ;

lconstant macro (
    USP   = "r10",
    LR    = "lr",
    SP    = "sp",
    _KEY            = @@KEY,
    _PGT_FUNCTOR    = @@PGT_FUNCTOR,
    _PGT_LENGTH     = @@PGT_LENGTH,
    _PGV_CONT       = @@PGV_CONT,
    _PGV_SIZE       = @@(struct PLOGVAR)++,
    _P_BACK         = @@P_BACK,
    _P_FRONT        = @@P_FRONT,
    _SF_PLGSV_NEXT_VAR  = @@SF_PLGSV_NEXT_VAR,
    _SF_PLGSV_CONTN_TOP = @@SF_PLGSV_CONTN_TOP,
    _SF_PLGSV_TRAIL_SP  = @@SF_PLGSV_TRAIL_SP,
);

>_#

    .arch armv5
    .file   "aprolog.s"
    .text

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

.L11.special:
    .word C_LAB(_special_var_block)

DEF_C_LAB (_prolog_save_check)
    ldr  r0, .L11.special
    ldr  r1, [r0, #_SVB_OFFS(_plog_next_var)]
    str  r1, [SP, #_SF_PLGSV_NEXT_VAR]
    ldr  r1, [r0, #_SVB_OFFS(_plog_trail_sp)]
    ldr  r2, [r0, #_SVB_OFFS(_plog_trail_barrier)]
    sub  r1, r1, r2
    str  r1, [SP, #_SF_PLGSV_TRAIL_SP]

    ldr  r1, [r0, #_SVB_OFFS(_plog_contn_sp)]
    str  r1, [r0, #_SVB_OFFS(_plog_save_contn_sp)]
    ldr  r1, [r0, #_SVB_OFFS(_plog_contn_top)]
    str  r1, [SP, #_SF_PLGSV_CONTN_TOP]

    ;;; Check for overflow etc.
    b C_LAB (_checkplogall)

DEF_C_LAB (_prolog_restore)
    ldr  r0, .L11.special
    ldr  r1, [SP, #_SF_PLGSV_NEXT_VAR]
    str  r1, [r0, #_SVB_OFFS(_plog_next_var)]
    ldr  r1, [r0, #_SVB_OFFS(_plog_save_contn_sp)]
    str  r1, [r0, #_SVB_OFFS(_plog_contn_sp)]
    ldr  r1, [SP, #_SF_PLGSV_CONTN_TOP]
    str  r1, [r0, #_SVB_OFFS(_plog_contn_top)]

    ldr  r2, [r0, #_SVB_OFFS(_plog_trail_barrier)]
    ldr  r1, [SP, #_SF_PLGSV_TRAIL_SP]
    add  r1, r1, r2
    ldr  r2, [r0, #_SVB_OFFS(_plog_trail_sp)]
    cmp  r1, r2
    bxeq LR
    str  r1, [r0, #_SVB_OFFS(_plog_trail_sp)]
trail.loop:
    ldr  r3, [r1], #4
    str  r3, [r3, #_PGV_CONT]
    cmp  r1, r2
    bne  trail.loop
    bx   LR

DEF_C_LAB (_prolog_unify_atom)
deref.loop3:
    tst r0, #1
    bne notvar.ret
    ldr r3, [r0, #_KEY]
    ldr r12, plog.var_key
    cmp r3, r12
    bne notvar.ret
    mov r12, r0
    ldr r0, [r0, #_PGV_CONT]
    cmp r0, r12
    bne deref.loop3
    str r1, [r0, #_PGV_CONT]
    ldr r2, plog_trail_sp.lab
    ldr r3, [r2]
    str r0, [r3], #4
    str r3, [r2]
    ;;; Flags are still set to EQ
    bx  LR
notvar.ret:
    cmp r0, r1
    bx  LR

plog.pair_key:
    .word C_LAB(pair_key)

DEF_C_LAB (_prolog_pair_switch)
deref.loop2:
    tst r0, #1
    bne fail.ret
    ldr r3, [r0, #_KEY]
    ldr r12, plog.var_key
    cmp r3, r12
    bne deref.notvar2
    mov r12, r0
    ldr r0, [r0, #_PGV_CONT]
    cmp r0, r12
    bne deref.loop2
    str r0, [USP, #-4]!
    ;;; set flags: r0 is nonzero so we get HI (unsigned higher)
    cmp r0, #0
    bx  LR
deref.notvar2:
    ldr r12, plog.pair_key
    cmp r3, r12
    bne fail.ret
    bx LR

plog.term_key:
    .word C_LAB(prologterm_key)

DEF_C_LAB (_prolog_term_switch)
deref.loop1:
    tst r0, #1
    bne fail.ret
    ldr r3, [r0, #_KEY]
    ldr r12, plog.var_key
    cmp r3, r12
    bne deref.notvar
    mov r12, r0
    ldr r0, [r0, #_PGV_CONT]
    cmp r0, r12
    bne deref.loop1
    str r0, [USP, #-4]!
    ;;; set flags: r0 is nonzero so we get HI (unsigned higher)
    cmp r0, #0
    bx  LR
deref.notvar:
    ldr r12, plog.term_key
    cmp r3, r12
    bne fail.ret
    ldr r3, [r0, #_PGT_FUNCTOR]
    cmp r1, r3
    bne fail.ret
    ldr r3, [r0, #_PGT_LENGTH]
    cmp r3, r2, asr #2
    bxeq LR
fail.ret:
    ;;; set flags to CC (unsigned lower)
    mov r3, #0
    cmp r3, #1
    bx  LR

plog_trail_sp.lab:
    .word I_LAB(_plog_trail_sp)

DEF_C_LAB (_prolog_assign)
    ldr r0, [USP, #4]
    ldr r1, [USP], #8
    str r1, [r0, #_PGV_CONT]
    ;;; Put var on trail
    ldr r2, plog_trail_sp.lab
    ldr r3, [r2]
    str r0, [r3], #4
    str r3, [r2]
    bx  LR

free_pairs.lab:
    .word I_LAB(Sys$- _free_pairs)

DEF_C_LAB (_prolog_assign_pair)
    ldr r1, free_pairs.lab
    ldr r0, [r1]
    tst r0, #1
    bne XC_LAB(Sys$-Plog$-Assign_pair)
    ldr r2, [r0, #_P_BACK]
    str r2, [r1]
    ldr r2, [USP, #8]
    ldr r1, [USP, #4]
    ldr r3, [USP], #12
    str r3, [r0, #_P_BACK]
    str r1, [r0, #_P_FRONT]
    str r0, [r2, #_PGV_CONT]
    ;;; Put var on trail
    ldr r0, plog_trail_sp.lab
    ldr r3, [r0]
    str r2, [r3], #4
    str r3, [r0]
    bx  LR

plog.var_key:
    .word C_LAB(prologvar_key)

DEF_C_LAB (_prolog_deref)
    ldr r0, [USP]
deref.loop:
    tst r0, #1
    bne deref.ret
    ldr r1, [r0, #_KEY]
    ldr r2, plog.var_key
    cmp r1, r2
    bne deref.ret
    mov r3, r0
    ldr r0, [r0, #_PGV_CONT]
    cmp r0, r3
    bne deref.loop
deref.ret:
    str r0, [USP]
    bx  LR

ref_key.lab:
    .word C_LAB(ref_key)

next_var.lab:
    .word I_LAB(_plog_next_var)

DEF_C_LAB (_prolog_newvar)
    ldr r1, next_var.lab
    ldr r0, [r1]
    ldr r2, ref_key.lab
    ldr r3, [r0, #_KEY]
    cmp r2, r3
    beq XC_LAB(Sys$-Plog$-New_var)
    str r0, [r0, #_PGV_CONT]
    str r0, [USP, #-4]!
    add r0, #_PGV_SIZE
    str r0, [r1]
    bx  LR

    .align  3

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
