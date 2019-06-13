/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Misc assembly routines for ARM
   Author:  Waldek Hebisch
*/

#_<

#_INCLUDE 'declare.ph'

vars
    _call_stack_lim, _plog_trail_sp, _plog_trail_lim,
    ;

lconstant macro (
    USP             = "r10",
    PB              = "r11",
    LR              = "lr",
    W0              = "r0",
    W1              = "r1",
    CHAIN_REG       = "r2",
    _K_APPLY        = @@K_APPLY,
    _KEY            = @@KEY,
    _P_BACK         = @@P_BACK,
    _P_FRONT        = @@P_FRONT,
    _PD_EXECUTE     = @@PD_EXECUTE,
    _PD_EXIT        = @@PD_EXIT,
    _PD_FRAME_LEN   = @@PD_FRAME_LEN,
    _PD_UPDATER     = @@PD_UPDATER,
    _RF_CONT        = @@RF_CONT,
    _SF_OWNER       = @@SF_OWNER,
    _V_BYTES        = @@V_BYTES,
    
);

section $-Sys;

constant
        procedure (Async_raise_signal, Call_overflow, Conspair,
                   Plog$-Area_overflow, User_overflow, 
                   dummy_procedure_callback_helper),
;

endsection;

>_#

    .arch armv5
    .file   "amisc.s"

;;; Wrapping in POP object
   .text
   .word   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

;;; _popenter: calling (applying) Pop object
.L1.p_key:
    .word C_LAB(procedure_key)
.L1.i_key:
    .word C_LAB(integer_key) + _K_APPLY
.L1.d_key:
    .word C_LAB(weakref decimal_key) + _K_APPLY
    ;;; r0 constains called object
DEF_C_LAB (_popenter)
    ldr r1, .L1.p_key
    tst r0, #1
    ldreq r3, [r0, #_KEY]
    bne .L2.simple
    cmp r1, r3
    bne .L2.struct
    ldr pc, [r0, #_PD_EXECUTE]
    ;;; simple object
.L2.simple:
    str r0, [USP, #-4]!
    tst r0, #2
    beq .L2.dec
    ;;; integer
    ldr r0, .L1.i_key
    ldr r0, [r0]
    ldr r0, [r0, #_RF_CONT]
    ldr pc, [r0, #_PD_EXECUTE]
    ;;; decimal
.L2.dec:
    ldr r0, .L1.d_key
    ldr r0, [r0]
    ldr r0, [r0, #_RF_CONT]
    ldr pc, [r0, #_PD_EXECUTE]
    ;;; composite, but not a procedure
.L2.struct:
    str r0, [USP, #-4]!
    ldr r0, [r0, #_KEY]
    ldr r0, [r0, #_K_APPLY]
    ldr r0, [r0, #_RF_CONT]
    ldr pc, [r0, #_PD_EXECUTE]
;;; _popuenter: calling (applying) updater of Pop object
;;; r0 contains the object
DEF_C_LAB (_popuenter)
    ldr r1, .L1.p_key
    tst r0, #1
    ldreq r3, [r0, #_KEY]
    bne .L3.simple
    cmp r1, r3
    bne .L3.struct
    ;;; fall through
;;; Unchecked entry into updater, assumes object in r0 is a procedure,
;;; but still checks for existence of updater
DEF_C_LAB (_popuncenter)
    mov r3, r0
    ldr r0, [r0, #_PD_UPDATER]
    ldr r1, .L.false
    cmp r0, r1
    ldrne pc, [r0, #_PD_EXECUTE]
    str r3, [USP, #-4]!
    ldr pc, .L3.nonpd
.L3.nonpd:
    .word XC_LAB(-> Sys$-Exec_nonpd)
    ;;; simple object
.L3.simple:
    str r0, [USP, #-4]!
    tst r0, #2
    beq .L3.dec
    ;;; integer
    ldr r0, .L1.i_key
    ldr r0, [r0]
    b .L3.call_upd
    ;;; decimal
.L3.dec:
    ldr r0, .L1.d_key
    ldr r0, [r0]
    b .L3.call_upd
    ;;; composite, but not a procedure
.L3.struct:
    str r0, [USP, #-4]!
    ldr r0, [r0, #_KEY]
    ldr r0, [r0, #_K_APPLY]
.L3.call_upd:
    ldr r0, [r0, #_RF_CONT]
    ldr r0, [r0, #_PD_UPDATER]
    ldr r1, .L.false
    cmp r0, r1
    ldrne pc, [r0, #_PD_EXECUTE]
    ldr pc, .L3.nonpd

;;; _erase_sp_1: erase 1 word from call stack, used for error
;;; recovery
DEF_C_LAB (_erase_sp_1)
    add sp, sp, #4
    bx lr

DEF_C_LAB (_nextframe)
    ldr r0, [USP], #4
    ldr r1, [r0, #_SF_OWNER]
    ldrb r1, [r1, #_PD_FRAME_LEN]
    mov r1, r1, asl #2
    add r0, r0, r1
    str r0, [USP, #-4]!
    bx lr

DEF_C_LAB (_unwind_frame)
    ldrb r0, [PB, #_PD_FRAME_LEN]
    mov r0, r0, asl #2
    add r0, sp, r0
    ldr CHAIN_REG, [r0, #-4]
    str lr, [r0, #-4]
    ldr pc, [PB, #_PD_EXIT]

DEF_C_LAB (_syschain_caller)
    bl C_LAB (_unwind_frame)
    ;;; Fall trough
DEF_C_LAB (_syschain)
    ldr r0, [USP], #4
    mov LR, CHAIN_REG
    b C_LAB(_popenter)

DEF_C_LAB (_sysncchain_caller)
    b C_LAB (_sysncchain_caller)
    bl C_LAB (_unwind_frame)
    ;;; Fall trough
DEF_C_LAB (_sysncchain)
    ldr r0, [USP], #4
    mov LR, CHAIN_REG
    ldr pc, [r0, #_PD_EXECUTE]
    ;;; ldr r3, [r0, #_PD_EXECUTE]
    ;;; bx r3

DEF_C_LAB (_iscompound)
    b C_LAB (_iscompound)
    ldr r0, [USP]
    tst   r0, #1
    ldreq r0, .L.true
    ldrne r0, .L.false
    str r0, [USP]
    bx lr

DEF_C_LAB (_issimple)
    ldr r0, [USP]
    tst   r0, #1
    ldrne r0, .L.true
    ldreq r0, .L.false
    str r0, [USP]
    bx lr

DEF_C_LAB (_isinteger)
    ldr r0, [USP], #4
    tst   r0, #2
    ldrne r0, .L.true
    ldreq r0, .L.false
    str r0, [USP, #-4]!
    bx lr

DEF_C_LAB (_neg)
    b C_LAB (_neg)
    ldr     W0, [USP]
    cmp     W0, #0
    ldrlt   W0, .L.true
    ldrge   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB (_zero)
    b C_LAB (_zero)
    ldr    W0, [USP]
    cmp    W0, #0
    ldreq   W0, .L.true
    ldrne   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB (_not)
    ldr     W0, .L.false
    ldr     W1, [USP]
    cmp     W1, W0
    ldreq   W0, .L.true
    str     W0, [USP]
    bx      LR

DEF_C_LAB 7 (_eq)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldreq   W0, .L.true
    ldrne   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 7 (_neq)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrne   W0, .L.true
    ldreq   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_gr)
    b C_LAB (_gr)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrhi   W0, .L.true
    ldrls   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_greq)
    b C_LAB (_greq)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrcs   W0, .L.true
    ldrcc   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_lt)
    b C_LAB (_lt)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrcc   W0, .L.true
    ldrcs   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_lteq)
    b C_LAB (_lteq)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrls   W0, .L.true
    ldrhi   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_sgr)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrgt   W0, .L.true
    ldrle   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_sgreq)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrge   W0, .L.true
    ldrlt   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 6 (_slt)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrlt   W0, .L.true
    ldrge   W0, .L.false
    str     W0, [USP]
    bx      LR

.L.false:
    .word C_LAB(false)
.L.true:
    .word C_LAB(true)

DEF_C_LAB 6 (_slteq)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    cmp     W1, W0
    ldrle   W0, .L.true
    ldrgt   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB 4 (_bitst)
    b C_LAB (_bitst)
    ldr     W0, [USP], #4
    ldr     W1, [USP]
    tst     W1, W0
    ldrne   W0, .L.true
    ldreq   W0, .L.false
    str     W0, [USP]
    bx      LR

DEF_C_LAB (_haskey)
    ldr r0, [USP, #4]
    ldr r1, [USP], #4
    tst r0, #1
    bne .L6.1
    ldr r0, [r0, #_KEY]
    cmp r0, r1
    ldreq r0, .L.true
.L6.1:
    ldrne r0, .L.false
    str r0, [USP]
    bx lr
 
.L7.1:
    .word C_LAB(integer_key)
.L7.2:
    .word C_LAB(weakref decimal_key)
DEF_C_LAB (_datakey)
    ldr r0, [USP], #4
    tst r0, #1
    bne .L7.3
    ldr r0, [r0, #_KEY]
    str r0, [USP, #-4]!
    bx lr
.L7.3:
    tst r0, #2
    ldrne r0, .L7.1
    ldreq r0, .L7.2
    str r0, [USP, #-4]!
    bx lr
    
.L8.1:
    .word I_LAB(Sys$- _free_pairs)
DEF_C_LAB (_conspair)
    ldr r0, .L8.1
    ldr r1, [r0]
    tst r1, #1
    bne XC_LAB(Sys$-Conspair)
    ldr r2, [r1, #_P_BACK]
    str r2, [r0]
    ldr r0, [USP], #4
    str r0, [r1, #_P_BACK]
    ldr r0, [USP]
    str r0, [r1, #_P_FRONT]
    str r1, [USP]
    bx lr

DEF_C_LAB (_subss)
    ldr r0, [USP], #4
    ldr r1, [USP]
    add r0, #_V_BYTES-1
    ldrb r0, [r0, r1, asr #2]
    mov r0, r0, asl #2
    orr r0, #3
    str r0, [USP]
    bx lr

DEF_C_LAB (-> _subss)
DEF_C_LAB (_u_subss)
    ldr r0, [USP], #4
    ldr r1, [USP], #4
    ldr r2, [USP], #4
    add r0, #_V_BYTES-1
    mov r2, r2, asr #2
    strb r2, [r0, r1, asr #2]
    bx lr

DEF_C_LAB (_locc)
    ldr r12, [USP], #4
    ldr r1, [USP], #4
    ldr r2, [USP]
    cmp r1, #0
    beq .L10.not_found
    mov r0, #0
.L10.loop:
    ldrb r3, [r2, r0]
    cmp r3, r12
    beq .L10.found
    add r0, r0, #1
    cmp r0, r1
    bne .L10.loop
.L10.not_found:
    mov r0, #-1
.L10.found:
    str r0, [USP]
    bx LR

DEF_C_LAB (_skpc)
    ldr r12, [USP], #4
    ldr r1, [USP], #4
    ldr r2, [USP]
    cmp r1, #0
    beq .L19.not_found
    mov r0, #0
.L19.loop:
    ldrb r3, [r2, r0]
    cmp r3, r12
    bne .L19.found
    add r0, r0, #1
    cmp r0, r1
    bne .L19.loop
.L19.not_found:
    mov r0, #-1
.L19.found:
    str r0, [USP]
    bx LR

;;; FIXME: really implement
DEF_C_LAB (_checkplogall)
    ldr r0, .L11.special
    ldr r1, [r0, #_SVB_OFFS(_plog_trail_sp)]
    ldr r2, [r0, #_SVB_OFFS(_plog_trail_lim)]
    cmp r1, r2
    bhi C_LAB (_checkall)
    b XC_LAB(weakref[prologvar_key] Sys$-Plog$-Area_overflow)

DEF_C_LAB (_checkall)
    ldr r0, .L11.special
.L11.do_checkall:
    ldr r1, [r0, #_SVB_OFFS(_call_stack_lim)]
    cmp sp, r1
    bcc .L11.do_call_overflow
.L11.check_user:
    ldr r1, [r0, #_SVB_OFFS(_userlim)]
    cmp USP, r1
    bcc .L11.do_user_overflow
.L11.check_interrupt:
    ldr r1, [r0, #_SVB_OFFS(_trap)]
    tst r1, #1
    bxeq LR
    ldr r1, .L11.disable
    ldr r1, [r1]
    tst r1, #1
    beq XC_LAB(Sys$-Async_raise_signal)
    bx  LR

.L11.do_call_overflow:
    ldr r1, .L11.disable
    ldr r1, [r1]
    tst r1, #2
    beq XC_LAB(Sys$-Call_overflow)
    b .L11.check_user

.L11.do_user_overflow:
    ldr r1, .L11.disable
    ldr r1, [r1]
    tst r1, #2
    beq XC_LAB(Sys$-User_overflow)
    b .L11.check_interrupt
    
DEF_C_LAB (_checkinterrupt)
    ldr  r1, .L11.trap
    ldr  r1, [r1]
    tst  r1, #1
    bxeq  LR
    b C_LAB (_checkall)

.L11.special:
    .word C_LAB(_special_var_block)
.L11.disable:
    .word I_LAB(_disable)
.L11.trap:
    .word I_LAB(_trap)
.L11.dummy_procedure_helper:
    .word XC_LAB(Sys$-dummy_procedure_callback_helper)
.L11.mess:
    .ascii "0x%x\\n\\000"
    .align 2
.L11.messp:
     .word .L11.mess
EXTERN_NAME(pop_print):
    stmfd sp!, {r4, r5, PB, LR}
    ldr PB, .L11.dummy_procedure_helper
    str PB, [sp, #-4]!
    ;;; ldr r0, .L11.messp
    ;;; mov r1, r10
    ;;; bl printf
    str r0, [USP, #-4]!
    bl  XC_LAB(sys_syspr)
    add sp, sp, #4
    ldmfd sp!, {r4, r5, PB, pc}

;;; End wrapper: set size
    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
