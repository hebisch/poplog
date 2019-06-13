/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   Purpose: Assembly routines supporting process switching for ARM
   Author:  Waldek Hebisch
*/

#_<

#_INCLUDE 'declare.ph'
#_INCLUDE 'process.ph'

lconstant macro (

    USP                     = "r10",
    PB                      = "r11",
    SP                      = "sp",

    _PD_EXECUTE             = @@PD_EXECUTE,
    _PD_EXIT                = @@PD_EXIT,
    _PD_FLAGS               = @@PD_FLAGS,
    _PD_FRAME_LEN           = @@PD_FRAME_LEN,
    _PD_NLOCALS             = @@PD_NLOCALS,
    _PD_NUM_STK_VARS        = @@PD_NUM_STK_VARS,
    _PD_TABLE               = @@PD_TABLE,
    _PS_CALLSTACK_LIM       = @@PS_CALLSTACK_LIM,
    _PS_CALLSTACK_PARTIAL   = @@PS_CALLSTACK_PARTIAL,
    _PS_FLAGS               = @@PS_FLAGS,
    _PS_PARTIAL_RETURN      = @@PS_PARTIAL_RETURN,
    _PS_STATE               = @@PS_STATE,
    
    BRANCH_std              = '4',
);

lconstant macro MOVFL   = "ldrb";

>_#

    .arch armv5
    .file   "aprocess.s"
    .text

    /* Swap out (suspend) process
    */
DEF_C_LAB (_swap_out_callstack)
    ;;; load process from user stack
    ldr  r0, [USP]
    ldr  r3, [r0, #_PS_CALLSTACK_LIM]
    str  r3, [r0, #_PS_CALLSTACK_PARTIAL]
    b so_test_finished

so_loop:
    ;;; make lr relative
    sub   lr, lr, PB

    ;;; If needed run dlocal expressions
    ldrb r1, [PB, #_PD_FLAGS]
    tst  r1, #_:M_PD_PROC_DLEXPR_CODE
    bne  so_do_dlexpr

so_cont:
    ;;; Continue swap out after runnig dlocal expressions
    ;;; Stack pointer here still point to the stack frame

    MOVFL r1, [PB, #_PD_FRAME_LEN]
    sub   r3, r1, asl #2
    str   r3, [r0, #_PS_CALLSTACK_PARTIAL]

    str   lr, [r3], #4
    str   PB, [r3], #4
    add SP, SP, #4

    ;;; Save on-stack lvars into the process record

    MOVFL r0, [PB, #_PD_NUM_STK_VARS]
    cmp   r0, #0
    beq   do_save_dl
l_save_loop:
    ldr   r12, [SP], #4
    str   r12, [r3], #4
    subs  r0, #1
    bne   l_save_loop
do_save_dl:
    ;;; Save dynamic locals

    ;;;   test if any

    MOVFL r0, [PB, #_PD_NLOCALS]
    cmp   r0, #0
    beq   perm_reg_save

    add   r2, PB, r0, asl #2
    add   r2, r2, #(_PD_TABLE - 4)

dl_save_loop:
    ;;;      save idval in process record and restore previous from stack

    ldr   r1,  [r2], #-4
    ldr   r12, [r1]
    str   r12, [r3], #4
    ldr   r12, [SP], #4
    str   r12, [r1]

    subs  r0, #1
    bne   dl_save_loop

perm_reg_save:
    ;;; Save permanent registers to process struct, restore
    ;;; previous values from stack frame

    ;;; FIXME: implement

    ldr   lr, [SP], #4
    ldr   PB, [SP]
    
so_test_finished:
    ldr   r0, [USP]
    ldr   r1, [r0, #_PS_STATE]
    ldr   r3, [r0, #_PS_CALLSTACK_PARTIAL]
    cmp   r3, r1
    bhi   so_loop

    ;;; Finished, save flags and chain to procedure on user stack
    add   USP, USP, #4
    mov   r1, #0
    str   r1, [r0, #_PS_CALLSTACK_PARTIAL]
    strh  r1, [r0, #_PS_FLAGS]
    ldr   r0, [USP], #4
    ldr   pc, [r0, #_PD_EXECUTE]

so_do_dlexpr:
    str   lr, [r0, #_PS_PARTIAL_RETURN]
    ;;; Jump to suspend code
    ldr   r0, [PB, #_PD_EXIT]
    sub   pc, r0, #(BRANCH_std << 1)

DEF_C_LAB (_swap_out_continue)
    ldr   r0, [USP]
    ldr   r3, [r0, #_PS_CALLSTACK_PARTIAL]
    ldr   lr, [r0, #_PS_PARTIAL_RETURN]
    b so_cont

    ;;; Swap in (resume) process
DEF_C_LAB (_swap_in_callstack)
    ;;; load process from user stack
    ldr   r0, [USP]
    ldr   r3, [r0, #_PS_STATE]
    str   r3, [r0, #_PS_CALLSTACK_PARTIAL]
    b si_test_finished

si_loop:
    str   lr, [SP, #-4]!
    ldr   PB, [r3, #4]
    MOVFL r1, [PB, #_PD_FRAME_LEN]
    add   r3, r1, asl #2
    str   r3, [r0, #_PS_CALLSTACK_PARTIAL]
    sub   r3, #4
    
    ;;; Restore registers from process record and store values
    ;;; to stack

    ;;; FIXME: implement

    ;;; Restore dynamic locals

    ;;;   test if any

    MOVFL r0, [PB, #_PD_NLOCALS]
    cmp   r0, #0
    beq   do_restore_lvars

    add   r2, PB, #_PD_TABLE

dl_restore_loop:
    ;;;      save idval in stack and restore previous from process record
    ldr   r1,  [r2], #4
    ldr   r12, [r1]
    str   r12, [SP, #-4]!
    ldr   r12, [r3], #-4
    str   r12, [r1]

    subs  r0, #1
    bne   dl_restore_loop

do_restore_lvars:
    ;;; Restore on-stack lvars from the process record
    MOVFL r0, [PB, #_PD_NUM_STK_VARS]
    cmp   r0, #0
    beq   si_lvars_done
l_restore_loop:
    ldr   r12, [r3], #-4
    str   r12, [SP, #-4]!
    subs  r0, #1
    bne   l_restore_loop

si_lvars_done:
    str   PB, [SP, #-4]!
    ldr   lr, [r3, #-4]

    ldr   r0, [USP]

    ;;; If needed run dlocal expressions
    ldrb  r1, [PB, #_PD_FLAGS]
    tst   r1, #_:M_PD_PROC_DLEXPR_CODE
    bne   si_do_dlexpr

si_cont:
    add   lr, lr, PB
    
si_test_finished:
    ldr   r0, [USP]
    ldr   r1, [r0, #_PS_CALLSTACK_LIM]
    ldr   r3, [r0, #_PS_CALLSTACK_PARTIAL]
    cmp   r3, r1
    bcc   si_loop
    ;;; Finished, chain to procedure on user stack
    add   USP, USP, #4
    mov   r1, #0
    str   r1, [r0, #_PS_CALLSTACK_PARTIAL]
    ldr   r0, [USP], #4
    ldr   pc, [r0, #_PD_EXECUTE]

si_do_dlexpr:
    str   lr, [r0, #_PS_PARTIAL_RETURN]
    ;;; Jump to resume code
    ldr   r0, [PB, #_PD_EXIT]
    sub   pc, r0, #BRANCH_std

 
    ;;; Continue swap in after runnig procedure init code
DEF_C_LAB (_swap_in_continue)
    ldr   r0, [USP]
    ldr   r3, [r0, #_PS_CALLSTACK_PARTIAL]
    ldr   lr, [r0, #_PS_PARTIAL_RETURN]
    b si_cont

usrhi_lab:
    .word I_LAB(_userhi)
DEF_C_LAB (_ussave)
    b C_LAB (_ussave)

DEF_C_LAB (_usrestore)
    ldr   r0, [USP, #4]
    ldr   r1, [USP], #8
    cmp   r0, #0
    bxeq  lr
    ldr   r2, usrhi_lab
    mov   r3, USP
    ldr   r2, [r2]
    sub   USP, USP, r0
    cmp   r2, r3
    mov   r0, USP
    beq   usr_loop2
usr_loop1:
    ldr   r12, [r3], #4
    str   r12, [r0], #4
    cmp   r3, r2
    bne   usr_loop1
usr_loop2:
    ldr   r12, [r1], #4
    str   r12, [r0], #4
    cmp   r0, r2
    bne   usr_loop2
    bx lr

DEF_C_LAB (_userasund)
    b C_LAB (_userasund)
