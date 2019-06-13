#_<

#_INCLUDE 'declare.ph'

section $-Sys;

constant procedure (Error_signal,)
;


vars
        Extern$- _saved_sp,
;

endsection;

lconstant macro (

        USP = "r10",    ;;; User stack pointer
        PB  = "r11",    ;;; Procedure base register

        SAVED_SP = [I_LAB(Sys$-Extern$- _saved_sp)],

);

>_#

        .file   "asignals.s"
        .arch armv5

        .text

L0.pop_in_extern:
    .word EXTERN_NAME(__pop_in_user_extern)
L0._userhi:
    .word I_LAB(_userhi)
    .globl  EXTERN_NAME(__pop_errsig)
EXTERN_NAME(__pop_errsig):

    ;;; reset_pop_environ
    ldr r0, L0.pop_in_extern
    mov r1, #0
    str r1, [r0]
    ldr r0, L0._userhi
    ldr USP, [r0]
    ;;; FIXME: handle FPE handler

    ldr r0, L0.saved_sp
    ldr r1, [r0]
    cmp r1, #0
    beq L0.done
    mov sp, r1
    ldr PB, [sp]
    ;;; never return
L0.done:
    bl    XC_LAB(Sys$-Error_signal)
    b L0.done

L0.saved_sp:
    .word SAVED_SP
DEF_C_LAB (_call_sys)
    ldr r1, L0.saved_sp
    str sp, [r1]
    stmfd sp!, {PB, lr}
    ;;; Get the system call address in r12 and the argument count in r9
    ldr r12, [USP], #4
    ldr r9, [USP], #4
    subs r7, r9, #4
    ble L1.2
    ;;; align call stack
    and r8, r7, #1
    sub sp, r8, asl #2
    ;;; Copy from user to system stack
L1.1:
    ldr r8, [USP], #4
    str r8, [sp, #-4]!
    subs r7, r7, #1
    bgt L1.1
    mov r9, #4
L1.2:
    ;;; Register arguments
    cmp r9, #3
    ldrgt r3, [USP], #4
    ldrge r2, [USP], #4
    cmp r9, #1
    ldrgt r1, [USP], #4
    ldrge r0, [USP], #4
    blx r12
    str r0, [USP, #-4]!
    ldr r1, L0.saved_sp
    mov r2, #0
    ldr r0, [r1]
    str r2, [r1]
    ;;; ldr PB, [r0]
    sub sp, r0, #8
    ldmfd sp!, {PB, lr}
    bx lr
