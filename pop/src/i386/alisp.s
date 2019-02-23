/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:        S.pcunix/src/alisp.s
 * Purpose:     Assembler support for Common Lisp on Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

constant    _setstklen_diff
    ;

lconstant macro (

    ;;; User stack pointer

    USP     = "ebx",

);

>_#

    .file   "alisp.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text


;;; _SETSTKLEN:
;;; adjusts the number of results returned by a Lisp function.
;;; The subroutine takes the length of the userstack before the function
;;; call and the number of results expected (both as popints).
;;; The actual stacklength is adjusted to fit either by erasing items
;;; or by pushing -nil-.

;;; Call:
;;; _setstklen(SAVED_STACKLENGTH, NRESULTS);

;;; Registers used:
;;; EAX desired stacklength
;;; ECX desired stack pointer

;;; Usage:
;;; implements the I_SETSTACKLENGTH instruction. Where NRESULTS is
;;; available to the run-time assembler, the code for SETSTKLEN is
;;; expanded inline and a jump made direct to SETSTKLEN_DIFF
;;; if necessary.

DEF_C_LAB (_setstklen)

    ;;; Compute desired stacklength (SAVED_STACKLENGTH + NRESULTS)
    ;;; in EAX. The two arguments are in popint words; subtracting
    ;;; 6 from the total converts to sysint bytes.

    movl    (%USP), %eax
    addl    4(%USP), %eax
    subl    $6, %eax
    addl    $8, %USP

    ;;; Compute the desired stack pointer (_userhi - desired_length)
    ;;; in ECX

    movl    I_LAB(_userhi), %ecx
    subl    %eax, %ecx

    ;;; Compare desired and actual stack pointers: if different,
    ;;; jump to fix

    cmpl    %ecx, %USP
    jne C_LAB(_setstklen_diff)
    ret

    .align  4

DEF_C_LAB (_setstklen_diff)

    ;;; Desired and actual stack pointers are different:
    ;;; push or pop results according to direction of difference

    jb  L2.1

L1.1:   ;;; Actual stack is too short: push nils until the desired size
    ;;; is reached

    subl    $4, %USP
    movl    $C_LAB(nil), (%USP)
    cmpl    %ecx, %USP
    jne L1.1
    ret

L2.1:   ;;; Actual stack is too long: erase the extra results simply by
    ;;; setting USP to the desired value

    movl    %ecx, %USP
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
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
 */
