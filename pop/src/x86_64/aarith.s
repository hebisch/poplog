/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/aarith.s
 * Purpose:     Arithmetic routines for Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

constant
    procedure Sys$-Array$-Sub_error
    ;

lconstant macro (

    USP             = "rbx",
    PB          = "rbp",

    _PD_EXECUTE         = @@PD_EXECUTE,
    _PD_ARRAY_TABLE     = @@PD_ARRAY_TABLE,
    _PD_ARRAY_VECTOR    = @@PD_ARRAY_VECTOR,
    _PD_ARRAY_SUBSCR_PDR    = @@PD_ARRAY_SUBSCR_PDR,

);

>_#

    .file   "aarith.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:

/**********************************************************************/


    .text

;;; === LOGICAL BIT ROUTINES ==========================================

;;; _BISET
;;; _BICLEAR
;;; _BIMASK
;;; _BIXOR
;;;     Bit operations on machine integers

;;; Call:
;;; _I _bitop _J -> _K

DEF_C_LAB 4 (_biset)

    movq    (%USP), %rax
    addq    $8, %USP
    orq %rax, (%USP)
    ret

    .align  16

DEF_C_LAB 4 (_biclear)

    movq    (%USP), %rax
    addq    $8, %USP
    notq    %rax
    andq    %rax, (%USP)
    ret

    .align  16

DEF_C_LAB 4 (_bimask)

    movq    (%USP), %rax
    addq    $8, %USP
    andq    %rax, (%USP)
    ret

    .align  16

DEF_C_LAB 4 (_bixor)

    movq    (%USP), %rax
    addq    $8, %USP
    xorq    %rax, (%USP)
    ret

    .align  16


;;; === MACHINE INTEGER ARITHMETIC ====================================

;;; _MULT
;;; Signed multiplication

;;; Call:
;;; _I _mult _J -> _K

DEF_C_LAB 4 (_mult)

    movq    (%USP), %rax
    addq    $8, %USP

    ;;; Multiply EAX by (%USP); result goes to EDX:EAX

    imulq   (%USP)

    ;;; Return low half of result

    movq    %rax, (%USP)
    ret

    .align  16

;;; _DIV
;;; Signed division

;;; Call:
;;; _I _div _J -> _QUOT -> _REM

DEF_C_LAB 4 (_div)

    ;;; Move the dividend into EAX and sign-extend into EDX:EAX

    movq    8(%USP), %rax
    movq    %rax, %rdx
    sarq    $63, %rdx

    ;;; Divide EDX:EAX by the divisor, (%USP),
    ;;; leaving the quotient in EAX and remainder in EDX

    idivq   (%USP)

    ;;; Stack the remainder, then the quotient

    movq    %rdx, 8(%USP)
    movq    %rax, (%USP)
    ret

    .align  16

;;; _DIVQ
;;; Signed division, quotient only

;;; Call:
;;; _I _divq _J -> _QUOT

DEF_C_LAB 2 (_divq)

    ;;; Move the dividend into EAX and sign-extend into EDX:EAX

    movq    8(%USP), %rax
    movq    %rax,%rdx
    sarq    $63,%rdx

    ;;; Divide EDX:EAX by the divisor, (%USP),
    ;;; leaving the quotient in EAX and remainder in EDX

    idivq   (%USP)

    ;;; Stack the quotient
    movq    %rax, 8(%USP)
    addq    $8, %USP
    ret

    .align  16

;;; _M_ASH
;;;     Shift a machine integer by a signed quantity.
;;;     This implements the M_ASH M-code instruction for a non-immediate
;;;     shift. It can't be called generally from POP because it expects
;;;     its arguments in registers rather than on the user stack.
;;;     (cf. -C_LAB(_shift)- below)

;;; Arguments:
;;; EAX integer to be shifted
;;; ECX amount of the shift -- positive means left shift,
;;;     negative means right shift
;;; Results:
;;; EAX the shifted word

DEF_C_LAB (_m_ash)

    ;;; Test the sign of the shift

    testq   %rcx, %rcx
    js  L1.1

    ;;; Sign is positive: shift left and return

    salq    %cl, %rax
    ret

L1.1:   ;;; Sign is negative: shift right by the absolute amount and return

    negl    %ecx
    sarq    %cl, %rax
    ret

    .align  16

;;; _SHIFT
;;;     POP interface to _M_ASH: arguments and results go via the user stack

;;; Call:
;;; _shift(_I, _J) -> _K

DEF_C_LAB (_shift)

    movq    (%USP), %rcx
    addq    $8, %USP
    movq    (%USP), %rax
    call    C_LAB(_m_ash)
    movq    %rax, (%USP)
    ret

    .align  16


;;; === POP INTEGER ARITHMETIC ========================================

;;; _PMULT
;;; POP integer multiply, with no overflow check (implements fi_*)

;;; Call:
;;; I _pmult J -> K

DEF_C_LAB 4 (_pmult)

    ;;; Pop the first popint into EAX, and make it a machine integer

    movq    (%USP), %rax
    addq    $8, %USP
    sarq    $2, %rax

    ;;; Copy the second operand into EDX and clear the popint bits

    movq    (%USP), %rdx
    subq    $3, %rdx

    ;;; Multiply EAX by EDX; result goes to EDX:EAX

    imulq   %rdx

    ;;; Return lower half of result (EAX) with popint bits restored

    addq    $3, %rax
    movq    %rax, (%USP)
    ret

    .align  16

;;; _PMULT_TESTOVF
;;; Multiply two simple pop integers with a test for overflow.
;;; Returns a result, plus <true> if OK or <false> if overflow.

;;; Call:
;;; _pmult_testovf(I, J) -> BOOL -> K

DEF_C_LAB (_pmult_testovf)

    ;;; Copy the first popint into EAX, and make it a machine integer

    movq    (%USP), %rax
    sarq    $2, %rax

    ;;; Copy the second operand into EDX and clear the popint bits

    movq    8(%USP), %rdx
    subq    $3, %rdx

    ;;; Multiply EAX by EDX; result goes to EDX:EAX
    ;;; Overflow flag will be set if EDX:EAX is not a sign-extension
    ;;; of EAX

    imulq   %rdx

    ;;; Reset the popint bits in the lower half of the result (EAX)
    ;;; and put on the stack; LEAL and MOVL won't change the flags

    leaq    3(%rax), %rax
    movq    %rax, 8(%USP)

    ;;; Return <true> for OK, <false> for overflow

    jo  L1.2
    movq    $C_LAB(true), (%USP)
    ret
L1.2:   movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PDIV:
;;; Divide two simple pop integers with no checking (implements fi_//)

;;; Call:
;;; I _pdiv J -> QUOT -> REM

DEF_C_LAB 4 (_pdiv)

    ;;; Copy the dividend into EAX, convert it to a machine integer
    ;;; and sign-extend through EDX:EAX

    movq    8(%USP), %rax
    sarq    $2, %rax

;;; XXXX @@@@@  cltd
    movq    %rax,%rdx
    sarq    $63,%rdx

    ;;; Copy the divisor to ECX and convert to a machine integer

    movq    (%USP), %rcx
    sarq    $2, %rcx

    ;;; Divide EDX:EAX by ECX, leaving the quotient in EAX and
    ;;; remainder in EDX

    idivq   %rcx

    ;;; Convert quotient and remainder back to popints and return

    leaq    3(, %rdx, 4), %rdx
    movq    %rdx, 8(%USP)
    leaq    3(, %rax, 4), %rax
    movq    %rax, (%USP)
    ret

    .align  16

;;; _PINT_TESTOVF:
;;; Convert machine integer to pop integer, checking for overflow.
;;; Returns a popint plus <true> if OK, or just <false> if overflow.

;;; Call:
;;; _pint_testovf(_I) -> <true> -> I
;;; _pint_testovf(_I) -> <false>

DEF_C_LAB (_pint_testovf)

    ;;; The conversion is just (_I << 2 + 3), but the shift has to be
    ;;; done in two 1-bit steps to allow the test

    movq    (%USP), %rax
    salq    $1, %rax
    jo  L1.3
    salq    $1, %rax
    jo  L1.3
    addq    $3, %rax
    movq    %rax, (%USP)
    subq    $8, %USP
    movq    $C_LAB(true), (%USP)
    ret
L1.3:   movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PSHIFT_TESTOVF:
;;; Left-shift a simple popint by a positive machine integer amount,
;;; and test for overflow. Returns a popint result plus <true> if OK,
;;; or just <false> for overflow.

;;; Call:
;;; _pshift_testovf(I, _N) -> <true> -> J
;;; _pshift_testovf(I, _N) -> <false>

DEF_C_LAB (_pshift_testovf)

    ;;; Copy the popint (I) to EAX and clear the popint bits.
    ;;; If it's zero, there's no need to shift.

    movq    8(%USP), %rax
    subq    $3, %rax
    je  L1.4

    ;;; Copy the shift amount (_N) to ECX. If it's > 61, then the shift
    ;;; will definitely overflow, so jump out.

    movq    (%USP), %rcx
    cmpq    $62, %rcx
    jg  L2.1

    ;;; The left shift (SALL) only sets the overflow flag for a shift
    ;;; of 1 bit, so to do the test we have to make a copy of the
    ;;; original number in ESI, do the shift, then do a right shift by
    ;;; the same amount and compare the result with the copied original.
    ;;; If they're not the same, the left shift must have overflowed.

    movq    %rax, %rsi
    salq    %cl, %rax
    movq    %rax, %rdx
    sarq    %cl, %rdx
    cmpq    %rdx, %rsi
    jne L2.1

    ;;; No overflow -- reset the popint bits in EAX, then return it
    ;;; with <true>

    addq    $3, %rax
    movq    %rax, 8(%USP)
L1.4:
    movq    $C_LAB(true), (%USP)
    ret

L2.1:   ;;; Overflow -- pop the stack and return <false>

    addq    $8, %USP
    movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; === BIGINTEGER ARITHMETIC =========================================


;;; Helper for random number generator
;;; This is really 32-bit code!
DEF_C_LAB (_posword_mul_high)
    movl    (%USP), %eax
    movl    8(%USP), %edx
    shll    $1, %eax
    addq    $8, %USP
    imull   %edx
    movq    %rdx, (%USP)
    ret

    .align  16


;;; Bignum routines
DEF_C_LAB (_bgi_add)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    32(%USP), %r8
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $40, %USP
    call    do_bgi_add
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_bgi_sub)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    32(%USP), %r8
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $40, %USP
    call    do_bgi_sub
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_bgi_negate)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $24, %USP
    call    do_bgi_negate
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_bgi_negate_no_ov)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $24, %USP
    call    do_bgi_negate_no_ov
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

;;; left shift
;;; Arguments:
;;;   destination is on top of the user stack
;;;   next is shift amount
;;;   next is lenght of biginit argument
;;;   next is biginit argument
DEF_C_LAB (_bgi_lshift)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $24, %USP
    call    do_bgi_lshift
    movq    %rax, (%USP)
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

;;; logical right shift
DEF_C_LAB (_bgi_rshiftl)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $32, %USP
    call    do_bgi_rshiftl
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

;;; Multiply unsigned biginit by unsigned machine integer (slice)
;;; Arguments:
;;;   destination is on top of the user stack
;;;   next is length of biginit argument
;;;   next is biginit argument
;;;   next is machine integer
DEF_C_LAB (_bgi_mult)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $32, %USP
    call    do_bgi_mult
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_bgi_mult_add)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $32, %USP
    call    do_bgi_mult_add
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

;;; Subtruct product from destination
DEF_C_LAB (_bgi_sub_mult)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $32, %USP
    call    do_bgi_sub_mult
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_bgi_div)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    24(%USP), %rcx
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $24, %USP
    call    do_bgi_div
    movq    %rax, (%USP)
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_quotient_estimate_init)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $16, %USP
    call    do_quotient_estimate_init
    movq    %rax, (%USP)
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

DEF_C_LAB (_quotient_estimate)
    pushq   %r8
    pushq   %r10
    pushq   %r11
    movq    16(%USP), %rdx
    movq    8(%USP), %rsi
    movq    (%USP), %rdi
    addq    $16, %USP
    call    do_quotient_estimate
    movq    %rax, (%USP)
    popq    %r11
    popq    %r10
    popq    %r8
    ret

    .align  16

;;; === COMPUTE ARRAY SUBSCRIPTS ======================================

;;; _ARRAY_SUB:
;;; computes the total subscript for a multi-dimensional array.

;;; Call:
;;; _array_sub(/* INDEXES */) -> _SUBSCRIPT;

;;; Arguments:
;;;     A set of indexes on the stack, one for each dimension. The array
;;;     procedure table contains additional parameters arranged as
;;;     follows:

;;;         offset          ;;; popint
;;;     dimension-1         ;;; sysint, base 0
;;;     lower_bound-1       ;;; popint
;;;     scale_factor-1      ;;; sysint
;;;     ...
;;;     dimension-n
;;;     lower_bound-n
;;;     scale_factor-n
;;;     0               ;;; marks the end

;;; Register usage:
;;; ECX accumulates the total subscript
;;; ESI pointer into procedure table
;;; EAX dimension; scale factor; scale factor * index
;;; EDX index

;;; Usage:
;;; called from Cons_array; parameters set up by Array$-Get

DEF_C_LAB (_array_sub)

    ;;; Initialise ESI to point at array parameters

    leaq    _PD_ARRAY_TABLE(%PB), %rsi
    cld

    ;;; Initialise ECX with subscript offset

    movq    (%rsi), %rcx
    addq    $8, %rsi

    ;;; Load first dimension to EAX: may be zero already for a
    ;;; 0-dimensional array

    slodq
    testq   %rax, %rax
    jz  L1.10

L1.8:   ;;; Start of loop:
    ;;; get the next index from the stack into EDX and check it's
    ;;; a pop integer

    movq    (%USP), %rdx
    addq    $8, %USP
    testq   $2, %rdx
    jz  array_sub_error

    ;;; Subtract the next lower bound from the index and check it for
    ;;; range against the dimension in EAX.
    ;;; Both index and lower bound are popints; the subtraction clears
    ;;; the bottom two bits.

    subq    (%rsi), %rdx
    addq    $8, %rsi            ;;; bump up the arg pointer
    cmpq    %rax, %rdx
    jae array_sub_error

    ;;; Get the dimension scaling factor in EAX and use it to scale
    ;;; the index (NB: 0 means 1, so don't multiply!)

    slodq
    testq   %rax, %rax
    jz  L1.9
    imulq   %rax, %rdx

L1.9:   ;;; Add to the running total

    addq    %rdx, %rcx

    ;;; Get the next dimension in EAX and loop if non-zero

    slodq
    testq   %rax, %rax
    jnz L1.8

L1.10:  ;;; Finished -- push total subscript to stack, push array vector
    ;;; to stack, and then chain subscripting procedure

    subq    $16, %USP
    movq    %rcx, 8(%USP)
    movq    _PD_ARRAY_VECTOR(%PB), %rax
    movq    %rax, (%USP)
    movq    _PD_ARRAY_SUBSCR_PDR(%PB), %rax
    jmp *_PD_EXECUTE(%rax)

array_sub_error:

    ;;; Index on top of stack is invalid (either not a popint, or
    ;;; out of range)

    subq    $8, %USP        ;;; reveal the last index again
    jmp XC_LAB(weakref Sys$-Array$-Sub_error)
    call    XC_LAB(setpop)      ;;; in case the error returns

    .align  16

/***************** end labels for wrapping structures *****************/

    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start

/**********************************************************************/



/* --- Revision History ---------------------------------------------------
--- Waldek Hebisch, Feb 8 2005
        Converted from i386 to AMD64
--- Aaron Sloman, Jan  7 2003
       Replaced
               call    XC_LAB(weakref Sys$-Array$-Sub_error)
       with
               jmp     XC_LAB(weakref Sys$-Array$-Sub_error)
       as suggested by
               hebisch@math.uni.wroc.pl (Waldek Hebisch)
       following error report by
               lucb@telus.net
       both posted to comp.lang.pop

--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Added mods for Linux & S*CO.
--- Robert John Duncan, Jan 11 1995
    Fixed bug in _array_sub (for case where scale factor = 0)
--- Robert John Duncan, Sep  1 1994
    Changes to _array_sub
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- Rob Duncan, Feb 17 1989
    Changed -array_sub- not to expect an initial value in ECX but to
    start from 1
--- John Gibson, Dec  2 1988
    Added _divq
 */
