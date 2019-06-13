/* --- Copyright University of Sussex 2002. All rights reserved. ----------
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

    USP         = "ebx",
    SP          = "esp",
    PB          = "ebp",

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
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

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

    movl    (%USP), %eax
    addl    $4, %USP
    orl %eax, (%USP)
    ret

    .align  4

DEF_C_LAB 4 (_biclear)

    movl    (%USP), %eax
    addl    $4, %USP
    notl    %eax
    andl    %eax, (%USP)
    ret

    .align  4

DEF_C_LAB 4 (_bimask)

    movl    (%USP), %eax
    addl    $4, %USP
    andl    %eax, (%USP)
    ret

    .align  4

DEF_C_LAB 4 (_bixor)

    movl    (%USP), %eax
    addl    $4, %USP
    xorl    %eax, (%USP)
    ret

    .align  4


;;; === MACHINE INTEGER ARITHMETIC ====================================

;;; _MULT
;;; Signed multiplication

;;; Call:
;;; _I _mult _J -> _K

DEF_C_LAB 4 (_mult)

    movl    (%USP), %eax
    addl    $4, %USP

    ;;; Multiply EAX by (%USP); result goes to EDX:EAX

    imull   (%USP)

    ;;; Return low half of result

    movl    %eax, (%USP)
    ret

    .align  4

;;; _DIV
;;; Signed division

;;; Call:
;;; _I _div _J -> _QUOT -> _REM

DEF_C_LAB 4 (_div)

    ;;; Move the dividend into EAX and sign-extend into EDX:EAX

    movl    4(%USP), %eax
    cltd

    ;;; Divide EDX:EAX by the divisor, (%USP),
    ;;; leaving the quotient in EAX and remainder in EDX

    idivl   (%USP)

    ;;; Stack the remainder, then the quotient

    movl    %edx, 4(%USP)
    movl    %eax, (%USP)
    ret

    .align  4

;;; _DIVQ
;;; Signed division, quotient only

;;; Call:
;;; _I _divq _J -> _QUOT

DEF_C_LAB 2 (_divq)

    ;;; Move the dividend into EAX and sign-extend into EDX:EAX

    movl    4(%USP), %eax
    cltd

    ;;; Divide EDX:EAX by the divisor, (%USP),
    ;;; leaving the quotient in EAX and remainder in EDX

    idivl   (%USP)

    ;;; Stack the quotient
    movl    %eax, 4(%USP)
    addl    $4, %USP
    ret

    .align  4

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

    testl   %ecx, %ecx
    js  L1.1

    ;;; Sign is positive: shift left and return

    sall    %cl, %eax
    ret

L1.1:   ;;; Sign is negative: shift right by the absolute amount and return

    negl    %ecx
    sarl    %cl, %eax
    ret

    .align  4

;;; _SHIFT
;;;     POP interface to _M_ASH: arguments and results go via the user stack

;;; Call:
;;; _shift(_I, _J) -> _K

DEF_C_LAB (_shift)

    movl    (%USP), %ecx
    addl    $4, %USP
    movl    (%USP), %eax
    call    C_LAB(_m_ash)
    movl    %eax, (%USP)
    ret

    .align  4


;;; === POP INTEGER ARITHMETIC ========================================

;;; _PMULT
;;; POP integer multiply, with no overflow check (implements fi_*)

;;; Call:
;;; I _pmult J -> K

DEF_C_LAB 4 (_pmult)

    ;;; Pop the first popint into EAX, and make it a machine integer

    movl    (%USP), %eax
    addl    $4, %USP
    sarl    $2, %eax

    ;;; Copy the second operand into EDX and clear the popint bits

    movl    (%USP), %edx
    subl    $3, %edx

    ;;; Multiply EAX by EDX; result goes to EDX:EAX

    imull   %edx

    ;;; Return lower half of result (EAX) with popint bits restored

    addl    $3, %eax
    movl    %eax, (%USP)
    ret

    .align  4

;;; _PMULT_TESTOVF
;;; Multiply two simple pop integers with a test for overflow.
;;; Returns a result, plus <true> if OK or <false> if overflow.

;;; Call:
;;; _pmult_testovf(I, J) -> BOOL -> K

DEF_C_LAB (_pmult_testovf)

    ;;; Copy the first popint into EAX, and make it a machine integer

    movl    (%USP), %eax
    sarl    $2, %eax

    ;;; Copy the second operand into EDX and clear the popint bits

    movl    4(%USP), %edx
    subl    $3, %edx

    ;;; Multiply EAX by EDX; result goes to EDX:EAX
    ;;; Overflow flag will be set if EDX:EAX is not a sign-extension
    ;;; of EAX

    imull   %edx

    ;;; Reset the popint bits in the lower half of the result (EAX)
    ;;; and put on the stack; LEAL and MOVL won't change the flags

    leal    3(%eax), %eax
    movl    %eax, 4(%USP)

    ;;; Return <true> for OK, <false> for overflow

    jo  L1.2
    movl    $C_LAB(true), (%USP)
    ret
L1.2:   movl    $C_LAB(false), (%USP)
    ret

    .align  4

;;; _PDIV:
;;; Divide two simple pop integers with no checking (implements fi_//)

;;; Call:
;;; I _pdiv J -> QUOT -> REM

DEF_C_LAB 4 (_pdiv)

    ;;; Copy the dividend into EAX, convert it to a machine integer
    ;;; and sign-extend through EDX:EAX

    movl    4(%USP), %eax
    sarl    $2, %eax
    cltd

    ;;; Copy the divisor to ECX and convert to a machine integer

    movl    (%USP), %ecx
    sarl    $2, %ecx

    ;;; Divide EDX:EAX by ECX, leaving the quotient in EAX and
    ;;; remainder in EDX

    idivl   %ecx

    ;;; Convert quotient and remainder back to popints and return

    leal    3(, %edx, 4), %edx
    movl    %edx, 4(%USP)
    leal    3(, %eax, 4), %eax
    movl    %eax, (%USP)
    ret

    .align  4

;;; _PINT_TESTOVF:
;;; Convert machine integer to pop integer, checking for overflow.
;;; Returns a popint plus <true> if OK, or just <false> if overflow.

;;; Call:
;;; _pint_testovf(_I) -> <true> -> I
;;; _pint_testovf(_I) -> <false>

DEF_C_LAB (_pint_testovf)

    ;;; The conversion is just (_I << 2 + 3), but the shift has to be
    ;;; done in two 1-bit steps to allow the test

    movl    (%USP), %eax
    sall    $1, %eax
    jo  L1.3
    sall    $1, %eax
    jo  L1.3
    addl    $3, %eax
    movl    %eax, (%USP)
    subl    $4, %USP
    movl    $C_LAB(true), (%USP)
    ret
L1.3:   movl    $C_LAB(false), (%USP)
    ret

    .align  4

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

    movl    4(%USP), %eax
    subl    $3, %eax
    je  L1.4

    ;;; Copy the shift amount (_N) to ECX. If it's > 30, then the shift
    ;;; will definitely overflow, so jump out.

    movl    (%USP), %ecx
    cmpl    $30, %ecx
    jg  L2.1

    ;;; The left shift (SALL) only sets the overflow flag for a shift
    ;;; of 1 bit, so to do the test we have to make a copy of the
    ;;; original number in ESI, do the shift, then do a right shift by
    ;;; the same amount and compare the result with the copied original.
    ;;; If they're not the same, the left shift must have overflowed.

    movl    %eax, %esi
    sall    %cl, %eax
    movl    %eax, %edx
    sarl    %cl, %edx
    cmpl    %edx, %esi
    jne L2.1

    ;;; No overflow -- reset the popint bits in EAX, then return it
    ;;; with <true>

    addl    $3, %eax
    movl    %eax, 4(%USP)
L1.4:   movl    $C_LAB(true), (%USP)
    ret

L2.1:   ;;; Overflow -- pop the stack and return <false>

    addl    $4, %USP
    movl    $C_LAB(false), (%USP)
    ret

    .align  4

;;; === BIGINTEGER ARITHMETIC =========================================

;;; Helper for random number generator
DEF_C_LAB (_posword_mul_high)
    movl    (%USP), %eax
    movl    4(%USP), %edx
    addl    $4, %USP
    imull   %edx
    movl    %edx, (%USP)
    ret

    .align  4

;;; Bignum routines
DEF_C_LAB (_bgi_add)
    movl    16(%USP), %eax
    pushl   %eax
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $20, %USP
    call    do_bgi_add
    addl    $20, %SP
    ret

    .align  4

DEF_C_LAB (_bgi_sub)
    movl    16(%USP), %eax
    pushl   %eax
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $20, %USP
    call    do_bgi_sub
    addl    $20, %SP
    ret

    .align  4

DEF_C_LAB (_bgi_negate)
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $12, %USP
    call    do_bgi_negate
    addl    $12, %SP
    ret

    .align  4
DEF_C_LAB (_bgi_negate_no_ov)
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $12, %USP
    call    do_bgi_negate_no_ov
    addl    $12, %SP
    ret

    .align  4

;;; left shift
;;; Arguments:
;;;   destination is on top of the user stack
;;;   next is shift amount
;;;   next is lenght of biginit argument
;;;   next is biginit argument
DEF_C_LAB (_bgi_lshift)
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $12, %USP
    call    do_bgi_lshift
    addl    $16, %SP
    movl    %eax, (%USP)
    ret

;;; logical right shift
DEF_C_LAB (_bgi_rshiftl)
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $16, %USP
    call    do_bgi_rshiftl
    addl    $16, %SP
    ret

    .align  4

;;; Multiply unsigned biginit by unsigned machine integer (slice)
;;; Arguments:
;;;   destination is on top of the user stack
;;;   next is length of biginit argument
;;;   next is biginit argument
;;;   next is machine integer
DEF_C_LAB (_bgi_mult)
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $16, %USP
    call    do_bgi_mult
    addl    $16, %SP
    ret

    .align  4

DEF_C_LAB (_bgi_mult_add)
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $16, %USP
    call    do_bgi_mult_add
    addl    $16, %SP
    ret

    .align  4

;;; Subtruct product from destination
DEF_C_LAB (_bgi_sub_mult)
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $16, %USP
    call    do_bgi_sub_mult
    addl    $16, %SP
    ret

    .align  4

DEF_C_LAB (_bgi_div)
    movl    12(%USP), %eax
    pushl   %eax
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $12, %USP
    call    do_bgi_div
    addl    $16, %SP
    movl    %eax, (%USP)
    ret

    .align  4

DEF_C_LAB (_quotient_estimate_init)
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $8, %USP
    call    do_quotient_estimate_init
    addl    $12, %SP
    movl    %eax, (%USP)
    ret

    .align  4

DEF_C_LAB (_quotient_estimate)
    movl    8(%USP), %eax
    pushl   %eax
    movl    4(%USP), %eax
    pushl   %eax
    movl    (%USP), %eax
    pushl   %eax
    addl    $8, %USP
    call    do_quotient_estimate
    addl    $12, %SP
    movl    %eax, (%USP)
    ret

    .align  4

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

    leal    _PD_ARRAY_TABLE(%PB), %esi
    cld

    ;;; Initialise ECX with subscript offset

    movl    (%esi), %ecx
    addl    $4, %esi

    ;;; Load first dimension to EAX: may be zero already for a
    ;;; 0-dimensional array

    slodl
    testl   %eax, %eax
    jz  L1.10

L1.8:   ;;; Start of loop:
    ;;; get the next index from the stack into EDX and check it's
    ;;; a pop integer

    movl    (%USP), %edx
    addl    $4, %USP
    testl   $2, %edx
    jz  array_sub_error

    ;;; Subtract the next lower bound from the index and check it for
    ;;; range against the dimension in EAX.
    ;;; Both index and lower bound are popints; the subtraction clears
    ;;; the bottom two bits.

    subl    (%esi), %edx
    addl    $4, %esi            ;;; bump up the arg pointer
    cmpl    %eax, %edx
    jae array_sub_error

    ;;; Get the dimension scaling factor in EAX and use it to scale
    ;;; the index (NB: 0 means 1, so don't multiply!)

    slodl
    testl   %eax, %eax
    jz  L1.9
    imull   %eax, %edx

L1.9:   ;;; Add to the running total

    addl    %edx, %ecx

    ;;; Get the next dimension in EAX and loop if non-zero

    slodl
    testl   %eax, %eax
    jnz L1.8

L1.10:  ;;; Finished -- push total subscript to stack, push array vector
    ;;; to stack, and then chain subscripting procedure

    subl    $8, %USP
    movl    %ecx, 4(%USP)
    movl    _PD_ARRAY_VECTOR(%PB), %eax
    movl    %eax, (%USP)
    movl    _PD_ARRAY_SUBSCR_PDR(%PB), %eax
    jmp *_PD_EXECUTE(%eax)

array_sub_error:

    ;;; Index on top of stack is invalid (either not a popint, or
    ;;; out of range)

    subl    $4, %USP        ;;; reveal the last index again
    jmp XC_LAB(weakref Sys$-Array$-Sub_error)
    call    XC_LAB(setpop)      ;;; in case the error returns

    .align  4

    .data
SRCLIM:
    .long   0

/***************** end labels for wrapping structures *****************/

    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
    .data
Ldata_end:
    .set Ldata_size, Ldata_end-Ldata_start

/**********************************************************************/



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  7 2003
    Replaced
        call    XC_LAB(weakref Sys$-Array$-Sub_error)
    with
        jmp XC_LAB(weakref Sys$-Array$-Sub_error)
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
