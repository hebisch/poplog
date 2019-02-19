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

;;; _EMUL:
;;; _POSWORD_EMUL:
;;; Multiply two biginteger slices to get a double length result.
;;; A "slice" is a full word, but only the lower 31 bits are used;
;;; the top bit is always 0.

;;; Call:
;;; _emul(_MULTIPLICAND, _MULTIPLIER) -> _HIPART -> _LOPART

DEF_C_LAB (_emul)
DEF_C_LAB (_posword_emul)

    ;;; Copy argument slices to EAX, EDX

    movq    (%USP), %rax
    movq    8(%USP), %rdx

    ;;; Multiply EAX by EDX, leaving double length result in EDX:EAX

    imulq   %rdx

    ;;; Shift the top bit of the low half (EAX) into the bottom bit of
    ;;; the high half (EDX), then clear the top bit in EAX

    movq    %rax, %rdx
;;; altered for AMD64
    sarq    $31, %rdx
    andq    $0x7FFFFFFF, %rax

    ;;; Return the low part, then the high part

    movq    %rax, 8(%USP)
    movq    %rdx, (%USP)
    ret

    .align  16

;;; _EDIV:
;;; Divide a two-slice dividend by a single divisor.

;;; Call:
;;; _ediv(_HIPART, _LOPART, _DIVISOR) -> _QUOT -> _REM

DEF_C_LAB (_ediv)

    ;;; Pop divisor into ECX, then move lopart/hipart into EAX/EDX

    movq    (%USP), %rcx
    addq    $8, %USP
    movq    (%USP), %rax
    movq    8(%USP), %rdx

    ;;; Combine the two slices by shifting EDX right 1, and transferring
    ;;; its bottom bit into the top bit of EAX.

;;; sall    $1, %eax
;;; shrdl   $1, %edx, %eax
;;; sarl    $1, %edx

    salq    $31, %rdx
    orq     %rdx, %rax
    movq    $0, %rdx

    ;;; Divide EDX:EAX by ECX

    idivq   %rcx

    ;;; Return the remainder, then the quotient

    movq    %rdx, 8(%USP)
    movq    %rax, (%USP)
    ret

    .align  16

;;; _BGI_MULT:
;;; Multiply a biginteger by a signed machine integer and store the
;;; result in a destination bigint. Return a pointer to the next free
;;; slice in the destination and the last carry slice.

;;; Call:
;;;     _bgi_mult(_MULTIPLIER, _SRCADDR, _SRCLIM, _DSTADDR) -> _NEXT_DSTADDR
;;;                             -> _CARRY

;;; Register usage:
;;; ESI source pointer
;;; EDI destination pointer
;;; EAX working slice; the low half of multiplications
;;; EDX the high half of multiplications
;;; ECX the carry slice

;;; Memory usage:
;;; 8(%USP) the multiplier, left on the user stack
;;; SRCLIM  the source limit -- points one beyond the end of the source

DEF_C_LAB (_bgi_mult)

    ;;; Load arguments to registers

    movq    (%USP), %rdi    ;;; destination start
    movq    8(%USP), %rax   ;;; source limit
    movq    16(%USP), %rsi  ;;; source start
    addq    $16, %USP

    ;;; Save the source limit

    movq    %rax, SRCLIM

    ;;; Initialise the carry slice to zero

    movl    $0, %ecx

    ;;; Clear the direction flag: ESI/EDI are to be incremented after
    ;;; each slice is loaded/stored

    cld

L1.5:   ;;; Top of loop:
    ;;; load the next slice into EAX and multiply into EDX:EAX

    slodl
    imull   8(%USP)

    ;;; Add the previous carry slice to the double-length result.

    addl    %ecx, %eax  ;;; Add to the low half
    adcl    $0, %edx    ;;; and carry into the high half.
    sarl    $31, %ecx   ;;; Shift the sign of the carry into ECX
    addl    %ecx, %edx  ;;; and add it into the high half

    ;;; Shift the top bit of EAX into the bottom bit of EDX and zero
    ;;; the top bit of EAX

    shldl   $1, %eax, %edx
    andl    $0x7FFFFFFF, %eax

    ;;; Store EAX into the next destination slice, and make EDX the
    ;;; next carry slice

    sstol
    movl    %edx, %ecx

    ;;; Compare the source pointer (ESI) with the source limit;
    ;;; loop if ESI < SRCLIM.

    cmpq    %rsi, SRCLIM
    ja  L1.5

    ;;; Finished -- return the carry slice, then the next destination
    ;;; pointer

    movq    %rdi, (%USP)
    movslq  %ecx, %rdi
    movq    %rdi, 8(%USP)
    ret

    .align  16

;;; _BGI_MULT_ADD:
;;; Multiply a biginteger by a signed machine integer and add it into
;;; a destination bigint. Returns pointer to the next free slice in the
;;; destination, and the last carry slice.

;;; Call:
;;;     _bgi_mult_add(_MULTIPLIER, _SRCADDR, _SRCLIM, _DSTADDR)
;;;                     -> _NEXT_DSTADDR -> _CARRY

;;; Register usage:
;;; ESI source pointer
;;; EDI destination pointer
;;; EAX working slice; the low half of multiplications
;;; EDX the high half of multiplications
;;; ECX the carry slice and the destination slice for the addition

;;; Memory usage:
;;; 8(%USP) the multiplier, left on the user stack
;;; SRCLIM  the source limit -- points one beyond the end of the source
;;;     (declared above)

DEF_C_LAB (_bgi_mult_add)

    ;;; Load arguments to registers

    movq    (%USP), %rdi    ;;; destination start
    movq    8(%USP), %rax   ;;; source limit
    movq    16(%USP), %rsi  ;;; source start
    addq    $16, %USP

    ;;; Save the source limit

    movq    %rax, SRCLIM

    ;;; Set the initial carry slice to zero

    movl    $0, %ecx

    ;;; Clear the direction flag: ESI/EDI are to be incremented after
    ;;; each slice is loaded/stored

    cld

L1.6:   ;;; Top of loop:
    ;;; load the next slice into EAX and multiply into EDX:EAX

    slodl
    imull   8(%USP)

    ;;; Add the previous carry slice to the double-length result.

    addl    %ecx, %eax
    adcl    $0, %edx
    sarl    $31, %ecx
    addl    %ecx, %edx

    ;;; Load the next destination slice into ECX and add it into
    ;;; EDX:EAX (just like the carry slice)

    movl    (%rdi), %ecx
    addl    %ecx, %eax
    adcl    $0, %edx
    sarl    $31, %ecx
    addl    %ecx, %edx

    ;;; Shift the top bit of EAX into the bottom bit of EDX and zero
    ;;; the top bit of EAX

    shldl   $1, %eax, %edx
    andl    $0x7FFFFFFF, %eax

    ;;; Store EAX into the next destination slice, and make EDX the
    ;;; next carry slice

    sstol
    movl    %edx, %ecx

    ;;; Compare the source pointer (ESI) with the source limit;
    ;;; loop if ESI < SRCLIM

    cmpq    %rsi, SRCLIM
    ja  L1.6

    ;;; Finished -- return carry slice, then the next destination
    ;;; pointer

    movq    %rdi, (%USP)
    movslq  %ecx, %rdi
    movq    %rdi, 8(%USP)
    ret

    .align  16

;;; _BGI_DIV:
;;; Divide a biginteger by a signed machine integer into a destination
;;;     bigint and return the last remainder.

;;; Call:
;;; _bgi_div(_DIVISOR, _SRCADDR, _SRCLIM, _DSTLIM) -> _REM

;;; Register usage:
;;; ESI source pointer
;;; EDI destination pointer
;;; EAX working slice; low half of the dividend; quotient
;;; EDX high half of the dividend; remainder
;;; ECX start of the source bigint

;;; Memory usage:
;;; (%USP)  the divisor, left on the user stack

DEF_C_LAB (_bgi_div)

    ;;; Load arguments to registers

    movq    (%USP), %rdi    ;;; destination end
    movq    8(%USP), %rsi   ;;; source end
    movq    16(%USP), %rcx  ;;; source start
    addq    $24, %USP

    ;;; Set the direction flag:
    ;;; division works from the last slice back to the first slice,
    ;;; so ESI/EDI must be decremented after each load/store.

    std

    ;;; ESI and EDI point one beyond the last source and destination
    ;;; slices, so do an initial adjustment

    subq    $4, %rsi
    subq    $4, %rdi

    ;;; Load the top source slice (which carries the sign bit) into EAX;
    ;;; sign-extend into EDX:EAX then branch to do the first division

    slodl
    cltd
    jmp L2.2

L1.7:   ;;; Top of loop: load the next source slice into EAX

    slodl

    ;;; Combine it with the remainder from the previous division
    ;;; (i.e. shift the bottom bit of EDX into the top of EAX)

    sall    $1, %eax
    shrdl   $1, %edx, %eax
    sarl    $1, %edx

L2.2:   ;;; Divide EDX:EAX by (%USP); remainder goes to EDX, quotient to EAX

    idivl   (%USP)

    ;;; Store the quotient at the destination

    sstol

    ;;; Compare the source pointer (ESI) with the source start (ECX);
    ;;; loop if ESI >= ECX

    cmpq    %rsi, %rcx
    jbe L1.7

    ;;; Finished -- return the last remainder

    movslq  %edx, %rax
    movq    %rax, (%USP)
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

    .data
SRCLIM:
    .quad   0

/***************** end labels for wrapping structures *****************/

    .text
Ltext_end:
    .set Ltext_size, Ltext_end-Ltext_start
    .data
Ldata_end:
    .set Ldata_size, Ldata_end-Ldata_start

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
