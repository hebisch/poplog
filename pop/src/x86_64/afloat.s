/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:    S.pcunix/src/afloat.s
 * Purpose: Floating point arithmetic for Intel 80x86/87 (Unix assembler)
 * Author:  Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

constant
    procedure Sys$-Float_qrem
    ;

lconstant macro (

    ;;; User stack pointer

    USP = "rbx",

    ;;; Pop ddecimal structure fields:
    ;;; _DD_1 = MS part, _DD_2 = LS part

    _DD_1   = @@DD_1,
;;; _DD_2   = @@DD_2,

);

>_#

    .file   "afloat.s"

/************************* wrapping structures ************************/

    .text
    .quad   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .quad   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .data

CW: ;;; For changing the control word
    .quad   0
WORK:
    .quad   0, 0

    .text

;;; Masks for extracting the exponent field from single and double floats

.set    S_EXP,  0x7f800000  ;;; Single, bits 23-30
.set    D_EXP,  0x7ff00000  ;;; Double, bits 52-62


;;; === FPU INITIALISATION (80387) ====================================

;;; Control word mask: this alters the default setting for the control
;;; word to unmask the Invalid Operation exception and to reduce precision
;;; control to 53 bits

.set    CW_MASK, 0xfefe

    .globl  fpu_init
fpu_init:

    fninit
    fstcw   CW
    andw    $CW_MASK, CW
    fldcw   CW
    ret

    .align  16


;;; === MOVEMENT AND TYPE CONVERSION ==================================

;;; _PFCOPY:
;;; Copy a machine-format double float (64-bit real).

;;; Call:
;;; _pfcopy(_DST_DFLOAT_ADDR, _SRC_DFLOAT_ADDR);

;;; Register usage:
;;; ESI source dfloat address
;;; EDI destination dfloat address

DEF_C_LAB (_pfcopy)

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    addq    $16, %USP

    ;;; Do the move

    movq    (%rsi), %rax
    movq    %rax, (%rdi)
    ret

    .align  16

;;; _PF_SFLOAT_DEC:
;;; Float a pop decimal.

;;; Call:
;;; _pf_sfloat_dec(DEC) -> _SFLOAT;

DEF_C_LAB (_pf_sfloat_dec)

;;; decl    (%USP)
    decq    (%USP)
    ret

    .align  16

;;; _PF_DFLOAT_INT:
;;; Double float a system integer.

;;; Call:
;;; _pf_dfloat_int(_INT, _DFLOAT_ADDR);

;;; Register usage:
;;; EDI destination dfloat address
;;; ST(0)   the value, loaded as integer, stored as real

DEF_C_LAB (_pf_dfloat_int)

    movq    (%USP), %rdi
    fildq   8(%USP)
    addq    $16, %USP
    fstpl   (%rdi)
    wait
    ret

    .align  16

DEF_C_LAB (_pf_dfloat_uint)
    movq   (%USP), %rsi    
    movq    8(%USP), %rdi
    addq    $16, %USP
    testq   %rdi, %rdi
    js      L.high_bit
    pxor    %xmm0, %xmm0
    cvtsi2sdq  %rdi, %xmm0
    jmp     L.store1
L.high_bit:
    movq    %rdi, %rax
    shrq    %rax
    andl    $1, %edi
    orq     %rdi, %rax
    pxor    %xmm0, %xmm0
    cvtsi2sdq       %rax, %xmm0
    addsd   %xmm0, %xmm0
L.store1:
    movsd   %xmm0, (%rsi)
    ret

;;; _PF_DFLOAT_DEC:
;;; Double float a pop decimal.

;;; Call:
;;; _pf_dfloat_dec(DEC, _DFLOAT_ADDR);

;;; Register usage:
;;; EDI destination dfloat address
;;; ST(0)   the value, loaded as single and stored as double

DEF_C_LAB (_pf_dfloat_dec)

    movq    (%USP), %rdi

    ;;; Clear the bottom tag bit of DEC before loading

    movq    8(%USP), %rax
    decq    %rax
;;; fldl    8(%USP)
    addq    $16, %USP
;;; fstpl   (%rdi)
    movq    %rax, (%rdi)
    wait
    ret

    .align  16

;;; _PF_DFLOAT_DDEC:
;;; Double float a pop ddecimal.

;;; Call:
;;; _pf_dfloat_ddec(DDEC, _DFLOAT_ADDR);

;;; Register usage:
;;; ESI source pop ddecimal
;;; EDI destination dfloat address
;;; EAX work

DEF_C_LAB (_pf_dfloat_ddec)

    ;;; Copy the low and high halves of the pop ddecimal in two
    ;;; longword moves

    movq    (%USP), %rdi
    movq    8(%USP), %rsi
    addq    $16, %USP
;;; movl    _DD_2(%esi), %eax
;;; movl    %eax, (%rdi)
    movq    _DD_1(%rsi), %rax
    movq    %rax, (%rdi)
    ret

    .align  16

;;; _PF_CVT_TO_DEC:
;;; Convert a machine double float to a pop decimal.
;;; Return <false> if overflow.

;;; Call:
;;; _pf_cvt_to_dec(_DFLOAT_ADDR) -> DEC
;;; _pf_cvt_to_dec(_DFLOAT_ADDR) -> <false>

;;; Register usage:
;;; ESI source dfloat address
;;; EAX low word of dfloat argument; single float result; exponent
;;; EDX high word of dfloat argument
;;; ST(0)   conversion register

;;; Memory usage:
;;; WORK    temporary for communication with the 387

DEF_C_LAB (_pf_cvt_to_dec)

    ;;; Load the double float to EDX:EAX

    movq    (%USP), %rsi
    movq    (%rsi), %rax
;;; movl    4(%esi), %edx

    ;;; Set bits in the low word to force rounding of 22nd bit of
    ;;; mantissa except where 'tied' and 21st bit is 0 (even)

#_IF false
    cmpl    $0x40000000, %eax
    je  L1.1
    orl $0x38000000, %eax

L1.1:   ;;; Copy the transformed dfloat to the work area, load it to the
    ;;; 387 as a double float then store it back as a single

    movl    %eax, WORK
#_IF DEF LINUX
    movl    %edx, (WORK+4)
#_ELSE
    movl    %edx, WORK+4
#_ENDIF
    fldl    WORK
    fstps   WORK
    wait

    ;;; Get the result back and convert to a pop decimal by setting
    ;;; the bottom tag bits to 01

    movl    WORK, %eax
    andb    $0xfc, %al
    orb $1, %al
#_ENDIF
    andq    $-3, %rax
    orq     $1, %rax
    movq    %rax, (%USP)
    shrq    $32, %rax
    testl   $D_EXP, %eax
    jz      L1.2
    ret
L1.2:
    movq    $1, (%USP)
    ret

#_IF false
    ;;; Now check for overflow/underflow by examining the exponent of
    ;;; the result

    andl    $S_EXP, %eax

    ;;; If exponent is zero, result must be +/-0.0 or denormal:
    ;;; return pop zero

    jz  L1.2

    ;;; If exponent is all ones, result must be inifinite or NaN:
    ;;; return <false>

    cmpl    $S_EXP, %eax
    je  L2.1

    ;;; Otherwise OK

    ret

L1.2:   ;;; Underflow -- return pop 0.0

    movl    $1, (%USP)
    ret

L2.1:   ;;; Overflow -- return <false>

    movl    $C_LAB(false), (%USP)
    ret
#_ENDIF
    .align  16

;;; _PF_CVT_TO_DDEC:
;;; Convert machine double float to pop ddecimal, converting -0.0 and
;;; denormals to plain zero.

;;; Call:
;;; _pf_cvt_to_ddec(_DFLOAT_ADDR, DDEC);

;;; Register usage:
;;; ESI address of the machine double float
;;; EDI address of the Pop ddecimal structure
;;; EAX work

DEF_C_LAB (_pf_cvt_to_ddec)

    movq    (%USP), %rdi
    movq    8(%USP), %rsi
    addq    $16, %USP

    ;;; Load high part of dfloat.
    ;;; Test the exponent for zero (implies zero or denormal)

    movq    (%rsi), %rax
    movq    %rax, _DD_1(%rdi)
    shrq    $32, %rax
    testl   $D_EXP, %eax
    jz  L1.3
    ret

#_IF false
    ;;; Non-zero number: store high and low halves in the pop structure
    ;;; and return

    movl    %eax, _DD_1(%edi)
    movl    (%esi), %eax
    movl    %eax, _DD_2(%edi)
    ret

#_ENDIF
L1.3:   ;;; Zero or denormal: make the pop ddecimal really zero

    movq    $0, _DD_1(%rdi)
    ret

    .align  16

;;; _PF_ROUND_D_TO_S:
;;; Round a machine double float to a single float. The double float
;;; argument is overwritten with its single equivalent. Returns <true>
;;; for OK, <false> for overflow.

;;; Call:
;;; _pf_round_d_to_s(_SRCADDR) -> BOOL

;;; Register usage:
;;; ESI address of input double/output single
;;; EAX work

DEF_C_LAB (_pf_round_d_to_s)

    ;;; Load the argument to the 387 as a double and store as a single

    movq    (%USP), %rsi
    fldl    (%rsi)
    fstps   (%rsi)
    wait

    ;;; Examine the result for infinity/NaN: return <false> if so

    movl    (%rsi), %eax
    andl    $S_EXP, %eax
    cmpl    $S_EXP, %eax
    je  L1.4

    ;;; Result OK: return <true>

    movq    $C_LAB(true), (%USP)
    ret

L1.4:   ;;; Overflow: return <false>

    movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PF_EXTEND_S_TO_D:
;;; Extend a machine single float to a double float. The argument
;;; points to the single float but in a double float space: the
;;; converted result is written back in the space.
;;; Checks first for Inf or NaN and returns <false> if so.

;;; Call:
;;; _pf_extend_s_to_d(_SRCADDR) -> _SRCADDR
;;; _pf_extend_s_to_d(_SRCADDR) -> <false>

;;; Register usage:
;;; EAX single float exponent
;;; ESI address of input single/output double

DEF_C_LAB (_pf_extend_s_to_d)

    movq    (%USP), %rsi
    movl    (%rsi), %eax

    ;;; Check exponent for Inf or NaN

    andl    $S_EXP, %eax
    cmpl    $S_EXP, %eax
    je  L1.5

    ;;; OK -- do the conversion

    flds    (%rsi)      ;;; Load as single
    fstpl   (%rsi)      ;;; Store as double
    wait
    ret

L1.5:   ;;; Inf or NaN -- return <false>

    movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PF_CHECK_D:
;;; Check double float for Inf or NaN.

;;; Call:
;;; _pf_check_d(_DFLOAT_ADDR) -> _DFLOAT_ADDR
;;; _pf_check_d(_DFLOAT_ADDR) -> <false>

;;; Registers:
;;; EAX MS word of dfloat/exponent
;;; ESI address of dfloat

DEF_C_LAB(_pf_check_d)

    movq    (%USP), %rsi
    movl    4(%rsi), %eax
    andl    $D_EXP, %eax
    cmpl    $D_EXP, %eax
    je  L1.6
    ret
L1.6:   movq    $C_LAB(false), (%USP)
    ret

    .align  16


;;; === DISSECTING FLOATS =============================================

;;; _PF_INTOF:
;;; Return the integer part of a double float (i.e truncate the
;;; argument to a system integer). Return <true> for OK, <false> for
;;; overflow.

;;; Call:
;;; _pf_intof(_DFLOAT_ADDR) -> <true> -> _INT
;;; _pf_intof(_DFLOAT_ADDR) -> <false>

;;; Register usage:
;;; ESI source dfloat address
;;; AX  the 387 control word, then status word

;;; Memory usage:
;;; CW  for loading and storing the 387 control word

DEF_C_LAB (_pf_intof)

    ;;; Get the address of the dfloat argument in ESI and load the
    ;;; argument to the 387

    movq    (%USP), %rsi
    fldl    (%rsi)

    ;;; Change the rounding mode of the 387 to "chop" by setting bits
    ;;; 10 & 11 of the control word

    fstcw   CW
    wait
    movw    CW, %ax
    orw $0x0c00, %ax
    movw    %ax, CW+2
    fldcw   CW+2

    ;;; Store the value back to memory as an integer;
    ;;; guard the operation with an exception handler which will catch
    ;;; any 'Invalid Operation' exception arising from the store

    movq    $pf_intof_exception, EXTERN_NAME(__pop_fpe_handler)
    fistpq  (%USP)
    wait                ;;; Flushes out any exceptions
    movq    $0, EXTERN_NAME(__pop_fpe_handler)

    ;;; Restore the original control word and return <true>

    fldcw   CW
    subq    $8, %USP
    movq    $C_LAB(true), (%USP)
    ret

    .align  16

pf_intof_exception:

    ;;; Invalid operation exception - value too big for an integer store.
    ;;; Reset the FPU and return <false> to indicate overflow.

    movq    $0, EXTERN_NAME(__pop_fpe_handler)
    call    fpu_init
    movq    $C_LAB(false), (%USP)
    ret

    .align  16

DEF_C_LAB (_pf_uintof)
    movq    (%USP), %rsi
    movsd   (%rsi), %xmm0
    ucomisd .LC0(%rip), %xmm0
    jb      L.out_of_range
    ucomisd .LC1(%rip), %xmm0
    jnb     L.out_of_range
    ucomisd .LC2(%rip), %xmm0
    jnb     L.big_case
    cvttsd2siq      %xmm0, %rax
    movq    %rax, (%USP)
    subq    $8, %USP
    movq    $C_LAB(true), (%USP)
    ret
L.big_case:
    subsd   .LC2(%rip), %xmm0
    cvttsd2siq      %xmm0, %rax
    movabsq $-9223372036854775808, %rsi
    xorq    %rsi, %rax
    movq    %rax, (%USP)
    subq    $8, %USP
    movq    $C_LAB(true), (%USP)
    ret
L.out_of_range:
    movq    $C_LAB(false), (%USP)
    ret

    .align 8
.LC0:
    .long   0
    .long   0
    .align 8
.LC1:
    .long   0
    .long   1139802112
    .align 8
.LC2:
    .long   0
    .long   1138753536

    .align  16

;;; _PFMODF:
;;; Extract integer and fractional parts of a double float (both parts
;;; as double floats). The source is overwritten with its integer part,
;;; the fractional part is written to the first argument.

;;; Call:
;;; _pfmodf(_FRAC, _DFLOAT_ADDR);

;;; Register usage:
;;; ESI address of input double float/output integer part (as dfloat)
;;; EDI address for fractional part (also as dfloat)
;;; AX  the 387 control word
;;; ST(0), ST(1)
;;;     work

;;; Memory usage:
;;; CW  for loading and storing the 387 control word

DEF_C_LAB (_pfmodf)

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    addq    $16, %USP

    ;;; Load the source dfloat to ST and truncate to an integer:
    ;;; this means changing the rounding mode to "chop" by setting
    ;;; bits 10 & 11 of the control word

    fldl    (%rsi)
    fstcw   CW
    wait
    movw    CW, %ax
    orw $0x0c00, %ax
    movw    %ax, CW+2
    fldcw   CW+2
    frndint
    fldcw   CW

    ;;; Copy int value to ST(1)

    fst %st(1)

    ;;; Reverse-subtract the original value to leave fractional part
    ;;; in ST

    fsubrl  (%rsi)

    ;;; Store results

    fstpl   (%rdi)
    fstpl   (%rsi)

    wait
    ret

    .align  16

;;; _PF_EXPOF:
;;; Get and set the exponent, E, of a double float, where E is the
;;; number of bits needed for the integer part, i.e 1 for 1.0,
;;; 0 for 0.5, -1 for 0.05 etc.
;;; The updater returns a flag indicating whether or not the new value
;;; was OK.

;;; Call:
;;; _pf_expof(_DFLOAT_ADDR) -> _INT
;;; _INT -> _pf_expof(_DFLOAT_ADDR) -> BOOL;

;;; Register usage:
;;; ESI address of the machine double float
;;; EAX the high word of the dfloat (containing the exponent)
;;; ECX (updater only): the new exponent

DEF_C_LAB (_pf_expof)

    ;;; Load high part of argument to EAX

    movq    (%USP), %rsi
    movl    4(%rsi), %eax

    ;;; Mask out everything except the exponent and shift it to the
    ;;; bottom of EAX

    andl    $D_EXP, %eax
    shrl    $20, %eax

    ;;; Unbias the exponent by subtracting 1022 and return

    subl    $1022, %eax
    movslq  %eax, %rsi
    movq    %rsi, (%USP)
    ret

    .align  16

DEF_C_LAB(-> _pf_expof)

    ;;; Load high part of the argument to EAX, new exponent to ECX

    movq    (%USP), %rsi
    movl    4(%rsi), %eax
    movq    8(%USP), %rcx
    addq    $8, %USP

    ;;; Bias the exponent by adding 1022: check it's in the 11-bit
    ;;; range 0 - 2047 and return <false> if not.
    ;;; Shift it up to the correct place in the word

    addq    $1022, %rcx
    cmpq    $2047, %rcx
    ja  L1.7
    shlq    $20, %rcx

    ;;; Mask out the exponent from EAX and OR in the new one;
    ;;; store it back to the argument and return <true>

#_IF DEF LINUX
    andl    $(~D_EXP), %eax
#_ELSE
    andl    $-1!D_EXP, %eax
#_ENDIF
    orl %ecx, %eax
    movl    %eax, 4(%rsi)
    movq    $C_LAB(true), (%USP)
    ret

L1.7:   ;;; New exponent out of range: return <false>

    movq    $C_LAB(false), (%USP)
    ret

    .align  16


;;; === PREDICATES ON DOUBLE FLOATS ===================================

;;; _PFZERO:
;;; Test a machine double float for zero (including -0.0).

;;; Call:
;;; _pfzero(_DFLOAT_ADDR) -> BOOL

DEF_C_LAB (_pfzero)

    ;;; Get the dfloat address in ESI

    movq    (%USP), %rsi

    ;;; Test all but the sign bit of the high word for zero

    testl   $0x7fffffff, 4(%rsi)
    jnz L1.8

    ;;; Test the low word for zero

    cmpl    $0, (%rsi)
    je  return_true
L1.8:   movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PFNEG:
;;; Test a machine double float for genuine negative (not -0.0).

;;; Call:
;;; _pfneg(_DFLOAT_ADDR) -> BOOL

DEF_C_LAB (_pfneg)

    ;;; Get the dfloat address in ESI and the high word in EAX

    movq    (%USP), %rsi
    movl    4(%rsi), %eax

    ;;; Test the sign bit: return <false> if not set

    testl   %eax, %eax
    jns L1.9

    ;;; Otherwise test for -0.0

    testl   $0x7fffffff, %eax
    jnz return_true
    cmpl    $0, (%rsi)
    jne return_true
L1.9:   movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PFEQ:
;;; Test two double floats for identity.

;;; Call:
;;; _pfeq(_DFLOAT_ADDR_1, _DFLOAT_ADDR_2) -> BOOL

DEF_C_LAB (_pfeq)

    ;;; Load addresses to ESI/EDI

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    addq    $8, %USP

    ;;; Compare high words

    movq    (%rsi), %rax
    cmpq    %rax, (%rdi)
    jne L1.10

    ;;; Compare low words

    je  return_true
L1.10:  movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PFSGR:
;;; _PFSGREQ:
;;; Test for double float greater-than/greater-than-or-equal.

;;; Call:
;;; _pfsgr(_DFLOAT_ADDR_1, _DFLOAT_ADDR_2) -> BOOL
;;; [means: _DFLOAT_1 > _DFLOAT_2]

;;; Register usage:
;;; ESI address of dfloat1
;;; EDI address of dfloat2
;;; AX  387 status word, for checking flags
;;; ST(0)   dfloat2
;;; ST(1)   dfloat1

DEF_C_LAB (_pfsgreq)

    ;;; Condition ST(1) >= ST(0) will set either bit 8 or bit
    ;;; 14 in the 387 status word: put an appropriate mask in CX and
    ;;; jump to the comparison

    movw    $0x4100, %cx
    jmp pfcmp

    .align  16

DEF_C_LAB (_pfsgr)

    ;;; Condition ST(1) > ST(0) sets bit 8 in the 387 status word:
    ;;; put an appropriate mask in CX and fall through to the comparison

    movw    $0x100, %cx

pfcmp:

    ;;; Load DFLOAT1 into ST(1) and DFLOAT2 into ST(0).

    movq    8(%USP), %rsi
    fldl    (%rsi)
    movq    (%USP), %rsi
    fldl    (%rsi)
    addq    $8, %USP

    ;;; Compare and pop ST(0) & ST(1). Transfer the flags to AX.

    fcompp
    fstsw   %ax

    ;;; Mask the flags with the condition bits set earlier in CX

    testw   %cx, %ax
    jnz return_true
    movq    $C_LAB(false), (%USP)
    ret

    .align  16

return_true:

    movq    $C_LAB(true), (%USP)
    ret

    .align  16


;;; === FLOATING POINT ARITHMETIC =====================================

;;; _PFABS:
;;; Absolute value of a double float.

;;; Call:
;;; _pfabs(_DFLOAT_ADDR);

DEF_C_LAB (_pfabs)

    movq    (%USP), %rsi
    addq    $8, %USP

    ;;; Clear the sign bit in the high word of the argument

    andl    $0x7fffffff, 4(%rsi)
    ret

    .align  16

;;; _PFNEGATE:
;;; Negate a double float.

;;; Call:
;;; _pfnegate(_DFLOAT_ADDR);

DEF_C_LAB (_pfnegate)

    movq    (%USP), %rsi
    addq    $8, %USP

    ;;; Complement the sign bit in the high word of the argument

    btcl    $31, 4(%rsi)
    ret

    .align  16

;;; _PFADD:
;;; _PFSUB:
;;; _PFMULT:
;;; _PFDIV:
;;; Double float dyadic maths operations. Return a boolean flag to
;;; indicate whether or not the operation overflowed; the numeric
;;; result is written back to the first argument.

;;; Example call:
;;; _pfsub(_DFLOAT_ADDR_1, _DFLOAT_ADDR_2) -> BOOL
;;; [means: DFLOAT1 := DFLOAT1 - DFLOAT2]

;;; Register usage:
;;; EDI _DFLOAT_ADDR_1 (which is also the destination address)
;;; ESI _DFLOAT_ADDR_2
;;; EAX work (used for overflow checking)
;;; ST(0)   _DFLOAT_1

DEF_C_LAB (_pfadd)

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    fldl    (%rdi)
    faddl   (%rsi)
    jmp overflow_check

    .align  16

DEF_C_LAB (_pfsub)

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    fldl    (%rdi)
    fsubl   (%rsi)
    jmp overflow_check

    .align  16

DEF_C_LAB (_pfmult)

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    fldl    (%rdi)
    fmull   (%rsi)
    jmp overflow_check

    .align  16

DEF_C_LAB (_pfdiv)

    movq    (%USP), %rsi
    movq    8(%USP), %rdi
    fldl    (%rdi)

    ;;; Guard the division with an exception handler to catch
    ;;; any invalid operation exceptions (for 0.0/0.0)

    movq    $pfdiv_exception, EXTERN_NAME(__pop_fpe_handler)
    fdivl   (%rsi)
    wait                ;;; Flush any exceptions
    movq    $0, EXTERN_NAME(__pop_fpe_handler)

overflow_check:

    ;;; Pop result from ST(0) back to DFLOAT1 and pop the user stack

    fstpl   (%rdi)
    addq    $8, %USP
    wait

    ;;; Load high word of result and check for exponent all 1's
    ;;; (implies infinity/NaN). If OK, return <true>.

    movl    4(%rdi), %eax
    andl    $D_EXP, %eax
    cmpl    $D_EXP, %eax
    je  L1.11
    movq    $C_LAB(true), (%USP)
    ret

L1.11:  ;;; Overflow: return <false>

    movq    $C_LAB(false), (%USP)
    ret

    .align  16

pfdiv_exception:

    ;;; Invalid operation: reset the FPU and return false

    movq    $0, EXTERN_NAME(__pop_fpe_handler)
    call    fpu_init
    addq    $8, %USP
    movq    $C_LAB(false), (%USP)
    ret

    .align  16

;;; _PFQREM:
;;; Divide two double floats and return quotient and remainder.
;;; Just uses POP-11 procedure -Float_qrem-.

DEF_C_LAB (_pfqrem)

    jmp XC_LAB(Sys$-Float_qrem)

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
--- Julian Clinton, Sep 23 1997
    Added conditional compilations for Linux.
--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO. Changed _pfmodf slightly to avoid
    use of fsub without operands, since assemblers seem to vary in how
    they interpret that.
--- John Gibson, Feb 25 1995
    Removed _m*ath1/2 subroutines (library math functions now called
    via _extern)
--- Poplog System, Jan 18 1995 (Julian Clinton)
    Changes for Linux and SCO (some expression values have been
    hardwired for SCO although they could in fact be converted
    back to expressions).
--- Robert John Duncan, May 24 1994
    No longer needs to set _sys*error
--- Robert John Duncan, Apr 15 1994
    Minor changes to simplify auto-generation of MASM version
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- Robert John Duncan, Mar 27 1991
    Changed _pf_extend_s_to_d to check for Inf and NaN;
    added _pf_check_d to do likewise.
--- Robert John Duncan, Jan 30 1991
    __pop_*fpe_handler now defined in asignals.s
--- Robert John Duncan, Dec  6 1990
    Added _pf_sfloat_dec
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- John Gibson, Jan 15 1989
    Replaced use of UC_LAB etc for updater with -> before pathname
 */
