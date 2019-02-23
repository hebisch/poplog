/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:        S.pcunix/src/amove.s
 * Purpose:     Memory moves and compares for Intel 80x86 (Unix assembler)
 * Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


#_<

#_INCLUDE 'declare.ph'

lconstant macro (
    USP         = "ebx",
);

>_#

    .file   "amove.s"

/************************* wrapping structures ************************/

    .text
    .long   Ltext_size, C_LAB(Sys$-objmod_pad_key)
Ltext_start:
    .data
    .long   Ldata_size, C_LAB(Sys$-objmod_pad_key)
Ldata_start:

/**********************************************************************/


    .text


;;; === COMPARISONS ===================================================

;;; _BCMP:
;;; Compare two byte regions of the same length

;;; Call:
;;; _bcmp(_BYTE_LENGTH, _SRC1, _SRC2) -> BOOL

;;; Registers used:
;;; ESI pointer to region 1
;;; EDI pointer to region 2
;;; ECX length of the comparison in bytes

DEF_C_LAB (_bcmp)

    movl    (%USP), %edi
    movl    4(%USP), %esi
    movl    8(%USP), %ecx
    addl    $8, %USP

    ;;; If size (ECX) is zero, then return <true> immediately

    cmpl    $0, %ecx
    je  L1.1

    ;;; Clear the direction flag to increment ESI/EDI

    cld

    ;;; Compare the regions

    repz
    scmpb

    ;;; If the last bytes compared equal return <true>,
    ;;; otherwise return <false>

    je  L1.1
    movl    $C_LAB(false), (%USP)
    ret
L1.1:   movl    $C_LAB(true), (%USP)
    ret

    .align  4

;;; _SCMP:
;;; Compare two short (word) regions of the same length

;;; Call:
;;; _scmp(_BYTE_LENGTH, _SRC1, _SRC2) -> BOOL

;;; Registers used:
;;; ESI pointer to region 1
;;; EDI pointer to region 2
;;; ECX length of the comparison

DEF_C_LAB (_scmp)

    movl    (%USP), %edi
    movl    4(%USP), %esi
    movl    8(%USP), %ecx
    addl    $8, %USP

    ;;; Convert ECX to size in words; if zero, then return <true>
    ;;; immediately

    sarl    $1, %ecx
    jz  L1.2

    ;;; Clear the direction flag to increment ESI/EDI

    cld

    ;;; Compare the word regions

    repz
    scmpw

    ;;; If the last words compared equal return <true>,
    ;;; otherwise return <false>

    je  L1.2
    movl    $C_LAB(false), (%USP)
    ret
L1.2:   movl    $C_LAB(true), (%USP)
    ret

    .align  4

;;; _ICMP:
;;; _CMP:
;;; Compare two long word regions of the same length

;;; Call:
;;; _cmp(_BYTE_LENGTH, _SRC1, _SRC2) -> BOOL

;;; Registers used:
;;; ESI pointer to region 1
;;; EDI pointer to region 2
;;; ECX length of the comparison

DEF_C_LAB (_icmp)
DEF_C_LAB (_cmp)

    movl    (%USP), %edi
    movl    4(%USP), %esi
    movl    8(%USP), %ecx
    addl    $8, %USP

    ;;; Convert ECX to size in longs; if zero, then return <true>
    ;;; immediately

    sarl    $2, %ecx
    jz  L1.3

    ;;; Clear the direction flag to increment ESI/EDI

    cld

    ;;; Compare the longword regions

    repz
    scmpl

    ;;; If the last longwords compared equal return <true>,
    ;;; otherwise return <false>

    je  L1.3
    movl    $C_LAB(false), (%USP)
    ret
L1.3:   movl    $C_LAB(true), (%USP)
    ret

    .align  4


;;; === MOVES =========================================================

;;; _MOVEQ:
;;; Quick longword move. The move is done from the lowest word first,
;;; so if the source and destination regions overlap, the direction of
;;; the move must be downwards to preserve correctness.
;;; Returns a pointer to the next destination longword.

;;; Call:
;;; _moveq(_BYTE_LENGTH, _SRCADDR, _DSTADDR) -> _NEXT_DSTADDR;

;;; Registers used:
;;; ESI source pointer
;;; EDI destination pointer
;;; ECX length of the move

DEF_C_LAB (_moveq)

    movl    (%USP), %edi
    movl    4(%USP), %esi
    movl    8(%USP), %ecx
    addl    $8, %USP

    ;;; Compare source and destination pointers and quit if the same

    cmpl    %esi, %edi
    je  L2.1

    ;;; Convert the byte-length in ECX to longwords and quit if zero

    sarl    $2, %ecx
    jz  L1.4

    ;;; Do the move

    cld
    rep
    smovl

L1.4:   ;;; Return the next destination address

    movl    %edi, (%USP)
    ret

L2.1:   ;;; Source and destination the same:
    ;;; compute and return the next destination address

    addl    %ecx, %edi
    movl    %edi, (%USP)
    ret

    .align  4

;;; _BMOVE:
;;; _SMOVE:
;;; _IMOVE:
;;; _MOVE:
;;; General purpose moves for bytes, words and longs. Cope with any
;;; alignment and with moves in both directions. Return address of the
;;; next destination.

;;; Call:
;;; _move(_BYTE_LENGTH, _SRCADDR, _DSTADDR) -> _NEXT_DSTADDR;

;;; Uses:
;;; movchars, movchars_l (defined below)

DEF_C_LAB (_bmove)
DEF_C_LAB (_smove)

    ;;; Set register arguments for -movchars-

    movl    (%USP), %edi
    movl    4(%USP), %esi
    movl    8(%USP), %ecx
    addl    $8, %USP

    ;;; Push the next destination address as the return value

    leal    (%edi, %ecx), %eax
    movl    %eax, (%USP)

    ;;; Do the move

    jmp movchars

    .align  4

DEF_C_LAB (_imove)
DEF_C_LAB (_dmove)
DEF_C_LAB (_move)

    ;;; Set register arguments for movchars

    movl    (%USP), %edi
    movl    4(%USP), %esi
    movl    8(%USP), %ecx
    addl    $8, %USP

    ;;; Push the next destination address as the return value

    leal    (%edi, %ecx), %eax
    movl    %eax, (%USP)

    ;;; Do the move in longword steps

    jmp movchars_l

    .align  4

;;; MOVCHARS:
;;; MOVCHARS_L:
;;;     General purpose memory move. Not callable from Pop, as it takes its
;;; arguments in registers rather than on the stack (but cf. _bmove,
;;; _smove, _move above). MOVCHARS_L is for use when the move is
;;; longword-aligned.

;;; Arguments:
;;; ESI source address
;;; EDI destination address
;;; ECX length of move in bytes

;;; Results:
;;; none

;;; Other registers used:
;;; EAX work

movchars:

    ;;; OR together the bits of the source address, destination address
    ;;; and byte count to test for word alignment

    movl    %esi, %eax
    orl %edi, %eax
    orl %ecx, %eax

    ;;; If neither of the bottom two bits is set, the move is longword
    ;;; aligned so move a longword at a time

    testl   $3, %eax
    jz  movchars_l

    ;;; Otherwise, move a byte at a time

movchars_b:

    ;;; If ECX is zero there's nothing to move, so exit

    cmpl    $0, %ecx
    je  L2.2

    ;;; Clear the direction flag for a default downwards move

    cld

    ;;; Test the direction of the move:
    ;;; if ESI > EDI, then the move is down, so jump straight to it;
    ;;; if ESI = EDI, then there's no move at all, so return

    cmpl    %edi, %esi
    ja  L1.5
    je  L2.2

    ;;; Otherwise, ESI < EDI, so it's an upward move: change the
    ;;; direction flag, and adjust the pointers to point to the last
    ;;; byte in each region rather than the first

    std
    leal    -1(%esi, %ecx), %esi
    leal    -1(%edi, %ecx), %edi

L1.5:   ;;; Do the move

    rep
    smovb

L2.2:   ;;; Return

    ret

    .align  4

movchars_l:

    ;;; Convert ECX to the length of the move in longwords; if zero,
    ;;; jump to the end.

    sarl    $2, %ecx
    jz  L2.3

    ;;; Clear the direction flag for a default downwards move

    cld

    ;;; Test the direction of the move:
    ;;; if ESI > EDI, then the move is down, so jump straight to it;
    ;;; if ESI = EDI, then there's no move at all, so return

    cmpl    %edi, %esi
    ja  L1.6
    je  L2.3

    ;;; Otherwise, ESI < EDI, so it's an upward move: change the
    ;;; direction flag, and adjust the pointers to point to the last
    ;;; word in each region rather than the first

    std
    leal    -4(%esi, %ecx, 4), %esi
    leal    -4(%edi, %ecx, 4), %edi

L1.6:   ;;; Do the move

    rep
    smovl

L2.3:   ;;; Return

    ret

    .align  4


;;; === FILLING =======================================================

;;; _BFILL:
;;; Fill a region of bytes with a given byte.

;;; Call:
;;; _bfill(_BYTE, _BYTE_LENGTH, _DSTADDR);

;;; Registers used:
;;; EAX the byte
;;; ECX length of the region in bytes
;;; EDI destination pointer

DEF_C_LAB (_bfill)

    movl    (%USP), %edi
    movl    4(%USP), %ecx
    movl    8(%USP), %eax
    addl    $12, %USP

    ;;; Clear the direction flag to increment EDI and do the fill

    cld
    rep
    sstob
    ret

    .align  4

;;; _IFILL:
;;; _FILL:
;;; Fill a region of longwords with a given longword.

;;; Call:
;;; _fill(_LONG, _BYTE_LENGTH, _DSTADDR);

;;; Registers used:
;;; EAX the longword
;;; ECX length of the region in bytes
;;; EDI destination pointer

DEF_C_LAB (_ifill)
DEF_C_LAB (_fill)

    movl    (%USP), %edi
    movl    4(%USP), %ecx
    movl    8(%USP), %eax
    addl    $12, %USP

    ;;; Clear the direction flag to increment EDI, convert ECX to
    ;;; length in longwords and do the filling

    cld
    sarl    $2, %ecx
    rep
    sstol
    ret

    .align  4


;;; === RELOCATING POPLOG MEMORY REGIONS ==============================

;;; _MOVE_USERSTACK:
;;; Move the user stack up or down.

;;; Call:
;;; _move_userstack(_BYTE_OFFS)

;;; Register usage:
;;; ESI source pointer
;;; EDI destination pointer
;;; ECX _USERHI (start of user stack), then stack length in longwords
;;;     (the amount to move)
;;; EAX the amount of the shift in bytes (_BYTE_OFFS)

DEF_C_LAB (_move_userstack)

    ;;; Load the amount to shift (in bytes) into EAX

    movl    (%USP), %eax
    addl    $4, %USP

    ;;; Test the direction of the move

    testl   %eax, %eax
    jl  move_userstack_down
    jg  move_userstack_up

    ;;; Zero move -- return

    ret

    .align  4

move_userstack_down:

    ;;; Move from the bottom (USP) upwards

    ;;; Adjust the value of USERHI

    movl    I_LAB(_userhi), %ecx
    addl    %eax, %ecx
    movl    %ecx, I_LAB(_userhi)

    ;;; Adjust the stack pointer and make the old value the source
    ;;; pointer and the new value the destination pointer

    movl    %USP, %esi
    addl    %eax, %USP
    movl    %USP, %edi

    ;;; Compute the length of the move (USERHI - USP) in ECX and do it

    subl    %USP, %ecx
    sarl    $2, %ecx
    cld
    rep
    smovl
    ret

    .align  4

move_userstack_up:

    ;;; Move from the top (USERHI) downwards

    ;;; Adjust USERHI. Make the old value the source pointer and the new
    ;;; the destination pointer. Pointers are offset by 4 bytes because
    ;;; USERHI points one word beyond the top of stack.

    movl    I_LAB(_userhi), %ecx
    leal    -4(%ecx), %esi
    addl    %eax, %ecx
    leal    -4(%ecx), %edi
    movl    %ecx, I_LAB(_userhi)

    ;;; Adjust the stack pointer

    addl    %eax, %USP

    ;;; Compute the length of the move (USERHI - USP) in ECX and do it

    subl    %USP, %ecx
    sarl    $2, %ecx
    std
    rep
    smovl
    ret

    .align  4

;;; _MOVE_CALLSTACK:
;;; Relocate the top part of the callstack between the stack pointer
;;; ESP and the _LIMIT address. The size of the relocation is _BYTE_OFFS
;;; and may be up or down.

;;; Call:
;;; _move_callstack(_BYTE_OFFS, _LIMIT);

;;; Registers used:
;;; ESI source pointer
;;; EDI destination pointer
;;; ECX LIMIT address, then the amount to move (LIMIT - ESP)
;;; EAX the size of the shift (_BYTE_OFFS)

DEF_C_LAB (_move_callstack)

    ;;; Load limit address to ECX, byte offset to EAX

    movl    (%USP), %ecx
    movl    4(%USP), %eax
    addl    $8, %USP

    ;;; Test the direction of the move

    testl   %eax, %eax
    jl  move_callstack_down
    jg  move_callstack_up

    ;;; Zero move -- return

    ret

    .align  4

move_callstack_down:

    ;;; Move from the bottom (ESP) upwards

    ;;; The length of the move is (_LIMIT - ESP)

    subl    %esp, %ecx

    ;;; Adjust the stack pointer and make the old value the source
    ;;; pointer and the new value the destination pointer

    movl    %esp, %esi
    addl    %eax, %esp
    movl    %esp, %edi

    ;;; Do the move

    sarl    $2, %ecx
    cld
    rep
    smovl
    ret

    .align  4

move_callstack_up:

    ;;; Move from the _LIMIT address downwards

    ;;; Make _LIMIT the source address (offset by 4 to point to the
    ;;; first word to be moved) and (SOURCE + _BYTE_OFFS) the destination

    leal    -4(%ecx), %esi
    leal    (%esi, %eax), %edi

    ;;; The length of the move is (_LIMIT - ESP)

    subl    %esp, %ecx

    ;;; Do the move

    sarl    $2, %ecx
    std
    rep
    smovl

    ;;; Set the new stack pointer

    leal    4(%edi), %esp

    ret

    .align  4


;;; === BITFIELD OPERATIONS ===========================================

;;; _BFIELD:
;;; _SBFIELD:
;;; Extract an (un)signed bitfield. Not called directly from POP, but
;;; used to implement M_MOVE(s)bit and I_PUSH_FIELD instructions.

;;; Arguments:
;;; EDI structure address
;;; EAX bit offset of field within structure
;;; EDX length of field in bits

;;; Results:
;;; EAX the extracted bitfield, right-justified and zero- or
;;;     sign-extended as appropriate

DEF_C_LAB(_bfield)

    ;;; Copy bit offset to ECX

    movl    %eax, %ecx

    ;;; Convert bit offset to word index

    sarl    $5, %eax

    ;;; Compute address of the first word of the bitfield in EDI.
    ;;; Load first word to EAX and second to EDI in case of overspill.

    leal    (%edi, %eax, 4), %edi
    movl    (%edi), %eax
    movl    4(%edi), %edi

    ;;; Mask the copied offset in ECX to get a bit-within-word offset
    ;;; and shift EAX down by that amount to right-align the field
    ;;; (bits are pulled in to the top end from EDI)

    andl    $31, %ecx
    shrdl   %edi, %eax

    ;;; Zero the top (32 - field-width) bits

    movl    $32, %ecx
    subl    %edx, %ecx
    sall    %cl, %eax
    shrl    %cl, %eax

    ret

    .align  4

DEF_C_LAB(_sbfield)

    ;;; Copy bit offset to ECX

    movl    %eax, %ecx

    ;;; Convert bit offset to word index

    sarl    $5, %eax

    ;;; Compute address of the first word of the bitfield in EDI.
    ;;; Load first word to EAX and second to EDI in case of overspill.

    leal    (%edi, %eax, 4), %edi
    movl    (%edi), %eax
    movl    4(%edi), %edi

    ;;; Mask the copied offset in ECX to get a bit-within-word offset
    ;;; and shift EAX down by that amount to right-align the field
    ;;; (bits are pulled in to the top end from EDI)

    andl    $31, %ecx
    shrdl   %edi, %eax

    ;;; Sign-extend the field into the top (32 - field-width) bits

    movl    $32, %ecx
    subl    %edx, %ecx
    sall    %cl, %eax
    sarl    %cl, %eax

    ret

    .align  4

;;; _UBFIELD:
;;; Update a bitfield within a structure. Not called directly from POP
;;; but used to implement M_UPDbit and I_POP_FIELD instructions.

;;; Arguments:
;;; EDI structure address
;;; EAX bit offset of field within structure
;;; EDX length of the field in bits
;;; (%USP)  (top of user stack) the new value as a system integer

;;; Results:
;;; none

;;; Other registers used:
;;; EAX overwritten with the new value of the bitfield
;;; EDX the bit offset within the first word of the field
;;; ESI the current value of the bitfield

DEF_C_LAB (_ubfield)

    ;;; Compute (32 - field-width) in ECX

    movl    $32, %ecx
    subl    %edx, %ecx

    ;;; Copy the bit offset into EDX

    movl    %eax, %edx

    ;;; Convert bit offset to word index and add to the structure
    ;;; address to get a pointer to the first word of the bitfield

    sarl    $5, %eax
    leal    (%edi, %eax, 4), %edi

    ;;; Get the new value of the field from the stack top to EAX
    ;;; EAX = [ ___ | MMM' ]

    movl    (%USP), %eax
    addl    $4, %USP

    ;;; Shift left by (32 - field-width) to left-align it in EAX
    ;;; EAX = [ MMM' | ___ ]

    shll    %cl, %eax

    ;;; Mask the copied bit offset in EDX to get a bit-within-word
    ;;; offset

    andl    $31, %edx

    ;;; Subtract from the value in ECX: this gives the number of unwanted
    ;;; bits in the high end of the first word

    subl    %edx, %ecx

    ;;; If negative, the field must overspill into a second word,
    ;;; so jump to handle that

    jl  L1.7

    ;;; Otherwise, load the current value of the target word into ESI
    ;;; ESI = [ HHH | MMM | LLL ]

    movl    (%edi), %esi

    ;;; Rotate to move the current field value to the top
    ;;; ESI = [ MMM | LLL | HHH ]

    roll    %cl, %esi

    ;;; Reinstate the shift count and pull the extra bits from ESI
    ;;; into the top of EAX
    ;;; EAX = [ LLL | HHH | MMM' ]

    addl    %edx, %ecx
    shrdl   %esi, %eax

    ;;; Rotate the low order bits to their correct place
    ;;; EAX = [ HHH | MMM' | LLL ]

    movl    %edx, %ecx
    roll    %cl, %eax

    ;;; Store the updated word and return

    movl    %eax, (%edi)
    ret

L1.7:   ;;; Field overspills into a second word -- load that first
    ;;; ESI = [ HHH | MMH ]

    movl    4(%edi), %esi

    ;;; The size of the overspill is the absolute value of ECX;
    ;;; shift ESI by that amount, then pull in the top bits of the
    ;;; new value. Drop the corresponding bits from EAX.

    negl    %ecx
    shrl    %cl, %esi       ;;; ESI = [ 000 | HHH ]
    shldl   %eax, %esi      ;;; ESI = [ HHH | MMH' ]
    shll    %cl, %eax       ;;; EAX = [ MML' | 000 ]

    ;;; Store the updated high word

    movl    %esi, 4(%edi)

    ;;; Now get the low word
    ;;; ESI = [ MML | LLL ]

    movl    (%edi), %esi

    ;;; Only the bottom bits of this are wanted, the count being in
    ;;; EDX. Shift them into the high part of EAX, then rotate to
    ;;; place them in the low part

    movl    %edx, %ecx
    shrdl   %esi, %eax      ;;; EAX = [ LLL | MML' ]
    roll    %cl, %eax       ;;; EAX = [ MML' | LLL ]

    ;;; Store the low word and return

    movl    %eax, (%edi)
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
--- Robert Duncan, Aug  9 1996
    Removed special cases for S*CO
--- John Gibson, Apr  6 1995
    Added _icmp, _imove, _dmove, _ifill as equivalent to word versions
--- Poplog System, Jan 18 1995 (Julian Clinton).
    Changes for Linux and S*CO.
--- Robert John Duncan, Jan 26 1994
    Renamed from Sun386 (now defunct)
--- Robert John Duncan, Oct  1 1990
    Fixed -move_callstack_up- not to change ESP until after the move
    in case of interrupt
--- John Gibson, Dec 12 1989
    Removed "bfield_sub" (not used anywhere)
--- John Gibson, Aug 17 1989
    Replaced # EXEC ... # ENDEXEC with #_< ... >_#
--- Rob Duncan, Jan 10 1989
    Fixed -moveq- to return the next destination address when source
    & destination pointers are the same
 */
