/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.80386/src/pdr_compose.p
 > Purpose:         Composition of two procedures, Intel 80386
 > Author:          Robert Duncan, Aug 25 1988 (see revisions)
 */


#_INCLUDE 'declare.ph'


section $-Sys;

;;; Macros for dropping code at -_drop_ptr-

lconstant macro (
        QUAD    = [_drop_ptr!(l)++ -> _drop_ptr],
    LONG    = [_drop_ptr!(i)++ -> _drop_ptr],
    SHORT   = [_drop_ptr!(s)++ -> _drop_ptr],
    BYTE    = [_drop_ptr!(b)++ -> _drop_ptr],
);


;;; Cons_pcomposite:
;;;     Construct a raw procedure for composition of two procedures
;;;     Used by <> .

define Cons_pcomposite() -> _comp;
    lvars _drop_ptr, _comp, _size;

    ;;; Determine size of procedure
    ##(w)[_26 | b.r] -> _size;              ;;; code size in words
    @@PD_COMPOSITE_TABLE[_size] _sub @@POPBASE -> _size;    ;;; plus header

    ;;; Allocate new procedure record
    Get_store(_size) -> _comp;

    ;;; Initialise some of the header
    ##(w){_size} -> _comp!PD_LENGTH;
    _0 ->> _comp!PD_REGMASK -> _comp!PD_NLOCALS;
    _0 ->> _comp!PD_NUM_STK_VARS -> _comp!PD_NUM_PSTK_VARS;
    ##SF_LOCALS -> _comp!PD_GC_OFFSET_LEN;
    _0 -> _comp!PD_GC_SCAN_LEN;
    ##SF_LOCALS _sub ##SF_RETURN_ADDR -> _comp!PD_FRAME_LEN;

    ;;; Plant the executable code:
    _comp@PD_COMPOSITE_TABLE ->> _drop_ptr -> _comp!PD_EXECUTE;

    ;;; Set up the stack frame
    ;;; movq %rax, %PB
        _16:48 -> BYTE,
    _16:C589 -> SHORT;  ;;; 3 bytes
        ;;; pushq %PB
    _16:55 -> BYTE;     ;;; 1 byte

    ;;; Call PD_COMPOSITE_P1 and PD_COMPOSITE_P2
    ;;; (assuming that @@PD_EXECUTE == 0)
    ;;; movq [@@PD_COMPOSITE_P1](%PB), %rax
        _16:48 -> BYTE,
    _16:458B -> SHORT, @@PD_COMPOSITE_P1 -> BYTE;       ;;; 4 bytes
    ;;; call *(%rax)
    _16:10FF -> SHORT;      ;;; 2 bytes
    ;;; movq [@@PD_COMPOSITE_P2](%PB), %rax
        _16:48 -> BYTE,
    _16:458B -> SHORT, @@PD_COMPOSITE_P2 -> BYTE;       ;;; 4 bytes
    ;;; call *(%rax)
    _16:10FF -> SHORT;                                  ;;; 2 bytes

    ;;; Save pointer to exit code
    _drop_ptr -> _comp!PD_EXIT;

    ;;; Unwind the stack frame
    ;;; addq $8, %rsp
        _16:48 -> BYTE,
    _16:C483 -> SHORT, _8 -> BYTE;      ;;; 4 bytes
    ;;; movq 8(%rsp), %PB
        _16:48 -> BYTE,
    _16:6C8B -> SHORT, _16:24 -> BYTE, _8 -> BYTE;      ;;; 5 bytes
    ;;; ret
    _16:C3 -> BYTE;     ;;; 1 bytes
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 13 1989
        Changed procedure name and moved common code to data_concat.p
        (Uses new struct definition for fields PD_COMPOSITE_P1, P2 etc)
 */
