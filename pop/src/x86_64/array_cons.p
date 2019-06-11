/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.80386/src/array_cons.p
 > Purpose:         Construction of array procedures, Intel 80x86
 > Author:          Robert Duncan, Aug 25 1988 (see revisions)
 > Related Files:   S.pcunix/src/aarith.s for "_array_sub"
 */


#_INCLUDE 'declare.ph'

global constant
    _array_sub,
;

;;; ---------------------------------------------------------------------

section $-Sys;


    /*  Construct a raw array procedure. _tabsize is the word offset size
        of the array params starting at PD_ARRAY_TABLE.
    */
define Array$-Cons(_tabsize) -> _arrayp;
    lvars _tabsize, _arrayp, _drop_ptr, _size;

    ;;; Macros for dropping code at _drop_ptr
    lconstant macro (
                QUAD    = [_drop_ptr!(l)++ -> _drop_ptr],
        LONG    = [_drop_ptr!(i)++ -> _drop_ptr],
        SHORT   = [_drop_ptr!(s)++ -> _drop_ptr],
        BYTE    = [_drop_ptr!(b)++ -> _drop_ptr],
    );

    ;;; Get procedure record -- 26 bytes of code
    @@PD_ARRAY_TABLE{_tabsize} _add @@(w)[_26|b.r] _sub @@POPBASE -> _size;
    Get_store(_size) -> _arrayp;

    ;;; initialise some of procedure header
    ##(w){_size} -> _arrayp!PD_LENGTH;
    _0  ->> _arrayp!PD_REGMASK
        ->> _arrayp!PD_NUM_STK_VARS
        ->> _arrayp!PD_NUM_PSTK_VARS
        ->> _arrayp!PD_NLOCALS
        ->  _arrayp!PD_GC_SCAN_LEN;
    ##SF_LOCALS -> _arrayp!PD_GC_OFFSET_LEN;
    ##SF_LOCALS _sub ##SF_RETURN_ADDR -> _arrayp!PD_FRAME_LEN;

    ;;; Start of code
    _arrayp@PD_ARRAY_TABLE{_tabsize} ->> _drop_ptr -> _arrayp!PD_EXECUTE;

    ;;; Create stack frame
    ;;; movl %rax, %PB
        _16:48 -> BYTE;
    _16:C589 -> SHORT;   ;;; 1 + 2 = 3 bytes
        ;;; pushl %PB
    _16:55 -> BYTE;                                 ;;; 1 byte

    ;;; Call the array subscript routine
    ;;; (picks up parameters from PD_ARRAY_TABLE, stacks computed
    ;;; subscript and PD_ARRAY_VECTOR, and then chains PD_ARRAY_SUBSCR_PDR)
    ;;; movl $[_array_sub], %eax
    _16:48 -> BYTE; 
        _16:B8 -> BYTE, 
        _array_sub -> QUAD; ;;; 10 bytes
    ;;; call *%rax
    _16:D0FF -> SHORT;                              ;;; 2 bytes

    ;;; Unwind stack frame and return
    _drop_ptr -> _arrayp!PD_EXIT;
    ;;; addl $8, %rsp
        _16:48 -> BYTE;
    _16:C483 -> SHORT,
        _8 -> BYTE; ;;; 4 bytes
    ;;; movl 8(%rsp), %PB
        _16:48 -> BYTE;
    _16:6C8B -> SHORT,
        _16:24 -> BYTE, 
        _8 -> BYTE; ;;; 5 bytes
    ;;; ret
    _16:C3 -> BYTE;                                 ;;; 1 bytes
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Sep  1 1994
        Simplified so that this machine-specific file now only has to
        construct a procedure with a stack frame and a call to _array_sub
        (_array_sub  picks up the parameters from PD_ARRAY_TABLE; these are
        planted by the machine-independent Get in arrays.p)
--- Rob Duncan, Feb 17 1989
        Changed the interface to "array_sub" to add the offset at the end
        instead of at the beginning (for compatability with M_ARRAY_SUB in
        "genproc.p")
--- John Gibson, Feb  5 1989
        Initialisation of some procedure header fields moved to
        -newanyarray-
 */
