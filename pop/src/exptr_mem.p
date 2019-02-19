/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/exptr_mem.p
 > Purpose:
 > Author:          John Gibson, Sep  3 1992 (see revisions)
 > Documentation:   REF *EXTERNAL_DATA
 */

;;; ----------------- `EXTERNAL' MEMORY POINTERS ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (is_fixed, free_fixed_hold)
    ;

section $-Sys;

global constant
        procedure (General_fixed_alloc, Make_free_block, Eq__Extern_ptr,
        Extern_ptr_hash, Rawstruct_getsize)
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => initexptr_mem, isexptr_mem, exptr_mem_key, sys_grbg_fixed;

    /*  To save having to look thru the main fixed hold list in
        the call of free_fixed_hold in sys_grbg_fixed, maintain a
        separate hold list for these
    */
vars
    exptr_mem_fixed_hold_list   = [],
;


#_IF ##(<w>)[_1 | d ] /= _1         ;;; i.e. if type <w> smaller than double
lconstant macro EXPTR_NOT_DOUBLE = true;
#_ENDIF


define lconstant Init_em(_nbytes, _hold) -> em;
    lvars em, _size, _ptr, _dalign = false, _nbytes, _hold;
    Check_integer(_nbytes, 1);
    @@(w)[_int(_nbytes) | b.r] -> _size;
#_IF DEF EXPTR_NOT_DOUBLE
    if _size _gr @@(w)[_1] then
        ;;; add an extra word for double alignment
        @@(w){_size}++ -> _size;
        true -> _dalign
    endif;
#_ENDIF
    @@(struct EXTERNAL_PTR)++ _add _size -> _size;
    Get_store(_size) -> em;     ;;; does fixed alloc then clears fixed
    exptr_mem_key -> em!KEY;
    _size -> em!RAW_SIZE;
    em@XP_PTR[_1] -> _ptr;
    ;;; ensure pointer doubleword-aligned if necessary
    if _dalign then
        _ptr@(w.r->d)
    else
        _ptr
    endif -> em!XP_PTR;
    if _hold then
        conspair(em, exptr_mem_fixed_hold_list) -> exptr_mem_fixed_hold_list
    endif
enddefine;

define initexptr_mem() with_nargs 1;
    General_fixed_alloc((), Init_em)
enddefine;

    /*  Also called from datalength */
define Exptrmem_size(/*_em*/);
    lvars _size = ()!RAW_SIZE _sub @@(struct EXTERNAL_PTR)++;
#_IF DEF EXPTR_NOT_DOUBLE
    if _size _gr @@(w)[_1] then
        ;;; subtract the extra word for double alignment
        --@@(w){_size} -> _size
    endif;
#_ENDIF
    _pint(_size)
enddefine;

    /*  Called from copy_fixed */
define Copy_exptrmem(em, _hold) -> new;
    lvars   em, _nbytes = Exptrmem_size(em), _hold,
            new = Init_em(_nbytes, _hold);
    _moveq(@@(w)[_int(_nbytes) | b.r], em!XP_PTR, new!XP_PTR) ->
enddefine;

define isexptr_mem(_item);
    lvars _item;
    iscompound(_item) and _item!KEY == exptr_mem_key
enddefine;


define sys_grbg_fixed(fixed);
    lvars fixed, key, _n = _1, _seg;
    if isinteger(fixed) and fixed fi_>= 0 then
        _int(fixed) -> _n
    else
        fixed
    endif;

    until _zero(_n) do
        () -> fixed;
        if iscompound(fixed)
        and ((fixed!KEY->>key)!K_FLAGS _bitst _:M_K_ALWAYS_FIXED
             or is_fixed(fixed))
        then
            free_fixed_hold(fixed);
            ;;; just ignore anything that's locked or in the system
            _lowest_heap_seg -> _seg;
            if _seg <@(struct SEG) _seg_table_next_free
            and _seg!SEG_BASE_PTR <=@(w) fixed then
                Make_free_block(fixed, if key == exptr_mem_key then
                                            fixed!RAW_SIZE
                                       else
                                            fast_apply(fixed, key!K_GET_SIZE)
                                       endif)
            endif
        else
            mishap(fixed, 1, 'FIXED-ADDRESS STRUCTURE NEEDED')
        endif;
        _n _sub _1 -> _n
    enduntil
enddefine;


;;; --- EXPTR MEM KEY ----------------------------------------------------

constant
    exptr_mem_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL           ;;; K_FLAGS
            _biset _:M_K_WRITEABLE
            _biset _:M_K_NO_FULL_FROM_PTR
            _biset _:M_K_ALWAYS_FIXED
            _biset _:M_K_COPY
            _biset _:M_K_EXTERN_PTR,
        _:GCTYPE_RAWSTRUCT,     ;;; K_GC_TYPE
        Rawstruct_getsize,      ;;; K_GET_SIZE

        "exptr_mem",            ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isexptr_mem,            ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Extern_ptr,         ;;; K_SYS_EQUALS
        WREF Eq__Extern_ptr,    ;;; K_EQUALS
        Minimal_print,          ;;; K_SYS_PRINT
        WREF Minimal_print,     ;;; K_PRINT
        WREF Extern_ptr_hash,   ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_DEREF,    ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %};

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 23 1995
        Removed EXPTR*_NEEDS_DALIGN stuff
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 29 1995
        Fixed outrageous bug in sys_grbg_fixed (was taking the struct size
        to be RAW_SIZE in every case)
--- John Gibson, Mar 18 1995
        Added #_IFs for EXPTR*_NEEDS_DALIGN etc
 */
