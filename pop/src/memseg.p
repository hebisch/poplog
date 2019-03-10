/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/memseg.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; ---------------- MANAGING MEMORY SEGMENTS -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'

section $-Sys;

constant
        procedure Clear_freelists,
        _data_seg_start, _data_seg_end
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys;

    ;;; table of memory segments
vars
    _seg_table,
    _seg_table_next_free,
    _seg_table_lim,
    ;


define Lock_heap_segs();
    _seg_table_next_free ->> _lowest_heap_seg -> _lowest_free_heap_seg;
    _NULL -> _curr_heap_seg;
    Clear_freelists(true)       ;;; including free blocks table
enddefine;

    ;;; Add segment table entry
define Add_seg_entry(_base, _free, _size, _flags);
    lvars _seg = _seg_table_next_free, _base, _free, _size, _flags, _next;
    _seg@(struct SEG)++ -> _next;
    if _next >@(struct SEG) _seg_table_lim then
        mishap(0, 'MEMORY SEGMENT TABLE FULL', 'memseg-table:mem-nomore')
    endif;
    _base -> _seg!SEG_BASE_PTR;
    _free -> _seg!SEG_FREE_PTR;
    _size -> _seg!SEG_SIZE;
    _flags -> _seg!SEG_FLAGS;
    _next -> _seg_table_next_free
enddefine;

    ;;; Add segment table entry for nonpop segment
define Add_nonpop_seg_entry(_base, _size, _flags);
    lvars _base, _size, _flags;
    Add_seg_entry(_base, _base@(w){_size}, _size, _flags _biset _M_SEG_NON_POP);
enddefine;

define Tab_open_seg();
    Add_seg_entry(_open_seg_base_ptr, _open_seg_free_ptr,
                @@(vpage){_open_seg_free_ptr, _open_seg_base_ptr | w.r}, _0)
enddefine;

define Detab_open_seg();
    lvars _seg;
    _seg_table_next_free--@(struct SEG) ->> _seg -> _seg_table_next_free;
    _seg!SEG_BASE_PTR -> _open_seg_base_ptr;
    _seg!SEG_FREE_PTR -> _open_seg_free_ptr
enddefine;

define Pop_seg_count(_seg, _excl_fixed) -> _n;
    lvars _seg, _n = _0, _excl_fixed;
    while _seg <@(struct SEG) _seg_table_next_free do
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP
        or (_excl_fixed and _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS) then
            _n _add _1 -> _n
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile
enddefine;

define Is_updateable(item);
    lvars item, _seg = _seg_table, _seglim = _seg_table_next_free;
    returnif(item >=@(w) _system_end) (true);

    if _seg >=@(struct SEG) _seglim or item <@(w) _seg!SEG_BASE_PTR then
        ;;; seg table empty or below first seg -- is in core sys
        item@POPBASE -> item;
        return(_data_seg_start <=@(w) item and item <@(w) _data_seg_end)
    endif;

    until item <@(w) _seg!SEG_FREE_PTR do
        _seg@(struct SEG)++ -> _seg;
        returnif(_seg >=@(struct SEG) _seglim
                    or item <@(w) _seg!SEG_BASE_PTR) (true)
    enduntil;

    ;;; item is in seg
    not(_seg!SEG_FLAGS _bitst _M_SEG_CONSTANT)
enddefine;


define $-sys_show_mem_segs();
    lvars _seg = _seg_table, _flags, print_sys = true, print_locked = true;

    Tab_open_seg();

    while _seg <@(struct SEG) _seg_table_next_free do
        if _seg!SEG_BASE_PTR >=@(w) _system_end and print_sys then
            printf('==================== system end ====================\n');
            false -> print_sys
        endif;
        if _seg >=@(struct SEG) _lowest_heap_seg and print_locked then
            printf('-------------------- locked end --------------------\n');
            false -> print_locked
        endif;
        printf(_seg!SEG_SIZE, _seg!SEG_FREE_PTR, _seg!SEG_BASE_PTR,
                    '%x    %x    %x    ');
        _seg!SEG_FLAGS -> _flags;
        if _flags _bitst _M_SEG_NON_POP then
            printf('-NONPOP');
            if _flags _bitst _M_SEG_EXT_DYN_MEM then printf('-EXTDYNMEM') endif;
        else
            if _flags _bitst _M_SEG_CONSTANT then printf('-CONST') endif;
            if _flags _bitst _M_SEG_FIXED_STRUCTS then printf('-FIXED') endif;
            if _flags _bitst _M_SEG_HAS_LOCK_REC then printf('-LOCKREC') endif;
        endif;
        if _flags _bitst _M_SEG_NO_SAVE then printf('-NOSAVE') endif;
        printf('\n');
        _seg@(struct SEG)++ -> _seg
    endwhile;

    Detab_open_seg();
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  3 1995
        Added Is_updateable
--- John Gibson, May 16 1995
        Added sys_show_mem_segs for debugging
--- John Gibson, Jan  6 1990
        Made -Lock_heap_segs- call -Clear_freelists-; changed
        -Pop_seg_count- to optionally exclude fixed segs from count.
--- John Gibson, Apr  5 1988
        Moved out of syslock.p
 */
