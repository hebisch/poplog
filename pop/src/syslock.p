/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syslock.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; -------------------- LOCKING THE HEAP ------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'

section $-Sys;

constant
        procedure (Heap_lock_change, Lock_heap_segs, Close_open_seg,
        Tab_open_seg, Detab_open_seg, Zap_nextfree_save, Make_filler_struct
        )
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => pop_heap_lock_count, sys_lock_heap, sys_unlock_heap;

    ;;; reset to 0 by sys_lock_system
vars heap_lock_count = 0;

    /*  Lock the open seg at the current _open_seg_free_ptr
        add the locked area to the segment table
    */
define sys_lock_heap();
    lvars _seg, _lockrec;
    if Close_open_seg(true) ->> _seg then
        ;;; (always leaves at least a page free in seg)
        _seg!SEG_FREE_PTR -> _lockrec;
        _lockrec@(struct LOCK_REC)++ -> _seg!SEG_FREE_PTR;
        Make_filler_struct(_lockrec, @@(struct LOCK_REC)++);
        Heap_lock_change(_lockrec, true);
        _seg!SEG_FLAGS _biset _M_SEG_HAS_LOCK_REC -> _seg!SEG_FLAGS;
        Lock_heap_segs();
        ;;; increment lock count
        heap_lock_count fi_+ 1 -> heap_lock_count
    else
        mishap(0, 'rom: NO MORE MEMORY AVAILABLE (locking heap)',
                                            'heap-lock:mem-nomore')
    endif
enddefine;

define Coalesce_heap_segs();
    lvars _seg = _lowest_heap_seg, _last_seg = _NULL, _ptr, _base, _lim, _free;
    Tab_open_seg();             ;;; add the open seg to the table

    while _seg <@(struct SEG) _seg_table_next_free do
        if _seg!SEG_FLAGS _bitst (_M_SEG_NON_POP _biset _M_SEG_FIXED_STRUCTS)
        then
            _NULL -> _last_seg
        elseif _last_seg == _NULL then
            _seg -> _last_seg
        else
            _last_seg!SEG_BASE_PTR -> _base;
            _base@(w){_last_seg!SEG_SIZE} -> _lim;
            @@(w){_lim, _last_seg!SEG_FREE_PTR} -> _free;

            if _seg!SEG_BASE_PTR /== _lim
            or _nonzero(_free) and _free _slt @@(struct POPREC1)++ then
                ;;; not contiguous, or insufficient free space for a string
                _seg -> _last_seg
            else
                ;;; can coalesce _seg with _last_seg, filling any unused
                ;;; space in _last_seg with a string
                unless _zero(_free) then
                    ;;; fill space (with a string)
                    Make_filler_struct(_last_seg!SEG_FREE_PTR, _free)
                endunless;
                _seg!SEG_FREE_PTR -> _last_seg!SEG_FREE_PTR;
                _seg!SEG_SIZE _add _last_seg!SEG_SIZE -> _last_seg!SEG_SIZE;
                ;;; remove _seg entry by shifting down rest
                _seg@(struct SEG)++ -> _ptr;
                _moveq(@@(w){_seg_table_next_free, _ptr}, _ptr, _seg)
                                    -> _seg_table_next_free;
                nextloop        ;;; leave _seg alone for next loop
            endif
        endif;
        _seg@(struct SEG)++ -> _seg
    endwhile;

    Detab_open_seg();           ;;; update open segs vars from table
    _lowest_heap_seg -> _lowest_free_heap_seg;
    _NULL -> _curr_heap_seg;
    ;;; to prevent memory being reused in coalesced segs
    Zap_nextfree_save()
enddefine;


define active pop_heap_lock_count;
    heap_lock_count
enddefine;

    /*  Unlock the heap back to the current system barrier
        remove relevant entries from the seg table
    */
define updaterof pop_heap_lock_count count;
    lvars _seg = _seg_table_next_free, _lockrec;
    Check_integer(count, 0);
    while _seg >@(struct SEG) _seg_table do
        _seg--@(struct SEG) -> _seg;
        quitif(_seg!SEG_BASE_PTR <@(w) _system_end);
        nextif(_seg!SEG_FLAGS _bitst _M_SEG_NON_POP);

        if _seg!SEG_FLAGS _bitst _M_SEG_HAS_LOCK_REC then
            quitif(count fi_>= heap_lock_count);
            _seg!SEG_FREE_PTR--@(struct LOCK_REC) ->> _lockrec
                                                -> _seg!SEG_FREE_PTR;
            _seg!SEG_FLAGS _biclear _M_SEG_HAS_LOCK_REC -> _seg!SEG_FLAGS;
            Heap_lock_change(_lockrec, false);
            heap_lock_count fi_- 1 -> heap_lock_count
        endif;
        _seg -> _lowest_heap_seg
    endwhile;

    ;;; finally combine adjacent heap segs, etc
    Coalesce_heap_segs()
enddefine;

define sys_unlock_heap();
    0 -> pop_heap_lock_count
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  1 1997
        Added pop_heap_lock_count.
--- John Gibson, May 16 1995
        Fixed bug in sys_unlock_heap -- wasn't clearing _M_SEG_HAS_LOCK_REC
        flag in a locked segment
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Mar  3 1990
        More changes for fixed-address structures.
--- John Gibson, Feb 19 1990
        Moved some procedures to getstore.p
--- John Gibson, Jan  4 1990
        Change to -Coalesce_heap_segs- for fixed structures.
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
--- John Gibson, Apr  5 1988
        Moved memory segment routines to memseg.p
--- John Gibson, Feb 25 1988
        Garbage collector routines now in section Sys$-Gc
--- John Gibson, Sep 27 1987
        Corrected bug in -Coalesce_heap_segs-
--- John Gibson, Sep 26 1987
        Removed code for old non-copying garbage collector
--- John Gibson, Aug 14 1987
        Changed for segmented system
 */
