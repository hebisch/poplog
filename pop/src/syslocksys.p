/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syslocksys.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; --------------------- LOCKING THE SYSTEM ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gcdefs.ph'
#_INCLUDE 'memseg.ph'

constant
        procedure (sysdaytime, sys_lock_heap, isarray),
        active (pop_heap_lock_count)
    ;

vars
        pop_record_writeable
    ;

section $-Sys;

constant
        procedure (Ensure_open_seg_open, Pop_seg_count, Reset_last_gc_data,
        Sr$-Save_checks, Sr$-Save)
    ;

vars
        max_mem_lim, min_mem_lim, heap_lock_count,
        Sr$- _prohibit_restore
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => sys_writeable_prop, sys_lock_system;

constant procedure
    sys_writeable_prop = newproperty([], 8, "undef", "tmparg");


    ;;; The flags _:SLS_NONWRITEABLE_DEFAULT, _:SLS_NONWRITEABLE_CLOSURES
    ;;; and _:SLS_NONWRITEABLE_CLASS may be set in this variable if writeable
    ;;; is called with pop_record_writeable false
vars
    _writeable_ignored = _0;

    /*  Called by writeable and nonwriteable etc
    */
define Set_writeable_prop(item, bool);
    lvars item, bool, tmp;
    item;           ;;; return item
    if testdef isarray and (weakref isarray(item) ->> tmp) then
        ;;; make the arrayvector (non)writeable
        tmp!PD_ARRAY_VECTOR -> item
    endif;
    bool -> sys_writeable_prop(item);
    if ispair(item) then
        ;;; make the whole of a list (non)writeable
        while ispair(fast_back(item) ->> item) do
            bool -> sys_writeable_prop(item)
        endwhile
    endif
enddefine;


section Gc;


    /*  Use the copying garbage collector and the GC segment table to
        partition all segments into writeable and non-writeable parts.
    */
define lconstant Partition_segs(_nwrit_size_tab, _flags);
    lvars   _gcseg, _nwrit_size_tab, _copy, _nextcopy, _size, _flags;
    dlocal  _disable = _DISABLE_ALL;

    ;;; Phase 1 -- For each seg, mark all structures to be made nonwriteable

    define lconstant Mark_area(_rec, lim);
        lvars lim, _rec, _key, _tmp;
        while _rec <@(w) lim do
            if issimple(_rec!KEY ->> _key) then
                ;;; already marked nonwriteable
                _mkcompound(_key) -> _key
            else
                if _key == word_key
                and (_rec!W_STRING ->> _tmp) >=@(w) _lowest_garbageable then
                    ;;; mark a word string (could already be marked)
                    _mksimple(string_key) -> _tmp!KEY
                endif;
                _key!K_FLAGS -> _tmp;
                unless _tmp _bitst _:M_K_WRITEABLE then
                    unless _tmp _bitst _:M_K_NONWRITEABLE then
                        ;;; use general default
                        ;;; (exclude live dynamic list pairs)
                        _flags _bitst _:SLS_NONWRITEABLE_DEFAULT
                        and not(_key == pair_key and fast_front(_rec) == true
                                    and isprocedure(fast_back(_rec)))
                    elseif _key == procedure_key
                    and _rec!PD_FLAGS _bitst _:M_PD_CLOSURE then
                        ;;; closure -- use closure default
                        _flags _bitst _:SLS_NONWRITEABLE_CLOSURES
                        or _rec!PD_FLAGS _bitst _:M_PD_CLOS_PROTECT
                    else
                        true
                    endunless;
                    if () then
                        ;;; mark it nonwriteable
                        _mksimple(_key) -> _rec!KEY
                    endif
                endunless
            endif;
            _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec
        endwhile
    enddefine;

    define lconstant do_writ_prop(table);
        lvars key, table, _cell, _arg, _entry, _lim;
        table@V_WORDS[_0] -> _cell;
        _cell@(w)[table!V_LENGTH] -> _lim;
        while _cell <@(w) _lim do
            _cell!(w) -> _entry;
            0 -> _cell!(w)++ -> _cell;      ;;; clears prop afterwards
            while iscompound(_entry) do
                _entry!PTE_ARG -> _arg;
                if iscompound(_arg) and _arg >=@(w) _lowest_garbageable then
                    _arg!KEY -> key;
                    if _entry!PTE_VALUE then
                        ;;; writeable
                        if issimple(key) then _mkcompound(key) -> _arg!KEY endif
                    else
                        ;;; nonwriteable
                        if iscompound(key) then _mksimple(key) -> _arg!KEY endif
                    endif
                endif;
                _entry!PTE_NEXT -> _entry
            endwhile
        endwhile
    enddefine;

    _gcseg_table -> _gcseg;
    while _gcseg!GC_SEG_BASE_PTR <@(w) _open_seg_base_ptr do
        unless _gcseg!GC_SEG_COPY_BASE == _NULL then
            ;;; not fixed-address seg
            Mark_area(_gcseg!GC_SEG_BASE_PTR, _gcseg!GC_SEG_LIM_PTR)
        endunless;
        _gcseg@(struct GC_SEG)++ -> _gcseg
    endwhile;

    ;;; deal with individual structures marked writeable/nonwriteable
    do_writ_prop(fast_frozval(1,sys_writeable_prop)!PT_TABLE);


    ;;; Phase 2 -- For each non-fixed segment, copy all marked
    ;;; structures (not their components), putting copied structures
    ;;; on the deferred chain. Ensure all structures in fixed segs
    ;;; are unmarked.

    define lconstant Unmark(_rec, lim);
        lvars lim, _rec, _key;
        while _rec <@(w) lim do
            if issimple(_rec!KEY ->> _key) then
                ;;; unmark
                _mkcompound(_key) ->> _key -> _rec!KEY
            endif;
            _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec
        endwhile
    enddefine;

    define lconstant Copy_marked(_rec, lim, _nextcopy, _reloc) -> _nextcopy;
        lvars lim, size, _rec, _copy, _key, _nextcopy, _reloc;
        while _rec <@(w) lim do
            if issimple(_rec!KEY ->> _key) then
                ;;; copy and put it on the deferred chain
                _mkcompound(_key) -> _key;
                fast_apply(_rec, _key!K_GET_SIZE) -> size;
                _nextcopy -> _copy;
                if _key!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN
                and _copy@(w.t->d) /== _copy then
                    ;;; put padding record in front
                    double_pad_key -> _copy!KEY;
                    _copy@(struct DOUBLE_PAD)++ -> _copy
                endif;
                _moveq(size, _rec@POPBASE, _copy@POPBASE)@~POPBASE -> _nextcopy;
                ;;; put forwarding address in old rec (already marked)
                _copy@(w)-{_reloc} -> _rec!FIRST;
                if _key == procedure_key then
                    ;;; correct execute addresses in copy
                    Adjust_pdr_exec(@@(w){_copy, _rec} _sub _reloc, _copy)
                endif;
                ;;; put on deferred chain
                _deferred -> _copy!KEY;
                _rec -> _deferred
            else
                fast_apply(_rec, _key!K_GET_SIZE) -> size
            endif;
            _rec@(w){size} -> _rec
        endwhile
    enddefine;

    _gcseg_table -> _gcseg;
    while _gcseg!GC_SEG_BASE_PTR <@(w) _open_seg_base_ptr do
        if (_gcseg!GC_SEG_COPY_BASE ->> _copy) == _NULL then
            ;;; fixed-address segment -- ensure everything unmarked
            Unmark(_gcseg!GC_SEG_BASE_PTR, _gcseg!GC_SEG_LIM_PTR)
        else
            ;;; ordinary segment
            Copy_marked(_gcseg!GC_SEG_BASE_PTR, _gcseg!GC_SEG_LIM_PTR,
                                    _copy, _gcseg!GC_SEG_RELOC) -> _nextcopy;
            ;;; size of the nonwriteable part -- enter in table
            @@(w){_nextcopy, _copy} ->> _size
                                -> _nwrit_size_tab!(w)++ -> _nwrit_size_tab;
            ;;; now bump up the copy pointer so that the rest (the writeable
            ;;; part) will start at the next page boundary when copied back
            _copy@(vpage){_size|w.r} -> _gcseg!GC_SEG_COPY_PTR
        endif;
        _gcseg@(struct GC_SEG)++ -> _gcseg
    endwhile;

    ;;; Finally, do a normal gc on the components of the structures copied
    Copyscan_deferred()
enddefine;

endsection;     /* Gc */


define sys_lock_system(save_file, _flags, system_version);
    lvars   nwrit_size_tab, save_file, system_version, _flags,
            _seg, _copy, _nwseg, _cstab, _cbase, _size, _tot, _nsegs;

    unless _flags then
        0 -> _flags
    elseif _flags == true then
        SLS_SHARE -> _flags
    else
        Check_integer(_flags, 0)
    endunless;
    _int(_flags) -> _flags;

    ;;; this flag is actually inverted, i.e. 1 means leave closures writeable
    _flags _bixor _:SLS_NONWRITEABLE_CLOSURES -> _flags;

    Check_string(system_version);

    if _writeable_ignored _bitst (_flags _biset _:SLS_NONWRITEABLE_CLASS) then
        mishap(0, 'sys_lock_system: CALLS OF writeable HAVE BEEN IGNORED (pop_record_writeable not set true)')
    endif;

    if save_file then
        Sr$-Save_checks(save_file)
    else
        Ensure_open_seg_open()
    endif;

    system_version <> '\s' <> sysdaytime() -> system_version;
    false -> sys_writeable_prop(system_version);

    0 -> pop_heap_lock_count;   ;;; unlock all heap segments not in the system
    _lowest_heap_seg -> _seg;   ;;; save this
    Sysgarbage(false, 'lock', 0);   ;;; remove garbage
    sys_lock_heap();            ;;; lock anything in the open seg

    ;;; Find number of (non-fixed) segments to deal with and check space
    ;;; in seg table
    Pop_seg_count(_seg, true) -> _nsegs;
    if _seg_table_next_free@(struct SEG)[_nsegs] >@(w) _seg_table_lim then
        mishap(0, 'NOT ENOUGH SPACE IN MEMORY SEGMENT TABLE TO LOCK SYSTEM')
    endif;
    ;;; allocate a rawstruct to hold the sizes of the nonwriteable parts of
    ;;; each seg
    Init_rawstruct(_nsegs) -> nwrit_size_tab;

    ;;; Then unlock all segs again and set up for a gc -- second arg an integer
    ;;; to Gc$-Setup means allow an extra page of copy space per segment
    _seg -> _lowest_heap_seg;       ;;; unlock all segs
    unless Gc$-Setup(_0, 1) then
        mishap(0, 'NO MEMORY AVAILABLE TO LOCK SYSTEM')
    endunless;

    ;;; Partition the all segments into writeable and nonwriteable parts,
    ;;; entering the nonwriteable sizes in the table
    Partition_segs(nwrit_size_tab@V_WORDS, _flags);

    ;;; Then complete the gc
    Gc$-Do_gc();

    ;;; Now insert entries for the nonwriteable segs in the seg table
    ;;; (backwards)
    _0 -> _tot;
    nwrit_size_tab@V_WORDS[_nsegs] -> _cstab;
    _seg_table_next_free -> _seg;
    _seg@(struct SEG)[_nsegs] ->> _copy -> _seg_table_next_free;
    while _seg >@(struct SEG) _lowest_heap_seg do
        _seg--@(struct SEG) -> _seg;
        _copy--@(struct SEG) -> _copy;
        _moveq(@@(struct SEG)++, _seg, _copy) -> ;
        nextif(_seg!SEG_FLAGS _bitst _M_SEG_NON_POP);
        _copy!SEG_SIZE _add _tot -> _tot;
        nextif(_seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS);
        ;;; insert nonwriteable seg before each pop seg
        _copy--@(struct SEG) -> _nwseg;
        _copy!SEG_BASE_PTR ->> _cbase -> _nwseg!SEG_BASE_PTR;
        _cstab--!(w) -> _cstab -> _size;
        _cbase@(w){_size} -> _nwseg!SEG_FREE_PTR;
        @@(vpage){_size|w.r} ->> _size -> _nwseg!SEG_SIZE;
        _M_SEG_CONSTANT -> _nwseg!SEG_FLAGS;
        ;;; remove nonwriteable part from writeable seg
        _cbase@(w){_size} -> _copy!SEG_BASE_PTR;
        _copy!SEG_SIZE _sub _size -> _copy!SEG_SIZE;
        ;;; set pages of nonwriteable seg non-writeable (if possible)
        Set_mem_prot(_cbase@POPBASE, _copy!SEG_BASE_PTR@POPBASE,
                                                    _M_PROT_NOWRITE) -> ;
        _nwseg -> _copy
    endwhile;
    _seg_table_next_free ->> _lowest_heap_seg -> _lowest_free_heap_seg;

    _open_seg_base_ptr -> _open_seg_free_ptr;   ;;; remove nwrit_size_tab
    0 -> nwrit_size_tab;                        ;;; so as not to cause trouble

    ;;; Adjust min_mem_lim & max_mem_lim
    _pint(##(w){_tot}) -> _tot;
    max(min_mem_lim - _tot, 0) -> min_mem_lim;
    max(max_mem_lim - _tot, 32768) -> max_mem_lim;

    false -> pop_record_writeable;  ;;; sys_writeable_prop already cleared
    _0 -> _writeable_ignored;
    0 -> heap_lock_count;

    if save_file then
        ;;; save an image for restoring the new system, giving Do_Save
        ;;; the new system version string - this will be assigned to
        ;;; pop_system_version after saving.
        Sr$-Save(save_file, _flags _bitst _:SLS_SHARE, system_version)
        ;;; return its result
    else
        ;;; just reset system version and move up _system_end
        system_version -> pop_system_version;
        _open_seg_base_ptr -> _system_end;
        false -> Sr$- _prohibit_restore;
        Reset_last_gc_data(true)
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  1 1997
        Sets heap_lock_count to 0.
--- John Gibson, May 15 1995
        Replaced boolean writeable_ignored with flags variable
        _writeable_ignored which has the appropriate SLS_ flag set by
        an ignored writeable.
--- John Gibson, Mar 16 1995
        Changed to use Init_rawstruct rather than initint*vec
--- John Gibson, Feb  2 1995
        Made sys_lock_system mishap if writeable_ignored is true (i.e. if
        one or writeables have been ignored)
--- John Gibson, Jan  9 1992
        Added call to Save_checks in sys_lock_system
--- John Gibson, Dec 11 1990
        Changed p*op(min)memlim to internal variables
--- John Gibson, Feb 27 1990
        More changes for fixed-address segments.
--- John Gibson, Jan  6 1990
        Changes to accomodate fixed-address segments.
--- John Gibson, Dec  5 1989
        Changes for new pop pointers
--- John Gibson, Aug 10 1989
        Save procedure name in Sys$-Sr$- changed to -Save-
--- John Gibson, Jun 18 1989
        Now allows all structures to be made nonwriteable by default,
        taking account of key flags for each class of structures, and
        -sys_writeable_prop- for individual structures.
--- John Gibson, Aug  8 1988
        Added support for doubleword-aligned structures.
--- John Gibson, Feb 25 1988
        Sectionised.
        Garbage collector routines now in section Sys$-Gc
--- John Gibson, Feb 22 1988
        Check_string into section Sys
--- John Gibson, Jan  7 1988
        Added adjustment of -min_mem_lim- together with -max_mem_lim-
 */
