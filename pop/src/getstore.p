/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/src/getstore.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; --------------------- STORE MANAGEMENT ----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'gcdefs.ph'
#_INCLUDE 'destroy.ph'

constant
        procedure (systime, fast_syswrite),
        _move_userstack
    ;

vars
        _plog_contn_barrier, _plog_contn_sp, _plog_area_hi, _plog_area_lo,
        _plog_trail_sp
    ;

section $-Sys;

constant
        procedure (Detab_open_seg, Reset_last_gc_data, Close_filetab_files)
    ;

vars
        _nr_seg_entry
    ;

weak constant
        procedure (Clear_free_blocks, Get_store_fixed, Do_destroy_actions,
        Float_opnd_vals, Bigint_neg,
        Xt$-XptGarbageHandler, Xt$-XptClearFreelists
        ),
        free_block_key, _freevectab, _free_clos_tab
    ;

weak vars
        _free_npfront_pairs, Sr$- _prohibit_restore
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys => popgctrace, popgctime, popgcratio, pop_after_gc,
                 popmemused, popmemlim, popminmemlim;

lconstant macro DEFAULT_GC_RATIO = 40;

protected vars
    popmemused  = 0,
    popgctime   = 0;

vars
    popgcratio  = DEFAULT_GC_RATIO,
    popgctrace  = false,
    procedure pop_after_gc = identfn,
    ;

vars
    ;;; max_mem_lim = 3e5,
    max_mem_lim = 15e5,
    min_mem_lim = 0,
    ;


define Rawstruct_getsize() with_nargs 1;
    ()!RAW_SIZE
enddefine;


constant

    ;;; key of general structure containing any old stuff
    rawstruct_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_NO_FULL_FROM_PTR, ;;; K_FLAGS
        _:GCTYPE_RAWSTRUCT,     ;;; K_GC_TYPE
        Rawstruct_getsize,      ;;; K_GET_SIZE
        %},

    ;;; key of 3 word structure for padding doubleword-aligned structures
    double_pad_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_WRITEABLE,        ;;; K_FLAGS     ;;; for sys_lock_system
        _:GCTYPE_DOUBLE_PAD,    ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE
        %},

    ;;; key of special structure for taking care of (possibly) unpredictable
    ;;; alignment padding between object modules
    objmod_pad_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _0,                     ;;; K_FLAGS
        _:GCTYPE_OBJMOD_PAD,    ;;; K_GC_TYPE
        erase,                  ;;; K_GET_SIZE (should never be called)
        %},
    ;


#_IF DEF AIX
    ;;; Similar to rawstruct_key, but says structure is nonwriteable
    ;;; (used in object files)
constant nonwriteable_rawstruct_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_NO_FULL_FROM_PTR, ;;; K_FLAGS
        _:GCTYPE_RAWSTRUCT,     ;;; K_GC_TYPE
        Rawstruct_getsize,      ;;; K_GET_SIZE
        %};
#_ENDIF


lconstant
    _temp_userstack = _INIT_NONPOP_STRUCT(w[16])@(w)[_16],
    ;

vars
    _nextfree_save      = _NULL,
    _inhibit_gc         = false,
    _curr_seg_free_ptr,
    _curr_seg_free_lim,
    _curr_heap_seg      = _NULL,
    _saved_curr_heap_seg= _NULL,    ;;; set while using special seg

    ;;; set up a temporary initial userstack (assigned from _saved_usp to
    ;;; _user_sp by assembler entry code and then replaced with the real
    ;;; thing by Setup_system)

    $- _userhi                  = _temp_userstack,
    $-Sys$-Extern$- _saved_usp  = _temp_userstack,
    ;



;;; --- MEMORY CONFIGURATION MANAGEMENT ----------------------------------


#_IF DEF VMS

#_INCLUDE 'vmsdefs.ph'

constant
        procedure (Vms_jpi_int, Set_call_stack_lim)
    ;

deftype limpage = vpage[1];         ;;; can use lim page

define Get_mem_break();
    Vms_jpi_int(_:'JPI$_FREP0VA')
enddefine;

define Set_mem_break(_need_break, _lessoff);
    lvars   _pages, _need_break, _curr_break = Get_mem_break(), _lessoff,
            _pglets;
    lstackmem struct MEMRANGE _mrp;

    ##(vpage){_need_break, _curr_break | w.r} -> _pages;
    ##(vpagelet)[_pages | vpage] -> _pglets;
    if _pages _sgr _0 then
        _-1 -> _mrp!MR_FIRST_ADDR;
        unless _extern sys\$expreg( /* pagcnt */    _pglets,
                                    /* retadr */    _mrp,
                                    /* acmode */    ,
                                    /* region */    _0)
        _bitst _1 then
            ;;; failed
            if _mrp!MR_FIRST_ADDR == _-1 then
                _0
            else
                ##(vpage){_mrp!MR_LAST_ADDR, _mrp!MR_FIRST_ADDR | b.r}
            endif -> _pages;
            if ##(vpage){_need_break@(w){_lessoff}, _curr_break | w.r}
                                                        _sgr _pages then
                ;;; not enough -- delete any pages allocated
                _extern sys\$deltva(/* inadr  */    _mrp,
                                    /* retadr */    ,
                                    /* acmode */    ) -> ;
                return(_-1)
            endif
        endunless
    elseif _pages _slt _0 then
        _extern sys\$cntreg(/* pagcnt */    _negate(_pglets),
                            /* retadr */    ,
                            /* acmode */    ,
                            /* region */    _0) ->

    endif;

    ;;; successful -- adjust _call_stack_lim on the basis of pages used
    Set_call_stack_lim();

    ;;; return new break
    Get_mem_break()
enddefine;


#_ELSEIF DEF UNIX

    #_IF DEF BSD_MPROTECT

    deftype limpage = vpage[1];     ;;; can use lim page

    #_ELSE

    deftype limpage = vpage[0];     ;;; no lim page

    #_ENDIF

    ;;; see sysdefs.p for definitions of GET/SET_REAL_BREAK
constant procedure
    Get_mem_break = GET_REAL_BREAK;

define Set_mem_break(_need_break, _lessoff);
    lvars _need_break, _break, _hi, _lo, _mid, _curr_break, _lessoff;
    lconstant procedure set_break = SET_REAL_BREAK;
    returnunless( (set_break(_need_break) ->> _break) == _-1
                    and _nonzero(_lessoff) ) (_break);

    ;;; can't set to requested value but can try for less
    ;;; use a binary chop to find the most that can be allocated
    Get_mem_break() -> _curr_break;
    ##(vpage){_need_break, _curr_break | w.r} -> _hi;
    _need_break@(w){_lessoff} -> _need_break;   ;;; minimum break
    ##(vpage){_need_break, _curr_break | w.r} -> _lo;
    if _lo _sgr _0 then
        returnif(set_break(_need_break) == _-1) (_-1)
    else
        _0 -> _lo
    endif;
    while (_shift(_hi _sub _lo, _-1) ->> _mid) _sgr _0 do
        _lo _add _mid -> _mid;
        if set_break(_curr_break@(vpage)[_mid]) == _-1 then
            _mid -> _hi
        else
            _mid -> _lo
        endif
    endwhile;
    Get_mem_break()     ;;; return value reached
enddefine;

#_ELSEIF DEF WIN32

;;; The memory ``break'' is manipulated by external functions
;;; pop_set_heap_ptr and pop_get_heap_ptr defined in "memory.c"

deftype limpage = vpage[1];     ;;; can use lim page

define Get_mem_break();
    _extern pop_get_heap_ptr();
enddefine;

define Set_mem_break(_need_break, _lessoff) -> _break;
    lvars _need_break, _lessoff, _break;
    _extern pop_set_heap_ptr(_need_break, _need_break@(w){_lessoff}) -> _break;
    if _zero(_break) then
        _-1 -> _break;
    endif;
enddefine;

#_ELSE_ERROR
#_ENDIF


define lconstant Get_user_break();
    ;;; allows for the inaccessible userstack limit page
    Get_mem_break()--@(limpage)
enddefine;

define lconstant Set_user_break(_need_break, _lessoff);
    lvars _need_break, _lessoff, _break;
    ;;; allows for the inaccessible userstack limit page
    if (Set_mem_break(_need_break@(limpage)++, _lessoff) ->> _break) == _-1
    then
        _-1
    else
        _break--@(limpage)
    endif
enddefine;

lconstant macro (
    FREE_LIM_PAGE   = [Set_mem_prot(_userhi, _userhi@(limpage)++, %_M_PROT_ALL%)->],
    LOCK_LIM_PAGE   = [Set_mem_prot(_userhi, _userhi@(limpage)++, %_M_PROT_NONE%)->],
    );


define Set_userhi_to(_addr, freeold);
    lvars freeold, _addr;
    if freeold then FREE_LIM_PAGE endif;
    ;;; set the new break
    if (Set_user_break(_addr, _0) ->> _addr) /== _-1 then
        _addr -> _userhi;
        LOCK_LIM_PAGE;
        true
    else
        if freeold then LOCK_LIM_PAGE endif;
        false
    endif
enddefine;

define lconstant Set_open_seg_after_exmem(_base, _exmem_base);
    lvars _base, _exmem_base;
    _base@~POPBASE ->> _open_seg_base_ptr -> _open_seg_free_ptr;
    _open_seg_free_ptr@(w){_USER_SAFEGAP} -> _userlim;

    ;;; Add a seg entry for the external dynamic mem
    Add_nonpop_seg_entry(_exmem_base, @@(w){_base, _exmem_base},
                            _M_SEG_EXT_DYN_MEM _biset _M_SEG_NO_SAVE);
    true -> weakref Sr$- _prohibit_restore
enddefine;

    /*  Try to leapfrog the open seg over blocking external mem, by
        creating a new one above it and closing off the old open seg.
    */
define lconstant Try_open_seg_leapfrog();
    lvars _old_break, _ubreak, _size, _exmem_base;
    Get_mem_break() -> _old_break;
    Set_user_break(_old_break@(w){_stklength() _add _USER_SAFEGAP}, _0)
                                                            -> _ubreak;
    ;;; return if not enough space to move the userstack
    returnif(_ubreak == _-1) (false);

    ;;; can do it -- make the old open seg a new closed one
    _userhi@(limpage)++ -> _exmem_base;
    Add_seg_entry(_open_seg_base_ptr, _open_seg_free_ptr,
                        @@(w){_exmem_base@~POPBASE, _open_seg_base_ptr}, _0);

    ;;; move the user stack
    FREE_LIM_PAGE;
    _move_userstack(@@(w){_ubreak, _userhi});
    LOCK_LIM_PAGE;

    Set_open_seg_after_exmem(_old_break, _exmem_base);
    true
enddefine;


#_IF DEF VMS and DEF SHARED_LIBRARIES

    /*  Called from Reactivate_shrims to enable shareable images
        to be mapped after restoring a system saved image without
        blocking-in the (empty) open segment. Temporarily deletes the
        open seg and the recreates after the shrims get mapped (if any
        do).
    */
define Allow_shrim_mapping(domap_p);
    lvars   procedure domap_p, _save_userhi = _userhi,
            _base = _open_seg_base_ptr@POPBASE, _diff;
    unless _open_seg_free_ptr == _open_seg_base_ptr then
        mishap(0, 'SYSTEM ERROR IN Allow_shrim_mapping')
    endunless;
    _temp_userstack -> _userhi;
    _temp_userstack -> _user_sp();
    Set_mem_break(_base, _0) -> ;
    domap_p();                      ;;; may map some images
    @@(vpage){Get_mem_break(), _base} -> _diff;
    unless Set_userhi_to(_save_userhi@(vpage){_diff}, false) then
        mishap(0, 'NO MEMORY AVAILABLE TO PERFORM OPERATION')
    endunless;
    _userhi -> _user_sp();
    if _nonzero(_diff) then
        ;;; mapped some images
        Set_open_seg_after_exmem(_base@(vpage){_diff}, _base)
    endif
enddefine;

#_ENDIF

define lconstant Open_seg_blocked(_try_leapfrog);
    lvars _try_leapfrog;
    Get_user_break() /== _userhi
    and not(_try_leapfrog and Try_open_seg_leapfrog())
enddefine;

define lconstant Open_seg_unblocked();
    Get_user_break() /== _userhi and Try_open_seg_leapfrog()
enddefine;

#_IF _pint(@@(limpage)++) == 0

    ;;; Not using a userstack lim page -- need this procedure to be
    ;;; chained by _call_external when the break has changed during
    ;;; an external call
define Mem_break_changed();
    lconstant ms = 'WARNING: NO MEMORY AVAILABLE TO POSITION USER STACK FOR UNDERFLOW CHECKING\n';
    if Open_seg_blocked(true) then
        fast_syswrite(popdeverr, 1, ms, datalength(ms))
    endif
enddefine;

#_ENDIF

define Ensure_open_seg_open();
    if Open_seg_blocked(true) then
        mishap(0, 'NO MEMORY AVAILABLE TO PERFORM OPERATION')
    endif
enddefine;

    /*  Signed number of words by which the open seg needs to be
        expanded to leave _nwords free, with safety margin.
    */
define Open_seg_expand_for(_nwords);
    lvars _nwords;
    ##(w)-{ _user_sp(), _open_seg_free_ptr@(w){_USER_SAFEGAP} } _add _nwords
enddefine;

define Expand_open_seg(_nwords, _minwords);
    lvars _nwords, _minwords, _break;
    ;;; test if open seg expansion now blocked by external memory
    ;;; and can't leapfrog
    returnif(Open_seg_blocked(false)) (_zero(_minwords));
    ;;; else try expansion
    ;;; free the limit page
    FREE_LIM_PAGE;
    ;;; set the new break
    Set_user_break(_userhi@(w)[_nwords], @@(w)[_minwords _sub _nwords])
                                                        -> _break;
    if _break == _-1 then
        false
    else
        ;;; shift userstack up by actual expansion amount
        _move_userstack(@@(w){_break, _userhi});
        true
    endif;
    ;;; relock the limit page
    LOCK_LIM_PAGE
enddefine;

define lconstant Contract_open_seg(_nwords);
    lvars _nwords, _maxcnt, _break;
    ;;; do nothing if open seg now closed off by external memory
    returnif(Open_seg_blocked(false));

    ;;; ensure contraction leaves at least some free space
    _negate(Open_seg_expand_for(_2:1e11)) -> _maxcnt;
    if _maxcnt _slt _nwords then _maxcnt -> _nwords endif;
    returnif(_nwords _slteq _0);

    ;;; free the limit page
    FREE_LIM_PAGE;
    ;;; shift the userstack down first before setting the break
    _move_userstack(@@(w)-[_nwords]);       ;;; alters _userhi
    ;;; set the real break and correct for it
    Set_user_break(_userhi, _0) -> _break;
    ;;; if it failed to contract (?), leave things as they were
    if _break == _-1 then _userhi@(w)[_nwords] -> _break endif;
    _move_userstack(@@(w){_break, _userhi});

    ;;; relock the limit page
    LOCK_LIM_PAGE
enddefine;

define lconstant Ensure_open_seg_free(_nwords);
    lvars _need = Open_seg_expand_for(_nwords), _nwords;
    _need _slteq _0 or Expand_open_seg(_need, _need)
enddefine;


lconstant macro NOMORE_STACK_ARGS = [
        'rom: NO MORE MEMORY AVAILABLE (needing stack space)',
        'userstack:mem-nomore'
];

    /*  Ensure there's a minimum amount of userstack space
        (16 words, enough for ordinary procedure calls in the garbage
        collector, etc).
    */
define Ensure_min_userstack();
    unless Ensure_open_seg_free(_16 _sub ##(w){_USER_SAFEGAP}) then
        mishap(0, NOMORE_STACK_ARGS)
    endunless
enddefine;

    /*  Close off the open segment and start a new one
    */
define Close_open_seg(_want_seg);
    lvars _lim, _ofree, _want_seg, _seg;

    unless _want_seg then
        returnif(_open_seg_free_ptr == _open_seg_base_ptr) (true)
    endunless;

    ;;; ensure enough space to leave 1 page free and then round
    ;;; to a page boundary (must have 1 page free for -sys_lock_system-)
    _seg_table_next_free -> _seg;
    _open_seg_free_ptr@POPBASE -> _ofree;
    if Ensure_open_seg_free(##(w){_ofree@(w.r->vpage)++, _ofree}) then
        ;;; i.e. non-empty -- round seg size
        ( _open_seg_free_ptr@POPBASE@(w.r->vpage)++ )@~POPBASE -> _lim;
        ;;; add to segment table (area base address and size)
        Add_seg_entry(_open_seg_base_ptr, _open_seg_free_ptr,
                                @@(w){_lim, _open_seg_base_ptr}, _0);
        _lim ->> _open_seg_base_ptr -> _open_seg_free_ptr;
        _lim@(w){_USER_SAFEGAP} -> _userlim
    else
        returnunless(Open_seg_unblocked()) (false)
    endif;
    if _want_seg then _seg else true endif
enddefine;

define Open_seg_shift_gap_base();
    lvars _seg = _seg_table_next_free--@(struct SEG);
    if _seg >=@(struct SEG) _lowest_heap_seg
    and _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS then
        ;;; allow for removal of reserved space in the segment
        _seg!SEG_FREE_PTR@POPBASE@(w.r->vpage)@~POPBASE
    else
        _open_seg_base_ptr
    endif
enddefine;

define lconstant Adjust_fixed_seg(_gapbase, _add_to_fixed);
    lvars   _seg = _seg_table_next_free--@(struct SEG), _gapbase, _gapsize,
            _lim, _add_to_fixed;
    if _seg >=@(struct SEG) _lowest_heap_seg
    and _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS then
        if _add_to_fixed then _open_seg_base_ptr else _gapbase endif -> _lim;
        if _zero(@@(vpage){_lim, _seg!SEG_BASE_PTR} ->> _seg!SEG_SIZE) then
            ;;; remove empty seg
            _seg -> _seg_table_next_free
        endif
    elseif _add_to_fixed then
        @@(vpage){_open_seg_base_ptr, _gapbase} -> _gapsize;
        unless _zero(_gapsize) then
            Add_seg_entry(dup(_gapbase), _gapsize, _M_SEG_FIXED_STRUCTS)
        endunless
    endif
enddefine;


    /*  Do a garbage collection that shifts the open seg to make
        a gap of word offset size _gapsize below it (_gapsize
        representing an exact number of pages). Any unused space
        in a fixed-address segment below is absorbed into the gap.
    */
define Do_open_seg_shift_gc(_gapsize, _add_to_fixed);
    lvars _tries = _2, _reloc, _gapsize, _need, _gapbase, _add_to_fixed;

    Open_seg_shift_gap_base() -> _gapbase;
    @@(vpage){_gapbase@(vpage){_gapsize}, _open_seg_base_ptr} -> _reloc;
    if _reloc _slteq _0 then
        ;;; shift is not up -- can do it with either garbage collector
        Gc$-Setup(_reloc, false) ->
    else
        ;;; shift is up -- requires the copying garbage collector
        until _neg(_tries _sub _1 ->> _tries) do
            if _tries /== _1 then
                ;;; do any gc to remove garbage
                Gc$-Setup(_0, false) ->, Gc$-Do_gc();
            endif;
            ;;; calculate shortfall in space below userstack
            if (Open_seg_expand_for(##(w){_reloc}) ->> _need) _sgr _0 then
                nextunless(Expand_open_seg(_need, _need))
            elseif _need _slt _0 and _tries /== _1 then
                Contract_open_seg(_negate(_need))
            endif;
            quitif(Gc$-Setup(_reloc, false))    ;;; can do it
        enduntil
    endif;
    if _neg(_tries) then
        ;;; can't do it
        false
    else
        ;;; complete the gc
        Gc$-Do_gc();
        Adjust_fixed_seg(_gapbase, _add_to_fixed);
        Reset_last_gc_data(false);
        true
    endif
enddefine;

define lconstant Add_fixed_seg(_segsize);
    lvars _segsize, _gapbase = _open_seg_base_ptr;
    _gapbase@(vpage){_segsize} ->> _open_seg_base_ptr -> _open_seg_free_ptr;
    _open_seg_free_ptr@(w){_USER_SAFEGAP} -> _userlim;
    Adjust_fixed_seg(_gapbase, true)
enddefine;

define Try_new_fixed_seg(_segsize, _minsize);
    lvars _segsize, _minsize, _seglim;

    if Open_seg_shift_gap_base() /== _open_seg_base_ptr then
        Do_open_seg_shift_gc(_0, true) ->
    endif;
    _seg_table_next_free -> _seglim;
    returnunless(Close_open_seg(false));
    if Open_seg_blocked(false) then
        if Open_seg_expand_for(##(w){_segsize}) _slteq _0 then
            Add_fixed_seg(_segsize);
            return
        else
            if _seg_table_next_free >@(struct SEG) _seglim then
                ;;; must be for old open seg
                Detab_open_seg()
            endif;
            Try_open_seg_leapfrog() ->
        endif
    endif;
    lvars _need = Open_seg_expand_for(##(w){_segsize}), _minneed;
    if _need _sgr _0 then
        _need _sub ##(w){_segsize _sub _minsize} -> _minneed;
        if _neg(_minneed) then _0 -> _minneed endif;
        Expand_open_seg(_need, _minneed) ->
    endif;
    lvars _got = @@(w)-[Open_seg_expand_for(_0)];
    if _got _sgr _segsize then _segsize -> _got endif;
    @@(vpage){_got|w.t} -> _got;
    if _got _sgreq _minsize then
        Add_fixed_seg(_got)
    endif
enddefine;


;;; --- GARBAGE COLLECTOR INTERFACE ------------------------------------

lconstant macro FREEWEAK = [weakref %"["% free_block_key %"]"%];

weak vars
    free_block_total_mem,
    _Lgc_allowed_fxd_alloc,
    ;

lvars
    _Lgc_allowed_rel_incr   = _0,

    ;;; Last gc data
    _Lgc_end_time       = _0,   ;;; Time after last gc
    _Lgc_fxd_used       = _0,   ;;; Fixed used after last gc
    _Lgc_all_used       = _0,   ::: All used after last gc

    ;;; 'Weighted' totals
    _Wtd_gc_time        = _1,
    _Wtd_prog_time      = _0,
    _Wtd_fxd_creat      = _0,
    _Wtd_all_creat      = _0,

    ;;; Variables for current memory statistics
    _Ms_stk_used,       ;;; words of userstack
    _Ms_fxd_used,       ;;; words used in fixed segments (excl locked)
    _Ms_all_used,       ;;; words used in all segments (excl locked)
    _Ms_fxd_free_res,   ;;; words free in fxd excl free blocks (excl locked)
    _Ms_fxd_free,       ;;; words free in fixed segments (excl locked)
    _Ms_all_free,       ;;; words free in all segments (excl locked)
    _Ms_locked,         ;;; total words in locked segments
    _Ms_total,          ;;; total words in heap
    ;

    /*  Called when the open seg is empty to set up an initial fixed seg
    */
define Initial_fixed_seg();
    lvars _nwords, _size;
    returnunless(testdef free_block_key);
    _shift(_negate(Open_seg_expand_for(_0)), _-2) -> _nwords;
    if _nwords _gr _2:1e13 then _2:1e13 -> _nwords endif;
    @@(vpage)[_nwords | w.t] -> _size;
    unless _zero(_size) then
        Add_fixed_seg(_size);
        ##(w){_size} -> FREEWEAK _Lgc_allowed_fxd_alloc
    endunless
enddefine;


define lconstant Set_mem_stats();
    lvars   _seg = _seg_table, _len, _tot = _0, _used = _0, _fxd_tot = _0,
            _locked = _0, _fxd_alloc = _0;
    ;;; closed segments
    while _seg <@(struct SEG) _seg_table_next_free do
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP
        or _seg!SEG_BASE_PTR <@(w) _system_end then
            ##(w){_seg!SEG_SIZE} -> _len;
            _tot _add _len -> _tot;
            if _seg <@(struct SEG) _lowest_heap_seg then
                ;;; locked
                _locked _add _len -> _locked
            elseif _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS then
                _fxd_tot _add _len -> _fxd_tot;
                ##(w){_seg!SEG_FREE_PTR, _seg!SEG_BASE_PTR}
                                                _add _fxd_alloc -> _fxd_alloc
            else
                ##(w){_seg!SEG_FREE_PTR, _seg!SEG_BASE_PTR}
                                                _add _used -> _used
            endif
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile;

    ;;; open segment
    ##(w){_userhi@~POPBASE, _open_seg_base_ptr} _add _tot -> _tot;
    ;;; used size includes the stack
    ##(w){_stklength()} -> _Ms_stk_used;
    ##(w){_open_seg_free_ptr, _open_seg_base_ptr} _add _Ms_stk_used
                                            _add _used -> _used;

    ;;; set globals
    _fxd_tot _sub _fxd_alloc -> _Ms_fxd_free_res;
    if _nonzero(_fxd_alloc) then
        _fxd_alloc _sub _int(FREEWEAK free_block_total_mem)
    else
        _0
    endif -> _Ms_fxd_used;
    _Ms_fxd_used _add _used -> _Ms_all_used;
    _fxd_tot _sub _Ms_fxd_used -> _Ms_fxd_free;
    _tot _sub _locked _sub _Ms_all_used -> _Ms_all_free;
    _locked -> _Ms_locked;
    _tot -> _Ms_total
enddefine;

define Reset_last_gc_data(_new_system);
    lvars _new_system;
    if _new_system then
        _1  -> _Wtd_gc_time;
        _0 ->> _Wtd_prog_time ->> _Wtd_fxd_creat -> _Wtd_all_creat
    endif;
    Set_mem_stats();
    _Ms_all_used -> _Lgc_all_used;
    _Ms_fxd_used -> _Lgc_fxd_used;
    _int(systime()) -> _Lgc_end_time;
    _0 -> _Lgc_allowed_rel_incr;
    _Ms_fxd_free_res -> FREEWEAK _Lgc_allowed_fxd_alloc
enddefine;

define Heap_lock_change(_lockrec, _locking);
    lvars _lockrec, _locking, _all, _fxd;
    if _locking then
        ;;; locking -- subtract from Lgc totals
        Set_mem_stats();
        _Ms_all_used _sub _Ms_stk_used ->> _all -> _lockrec!LOCK_ALL_USED;
        _Ms_fxd_used ->> _fxd -> _lockrec!LOCK_FXD_USED
    else
        ;;; unlocking -- add back to Lgc totals
        _negate(_lockrec!LOCK_ALL_USED) -> _all;
        _negate(_lockrec!LOCK_FXD_USED) -> _fxd
    endif;
    _Lgc_all_used _sub _all -> _Lgc_all_used;
    _Lgc_fxd_used _sub _fxd -> _Lgc_fxd_used
enddefine;

define Sysgarbage(_do_mem_adjust, why);
    lvars   gapsize_arg, _start_time, _gapsize, _size,
            _save_disable = _disable, _do_mem_adjust, _actual_gap,
            _last_Lgc_end_time;
    dlvars  why, _gc_time, _prog_time, _fxd_creat, _all_creat;
    lconstant GC_str = 'GC-%p', cont_str = ';;;\s\s\s\s\s\s\s\s\s\s\s\s';
    dlocal  pop_exception_final = Last_chance_exception_final,
            pr = sys_syspr,
            pop_pr_quotes = false,
            poplineprefix = #_< conspair(';;;\s', cont_str) >_#,
            cucharout, _disable, _inhibit_gc;

    define lconstant Flush_devs();
        if cucharout == charout then
            sysflush(pop_charout_device)
        elseif cucharout == charerr then
            sysflush(pop_charerr_device)
        endif
    enddefine;

    define lconstant Print_stats();
        lconstant macro (_FIXD = _2:0010, _CALL = _2:0101, _PLOG = _2:1001);
        lvars _options = _0, _uns, _tths, _hths, _gctype;
        dlocal poplinewidth, _disable;
        if cucharout == charout or cucharout == charerr then
            _DISABLE_ALL -> _disable
        else
            printf(why, GC_str)
        endif;
        if isinteger(popgctrace) then _int(popgctrace) -> _options endif;
        if isinteger(poplinewidth) then poplinemax -> poplinewidth endif;

        if Gc$- _copy_gc then `C` else `N` endif -> _gctype;
        (_gc_time _div _100 -> _uns) _div _10 -> _tths -> _hths;
        printf(_hths, _tths, _uns, _gctype, '(%c) %d.%d%d  MEM: ');
        if _nonzero(_Ms_locked) then printf(_Ms_locked, 'l %d + ') endif;
        printf( _Ms_total,
                _Ms_stk_used,
                _Ms_all_free,
                _Ms_all_used _sub _Ms_stk_used,
                'u %d + f %d + s %d = %d\n');

        #_< conspair(cont_str, cont_str) >_# -> poplineprefix;

        if _options _bitst _FIXD then
            printf( _Ms_fxd_used _add _Ms_fxd_free,
                    _Ms_fxd_free_res,
                    _Ms_fxd_free _sub _Ms_fxd_free_res,
                    _Ms_fxd_used,
                'FIXED: u %d + fb %d + f %d = %d\n')
        endif;

        if _options _bitst _CALL then
            printf( ##(csword){_call_stack_hi, _sp()},
                'CALLSTACK: %d\n');
        endif;

        if _options _bitst _PLOG then
            lvars _contn_sp = _plog_contn_barrier@(w){_plog_contn_sp};
            printf( ##(w){_plog_area_hi, _plog_area_lo},
                    ##(w){_plog_area_hi, _contn_sp},
                    ##(w){_contn_sp, _plog_trail_sp},
                    ##(w){_plog_trail_sp, _plog_area_lo},
                'PROLOG: trail %d + free %d + contn %d = %d\n');
        endif;

        Flush_devs()
    enddefine;      /* Print_stats */

    define lconstant Calc_allowed_incrs();
        lvars _ptime, _gtime, _incr, _f, _free, _max, _neggap, _gc_ratio;

        define lconstant weight_val(_inc, _last);
            lvars _inc, _last;
            ;;; (5*inc + 3*last)/8
            _shift(_shift(_inc _add _last, _2) _add _inc _sub _last, _-3)
        enddefine;

        define lconstant ratio_mult(_x, _n, _d);
            lvars _x, _n, _d, (_r, _q) = _n _div _d;
            if _q _sgr _2:1e8 then
                _2:1e8 -> _q
            elseif _q _slt _-2:1e8 then
                _-2:1e8 -> _q
            endif;
            (_x _mult _q) _add ((_x _mult _r) _divq _d)
        enddefine;

        popgcratio -> _gc_ratio;
        unless isinteger(_gc_ratio) and 1 fi_<= _gc_ratio then
            DEFAULT_GC_RATIO -> _gc_ratio
        elseunless _gc_ratio fi_<= 2:1e10 then
            2:1e10 -> _gc_ratio
        endunless;
        _gc_ratio -> popgcratio;

        ;;; updated decaying averages with figures for this gc
        weight_val(_gc_time,    _Wtd_gc_time)   -> _Wtd_gc_time;
        weight_val(_prog_time,  _Wtd_prog_time) -> _Wtd_prog_time;
        weight_val(_fxd_creat,  _Wtd_fxd_creat) -> _Wtd_fxd_creat;
        weight_val(_all_creat,  _Wtd_all_creat) -> _Wtd_all_creat;

        ;;; ensure all >= fixed (so (all<<7)/fixed computed below
        ;;; can't be zero)
        if _Wtd_fxd_creat _sgr _Wtd_all_creat then
            _Wtd_fxd_creat -> _Wtd_all_creat
        endif;

        if _zero(_Wtd_prog_time ->> _ptime) then
            _0
        else
            _Wtd_gc_time _mult _int(_gc_ratio) -> _gtime;
            ratio_mult(_Wtd_all_creat, _gtime _sub _ptime, _ptime) -> _incr;
            ratio_mult(_incr, _prog_time, _ptime) -> _incr;
            if _neg(_incr) then
                ;;; contract much more slowly ...
                _shift(_incr, _-2) -> _incr;
                if _incr _slt _-2:1e18 then _-2:1e18 else _incr endif
            else
                if _incr _sgr _2:1e18 then _2:1e18 else _incr endif
            endif
        endif _add _all_creat _sub _Ms_all_free -> _incr;   ;;; desired incr

        _int(max_mem_lim) _sub _Ms_total -> _max;
        if _max _slt _incr then _max -> _incr endif;

        ;;; translate incr into new total free space
        if _neg(_Ms_all_free _add _incr ->> _free) then _0 -> _free endif;
        ;;; then split that according to the fixed/all use ratio
        if _zero(_Wtd_fxd_creat) then
            _0
        else
            _shift(_Wtd_all_creat, _7) _divq _Wtd_fxd_creat -> _f;
            if _zero(_f) then   ;;; just in case
                _free
            else
                _shift(_free, _7) _divq _f
            endif
        endif -> _f;
        ;;; incr for fixed
        (_Ms_fxd_used _add _f) _sub _Ms_fxd_free -> _f;
        ##(w){Open_seg_shift_gap_base(), _open_seg_base_ptr} -> _neggap;
        if _neg(_f) and _negate(_Ms_fxd_free) == _neggap then
            _0 -> _f
        elseif _f _slt _neggap then
            ;;; can contract fixed only by _neggap
            _neggap -> _f
        endif;
        ;;; incr for relocatable
        _incr _sub _f -> _Lgc_allowed_rel_incr;
        ;;; incr for fixed
        _f _add _Ms_fxd_free_res -> FREEWEAK _Lgc_allowed_fxd_alloc
    enddefine;      /* Calc_allowed_incrs */


    false -> gapsize_arg;
    if isinteger(why) then
        ;;; gapsize arg specified
        ((), _do_mem_adjust, why) -> (_do_mem_adjust, why, gapsize_arg)
    endif;

    returnif(_inhibit_gc);
    Sysgarbage -> _inhibit_gc;  ;;; says inside Sysgarbage
    _DISABLE_ALL -> _disable;

    if popgctrace then
        if cucharout == identfn then charout -> cucharout endif;
        if cucharout == charout or cucharout == charerr then
            if (if cucharout == charout then pop_charout_col
                else pop_charerr_col
                endif) /== 0
            then
                cucharout(`\n`)
            endif;
            printf(why, GC_str), Flush_devs()
        endif
    endif;

    _int(systime()) -> _start_time;
    Set_mem_stats();
    _Ms_all_used _sub _Lgc_all_used -> _all_creat,
    _Ms_fxd_used _sub _Lgc_fxd_used -> _fxd_creat;
    _Lgc_end_time -> _last_Lgc_end_time;

    if gapsize_arg and gapsize_arg fi_>= 0 then
        _int(gapsize_arg) -> _gapsize
    else
        @@(vpage){_open_seg_base_ptr, Open_seg_shift_gap_base()}
                                            ->> _actual_gap -> _gapsize;
        if _do_mem_adjust == true and testdef free_block_key then
            ;;; if gapsize_arg then arg is neg -- called from Alloc_store.
            ;;; Decrement gap by given amount if over popmemlim
            if gapsize_arg
            and _int(max_mem_lim) _slt _Ms_total _sub ##(w){_int(gapsize_arg)}
            then
                 _gapsize _add _int(gapsize_arg) -> _gapsize
            endif;
            FREEWEAK _Lgc_allowed_fxd_alloc _sub _Ms_fxd_free_res
                                        _add ##(w){_actual_gap} -> _size;
            @@(vpage)[_size|w.r] -> _size;
            if _size _slt _gapsize then _size -> _gapsize endif;
            if _neg(_gapsize) then _0 -> _gapsize endif;

            lvars   _d = _actual_gap _sub _gapsize,
                    _x = @@(vpage)[_Ms_fxd_free_res _sub _2:1e11 | w.t];
            if _x _slt _d then _x -> _d endif;
            if _neg(_d) then _0 -> _d endif;
            _actual_gap _sub _d -> _gapsize
        endif
    endif;

    if testdef $-Sys$-Xt$-XptGarbageHandler then
        weakref $-Sys$-Xt$-XptGarbageHandler(why);
    endif;

    ;;; Calls Gc$-Do_gc, which calls Reset_last_gc_data (which calls
    ;;; Set_mem_stats)
    Do_open_seg_shift_gc(_gapsize, true) -> ;

    if testdef $-Sys$-Xt$-XptGarbageHandler then
        weakref $-Sys$-Xt$-XptGarbageHandler(false);
    endif;

    if _zero(_Lgc_end_time _sub _start_time ->> _gc_time) then
        _1 -> _gc_time
    endif;
    _gc_time _add _start_time _sub _last_Lgc_end_time -> _prog_time;

    if _do_mem_adjust == true then      ;;; not for sysgarbage
        popgctime fi_+ _pint(_gc_time) -> popgctime;
        Calc_allowed_incrs()
    endif;
    if _do_mem_adjust then
    RETRY:
        lvars _incr = _Lgc_allowed_rel_incr, _diff, _save_userhi = _userhi;
        ;;; do any contraction, but ensure upto popminmemlim
        _int(min_mem_lim) _sub _Ms_total -> _diff;
        if _diff _sgr _0 then
            if Open_seg_unblocked() then
                Set_mem_stats();
                goto RETRY
            endif;
            _incr _sub _diff -> _incr;
            Expand_open_seg(_diff, _0) ->
        else
            if _diff _sgr _incr then _diff -> _incr endif;
            if _incr _slt _0 then
                Contract_open_seg(_negate(_incr))
            endif
        endif;
        if _incr _slt _0 then _0 else _incr endif -> _Lgc_allowed_rel_incr;

        ##(w){_save_userhi, _userhi} -> _diff;
        _Ms_total _sub _diff -> _Ms_total;
        _Ms_all_free _sub _diff -> _Ms_all_free
    endif;

    _save_disable -> _disable;
    _pint(_Ms_all_used) -> popmemused;
    if popgctrace then Print_stats() endif;

    chain(  procedure;
                Close_filetab_files(false);     ;;; close garbage files
                if DPTEST then DPWEAK Do_destroy_actions() endif;
                _CHECKINTERRUPT;
                ;;; run user procedure after a gc
                chain(pop_after_gc)
            endprocedure)
enddefine;

define sysgarbage();
    Sysgarbage(0, 'user')
enddefine;

define active popmemlim;
    max_mem_lim
enddefine;
;;;
define updaterof active popmemlim n;
    lvars n;
;;;        lconstant _MAX_MEM_MAX = _int(1<<POPINT_BITS - 1);
    if isbiginteger(n) then
        if weakref[biginteger_key] Bigint_neg(n) then
            mishap(n, 1, '(BIG)INTEGER >= 0 NEEDED', ':type-integral-nonneg')
        else
            if n > #_< 1<<WORD_COUNT_BITS  - 1 >_# then
                false -> n
            else
               _pint(Pint_->_uint(n,
                  #_< _int(1<<WORD_COUNT_BITS - 1) >_#)) -> n
            endif
        endif
    endif;
    if n then
        Check_integer(n, 0)
    else
        ;;; take false to mean max possible
        #_< 1<<WORD_COUNT_BITS - 1 >_# -> n
    endif;
    Set_mem_stats();
    if _int(n) _slt _Ms_total then
        _pint(_Ms_total)
    else
        n
    endif -> max_mem_lim
enddefine;

define active popminmemlim;
    min_mem_lim
enddefine;
;;;
define updaterof active popminmemlim n;
    lvars n;
    Check_integer(n, 0);
    n -> min_mem_lim
enddefine;


;;;--------- ALLOCATING STORE FOR AND INITIALISING STRUCTURES ---------------

define Set_curr_heap_seg(_seg);
    lvars _seg, _base;
    _seg -> _curr_heap_seg;
    if _seg /== _NULL then
        _seg!SEG_FREE_PTR -> _curr_seg_free_ptr;
        _seg!SEG_BASE_PTR -> _base;
        if _seg!SEG_FLAGS _bitst _M_SEG_SPECIAL then
            ;;; special seg (e.g. fixed dummy or save/restore)
            _base@(w){_seg!SEG_SIZE}
        else
            ;;; we always ensure there is at least 1 virtual page free
            ;;; in any segment (necessary for sys_lock_system)
            _base--@(vpage){_seg!SEG_SIZE}
        endif -> _curr_seg_free_lim
    endif
enddefine;

    /*  Find space in a heap segment somewhere
    */
define lconstant Find_heap_space(_size);
    lvars _seg = _lowest_free_heap_seg, _ptr, _lim, _size;

    ;;; try closed segments
    while _seg <@(struct SEG) _seg_table_next_free do
        if _seg!SEG_FLAGS _bitst
                (_M_SEG_NON_POP _biset _M_SEG_FIXED_STRUCTS) then
            if _seg == _lowest_free_heap_seg then
                _seg@(struct SEG)++ -> _lowest_free_heap_seg
            endif
        else
            _seg!SEG_FREE_PTR@(w){_size} -> _ptr;
            ;;; we always ensure there is at least 1 virtual page free
            ;;; in any segment (necessary for -sys_lock_system-)
            _seg!SEG_BASE_PTR--@(vpage){_seg!SEG_SIZE} -> _lim;
            if _ptr <=@(w) _lim then
                ;;; space here
                _seg -> _curr_heap_seg;
                _seg!SEG_FREE_PTR;      ;;; return this
                _lim -> _curr_seg_free_lim;
                _ptr ->> _curr_seg_free_ptr -> _seg!SEG_FREE_PTR;
                return
            elseif _seg == _lowest_free_heap_seg
            and _seg!SEG_FREE_PTR@(w)[_16] >@(w) _lim then
                ;;; don't bother with any seg that has less than 16 words
                ;;; free
                _seg@(struct SEG)++ -> _lowest_free_heap_seg
            endif
        endif;
        _seg@(struct SEG)++ -> _seg
    endwhile;

    ;;; no space in any closed heap segment -- try the open one
    _open_seg_free_ptr@(w){_size} -> _ptr;
    if _user_sp() >=@(w) _ptr@(w){_USER_SAFEGAP} then
        ;;; space in open segment
        _NULL -> _curr_heap_seg;
        _open_seg_free_ptr;             ;;; return this
        _ptr -> _open_seg_free_ptr;
        _ptr@(w){_USER_SAFEGAP} -> _userlim ;;; push up userstack limit
    else
        _NULL
    endif
enddefine;

    /*  Allocate space by doing any allowable open seg expansion (computed
        at last garbage collection), or invoking a garbage collection
    */
define lconstant Alloc_store(_heap_need, _openseg_need);
    lvars _need, _allowed, _heap_need, _free, _openseg_need;

    ;;; See float.p and comments below
    lconstant macro _SFVALS = [(_sf1,_sf2,_sf3,_sf4,_sf5,_sf6,_sf7,_sf8,_sf9,_sf10)];
    lvars _sf1, _sf2, _sf3, _sf4, _sf5, _sf6, _sf7, _sf8, _sf9, _sf10;

    ;;; allowed expansion computed at last GC
    _Lgc_allowed_rel_incr -> _allowed;

    if (_allowed _sgr _0 or _inhibit_gc) and Open_seg_unblocked() then
        ;;; new open seg created -- ask caller to retry
        return(false)
    endif;

    ;;; compute total expansion needed in the open seg
    Open_seg_expand_for(_heap_need _add _openseg_need) -> _need;
    if _need _slteq _0 then _1 -> _need endif;  ;;; just in case ...

    ;;; do any allowable expansion and return true if enough
    if _inhibit_gc and _allowed _slt _need then _need -> _allowed endif;
    _0 -> _Lgc_allowed_rel_incr;
    returnif(_allowed _sgreq _need and Expand_open_seg(_allowed, _need))
                                                                    (true);

    ;;; insufficient gained thru expansion, must garbage collect
    lvars _save_userhi = _userhi;
    if _allowed _sgr _need then _allowed -> _need endif;

    if testdef ddecimal_key then
        ;;; Save the values of the fixed floating-point operands across
        ;;; the garbage collection (and any user procedures that may get
        ;;; called). Doing it here means their values don't have to be
        ;;; localised in procedures that use them, and works because
        ;;; those procedures are non-interruptible apart from calls to
        ;;; Get_store. Also note we don't want these values dlocalised in
        ;;; the normal way, because (a) abnormal exits make them junk and
        ;;; (b) process suspend/resume wants to restore the values as they
        ;;; are outside of this procedure.
        weakref[ddecimal_key] Float_opnd_vals() -> _SFVALS
    endif;

    ;;; recomputes allowed incr and may contract
    Sysgarbage(true, 'auto', _pint(@@(vpage)-[_need|w.r]));

    if testdef ddecimal_key then
        ;;; Restore floating-point operand values
        _SFVALS -> weakref[ddecimal_key] Float_opnd_vals()
    endif;

    ;;; reget allowed expansion computed by GC
    _Lgc_allowed_rel_incr -> _allowed;

    ;;; deal with heap need, tranferring it to openseg need unless it can
    ;;; be satisfied in another seg
retry:
    _openseg_need -> _need;
    if _heap_need _sgr _0 then
        if (Find_heap_space(@@(w)[_heap_need]) ->> _free) /== _NULL then
            _free -> Get_store()        ;;; free the allocated space
        endif;
        if _free == _NULL or _curr_heap_seg == _NULL then
            _heap_need _add _need -> _need
        endif
    endif;

    ;;; then deal with openseg need

    ;;; (the 8 here is just a small safety margin)
    _negate(Open_seg_expand_for(_8)) -> _free;

    if _int(max_mem_lim) _slt _Ms_total and _userhi >@(w) _save_userhi then
        ;;; discount any forced expansion caused by printing gc mess, etc,
        ;;; so as to prevent it not failing on popmemlim when it should
        ##(w){_save_userhi, _userhi} _add _free -> _free
    endif;

    if (_need _sub _free ->> _need) _sgr _0 then
        ;;; GC didn't reclaim enough
        if _int(max_mem_lim) _slt _Ms_total _add _need then
            ;;; 1 return means over popmemlim (or would go over)
            return(1)
        elseif Open_seg_unblocked() then
            ;;; new open seg created -- recompute
            goto retry
        else
            ;;; force minimum expansion needed (may fail)
            if Expand_open_seg(_need, _need) then
                _allowed _sub ##(w){@@(vpage)[_need|w.r]} -> _allowed
            endif;
            if _neg(_allowed) then _0 else _allowed endif
                                                -> _Lgc_allowed_rel_incr
        endif
    endif;

    true        ;;; says Alloc_store has done all it can
enddefine;


#_IF DEF DEBUG
constant procedure ($-random0, Print_doing_list);
vars $-ranseed;

vars
    $-pop_gc_every_alloc = false,
    $-pop_gc_every_alloc_print = true,
;

lconstant NCALLERS = 10;
constant $-last_callers_vec = writeable initv(NCALLERS);

weak vars procedure $-wved_mishap_reset;
vars charout_dev;

define lconstant every_alloc();
    lvars   i, p, props;
    dlocal  weakref wved_mishap_reset = identfn, cucharout = charout,
            charout_dev = dev_out;
    returnif(iscaller(sys_raise_exception));
    if isinteger(pop_gc_every_alloc) then
        unless isinteger(ranseed) then 0 -> ranseed endunless;
        returnunless(random0(pop_gc_every_alloc) == 0)
    endif;
    Sysgarbage(0, 'dbug');
    fast_for i to NCALLERS do
        caller(i) -> p;
        recursive_front(p!PD_PROPS) -> props;
        if pop_gc_every_alloc_print then
            if props then
                sys_syspr(props)
            elseif p >=@(w) _system_end then
                sys_syspr(false)
            else
                printf(p, '%x')
            endif;
            cucharout(`\s`);
        endif;

        if isstring(props) then
            props
        elseif isword(props) then
            fast_word_string(props)
        elseif p >=@(w) _system_end then
            false
        else
            p
        endif -> fast_subscrv(i,last_callers_vec);
    endfor;
    if pop_gc_every_alloc_print then
        cucharout(`\n`)
    endif
enddefine;

#_ENDIF


define Alloc_user_space(_size);
    lvars _size, _res;
#_IF DEF DEBUG
    if pop_gc_every_alloc then every_alloc() endif;
#_ENDIF
    repeat
        returnif(@@(w){_user_sp(), _userlim} _sgreq _size);
        quitif(Alloc_store(_0, ##(w){_size}) ->> _res)
    endrepeat;
    returnif(_res /== 1 and @@(w){_user_sp(), _userlim} _sgreq _size);
    ;;; else no can do
    Expand_open_seg(_512, _0) -> ;
    unless Ensure_open_seg_free(_16) then
        clearstack()
    endunless;
    mishap(0, if _res == 1 then
                'rom: MEMORY LIMIT (popmemlim) EXCEEDED (using stack space)',
                'userstack:mem-limit'
              else
                NOMORE_STACK_ARGS
              endif)
enddefine;

    /*  Get _size word offset units of store from heap space
    */
define Get_store(_size);
    lvars _size, _ptr, _seg;
#_IF DEF DEBUG
    if pop_gc_every_alloc
    and (_curr_heap_seg == _NULL or _curr_seg_free_ptr /== _NULL) then
        every_alloc()
    endif;
#_ENDIF

    if _curr_heap_seg == _NULL then
        ;;; allocating space from the open segment
        _open_seg_free_ptr@(w){_size} -> _ptr;
        if _user_sp() >=@(w) _ptr@(w){_USER_SAFEGAP} then
            ;;; space here
            _open_seg_free_ptr;             ;;; return this
            _ptr -> _open_seg_free_ptr;
            _ptr@(w){_USER_SAFEGAP} -> _userlim;    ;;; push up userstack limit
            return
        endif
    else
        ;;; allocating from current segment, or fixed-address
        ;;; (if latter, then _curr_seg_free_ptr = _curr_seg_free_lim = _NULL,
        ;;; so next test will fail).
        _curr_seg_free_ptr@(w){_size} -> _ptr;
        if _ptr <=@(w) _curr_seg_free_lim then
            ;;; space here
            _curr_seg_free_ptr;         ;;; return this
            ;;; keep seg entry updated also so we can change seg at any time
            ;;; by just assigning to _curr_heap_seg
            _ptr ->> _curr_seg_free_ptr -> _curr_heap_seg!SEG_FREE_PTR;
            return
        elseif _curr_seg_free_ptr == _NULL then
            ;;; fixed-address allocation
            chain(_size, FREEWEAK Get_store_fixed)
        elseif _curr_heap_seg!SEG_FLAGS _bitst _M_SEG_SPECIAL then
            ;;; error if we run out of space in a special seg
            mishap(if _curr_heap_seg == _nr_seg_entry then
                    ' (process arguments and/or env vars too many/too large)'
                   else nullstring
                   endif, 1,
                   '%SPECIAL FIXED-SIZE MEMORY SEGMENT EXHAUSTED%S',
                   'heap-special:mem-fixedsize')
        endif
    endif;

    ;;; Find space in a heap segment somewhere, or expand the open segment
    repeat
        returnif((Find_heap_space(_size) ->> _ptr) /== _NULL) (_ptr);
        quitif(Alloc_store(##(w){_size}, _0) ->> _ptr)
    endrepeat;
    returnif(_ptr /== 1 and (Find_heap_space(_size) ->> _ptr) /== _NULL)(_ptr);
    ;;; else no can do
    Expand_open_seg(_512, _0) -> ;
    mishap(0, if _ptr == 1 then
                'rom: MEMORY LIMIT (popmemlim) EXCEEDED (using heap space)',
                'heap:mem-limit'
              else
                'rom: NO MORE MEMORY AVAILABLE (needing heap space)',
                'heap:mem-nomore'
              endif)
enddefine;

    /*  Return some or all of the last chunk allocated by Get_store
    */
define updaterof Get_store(/* _freeptr */) with_nargs 1;
    if _curr_heap_seg == _NULL then
        ;;; allocated from the open seg
        (->> _open_seg_free_ptr)@(w){_USER_SAFEGAP} -> _userlim
    else
        ;;; allocated from current seg
        ->> _curr_seg_free_ptr -> _curr_heap_seg!SEG_FREE_PTR
    endif
enddefine;

    /*  Get_store for a doubleword-aligned structure
    */
define Get_store_dalign(_woffs) -> _ptr;
    lvars _ptr, _str, _woffs;
    Get_store(_woffs _add @@(struct DOUBLE_PAD)++) -> _ptr;
    if _ptr@(w.t->d) == _ptr then
        ;;; _ptr already aligned -- add padding after _ptr
        double_pad_key -> _ptr!KEY{_woffs}
    else
        ;;; not aligned -- add padding before _ptr
        double_pad_key -> _ptr!KEY;
        _ptr@(struct DOUBLE_PAD)++ -> _ptr
    endif
enddefine;


    /*  Get a fixed length record
    */
define Get_record(key) -> rec;
    lvars rec, key;
    Get_store(key!K_RECSIZE_R) -> rec;
    key -> rec!KEY
enddefine;

    /*  Init a rawstruct for _nwords words of space
    */
define Init_rawstruct(_nwords) -> _rs;
    lvars _nwords, _rs, _wsize = @@V_WORDS[_nwords] _sub @@POPBASE;
    Get_store(_wsize) -> _rs;
    _wsize -> _rs!RAW_SIZE;
    rawstruct_key -> _rs!KEY
enddefine;

    /*  Make a filler struct
    */
define Make_filler_struct(_ptr, _wsize);
    lvars _ptr, _wsize;
    _wsize -> _ptr!RAW_SIZE;
    rawstruct_key -> _ptr!KEY
enddefine;


    /*  Make _nextfree_save and its saved values on the callstack NULL.
        Called by interrupt type procedures, to prevent memory being
        reused by Clawback
    */
define Zap_nextfree_save();
    lvars owner, _sframe, _lim;
    returnif(_nextfree_save == _NULL);      ;;; not in use
    _NULL -> _nextfree_save;
    _caller_sp_flush() -> _sframe;
    _call_stack_seg_hi -> _lim;
    repeat
        if _sframe == _lim then
            quitif(_lim == _call_stack_hi);
            _sframe!SF_NEXT_SEG_HI -> _lim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;

        _sframe!SF_OWNER -> owner;
        if owner!PD_FLAGS _bitst _:M_PD_SAVES_NEXTFREE then
            ;;; has _nextfree_save as a local -- NULL the saved value
            _NULL -> _sframe!(csword){
                        Dlocal_frame_offset(ident _nextfree_save, owner, true)}
        endif;
        _nextframe(_sframe) -> _sframe
    endrepeat
enddefine;


    /*  This applies a procedure in an environment that guarantees to save
        the values of all 'dlocal' register lvars (by dlocal'ing special
        dummy variables recognised by Popc).
        Called

            (a) by the garbage collector to ensure that all pop-type register
                values are saved in a stack frame (so the values get scanned)
            (b) By syssave/sys_lock_system to make sure that all reg values
                are saved on the stack.

        For SPARC, the problem is different: there are no 'dlocal' registers,
        but the stack frames above this point must be flushed out of the
        register file into memory. This is done by inserting SPARC_MAX_WINDOWS
        extra procedure calls.
    */
define Reg_save_apply(p);
    lvars procedure p;

#_IF DEF SPARC

    define lconstant flush_dummy(_count);
        lvars _count;
        if _count == 0 then
            fast_apply()    ;;; p on stack with args
        else
            flush_dummy(_count fi_- 1)
        endif
    enddefine;

    ;;; go into enough procedure calls to spill this stack frame
    ;;; out into memory.
    ;;; p is then applied with the address of caller's stack frame
    ;;; and address of return address into that frame
    flush_dummy(_caller_sp(), _sp()@SF_CALLER_RETURN, p, SPARC_MAX_WINDOWS)

#_ELSE

    ;;; Dlocal'ing these dummy variables causes Popc to make the
    ;;; all the corresponding registers local (has no effect in SPARC).

    dlocal \<ALL_POP_REGISTERS\>,  _\<ALL_NONPOP_REGISTERS\>;

    ;;; apply the procedure, passing address of this stack frame
    ;;; and address of return address into this frame
    lvars _sframe = _sp();
    p(_sframe, _sframe@SF_RETURN_ADDR)

#_ENDIF
enddefine;

    /*  Clear all freelists in the system, including free blocks table
        if wanted
    */
define Clear_freelists(_clear_free_blocks);
    lvars _clear_free_blocks, _vlen;
    if testdef _freevectab then
        _0 -> _vlen;
        while _vlen _lt _8 do
            0 -> weakref _freevectab!(w)[_vlen];
            _vlen _add _1 -> _vlen
        endwhile
    endif;
    if testdef _free_clos_tab then
        _0 -> _vlen;
        while _vlen _lt _17 do
            0 -> weakref _free_clos_tab!(w)[_vlen];
            _vlen _add _1 -> _vlen
        endwhile
    endif;
    0   ->> weakref _free_pairs
        ->  weakref _free_npfront_pairs;
    if testdef $-Sys$-Xt$-XptClearFreelists then
        weakref $-Sys$-Xt$-XptClearFreelists()
    endif;
    if testdef free_block_key and _clear_free_blocks then
        FREEWEAK Clear_free_blocks()
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Mar 13 2005
        Patch to updater of popmemlim provided by Waldek Hebisch
        for AMD64 poplog
--- Aaron Sloman, 1 Nov 2003
    increased default max_mem_lim (popmemlim) from 3e5 tp 15e5
--- John Gibson, Jun  1 1998
        Added nonwriteable_rawstruct_key for AIX
--- John Gibson, Aug 28 1996
        Made Get_store mishap if space in a special segment is exhausted.
--- John Gibson, Aug 20 1996
        Replaced _free_1*closures with _free_clos_tab
--- John Gibson, Apr  1 1996
        Added some mishap id-strings.
--- John Gibson, Mar  7 1996
        Improvements to Calc_allowed_incrs; default popgcratio to 40.
--- John Gibson, Mar  5 1996
        Changed Sysgarbage to use poplineprefix
--- John Gibson, Jan 27 1996
        Added declaration for _saved_curr_heap_seg
--- John Gibson, Sep 13 1995
        Changed updater of popmemlim to allow a +ve biginteger as equivalent
        to false.
--- John Gibson, Apr 25 1995
        Added _free_npfront_pairs
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 16 1995
        Added Init_rawstruct
--- John Gibson, Nov 18 1994
        Added Allow_shrim_mapping for VMS
--- John Gibson, Oct 20 1994
        Changed Reg_save_apply to use new Popc mechanism for making
        all registers local in non-SPARC systems.
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs, free*1closures -> _free_1*closures
--- John Gibson, Oct  8 1994
        Added more pop lvars to Reg_save_apply (now 16)
--- John Gibson, Sep 19 1994
        Changed Clear_freelists to clear new _freevectab (instead of old
        f*ree2vecs)
--- Robert John Duncan, Sep  5 1994
        Added definitions of Get_mem_break and Set_mem_break for Win32
--- John Gibson, Aug 24 1994
        Fixed Contract_open_seg so that mem configuration is left unchanged if
        O/S fails to contract memory
--- John Gibson, Nov  9 1993
        Initialised _userhi to temporary userstack
--- John Gibson, Oct 18 1993
        Made Try_open_seg_leapfrog set _M_SEG_NO_SAVE in the flags
        for external dynamic memory segments (so they're not saved/restored).
--- John Gibson, Jun 21 1993
        Increased default popmemlim to 3e5
--- John Gibson, Feb 21 1993
        Fixed possible division by zero (with _divq) in Calc_allowed_incrs
--- Robert John Duncan, Jan 22 1993
        Added extra dummy lvars in -Reg_save_apply- to make six each of pop
        and non-pop.
--- John Gibson, Sep  1 1992
        Extended last change to allow pop_gc_every_alloc to be an int to
        specify random GCs
--- John Gibson, Aug 27 1992
        Added pop_gc_every_alloc when DEBUG defined
--- John Gibson, Aug 13 1992
        Made pop_pr_quotes false inside Sysgarbage
--- John Gibson, Apr 14 1992
        Changed -popmemlim- to allow false to mean largest integer
--- John Gibson, Apr  8 1992
        Added clearing of -free*1closures- in -Clear_freelists-
--- John Gibson, Nov 28 1991
        Made Sysgarbage assign itself to _inhibit_gc
--- John Gibson, Oct 17 1991
        Made -Alloc_store- call -Float_opnd_vals- to save and restore
        float operand values across auto GCs.
--- John Gibson, Sep 12 1991
        Added rawstruct_key
--- Roger Evans, Jun 28 1991 added XptClearFreelists
--- John Gibson, May 22 1991
        Changes to -Sysgarbage-
--- John Gibson, Apr 27 1991
        Added -Make_filler_struct-
--- John Gibson, Feb 18 1991
        SPARC_N*WINDOWS replaced with SPARC_MAX_WINDOWS to ensure independence
        from hardware implementation.
--- Roger Evans, Jan 29 1991 changed X garbage handler args
--- John Gibson, Dec 11 1990
        Made -popmemlim- and -popminmemlim- active variables.
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Oct 26 1990
        Made gc printing flush pop_charX_device rather than popdevX.
--- John Gibson, Oct 19 1990
        Made -Print_stats- test for cucharout = identfn, and if so
        locally reassign -charout- to it -- this gets round the problem
        of things like >< and sys_>< causing a GC while cucharout is identfn.
--- John Gibson, Oct 10 1990
        VMS _extern changed to return proper system call result (thus test
        for success is now result _bitst _1).
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Aug 16 1990
        Running of destroy actions now done from -Sysgarbage-
--- Roger Evans, Jul 13 1990 changed xt_garbage to XptGarbageHandler
--- John Gibson, May 25 1990
        Fixed a bug in -Alloc_store- where -Open_seg_expand_for- was
        being called with a number of words instead of a word offset
        (changed -Open_seg_expand_for- to take a number of words).
--- John Gibson, May 11 1990
        Corrected bug in -Open_seg_expand_for-.
--- John Gibson, Mar  3 1990
        Major overhaul of store-allocation routines to support allocation
        of both relocatable and fixed-address structures (main fixed-address
        routines in new file getstore_fixed.p).
--- Ian Rogers, Jan 11 1990
        Remove 'DEF XPOP' code from xt_garbage code
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
--- John Gibson, Nov 10 1989
        Changed -Zap_nextfree_save- for segmented callstack
--- John Gibson, Sep  1 1989
        Made VMS -Set_mem_break- call -Set_call_stack_lim- if successful
--- Roger Evans, Aug  7 1989
        Added xt_garbage_widget for XPOP
--- John Gibson, Apr  4 1989
        Removed assignment of -charerr- to -cucharout- inside -Sysgarbage-.
 */
