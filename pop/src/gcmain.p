/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/gcmain.p
 > Purpose:
 > Author:          John Gibson, Sep 26 1987 (see revisions)
 */

;;; --------------- GARBAGE COLLECTOR MAIN ROUTINES --------------------------
;;;         (USED BY BOTH COPYING AND NON-COPYING VERSIONS)

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'gcdefs.ph'
#_INCLUDE 'destroy.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'process.ph'

constant
        procedure prolog_push_continuation
    ;

section $-Sys;

weak constant
        free_block_key
    ;

constant
        procedure (Ensure_min_userstack, Tab_open_seg, Detab_open_seg,
        Clear_freelists, Reg_save_apply, Set_curr_heap_seg,
        Trim_callstack_lim, Nframe_cslen, Reset_last_gc_data
        ),
        objmod_pad_key
    ;

vars
        _memory_is_corrupt
    ;

endsection;

section $-Sys$-Gc;

constant
        procedure (Setup_copy_gc, Setup_noncopy_gc, Do_copy_gc, Do_noncopy_gc,
        Plog_frame_reset, App_plog_contns, App_plog_trail, Kill_plog_varnum,
        Garbage_plog_trail, Prune_plog_trail,
        Find_destroy_last, Add_destroy_actions
        )
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys => pop_gc_copy;

vars
    pop_gc_copy = true;     ;;; use copying gc by default

    ;;; dummy initialisations -- set up by poplink
constant
    _data_seg_start     = _data_seg_start,
    _data_seg_end       = _data_seg_end,
    ;

endsection;     /* $-Sys */


;;; ---------------------------------------------------------------------

section $-Sys$-Gc;

constant
    ;;; Structure to indicate that a temp prop entry has been killed
    dead_prop_entry = struct UNDEF =>> {%'<dead property entry>', undef_key%},

    ;;; Structure which if it occurs at the end of a procedure's literal table
    ;;; (i.e. immediately before PD_EXECUTE), says that the previous word
    ;;; contains a word offset size not to scan at the end of the table.
    ;;; (I.e. if PD_EXECUTE[_-1] == pd_table_noscan_region then the scan limit
    ;;; is PD_EXECUTE-{PD_EXECUTE[_-2]} instead of PD_EXECUTE.)
    pd_table_noscan_region = struct UNDEF =>> {%false, undef_key%},
    ;

lvars
    _user_start_ptr,
    _start_stack_frame,
    _start_retaddr_ptr,
    ;


;;; --- APP- PROCEDURES -------------------------------------------------

    /*  Apply the procedure app_p to the range of addresses
        from _addr to but (not including) _addr@{_size}
    */
define App_range(_addr, _size, app_p);
    lvars procedure app_p, _addr, _size;
    _addr@(w){_size} -> _size;
    while _addr <@(w) _size do
        app_p(_addr);
        _addr@(w)++ -> _addr
    endwhile
enddefine;

    /*  Apply app_p to an area of userstack in safe mode, i.e. check that
        items encountered look like proper pop structures
    */
define App_userstack_range(_addr, _size, app_p);
    lvars os_free = _open_seg_free_ptr, procedure app_p,
            _item, _addr, _size, _garblow = _lowest_garbageable;
    _addr@(w){_size} -> _size;
    if _copy_gc then
        while _addr <@(w) _size do
            if _isaddress(_addr!(w) ->> _item)
            and _garblow <=@(w) _item and _item <@(w) os_free then
                ;;; check has valid key
                _item!KEY -> _item;
                if issimple(_item) then _mkcompound(_item) -> _item endif;
                if _isaddress(_item)
                and _LOWEST_POP_ADDRESS <=@(w) _item
                and _item <@(w) os_free then
                    ;;; check key has valid key
                    _item!KEY -> _item;
                    if issimple(_item) then _mkcompound(_item) -> _item endif;
                    if _item == key_key then
                        ;;; looks like a valid structure
                        app_p(_addr)
                    endif
                endif
            ;;; else do nothing with it
            endif;
            _addr@(w)++ -> _addr
        endwhile
    else
        ;;; more limited version for non-copying gc (can't check keys)
        while _addr <@(w) _size do
            if _isaddress(_addr!(w) ->> _item)
            and _garblow <=@(w) _item and _item <@(w) os_free then
                app_p(_addr)
            endif;
            _addr@(w)++ -> _addr
        endwhile
    endif
enddefine;

define App_stack(_stackrec, _old, _reloc, app_p);
    lvars procedure app_p, _stackrec, _start = _stackrec@STK_DATA,
        _used, _reloc, _old;
    @@(w){_stackrec!STK_PTR, _old@STK_DATA} -> _used;   ;;; used size

    _start@(w){_used _sub _reloc} -> _stackrec!STK_PTR;
    _stackrec@POPBASE{_stackrec!RAW_SIZE _sub _reloc} -> _stackrec!STK_LIM;

    chain(_start, _used, app_p, App_userstack_range)    ;;; do data
enddefine;

define App_key(_key, app_p);
    lvars procedure app_p, _key, _flags;
    app_p(_key@K_DATAWORD);
    app_p(_key@K_SPEC);
    app_p(_key@K_APPLY);
    app_p(_key@K_RECOGNISER);
    app_p(_key@K_PRINT);
    app_p(_key@K_EQUALS);
    app_p(_key@K_HASH);
    _key!K_FLAGS -> _flags;
    if _flags _bitst _:M_K_VECTOR then
        app_p(_key@K_INIT_V);
        app_p(_key@K_CONS_V);
        app_p(_key@K_DEST_V);
        app_p(_key@K_SUBSCR_V);
        app_p(_key@K_FAST_SUBSCR_V)
    elseif _flags _bitst _:M_K_RECORD then
        app_p(_key@K_CONS_R);
        app_p(_key@K_DEST_R);
        app_p(_key@K_ACCESS_R)
    endif
enddefine;

    /*  Apply app_p to a call stack, either the actual stack
        or saved in a process record.
        _sframe
            points to the first frame
        _limit
            is the limit address for frames.
        _retaddr_ptr
            points to the location containing the return address
            into the owner procedure of _sframe
            (_NULL for processes, where return addresses are relativised)
        app_p
            procedure to apply to address of full items
        app_owner_p
            applied to the owner address and returns the
            actual address of the procedure record.
    */
define App_calls(_sframe, _limit, _retaddr_ptr, app_p, app_owner_p);
    lvars owner, procedure (app_owner_p, app_p), _sframe, _limit, _n,
        _retaddr_ptr;

    define lconstant App_cswords(_csptr, _n, app_p);
        lvars procedure app_p, _csptr, _n;
        _csptr@(csword)[_n] -> _n;
        while _csptr <@(csword) _n do
            app_p(_csptr@(csword->w));
            _csptr@(csword)++ -> _csptr
        endwhile
    enddefine;

    while _sframe <@(csword) _limit do
        ;;; apply app_owner_p to the frame and receive back the
        ;;; actual address of the procedure record
        app_owner_p(_sframe, _retaddr_ptr) -> owner;

        if testdef prologvar_key then
            weakref[prologvar_key] Plog_frame_reset(owner, _sframe)
        endif;

        if owner!PD_FLAGS _bitst _:M_PD_SAVES_NEXTFREE then
            ;;; has _nextfree_save as a local -- zero the saved value
            ;;; of this, so procedure will know a gc has happened
            _NULL -> _sframe!(csword){
                    Dlocal_frame_offset(ident _nextfree_save, owner, true)}
        endif;

        ;;; scan the on-stack pop lvars -- num of them in PD_NUM_PSTK_VARS
        ;;; (SPARC: number of pop registers used)
        unless _zero(owner!PD_NUM_PSTK_VARS ->> _n) then
#_IF DEF SPARC
            App_cswords(_sframe@SF_POP_REGS, _n, app_p)
#_ELSE
            App_cswords(_sframe@SF_LOCALS[owner!PD_NUM_STK_VARS _sub _n],
                                                                    _n, app_p)
#_ENDIF
        endunless;

        ;;; do the pop dlocals -- num of them in PD_GC_SCAN_LEN
        ;;; (SPARC: also includes the on-stack pop lvars)
        ;;; num of words from frame pointer in PD_GC_OFFSET_LEN
        App_cswords(_sframe@(csword)[owner!PD_GC_OFFSET_LEN],
                                                owner!PD_GC_SCAN_LEN, app_p);

        ;;; next frame
#_IF DEF SPARC  ;;; SF_CALLER_RETURN is return into next frame
        unless _retaddr_ptr == _NULL then
            _sframe@SF_CALLER_RETURN -> _retaddr_ptr
        endunless;
#_ENDIF

        ;;; can't use _nextframe because SF_OWNER may not point to the
        ;;; procedure
        _sframe@(csword)[owner!PD_FRAME_LEN] -> _sframe;

#_IF not(DEF SPARC) ;;; SF_RETURN_ADDR is return into current frame
        unless _retaddr_ptr == _NULL then
            _sframe@SF_RETURN_ADDR -> _retaddr_ptr
        endunless;
#_ENDIF
    endwhile
enddefine;

    /*  Clear the entries in a tmpclr property (_oldtab and _newtab are the
        same in a non-copying collection)
    */
define Clear_proptab(prop, _oldtab, _newtab);
    lvars   prop, _oldtab, _newtab, _lim, _entry, _n,
            _len = _newtab!V_LENGTH;    ;;; must get V_LENGTH from _newtab

    if prop!PT_EXPAND then
        ;;; adjust PT_COUNT (must use _oldtab for counting the entries)
        _0 -> _n;
        _oldtab@V_WORDS[_len] -> _lim;
        _oldtab@V_WORDS -> _oldtab;
        while _oldtab <@(w) _lim do
            _oldtab!(w)++ -> (_entry, _oldtab);
            while iscompound(_entry) do
                _n _add _1 -> _n;
                _entry!PTE_NEXT -> _entry
            endwhile
        endwhile;
        prop!PT_COUNT fi_+ _pint(_n) -> prop!PT_COUNT
    endif;

    ;;; clear all entries in _newtab
    _newtab@V_WORDS[_len] -> _lim;
    _newtab@V_WORDS -> _newtab;
    while _newtab <@(w) _lim do 0 -> _newtab!(w)++ -> _newtab endwhile
enddefine;

    /*  apply app_p to roots, i.e.
            built in words
            built in pop-type identifiers
            non-garbageable segments of the heap
            user stack and procedure calling stack
            words declared as identifiers in the dictionary
            prolog continuation stack
    */
define App_roots(app_p, app_clrproptab_p, app_owner_p, scan_deferred_p,
                                                            _scan_phase);
    lvars   procedure scan_deferred_p, _seg, _size;
    dlvars  procedure (app_p, app_clrproptab_p, app_owner_p), _scan_phase;

    ;;; Apply app_p to components of non-garbageable structures
    ;;; (only components which can be updated need be looked at)
    ;;; Do the area of structures from _rec to _lim
    define lconstant App_struct_area(_rec, _lim, _allow_objmod_pad);
        lvars work, garblow = _lowest_garbageable, _rec, _key, _ptr, _lim,
            _allow_objmod_pad;

        define :inline lconstant APP(field=item);
            ;;; test compound and garbageable first before calling app_p,
            ;;; on the assumption that most won't be (the >=@(w) garblow test
            ;;; is done first since it's more likely to fail).
            if (_rec!field ->> work) >=@(w) garblow and iscompound(work) then
                app_p(_rec@field)
            endif
        enddefine;

        while _rec <@(w) _lim do
            _rec!KEY -> _key;

            GCTYPE_GO_ON '' _pint(_key!K_GC_TYPE);  ;;; see gcdefs.ph

            PROCEDURE:
                APP(PD_PROPS);
                APP(PD_UPDATER);
                if _rec!PD_FLAGS _bitst _:M_PD_CLOSURE then
                    APP(PD_CLOS_PDPART);
                    App_range(_rec@PD_CLOS_FROZVALS, @@(w)[_rec!PD_CLOS_NFROZ],
                                                                app_p)
                endif;
                _rec@(w)[_rec!PD_LENGTH] -> _rec, nextloop;

            FULLREC1:
                APP(FIRST);
                _rec@(struct POPREC1)++ -> _rec, nextloop;

            FULLREC2:
                APP(FIRST); APP(SECOND);
            DOUBLE_PAD:
                _rec@(struct POPREC2)++ -> _rec, nextloop;

            FULLREC:
            USERREC:
                APP(FIRST);
                _rec@POPBASE{_key!K_RECSIZE_R} -> _ptr;
                _rec@SECOND -> _rec;
                while _rec <@(w) _ptr do
                    _rec!(w)++ -> _rec -> work;
                    if iscompound(work) and work >=@(w) garblow then
                        app_p(_rec--@(w))
                    endif
                endwhile;
                _rec@~POPBASE -> _rec, nextloop;

            BYTEVEC:
                _rec@(w){_BYTEVEC_SIZE(_rec!V_LENGTH)} -> _rec, nextloop;

            VECTOR:
            USERVEC:
                _rec@V_WORDS[_rec!V_LENGTH] -> _ptr;
                _rec@V_WORDS[_0] -> _rec;
                while _rec <@(w) _ptr do
                    _rec!(w)++ -> _rec -> work;
                    if iscompound(work) and work >=@(w) garblow then
                        app_p(_rec--@(w))
                    endif
                endwhile;
                _rec@~POPBASE -> _rec, nextloop;

            DDECIMAL:
                _rec@(struct DDECIMAL)++ -> _rec, nextloop;

            FULL2ND_REC2:
                APP(SECOND);
                _rec@(struct POPREC2)++ -> _rec, nextloop;

            WORD:
                APP(W_IDENTIFIER);
                _rec@(struct WORD)++ -> _rec, nextloop;

            KEY:
                App_key(_rec, app_p);
                _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec;
                nextloop;

            NFULLREC:
            USERNFREC:
                _key@K_FULL_OFFS_TAB[_0] -> _ptr;       ;;; full offset table address
                _ptr@(-i){_key!K_FULL_OFFS_SIZE} -> work;   ;;; lim address
                while _ptr <@(-i) work do
                    app_p( _rec@(w){_ptr!(-i)++ -> _ptr} )
                endwhile;
                _rec@(w){_key!K_RECSIZE_R} -> _rec, nextloop;

            PROPERTY:
                ;;; set to be rehashed
                unless _rec!PT_REHASH then true -> _rec!PT_REHASH endunless;
                if _rec!PT_ENTRY_KEY!K_GC_TYPE == _:GCTYPE_PROPENT_TMPCLR then
                    app_clrproptab_p(_rec)
                else
                    APP(PT_TABLE)                   ;;; this is updateable
                endif;
                _rec@(struct PROP)++ -> _rec, nextloop;

            PROPENT_PERM:
            PROPENT_TMPARG:
            PROPENT_TMPVAL:
            PROPENT_TMPBOTH:
                APP(PTE_VALUE);                     ;;; PTE_ARG not updateable
                APP(PTE_NEXT);
                _rec@(struct PROP_ENTRY)++ -> _rec, nextloop;

            PROPENT_TMPCLR:
                dead_prop_entry ->> _rec!PTE_ARG -> _rec!PTE_VALUE;
                APP(PTE_NEXT);
                _rec@(struct PROP_ENTRY)++ -> _rec, nextloop;

            DESTROY_PROPENT:
                APP(PTE_VALUE);              ;;; PTE_ARG not updateable
                APP(PTE_NEXT);
                APP(DPTE_DLINK);
                _rec@(struct DESTROY_PROP_ENTRY)++ -> _rec, nextloop;

            USERNFVEC:
                _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec;
                nextloop;

            STACK:
                App_stack(_rec, _rec, _0, app_p);
            PROCSTATE:      ;;; dealt with from process record
            RAWSTRUCT:
            step_rawsize:
                _rec@(w){_rec!RAW_SIZE} -> _rec, nextloop;

            PROCESS:
                PSWEAK App_process(_rec, app_p, app_owner_p, _scan_phase);
                _rec@(struct PROCESS)++ -> _rec, nextloop;

            PLOGPROC:
                PSWEAK App_plog_proc_state(_rec, app_p);
                goto step_rawsize;

            FREE_BLOCK:
                if _key == weakref free_block_key then
                    _rec@(w){_rec!FREEBLK_SIZE} -> _rec
                else
                    _rec@(struct POPREC1)++ -> _rec     ;;; 2-word block
                endif;
                nextloop;

            OBJMOD_PAD:
                unless _allow_objmod_pad then goto ERROR endunless;
                if _rec!V_LENGTH == _-1 then
                    ;;; length of this structure is 2 words and a pop
                    ;;; structure follows it
                    _rec@(struct POPREC1)++ -> _rec
                else
;;;             _extern printf('_rec!V_LENGTH = 0x%lx\n', _rec!V_LENGTH) -> _ ;
;;;             _extern fflush(_0) -> _ ;
                    ;;; V_LENGTH is a byte length -- get to next word after
                    ;;; end of this struct
                    _rec@V_WORDS[_rec!V_LENGTH | b.r]@~POPBASE -> _rec;
                    ;;; then find next objmod_pad key (should be at start of
                    ;;; next object module)
                    repeat
                        if _rec >=@(w) _lim then
        _extern printf('RUN OFF at 0x%lx, _rec = 0x%lx\n', _lim, _rec) -> _ ;
        _extern fflush(_0) -> _ ;
                            mishap(0, 'App_struct_area: RUN OFF END OF AREA')
                        endif;
                        quitif(_rec!KEY == objmod_pad_key);
                        _rec@(w)++ -> _rec
                    endrepeat
                endif;
                nextloop;

            DESCRIPTOR:
#_IF DEF VMS
                _rec@(w){_key!K_RECSIZE_R} -> _rec, nextloop;
#_ENDIF


            DESTROY_MARK:
            CONST_PLOGVAR:
            ERROR:
                mishap(_rec, 1, 'App_struct_area: BAD STRUCTURE');
                setpop()
        endwhile
    enddefine;      /* App_struct_area */


    ;;; Do the non-constant non-garbageable segments (system one first --
    ;;; this should be in the segment table). 3rd arg true allows objmod_pad
    ;;; structures to occur.
    App_struct_area(_data_seg_start@~POPBASE, _data_seg_end@~POPBASE, true);
    scan_deferred_p();          ;;; finish deferred structures
    _seg_table -> _seg;
    while _seg <@(struct SEG) _lowest_heap_seg do
        unless _seg!SEG_FLAGS _bitst (_M_SEG_CONSTANT _biset _M_SEG_NON_POP)
        then
            ;;; non-constant non-garbageable pop area
            ;;; -- do structures in it (no objmod_pad structures)
            App_struct_area(_seg!SEG_BASE_PTR, _seg!SEG_FREE_PTR, false);
            scan_deferred_p()           ;;; finish deferred structures
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile;

    ;;; Do the user stack -- in safe mode in case non-pop args
    ;;; are in the process of being passed
    App_userstack_range(_user_start_ptr, @@(w){_userhi, _user_start_ptr},
                                                                app_p);
    scan_deferred_p();          ;;; finish deferred structures

    ;;; Do the call stack segments
    lvars _lim;  dlvars _dummy = _0;
    _call_stack_seg_hi -> _lim;
    App_calls(_start_stack_frame, _lim, _start_retaddr_ptr, app_p, app_owner_p);
    until _lim == _call_stack_hi do
        ;;; OK to use dummy for retaddr_ptr because pop caller of external
        ;;; procedures is always a system procedure
        App_calls(_lim!SF_NEXT_SEG_SP, _lim!SF_NEXT_SEG_HI ->> _lim,
                    (ident _dummy)@(w->csword), app_p, app_owner_p)
    enduntil;
    scan_deferred_p();          ;;; finish deferred structures

    if testdef prolog_push_continuation then
        ;;; Do the prolog continuation stack (_NULL arg means do the
        ;;; main continuationstack)
        weakref[prolog_push_continuation] App_plog_contns(_NULL, _NULL, app_p)
    endif;

    scan_deferred_p()           ;;; finish deferred structures
enddefine;      /* App_roots */


    /*  Used by non-copying GC only
    */
define App_nonscan_roots(app_p);
    lvars procedure app_p;
    ;;; apply to word chains in dictionary struct
    App_range(dictionary@RAW_WORDS,
                @@POPBASE{dictionary!RAW_SIZE} _sub @@RAW_WORDS, app_p);
    if testdef prologvar_key then
        ;;; apply to prolog trail
        weakref[prologvar_key] App_plog_trail(app_p)
    endif;
    ;;; apply to file table
    App_range(_file_tab, @@(w){_file_tab_next_free, _file_tab}, app_p);
    App_range(_file_tab_close_ptr,
                @@(w){_file_tab_limit, _file_tab_close_ptr}, app_p)
enddefine;


;;; --- FINISHING THE SCAN PHASE -------------------------------------------

    /*  scan the dictionary -- scan an unmarked word only if
        declared as an identifier. don't put any new link addresses in.
        these will be inserted and dead entries removed after
        temp props have been dealt with
    */
define lconstant Scan_dict_idents(scan_p);
    lvars   procedure scan_p, _cell, _lim, _word;
    dlvars  _tmp;
    dictionary@RAW_WORDS -> _cell;
    dictionary@POPBASE{dictionary!RAW_SIZE} -> _lim;
    while _cell <@(w) _lim do
        _cell!(w)++ -> _cell -> _word;
        while _word >=@(w) _lowest_garbageable do
            if iscompound(_word!KEY)                ;;; i.e. not scanned
            and iscompound(_word!W_IDENTIFIER) then ;;; i.e. has identifier
                ;;; declared as an identifer, so scan it
                _word -> _tmp;
                scan_p(ident _tmp)
            endif;
            _word!W_DICT_NEXT -> _word
        endwhile
    endwhile
enddefine;


    /*  Scan chains of unscanned temp prop entries
        (_prop_tmparg_chain, _prop_tmpval_chain, _prop_tmpboth_chain),
        and destroy prop entries (_destroy_prop_chain).
        Keep rescanning till all have untraceable PTE_ARG/VALUE fields
        (as appropriate) or are marked for destroy action.
    */
define lconstant Rescan_temp_prop_entries(scan_p, scan_deferred_p);
    lvars   re_scan, procedure (scan_p, scan_deferred_p), destroy_done = false,
            _entry, _item, _key, _nxtentry;

    lconstant macro (
        SCAN_ARG =
            [   if _copy_gc then _item!FIRST -> _entry!PTE_VALUE endif;
                scan_p(_entry@PTE_ARG);
                true -> re_scan;                    ;;; set to rescan chains
            ],
        SCAN_VAL =
            [   if _copy_gc then _item!FIRST -> _entry!PTE_ARG endif;
                scan_p(_entry@PTE_VALUE);
                true -> re_scan;                    ;;; set to rescan chains
            ],
        VALUE_SCANNED =
            [   _entry!DPTE_FLAGS _bitst _DPTE_VSCANNED ],
        DESTROY_MARK_KEY =
            [   DPWEAK destroy_mark_key ],
        ITEM_UNMARKED =
            [   (iscompound(_item!KEY ->> _key)
                 and _key /== DESTROY_MARK_KEY) ],
        RESTORE_MARK =
            [   if _key == DESTROY_MARK_KEY then
                    ;;; restore mark key in case it got made simple
                    _key -> _item!KEY
                endif ],
        );

    define :inline lconstant GET_CHAIN(entchain=var);
        entchain -> _entry, 0 -> entchain;
    enddefine;

    define :inline lconstant ADD_TO_CHAIN(entchain=var);
        entchain -> _entry!PTE_LINK, _entry -> entchain;
    enddefine;

RE_SCAN:
    false -> re_scan;

    scan_deferred_p();      ;;; finish any deferred structures first

    ;;; N.B. In scanning anything new, further entries can get added to
    ;;; these chains!

    ;;; prop entries discarded for garbage arg
    GET_CHAIN(_prop_tmparg_chain);
    while iscompound(_entry) do
        _entry!PTE_LINK -> _nxtentry;
        if (_entry!PTE_ARG -> _item; ITEM_UNMARKED) then
            ADD_TO_CHAIN(_prop_tmparg_chain)
        else
            SCAN_VAL; RESTORE_MARK
        endif;
        _nxtentry -> _entry
    endwhile;

    ;;; prop entries discarded for garbage value
    GET_CHAIN(_prop_tmpval_chain);
    while iscompound(_entry) do
        _entry!PTE_LINK -> _nxtentry;
        if (_entry!PTE_VALUE -> _item; ITEM_UNMARKED) then
            ADD_TO_CHAIN(_prop_tmpval_chain)
        else
            SCAN_ARG; RESTORE_MARK
        endif;
        _nxtentry -> _entry
    endwhile;

    ;;; prop entries discarded for either garbage arg or garbage value
    GET_CHAIN(_prop_tmpboth_chain);
    while iscompound(_entry) do
        _entry!PTE_LINK -> _nxtentry;
        if (_entry!PTE_ARG -> _item; ITEM_UNMARKED) then
            ADD_TO_CHAIN(_prop_tmpboth_chain)
        elseif (_entry!PTE_VALUE -> _item; ITEM_UNMARKED) then
            ADD_TO_CHAIN(_prop_tmpval_chain)
        elseif _copy_gc then
            _entry!PTE_ARG!FIRST -> _entry!PTE_ARG;
            _item!FIRST -> _entry!PTE_VALUE
        endif;
        _nxtentry -> _entry
    endwhile;

    ;;; keep redoing prop entries until no new structures have been
    ;;; traced/copied before starting on destroy entries
    if re_scan then goto RE_SCAN endif;

    ;;; nothing more to do if already completed destroy entries
    returnif(destroy_done);

    ;;; destroy prop entries -- only force dependent ones to be kept first
    GET_CHAIN(_destroy_prop_chain);
    while iscompound(_entry) do
        _entry!PTE_LINK -> _nxtentry;
        if _copy_gc then _entry!DPTE_OLD else _entry!PTE_ARG endif -> _item;
        if issimple(_item!KEY) then
            ;;; arg is marked -- discard from chain
            unless VALUE_SCANNED then SCAN_VAL endunless;
            if _entry!DPTE_FLAGS _bitst _DPTE_KEYSAV then
                ;;; noncopy gc dependent -- arg key needs restoring
                _entry!DPTE_OLD -> _item!KEY
            endif
        else
            ;;; arg is unmarked
            if _entry!DPTE_FLAGS _bitst _DPTE_DEPEND and not(VALUE_SCANNED)
            then
                if _item!KEY /== DESTROY_MARK_KEY then
                    ;;; not seen arg before - needs scanning
                    scan_p(_entry@PTE_ARG);
                    unless _copy_gc then
                        ;;; save key if noncopying
                        _item!KEY -> _entry!DPTE_OLD;
                        _entry!DPTE_FLAGS _biset _DPTE_KEYSAV
                                                -> _entry!DPTE_FLAGS
                    endunless
                ;;; else destroy mark key already set by another entry
                endif;
                ;;; scan value and mark for destroy (even if already
                ;;; marked)
                SCAN_VAL;
                _entry!DPTE_FLAGS _biset _DPTE_VSCANNED -> _entry!DPTE_FLAGS;
                DESTROY_MARK_KEY -> _item!KEY
            ;;; else already processed or independent (latter done later)
            endif;
            ADD_TO_CHAIN(_destroy_prop_chain)
        endif;
        _nxtentry -> _entry
    endwhile;

    ;;; Keep redoing prop entries and dependent destroy entries until
    ;;; nothing new traced/copied before starting independent ones.
    if re_scan then goto RE_SCAN endif;

    ;;; Having removed all not-otherwise-garbage entries, and forced
    ;;; dependent destroy entries to be kept, now force any otherwise-
    ;;; garbage independent destroy entries to be kept. This may set
    ;;; -re_scan- again, in which case we redo the temp prop entries only.
    _destroy_prop_chain -> _entry;
    while iscompound(_entry) do
        unless _entry!DPTE_FLAGS _bitst _DPTE_DEPEND then
            ;;; independent -- not yet scanned
            ;;; (these calls could create yet MORE destroy entries!
            ;;; -- this needs fixing some time).
            scan_p(_entry@PTE_ARG);
            scan_p(_entry@PTE_VALUE);
            true -> re_scan         ;;; set to do everything again
        endunless;
        _entry!PTE_LINK -> _entry
    endwhile;

    if re_scan then
        true -> destroy_done;   ;;; redo props only
        goto RE_SCAN
    endif
enddefine;


    /*  Kill the remaining temp property entries in
        _prop_tmparg_chain, _prop_tmpval_chain, _prop_tmpboth_chain
        including prolog varnum entries in the first
    */
define lconstant Kill_temp_prop_entries();
    lvars dead = dead_prop_entry, _entry, _key;

    _prop_tmparg_chain -> _entry;
    while iscompound(_entry) do
        _entry!KEY -> _key;
        if issimple(_key) then _mkcompound(_key) -> _key endif;
        if _key!K_FLAGS _bitst _:M_K_PLOG_VARNUM then
            ;;; deal with prolog varnum entry
            weakref[prologvar_key] Kill_plog_varnum(_entry)
        else
            dead ->> _entry!PTE_ARG -> _entry!PTE_VALUE
        endif;
        _entry!PTE_LINK -> _entry
    endwhile;

    _prop_tmpval_chain -> _entry;
    while iscompound(_entry) do
        dead ->> _entry!PTE_ARG -> _entry!PTE_VALUE;
        _entry!PTE_LINK -> _entry
    endwhile;

    _prop_tmpboth_chain -> _entry;
    while iscompound(_entry) do
        dead ->> _entry!PTE_ARG -> _entry!PTE_VALUE;
        _entry!PTE_LINK -> _entry
    endwhile
enddefine;


    /*  correct the dictionary link field in each word and
        remove any garbage words
    */
define lconstant Kill_words();
    lvars _cell, _lim, _word, _lastword;
    dictionary@RAW_WORDS -> _cell;
    dictionary@POPBASE{dictionary!RAW_SIZE} -> _lim;
    while _cell <@(w) _lim do
        _cell@~W_DICT_NEXT -> _lastword;
        _cell!(w)++ -> _cell -> _word;
        while _word >=@(w) _lowest_garbageable do
            if issimple(_word!KEY) then
                ;;; marked - word to be kept.
                if _copy_gc then
                    ;;; Assign the address of the new word (as it will be)
                    ;;; into the last link field. The next link field is then
                    ;;; the W_DICT_NEXT field of the new word at its
                    ;;; current location
                    GC_SET_SEG _word;
                    _word!FIRST -> _lastword!W_DICT_NEXT;   ;;; its new address
                    _word!FIRST@(w){_curr_gcseg_reloc} -> _lastword
                else
                    _word -> _lastword!W_DICT_NEXT;  ;;; relink it
                    _word -> _lastword
                endif;
                _word!W_DICT_NEXT -> _word
            else
                _word!W_DICT_NEXT;
                false -> _word!W_DICT_NEXT; ;;; mark not in dictionary
                -> _word
            endif
        endwhile;
        _word -> _lastword!W_DICT_NEXT
    endwhile
enddefine;


    /*  Scan the file table and move garbage files for closing
        to the end of the file table area.
    */
define lconstant Scan_files(scan_p);
    lvars   dev, procedure scan_p, _entry, _table = _file_tab,
            _close_ptr = _file_tab_close_ptr, _lim = _file_tab_next_free;

    while _table <@(w) _lim do
        _table -> _entry;
        _table!(w)++ -> (dev, _table);
        if dev >=@(w) _lowest_garbageable and iscompound(dev!KEY) then
            ;;; was garbage -- move to end of table area
            _moveq(@@(w){_lim, _table}, _table, _entry) -> _lim;
            _entry -> _table;
            dev -> _close_ptr--!(w) -> _close_ptr
        else
            scan_p(_entry)          ;;; scan entry
        endif;
    endwhile;

    _lim -> _file_tab_next_free;
    _close_ptr -> _file_tab_close_ptr;
    App_range(_close_ptr, @@(w){_file_tab_limit, _close_ptr}, scan_p)
enddefine;

define Finish_scan(scan_p, scan_deferred_p);
    lvars procedure (scan_p, scan_deferred_p);

    ;;; scan dictionary identifiers
    Scan_dict_idents(scan_p);

    ;;; deal with temp/destroy prop entries ...
    Rescan_temp_prop_entries(scan_p, scan_deferred_p);
    ;;; ... then kill any remaining temp entries
    Kill_temp_prop_entries();
    ;;; ... remove dead words from the dictionary
    Kill_words();
    ;;; ... and accumulate new destroy actions
    if DPTEST then DPWEAK Add_destroy_actions(_destroy_prop_chain) endif;

    ;;; scan files in file table
    Scan_files(scan_p);
    scan_deferred_p();          ;;; finish all deferred stuff

    if testdef prologvar_key then
        ;;; garbage stuff on the prolog trail
        weakref[prologvar_key] Garbage_plog_trail()
    endif
enddefine;


;;; --- COMMON SETUP -------------------------------------------------------

define Setup(_open_seg_reloc, need_copy);
    lvars need_copy, _seg, _open_seg_reloc, _ptr;

    Ensure_min_userstack();

    ;;; add open seg to seg table
    Tab_open_seg();

    ;;; see if can do a copying gc (need_copy is an integer if doing
    ;;; sys_lock_system).
    unless (_open_seg_reloc _sgr _0 or need_copy or pop_gc_copy)
    and Setup_copy_gc(_open_seg_reloc, isinteger(need_copy)) then
        ;;; don't want to/can't use copying
        if need_copy or (_open_seg_reloc _sgr _0
                        and _open_seg_free_ptr /== _open_seg_base_ptr)
        then
            ;;; these need the copying algorithm
            Detab_open_seg();       ;;; undo Tab_open_seg
            return(false)
        endif;
        ;;; use non-copying
        if _nonzero(_open_seg_reloc) then
            Detab_open_seg();
            _open_seg_base_ptr@(w){_open_seg_reloc} ->> _ptr
                                                    -> _open_seg_base_ptr;
            if _open_seg_reloc _sgr _0 then
                ;;; open seg is empty
                _ptr -> _open_seg_free_ptr
            else
                ;;; make a noncopy gc shift open seg down by just
                ;;; creating a garbage rawstruct underneath
                Make_filler_struct(_ptr, _negate(_open_seg_reloc))
            endif;
            Tab_open_seg()
        endif;
        Setup_noncopy_gc()
    endunless;

    ;;; set low limit for garbageable structures
    _lowest_heap_seg -> _seg;
    while _seg!SEG_FLAGS _bitst _M_SEG_NON_POP do
        _seg@(struct SEG)++ -> _seg
    endwhile;
    _seg!SEG_BASE_PTR -> _lowest_garbageable;

    ;;; clear all free lists
    Clear_freelists(true);

    ;;; clear temp/destroy prop entry chains
    0 ->> _prop_tmparg_chain ->> _prop_tmpval_chain
      ->> _prop_tmpboth_chain -> _destroy_prop_chain;
    if DPTEST then DPWEAK Find_destroy_last() endif;

    ;;; zero _nextfree_save, so that procedures which save _open_seg_free_ptr
    ;;; in here in order to re-use heap space after producing intermediate
    ;;; results will know that a gc has occurred (this is always done by
    ;;; the procedure Clawback). Saved values of _nextfree_save in stack
    ;;; frames are zeroed by App_calls.
    _NULL -> _nextfree_save;

    if testdef prologvar_key then
        ;;; prune the prolog trail
        weakref[prologvar_key] Prune_plog_trail()
    endif;

    ;;; limit on call stack use during the gc -- this allows 2**11 stack frames
    ;;; of a procedure with 3 lvars (like Copyscan and Scan).
    Trim_callstack_lim( _sp()@(csword)-[Nframe_cslen(_2:1e11)], true)
                                                    -> _callstack_lim;

    true -> _memory_is_corrupt;     ;;; causes system exit on a mishap

    true
enddefine;

    /*  Called by Reg_save_apply with start frame for callstack processing
        and address of return address into that frame
    */
define lconstant Perform_gc(_start_stack_frame, _start_retaddr_ptr);
    dlocal _disable = _DISABLE_ALL, _start_stack_frame, _start_retaddr_ptr;

    ;;; start point for user stack processing
    _user_sp() -> _user_start_ptr;

    if _copy_gc then
        Do_copy_gc()
    else
        Do_noncopy_gc()
    endif;

    false -> _memory_is_corrupt;

    Detab_open_seg();               ;;; open seg tab entry -> open seg

#_IF DEF CACHEFLUSH
    ;;; Flush the instruction cache ...
    CACHEFLUSH(
        ;;; from here ...
        _lowest_garbageable,
        ;;; for this number of bytes ...
        @@(b){_open_seg_free_ptr, _lowest_garbageable});
#_ENDIF

    _open_seg_free_ptr@(w){_USER_SAFEGAP} -> _userlim;

    ;;; re-enable all heap segs for after the gc
    _lowest_heap_seg -> _lowest_free_heap_seg;
    ;;; reset free pointer of current heap seg (if any)
    Set_curr_heap_seg(_curr_heap_seg);

    Reset_last_gc_data(false)
enddefine;

    /*  do the gc thru Reg_save_apply, which saves all pop register lvars
        in a stack frame so they can be scanned.
    */
define Do_gc = Reg_save_apply(% Perform_gc %) enddefine;


endsection;     /* $-Sys$-Gc */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 26 1998
        Removed all the vad*vise stuff
--- John Gibson, Feb 14 1996
        Added assignments to _memory_is_corrupt
--- John Gibson, May  6 1995
        Changes to processing of tmpclr props
--- John Gibson, Apr 25 1995
        _:GCTYPE_ID*ENTIFIER -> _:GCTYPE_FULL2ND_REC2
--- John Gibson, Apr  8 1995
        Change to layout of full-field offset table in not-all-full records
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Prevented use of vad*vise for Linux.
--- John Gibson, Aug 26 1994
        Added pd_table_noscan_region
--- John Gibson, Aug 15 1994
        Changed Scan_files so that garbage files get moved to a separate table
        at the end of the file table area (i.e. from _file_tab_close_ptr)
--- John Gibson, Feb 19 1993
        In App_roots, converted 'ident dummy' arg for App_calls to type
        csword
--- John Gibson, Nov  9 1992
        Moved all _gctypes into section Sys
--- John Gibson, Sep 14 1991
        Dictionary now a rawstruct rather than a string
--- John Gibson, Sep 14 1991
        Changes for new format bytevecs
--- John Gibson, Sep 12 1991
        Added RAWSTRUCT label to GCTYPE switches
--- Robert John Duncan, Jun 21 1991
        Disabled vad*vise for SG IRIX
--- John Gibson, May  6 1991
        Removed e*o_after_gc
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Aug 16 1990
        Changes to handling of destroy prop entries
--- Rob Duncan, May  8 1990
        Added optional cache flush at end of -Perform_gc- (for MIPS).
--- John Gibson, Jan 28 1990
        Added code for dealing with fixed-address structures.
--- Ian Rogers, Jan 11 1990
        Removed 'DEF XPOP' code from around e*o_after_gc
--- John Gibson, Dec  5 1989
        Changes for new pop pointers
--- John Gibson, Nov 10 1989
        Changed -App_roots- to deal with segmented callstack
--- John Gibson, Sep  1 1989
        _callstack_lim now computed with differently
--- John Gibson, Aug 24 1989
        Revised -Rescan_temp_prop_entries- so that (1) repeated calls in
        -Finish_scan- no longer required, and (2) code for
        -Scan_indep_destroys- incorporated (making another pass on temp
        prop chains unnecessary if no independent destroy entries).
--- Roger Evans, Aug 23 1989
        Modified Rescan_temp_prop_entries and added Scan_indep_destroys
        so that independent destroy pros interact correctly with temp
        properties
--- John Gibson, Aug 22 1989
        Added not(HPUX) to test for using vad*vise (so Bobcat can
        have BERKELEY set true)
--- Roger Evans, Aug  3 1989
        added e*o_after_gc for XPOP version
--- John Gibson, Jul 31 1989
        Added "tmpclr" prop entry code
--- John Gibson, May 24 1989
        Added new temp property gc types
--- John Gibson, May 21 1989
        Split off destroy prop entries from _temp_prop_chain into
        separate _destroy_prop_chain, etc.
--- John Gibson, May 15 1989
        Included ident.ph
--- Roger Evans, Aug 30 1988
        Added DESTROY_MARK label to GCTYPE switches
        changes to Rescan_temp_prop_entries and Kill_temp_prop_entries to
        use destroy_mark format and independent destroy actions
--- John Gibson, Aug 22 1988
        Added OBJMOD_PAD code to -App_struct_area- to allow for padding/
        alignment structures between object modules.
--- Roger Evans, Aug 19 1988
        added destroy action code and DESTROY_PROPENT switch code
--- John Gibson, Aug  6 1988
        Changes to support doubleword-aligned structures.
--- John Gibson, Jul  5 1988
        New treatment of return addresses in stack frames, including
        SPARC code, changes to -Reg_save_apply- etc.
--- John Gibson, Feb 25 1988
        Sectionised garbage collector, weakref'ed stuff for processes
        and prolog.
--- John Gibson, Feb 15 1988
        Removed -Gc_do_files-, weakref'ed -ispw*m_id-.
--- John Gibson, Jan 17 1988
        Different data sections for words and identifiers generated by poplink
        now replaced by area from _data_seg_start to _data_seg_end
 */
