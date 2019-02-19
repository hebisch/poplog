/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/gcncopy.p
 > Purpose:
 > Author:          John Gibson, Sep 26 1987 (see revisions)
 */

;;; ------------- GARBAGE COLLECTOR (NON-COPYING VERSION) ---------------------
;;;              (Compaction using the Morris algorithm)

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'gcdefs.ph'
#_INCLUDE 'destroy.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'process.ph'

section $-Sys;

global constant
        procedure (Make_filler_struct),
        descriptor_key
    ;

weak global constant
        procedure Make_free_block, free_block_key
    ;

endsection;

section $-Sys$-Gc;

global constant
        procedure (App_roots, App_nonscan_roots, Finish_scan, App_stack,
        Clear_proptab)
    ;

weak global constant
        const_plogvar_key
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Gc;

lconstant   procedure (Scan);


;;; --- GENERAL -----------------------------------------------------------

define lconstant App_procedure(_p, app_p);
    lvars procedure app_p, _p, _ptr, _exec;
    ;;; do table/frozvals
    _p!PD_EXECUTE@(code->w) -> _exec;
    if _p!PD_FLAGS _bitst _:M_PD_CLOSURE then
        app_p(_p@PD_CLOS_PDPART);
        _p@PD_CLOS_FROZVALS -> _ptr
    else
        _p@PD_TABLE -> _ptr
    endif;
    if _ptr <@(w) _exec and _exec!(w)[_-1] == pd_table_noscan_region then
        ;;; word before PD_EXECUTE is pd_table_noscan_region -- previous word
        ;;; is offset size to ignore at end of table
        _exec@(w)-{_exec!(w)[_-2]} -> _exec
    endif;
    while _ptr <@(w) _exec do app_p(_ptr), _ptr@(w)++ -> _ptr endwhile;
    app_p(_p@PD_PROPS);
    chain(_p@PD_UPDATER, app_p)
enddefine;

    /*  Scan owner address in stack frame and return actual address of
        owner procedure
    */
define lconstant Scan_owner(_sframe, _retaddr_ptr);
    lvars _sframe, _retaddr_ptr;
    Scan(_sframe@SF_OWNER@(csword->w));
    _sframe!SF_OWNER
enddefine;

define lconstant Scan_clrproptab(_prop);
    lvars _tab = _prop!PT_TABLE, _prop;
    if _tab >=@(w) _lowest_garbageable and _iscompound(_tab!KEY) then
        _mksimple(_tab!KEY) -> _tab!KEY
    endif;
    chain(_prop, _tab, _tab, Clear_proptab)
enddefine;


;;; --- TRACE AND MARK ----------------------------------------------------

    /*  mark a record and scan components, if not already done
    */
define lconstant Scan(_addr);
    lvars _rec, _addr, _key;

    lconstant macro
        ;;; for checking whether to defer scanning of record
        CHECK_DEFER = [
            if _sp() <@(csword) _callstack_lim then
                ;;; put on deferred chain
                goto DEFER
            endif
            ];

    define :inline lconstant SCAN_USERKEY(_key);
        if _key >=@(w) _lowest_garbageable and iscompound(_key!KEY) then
            _mksimple(key_key) -> _key!KEY;
            App_key(_key, Scan)
        endif
    enddefine;

    define :inline lconstant UNTRACED(item, tmp);
        (item -> tmp; iscompound(tmp) and tmp >=@(w) _lowest_garbageable
          and iscompound(tmp!KEY))
    enddefine;

    define :inline lconstant ADD_TO_CHAIN(_rec, entchain);
        entchain -> _rec!PTE_LINK, _rec -> entchain
    enddefine;


SCAN_ADDR:
    if issimple(_addr!(w) ->> _rec) or _rec <@(w) _lowest_garbageable
    or issimple(_rec!KEY ->> _key) then
        ;;; simple object, or not a pointer to a garbageable structure,
        ;;; or already marked -- nothing to do
        return
    endif;

    ;;; else mark rec and process fields
    _mksimple(_key) -> _rec!KEY;        ;;; mark it as simple

    GCTYPE_GO_ON '' _pint(_key!K_GC_TYPE);  ;;; see gcdefs.ph

    PROCEDURE:
        CHECK_DEFER;
        chain(_rec, Scan, App_procedure);

    FULLREC1:
        _rec@FIRST -> _addr, goto SCAN_ADDR;

    FULLREC2:
        CHECK_DEFER;
        Scan(_rec@FIRST);
        _rec@SECOND -> _addr, goto SCAN_ADDR;

    USERREC:
        SCAN_USERKEY(_key);
        ;;; drop thru to FULLREC

    FULLREC:
        CHECK_DEFER;
        _rec@POPBASE{_key!K_RECSIZE_R} -> _addr;
        _rec@SECOND -> _key;
        while _key <@(w) _addr do Scan(_key), _key@(w)++ -> _key endwhile;
        _rec@FIRST -> _addr, goto SCAN_ADDR;

    BYTEVEC:
    DDECIMAL:
    RAWSTRUCT:
    FREE_BLOCK:
        return;

    FULL2ND_REC2:
        _rec@SECOND -> _addr, goto SCAN_ADDR;

    USERVEC:
        SCAN_USERKEY(_key);
        ;;; drop thru to VECTOR

    VECTOR:
        CHECK_DEFER;
        _rec@V_WORDS[_rec!V_LENGTH] -> _addr;
        _rec@V_WORDS[_0] -> _key;
        while _key <@(w) _addr do Scan(_key), _key@(w)++ -> _key endwhile;
        return;

    WORD:
        Scan(_rec@W_STRING);
        if iscompound(_rec!W_DICT_NEXT ->> _addr)
        and _addr >=@(w) _lowest_garbageable
        and (_addr!KEY ->> _addr) /== word_key
        and _addr /== _mksimple(word_key)
        then
            ;;; word not in dictionary
            Scan(_rec@W_DICT_NEXT)
        endif;
        _rec@W_IDENTIFIER -> _addr, goto SCAN_ADDR;

    KEY:
        chain(_rec, Scan, App_key);

    PROPERTY:
        unless _rec!PT_REHASH then true -> _rec!PT_REHASH endunless;
        Scan(_rec@PT_ACTIVE);
        Scan(_rec@PT_DEFAULT);
        Scan(_rec@PT_EQ_PDR);
        Scan(_rec@PT_HASH_PDR);
        if _rec!PT_ENTRY_KEY!K_GC_TYPE == _:GCTYPE_PROPENT_TMPCLR then
            chain(_rec, Scan_clrproptab)
        else
            _rec@PT_TABLE -> _addr;
            goto SCAN_ADDR
        endif;


    PROPENT_PERM:
        Scan(_rec@PTE_ARG);
        Scan(_rec@PTE_VALUE);
        _rec@PTE_NEXT -> _addr, goto SCAN_ADDR;

    PROPENT_TMPARG:
        if UNTRACED(_rec!PTE_ARG, _key) then
            ADD_TO_CHAIN(_rec, _prop_tmparg_chain)
        else
            Scan(_rec@PTE_VALUE)
        endif;
        _rec@PTE_NEXT -> _addr, goto SCAN_ADDR;

    PROPENT_TMPVAL:
        if UNTRACED(_rec!PTE_VALUE, _key) then
            ADD_TO_CHAIN(_rec, _prop_tmpval_chain)
        else
            Scan(_rec@PTE_ARG)
        endif;
        _rec@PTE_NEXT -> _addr, goto SCAN_ADDR;

    PROPENT_TMPBOTH:
        if UNTRACED(_rec!PTE_ARG, _key) then
            if UNTRACED(_rec!PTE_VALUE, _key) then
                ADD_TO_CHAIN(_rec, _prop_tmpboth_chain)
            else
                ADD_TO_CHAIN(_rec, _prop_tmparg_chain)
            endif
        elseif UNTRACED(_rec!PTE_VALUE, _key) then
            ADD_TO_CHAIN(_rec, _prop_tmpval_chain)
        endif;
        _rec@PTE_NEXT -> _addr, goto SCAN_ADDR;

    PROPENT_TMPCLR:
        dead_prop_entry ->> _rec!PTE_ARG -> _rec!PTE_VALUE;
        _rec@PTE_NEXT -> _addr, goto SCAN_ADDR;


    DESTROY_PROPENT:
        if _rec!DPTE_FLAGS _bitst _DPTE_SCHEDULED then
            ;;; already scheduled for execution -- scan next in
            ;;; scheduled chain (DLINK), then treat as perm
            Scan(_rec@DPTE_DLINK);
            goto PROPENT_PERM
        endif;
        if UNTRACED(_rec!PTE_ARG, _key) then
            ;;; pointer to self in DLINK field
            _rec -> _rec!DPTE_DLINK;
            ;;; clear VSCANNED and KEYSAV flags
            _rec!DPTE_FLAGS _biclear (_DPTE_VSCANNED _biset _DPTE_KEYSAV)
                                                -> _rec!DPTE_FLAGS;
            ADD_TO_CHAIN(_rec, _destroy_prop_chain)
        else
            0 -> _rec!DPTE_DLINK;   ;;; just to keep compaction phase happy
            ;;; scan value
            Scan(_rec@PTE_VALUE)
        endif;
        _rec@PTE_NEXT -> _addr, goto SCAN_ADDR;

    DESTROY_MARK:
        ;;; destroy_mark key superimposed on already processed data
        ;;; no work - address is correct and key already marked
        return;

    USERNFREC:
        SCAN_USERKEY(_key);
        ;;; drop thru to NFULLREC

    NFULLREC:
        CHECK_DEFER;
        _key@K_FULL_OFFS_TAB[_0] -> _addr;      ;;; full offset table addr
        _addr@(-i){_key!K_FULL_OFFS_SIZE} -> _key;  ;;; lim address
        while _addr <@(-i) _key do
            Scan( _rec@(w){_addr!(-i)++ -> _addr} );
        endwhile;
        return;

    USERNFVEC:
        SCAN_USERKEY(_key);
        return;

    STACK:
        chain(_rec, _rec, _0, Scan, App_stack);

    PROCESS:
        CHECK_DEFER;
        chain(_rec, Scan, Scan_owner, true, PSWEAK App_process);

    PROCSTATE:
        chain(_rec!PSS_PROCESS, _rec, _0, Scan, Scan_owner,
                                                    PSWEAK App_proc_state);

    PLOGPROC:
        chain(_rec, Scan, PSWEAK App_plog_proc_state);

    CONST_PLOGVAR:
        _key -> _rec!KEY;               ;;; unmark key
        _rec!PGV_CONT -> _key;          ;;; the var's contents
        ;;; deref any constant plogvars, to make sure that
        ;;; this var doesn't lead thru a chain of them back to itself...
        while iscompound(_key) and _key!KEY == weakref const_plogvar_key do
            _key!PGV_CONT -> _key
        endwhile;
        if _key == _rec then
            ;;; what to do - who knows?
            '<CIRCULAR PROLOGVAR CHAIN>' -> _key
        endif;
        _key -> _rec!PGV_CONT;          ;;; replace contents for another time
        _key -> _addr!(w);              ;;; also replaces current arg
        goto SCAN_ADDR;                 ;;; now scan that


    DESCRIPTOR:
#_IF DEF VMS
        _rec@DSC_ITEM -> _addr, goto SCAN_ADDR;
#_ENDIF

    DOUBLE_PAD:
    OBJMOD_PAD:
    ERROR:
        mishap(_rec, 1, 'Scan: BAD STRUCTURE');
        setpop();

    DEFER:
        ;;; come here to defer processing of structure fields
        _mksimple2(_key) -> _rec!KEY;   ;;; mark key as simple2
        if _rec <@(w) _deferred then
            ;;; record lowest-addressed structure deferred
            _rec -> _deferred
        endif;
        ;;; then return
enddefine;

    /*  Rescan components of records deferred by Scan. We search for
        such records from _deferred, which records the lowest-addressed
        record deferred.
    */
define lconstant Scan_deferred();
    lvars size, _rec = _deferred, _key, _seg;
    dlvars _tmp;
    returnunless(_rec <@(w) _open_seg_free_ptr);
    _lowest_heap_seg -> _seg;
    while _seg <@(struct SEG) _seg_table_next_free do
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP
        or _seg!SEG_FREE_PTR <=@(w) _rec then
            _seg!SEG_FREE_PTR -> _deferred;
            if _seg!SEG_BASE_PTR >@(w) _rec then
                _seg!SEG_BASE_PTR -> _rec
            endif;
            while _rec <@(w) _deferred do
                if issimple(_rec!KEY ->> _key) then
                    if _issimple2(_key) then
                        ;;; key marked simple2, i.e. deferred
                        _mkcompound2(_key) ->> _key -> _rec!KEY;    ;;; unmark
                        _rec -> _tmp;
                        Scan(ident _tmp)            ;;; rescan in _tmp
                    else
                        _mkcompound(_key) -> _key
                    endif
                endif;
                fast_apply(_rec, _key!K_GET_SIZE) -> size;
                _rec@(w){size} -> _rec
            endwhile;
            _deferred -> _rec;
            if _seg!SEG_FREE_PTR >@(w) _rec then
                ;;; new deferred point in or below current seg
                nextif(_seg!SEG_BASE_PTR <=@(w) _rec);  ;;; redo this seg
                chain(Scan_deferred)                    ;;; restart all segs
            endif
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile
enddefine;


;;; --- PASS 1 THROUGH GARBAGEABLE STRUCTURES -----------------------------
;;;     Create space records and Morris chain backward pointers

lvars
    _fdalign_chain,
    _last_fdalign;


define lconstant Pass_1(_seg);
    lvars garblow = _lowest_garbageable, reclim,
        _rec = _seg!SEG_BASE_PTR, _item, _key, _space = _NULL,
        _reloc = _0, _pre_dpad = false, _lim = _seg!SEG_FREE_PTR, _seg,
        _tab, _tablim, _fixedseg = _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS
        ;

    define :inline lconstant BCHAIN(field=item);
        ;;; if field contains a pointer to a relocatable structure behind or
        ;;; at the current record, replace the target structure's key with
        ;;; a pointer to the field, and the field with the previous value of
        ;;; the key
        if iscompound(_rec!field ->> _item)
        and garblow <=@(w) _item and _item <=@(w) _rec then
            _item!KEY -> _rec!field;
            _rec@field -> _item!KEY
        endif
    enddefine;

        ;;; same as a procedure, taking the field address
    define lconstant Bchain(_field);
        lvars _field, _item;
        if iscompound(_field!(w) ->> _item)
        and _lowest_garbageable <=@(w) _item
        and _item@POPBASE <=@(w) _field then
            _item!KEY -> _field!(w);
            _field -> _item!KEY
        endif
    enddefine;

        ;;; Called on process stack frames only (_retaddr_ptr always _NULL)
    define lconstant Bchain_owner(_sframe, _retaddr_ptr) -> _owner;
        lvars _sframe, _owner = _sframe!SF_OWNER, _retaddr_ptr;
        if _lowest_garbageable <=@(w) _owner
        and _owner <=@(w) _sframe@(csword->w) then  ;;; test backward
            _owner!KEY -> _sframe!SF_OWNER;
            _sframe@SF_OWNER@(csword->w) -> _owner!KEY
        endif
    enddefine;

    lconstant macro
        SUBS_USERKEY = [
            ;;; for a relocatable user key behind the current record we
            ;;; substitute the record's key field with the relocated addr
            ;;; of the key made simple
            if garblow <=@(w) _key and _key <@(w) _rec then
                _key!K_GC_RELOC -> _rec!KEY
            endif;
            ];

    while _rec <@(w) _lim do
        if issimple(_rec!KEY ->> _key) then
            ;;; marked
            if _space /== _NULL then
                ;;; finish up last space rec -- FIRST contains offset size
                (@@(w){_rec, _space} ->> _space!FIRST) _add _reloc -> _reloc;
                0 -> _space!KEY;        ;;; space record key is 0
                _NULL -> _space
            endif;
            _mkcompound(_key) -> _key;
            GCTYPE_GO_ON '' _pint(_key!K_GC_TYPE);  ;;; see gcdefs.ph
        else
            ;;; garbage
            if _space == _NULL then _rec -> _space endif;
            GCTYPE_GO_ON 'g' _pint(_key!K_GC_TYPE); ;;; see gcdefs.ph
        endif;

        PROCEDURE:
            App_procedure(_rec, Bchain);
        gPROCEDURE:
            _rec@(w)[_rec!PD_LENGTH] -> _rec, nextloop;

        FULLREC1:
            BCHAIN(FIRST);
        gFULLREC1: gCONST_PLOGVAR:
            _rec@(struct POPREC1)++ -> _rec, nextloop;

        FULLREC2:
            BCHAIN(FIRST), BCHAIN(SECOND);
        gFULLREC2:
            _rec@(struct POPREC2)++ -> _rec, nextloop;

        do_from_second:
            ;;; limit of rec is in reclim
            _rec -> _key;
            _rec@SECOND -> _rec;
            while _rec <@(w) reclim do
                if iscompound(_rec!(w) ->> _item)
                and garblow <=@(w) _item and _item <=@(w) _key then
                    _item!KEY -> _rec!(w);
                    _rec -> _item!KEY
                endif;
                _rec@(w)++ -> _rec
            endwhile;
            _rec@~POPBASE -> _rec, nextloop;

        USERREC:
            SUBS_USERKEY;
        FULLREC: PROPERTY:
            _rec@POPBASE{_key!K_RECSIZE_R} -> reclim;   ;;; limit of rec
            BCHAIN(FIRST);
            goto do_from_second;

        gUSERREC: gFULLREC: gPROPERTY:
            _rec@(w){_key!K_RECSIZE_R} -> _rec, nextloop;

        BYTEVEC: gBYTEVEC:
            _rec@(w){_BYTEVEC_SIZE(_rec!V_LENGTH)} -> _rec, nextloop;

        DDECIMAL: gDDECIMAL:
            _rec@(struct DDECIMAL)++ -> _rec, nextloop;

        FULL2ND_REC2:
            BCHAIN(SECOND);
        gFULL2ND_REC2:
            _rec@(struct POPREC2)++ -> _rec, nextloop;

        USERVEC:
            SUBS_USERKEY;
        VECTOR:
            _rec@V_WORDS[_rec!V_LENGTH] -> reclim;  ;;; limit of rec
            goto do_from_second;

        gUSERVEC: gVECTOR:
            _rec@V_WORDS[_rec!V_LENGTH]@~POPBASE -> _rec, nextloop;

        WORD:
            BCHAIN(W_IDENTIFIER), BCHAIN(W_STRING), BCHAIN(W_DICT_NEXT);
        gWORD:
            _rec@(struct WORD)++ -> _rec, nextloop;

        KEY:
            ;;; see SUBS_USERKEY
            if _fixedseg then _0 -> _reloc endif;
            _mksimple(_rec@(w)-{_reloc}) -> _rec!K_GC_RELOC;
            App_key(_rec, Bchain);
        gKEY:
            _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec;
            nextloop;

        PROPENT_PERM:
        PROPENT_TMPARG:
        PROPENT_TMPVAL:
        PROPENT_TMPBOTH:
            BCHAIN(PTE_ARG), BCHAIN(PTE_VALUE);
        PROPENT_TMPCLR:     ;;; arg & val always dead
            BCHAIN(PTE_NEXT);
        gPROPENT_PERM:
        gPROPENT_TMPARG:
        gPROPENT_TMPVAL:
        gPROPENT_TMPBOTH:
        gPROPENT_TMPCLR:
            _rec@(struct PROP_ENTRY)++ -> _rec, nextloop;

        DESTROY_PROPENT:
            BCHAIN(PTE_ARG), BCHAIN(PTE_VALUE), BCHAIN(PTE_NEXT);
            BCHAIN(DPTE_DLINK);
        gDESTROY_PROPENT:
            _rec@(struct DESTROY_PROP_ENTRY)++ -> _rec, nextloop;

        USERNFREC:
            SUBS_USERKEY;
        NFULLREC:
            _key@K_FULL_OFFS_TAB[_0] -> _tab;       ;;; full offset table addr
            _tab@(-i){_key!K_FULL_OFFS_SIZE} -> _tablim;    ;;; lim address
            while _tab <@(-i) _tablim do
                _rec@(w){_tab!(-i)++ -> _tab} -> _item;
                if iscompound(_item!(w) ->> reclim)
                and garblow <=@(w) reclim and reclim <=@(w) _rec then
                    reclim!KEY -> _item!(w);
                    _item -> reclim!KEY
                endif
            endwhile;
            _rec@(w){_key!K_RECSIZE_R} -> _item;

        keep_dpad:
            if _key!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN and not(_fixedseg) then
                unless _pre_dpad then
                    _rec@(w)-{_reloc} -> reclim;    ;;; new position
                    if reclim@(w.t->d) /== reclim then
                        ;;; double_pad follows and new position not doubleword
                        ;;; aligned -- put on chain
                        _item ->> _last_fdalign!FIRST -> _last_fdalign;
                        _NULL -> _item!FIRST;       ;;; marks end of chain
                        _rec -> _item!SECOND
                    endif;
                    _item@(struct DOUBLE_PAD)++ -> _item
                endunless;
                false -> _pre_dpad
            endif;
            _item -> _rec;
            nextloop;

        gUSERNFREC: gNFULLREC:
            _rec@(w){_key!K_RECSIZE_R} -> _rec;

        garbage_dpad:
            if _key!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN and not(_fixedseg) then
                unless _pre_dpad then
                    ;;; garbage following double_pad
                    _rec@(struct DOUBLE_PAD)++ -> _rec
                endunless;
                false -> _pre_dpad
            endif;
            nextloop;

        USERNFVEC:
            ;;; the K_GET_SIZE procedure uses the key, so must get size first
            ;;; before substituting the key
            fast_apply(_rec, _key!K_GET_SIZE) -> _item;
            SUBS_USERKEY;
            _rec@(w){_item} -> _item;
            goto keep_dpad;

        gUSERNFVEC:
            fast_apply(_rec, _key!K_GET_SIZE) -> _item;
            _rec@(w){_item} -> _rec;
            goto garbage_dpad;

        gDOUBLE_PAD:
            unless _fixedseg then
                true -> _pre_dpad;
                if issimple(_rec@(struct DOUBLE_PAD)++ !KEY) then
                    ;;; following double-align rec not garbage, so
                    ;;; keep this padding record
                    if _space == _rec then _NULL -> _space endif;
                    _mksimple(_key) -> _rec!KEY;
                    nextloop        ;;; reprocess as non-garbage (*)
                endif
            endunless;
        DOUBLE_PAD:                 ;;; allows for (*) case
            _rec@(struct DOUBLE_PAD)++ -> _rec, nextloop;

        STACK:
            App_stack(_rec, _rec, _0, Bchain);
        gSTACK:
        gPROCSTATE:
        gPLOGPROC:
        RAWSTRUCT: gRAWSTRUCT:
        step_rawsize:
            _rec@(w){_rec!RAW_SIZE} -> _rec, nextloop;

        PROCESS:
            PSWEAK App_process(_rec, Bchain, Bchain_owner, false);
        gPROCESS:
            _rec@(struct PROCESS)++ -> _rec, nextloop;

        PROCSTATE:
            PSWEAK App_proc_state(_rec!PSS_PROCESS, _rec, _0,
                                                    Bchain, Bchain_owner);
            Bchain(_rec@PSS_PROCESS);
            goto step_rawsize;

        PLOGPROC:
            PSWEAK App_plog_proc_state(_rec, Bchain);
            goto step_rawsize;

        FREE_BLOCK: gFREE_BLOCK:
            if _key == weakref free_block_key then
                _rec@(w){_rec!FREEBLK_SIZE} -> _rec
            else
                _rec@(struct POPREC1)++ -> _rec     ;;; 2-word block
            endif;
            nextloop;


#_IF DEF VMS
        DESCRIPTOR:
            ;;; make the DSPEC_PTR relative to the start of the item
            @@(w){_rec@DSC_SPEC!DSPEC_PTR, _rec!DSC_ITEM} -> _rec@DSC_SPEC!DSPEC_PTR;
            Bchain(_rec@DSC_ITEM);
        gDESCRIPTOR:
            _rec@(struct DESCRIPTOR)++ -> _rec, nextloop;

#_ELSE
        DESCRIPTOR: gDESCRIPTOR:
#_ENDIF

        DESTROY_MARK:
        gDESTROY_MARK:
        CONST_PLOGVAR:
        OBJMOD_PAD:
        gOBJMOD_PAD:
        ERROR:
            mishap(_rec, 1, 'Pass_1: BAD STRUCTURE');
            setpop();

    endwhile;

    if _space /== _NULL then
        ;;; no need to finish last space rec, just truncate used space
        _space -> _seg!SEG_FREE_PTR
    endif
enddefine;


;;; --- NON-GARBAGEABLE STRUCTURES BETWEEN PASS 1 AND PASS 2 ------------------
;;;     Chain all pointers in fields

    ;;; If field contains a pointer to a relocatable structure anywhere,
    ;;; replace the target structure's key with a pointer to the field,
    ;;; and the field with the previous value of the key
define lconstant FBchain(_field);
    lvars _field, _item;
    if iscompound(_field!(w) ->> _item)
    and _item >=@(w) _lowest_garbageable then
        _item!KEY -> _field!(w);
        _field -> _item!KEY
    endif
enddefine;

define lconstant FBchain_owner(_sframe, _retaddr_ptr) -> _owner;
    lvars _sframe, _owner = _sframe!SF_OWNER, _retaddr_ptr;
    returnunless(_owner >=@(w) _lowest_garbageable);
    _owner!KEY -> _sframe!SF_OWNER;
    if _retaddr_ptr == _NULL then
        ;;; process, return address doesn't need fixing
        _sframe@SF_OWNER@(csword->w) -> _owner!KEY
    else
        ;;; call stack frame, return address needs relocating
        ;;; make return address relative to owner start
        @@(code){_retaddr_ptr!(csword), _owner@(w->code)}
            -> _retaddr_ptr!(csword);
#_IF DEF SPARC
        ;;; Need to save _retaddr_ptr somewhere so that Pass_2 can get
        ;;; at it. We use the SF_FP slot (which Pass_2 restores)
        _retaddr_ptr -> _sframe!SF_FP;
#_ELSE
        ;;; no problem, since _retaddr_ptr = _sframe@SF_RETURN_ADDR
#_ENDIF
        ;;; replace owner procedure's key with a 'simple2' pointer to the
        ;;; stack frame. We use a simple2 pointer here so that Pass_2
        ;;; knows that it has to add back the relocated procedure's
        ;;; address to the return address
        _mksimple2(_sframe) -> _owner!KEY
    endif
enddefine;


;;; --- PASS 2 THROUGH GARBAGEABLE STRUCTURES -----------------------------
;;;     Morris chain forward pointers and shift down structures

lvars
    _descriptor_chain;      ;;; VMS only

define lconstant Pass_2(_seg);
    lvars   reclim, reloc, procedure (fixed_free_p),
            _rec = _seg!SEG_BASE_PTR, _new = _rec, _item,
            _key, _dpad_flags = _0, _lim, _seglim = _seg!SEG_FREE_PTR,
            _seg, _svrec, _svnew,
            _fixedseg = _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS
        ;

        ;;; for moving down structures
    define :inline lconstant MOVE_TO_NEW(_size);
        _moveq(_size, _rec@POPBASE, _new@POPBASE)@~POPBASE
    enddefine;

    define :inline lconstant FCHAINC(field);
        ;;; if field contains a pointer to a relocatable structure forward
        ;;; of the current record, replace the target structure's key with
        ;;; a pointer to the field, and the field with the previous value of
        ;;; the key, else just copy across
        if iscompound(_rec!field ->> _item) and _item >@(w) _rec then
            _item!KEY -> _new!field;
            _new@field -> _item!KEY
         else
            _item -> _new!field
        endif
    enddefine;

    lconstant macro (
        FCHAIN_USERKEY = [
            if _key >@(w) _rec then
                _key!KEY -> _new!KEY;
                _new@KEY -> _key!KEY
            endif
            ]
        );

        ;;; The same, as a procedure, taking the field address
    define lconstant Fchain(_field);
        lvars _field, _item;
        ;;; test _field@~POPBASE rather than _item@POPBASE
        ;;; in case _item is zero
        if iscompound(_field!(w) ->> _item)
        and _field@~POPBASE <@(w) _item then
            _item!KEY -> _field!(w);
            _field -> _item!KEY
        endif
    enddefine;

        ;;; Called on process stack frames only (_retaddr_ptr always _NULL)
    define lconstant Fchain_owner(_sframe, _retaddr_ptr) -> _owner;
        lvars _sframe, _owner = _sframe!SF_OWNER, _retaddr_ptr;
        if _sframe@(csword->w) <@(w) _owner then    ;;; test forward
            _owner!KEY -> _sframe!SF_OWNER;
            _sframe@SF_OWNER@(csword->w) -> _owner!KEY
        endif
    enddefine;

    if _fixedseg then
        if _seg!SEG_FLAGS _bitst _M_SEG_FIXED_EXPTRS then
            ;;; These contain fixed-address external pointers tied to an
            ;;; external relocation mechanism, and freed space can't be
            ;;; reused (or rather, the XP_PTR fields can't).
            Make_filler_struct
        else
            weakref[free_block_key] Make_free_block
        endif -> fixed_free_p
    endif;

    if _fdalign_chain == _NULL
    or (_fdalign_chain!SECOND ->> _lim) >@(w) _seglim then
        _seglim -> _lim
    endif;

    repeat
        if _rec >=@(w) _lim then
            quitif(_lim == _seglim);        ;;; end of seg
            ;;; deal with double padding where old double_pad record
            ;;; came after record
            double_pad_key -> _new!KEY;
            _new@(struct DOUBLE_PAD)++ -> _new;
            _2:10 -> _dpad_flags;
            ;;; step chain
            if (_fdalign_chain!FIRST ->> _fdalign_chain) == _NULL
            or (_fdalign_chain!SECOND ->> _lim) >@(w) _seglim then
                _seglim -> _lim
            endif;
            _rec!KEY -> reclim

        elseif (_rec!KEY ->> reclim) == 0 then
            ;;; space record -- FIRST contains size
            if _fixedseg then
                ;;; add to free blocks
                _rec!FIRST -> _item;
                ;;; this destroys FIRST
                fixed_free_p(_rec, _item);
                _rec@(w){_item} ->> _rec -> _new
            else
                ;;; just skip it
                _rec@(w){_rec!FIRST} -> _rec
            endif;
            nextloop
        endif;

        ;;; Follow thru chain of pointers from key field until the
        ;;; simple-marked key is reached, inserting new record address in
        ;;; each location. A simple2 value in the chain is a pointer to a
        ;;; stack frame owner slot; in this case we add the new procedure's
        ;;; address back to the return address
        repeat
            if iscompound(reclim ->> _key) then
                _key!(w) -> reclim;         ;;; next in chain
                _new -> _key!(w)
            elseif _issimple2(_key) then
                _mkcompound2(_key) -> _key; ;;; stack frame pointer
                _key!SF_OWNER -> reclim;    ;;; next in chain
                _new -> _key!SF_OWNER;
#_IF DEF SPARC
                ;;; SF_FP field contains address of return address
                _key!SF_FP -> _item;
                ;;; then restore SF_FP to point to next frame
                _key@(csword)[_rec!PD_FRAME_LEN] -> _key!SF_FP;
#_ELSE
                ;;; return address is at _key@SF_RETURN_ADDR
                _key@SF_RETURN_ADDR -> _item;
#_ENDIF
                ;;; make return address absolute again
                _new@(w->code){_item!(csword)} -> _item!(csword)
            else
                ;;; reached marked key
                quitloop
            endif
        endrepeat;

        ;;; copy key field across
        _mkcompound(_key) ->> _key -> _new!KEY; ;;; unmark key
        GCTYPE_GO_ON '' _pint(_key!K_GC_TYPE);  ;;; see gcdefs.ph

        PROCEDURE:
            _key -> _rec!KEY;
            @@(w)[_rec!PD_LENGTH] -> reclim;
            if _zero(@@(w){_new, _rec} ->> reloc) then
                _new@(w){reclim}
            else
                MOVE_TO_NEW(reclim);
                ;;; correct execute addresses
                Adjust_pdr_exec(reloc, _new)
            endif -> _item;
            App_procedure(_new, Fchain);

        step_reclim:
            _item -> _new, _rec@(w){reclim} -> _rec, nextloop;

        FULLREC1:
            FCHAINC(FIRST);
            _rec@(struct POPREC1)++ -> _rec, _new@(struct POPREC1)++ -> _new;
            nextloop;

        FULLREC2:
            FCHAINC(FIRST), FCHAINC(SECOND);
            _rec@(struct POPREC2)++ -> _rec, _new@(struct POPREC2)++ -> _new;
            nextloop;

        do_from_second:
            ;;; limit of rec is in reclim
            _rec -> reloc;
            _rec@SECOND -> _rec, _new@SECOND -> _new;
            while _rec <@(w) reclim do
                _rec!(w)++ -> _rec -> _item;
                if iscompound(_item) and _item >@(w) reloc then
                    _item!KEY -> _new!(w);
                    _new -> _item!KEY;
                    _new@(w)++ -> _new
                else
                    _item -> _new!(w)++ -> _new
                endif
            endwhile;
            _rec@~POPBASE -> _rec, _new@~POPBASE -> _new;
            nextloop;

        USERREC:
            FCHAIN_USERKEY;
        FULLREC:
        PROPERTY:
            _rec@POPBASE{_key!K_RECSIZE_R} -> reclim;       ;;; limit of rec
            FCHAINC(FIRST);
            goto do_from_second;

        BYTEVEC:
            _BYTEVEC_SIZE(_rec!V_LENGTH) -> reclim;
        copy_reclim:
            _key -> _rec!KEY;
            if _rec == _new then
                _new@(w){reclim}
            else
                MOVE_TO_NEW(reclim)
            endif -> _item;
            goto step_reclim;

        RAWSTRUCT:
            _rec!RAW_SIZE -> reclim, goto copy_reclim;

        FREE_BLOCK:
            unless _fixedseg then goto ERROR endunless;
            if _key == weakref free_block_key then
                _rec!FREEBLK_SIZE
            else
                @@(struct POPREC1)++
            endif -> reclim;
            fixed_free_p(_rec, reclim);
            _new@(w){reclim} -> _item;
            goto step_reclim;

        DDECIMAL:
            _rec!DD_1 -> _new!DD_1;
#_IF ##(w)[_1|struct DDECIMAL] = _3
            _rec!DD_2 -> _new!DD_2;
#_ENDIF
            _rec@(struct DDECIMAL)++ -> _rec, _new@(struct DDECIMAL)++ -> _new;
            nextloop;

        FULL2ND_REC2:
            _rec!FIRST -> _new!FIRST;
            FCHAINC(SECOND);
            _rec@(struct POPREC2)++ -> _rec, _new@(struct POPREC2)++ -> _new;
            nextloop;

        USERVEC:
            FCHAIN_USERKEY;
        VECTOR:
            _rec!V_LENGTH -> _new!V_LENGTH;
            _rec@V_WORDS[_rec!V_LENGTH] -> reclim;  ;;; limit of rec
            goto do_from_second;

        WORD:
            FCHAINC(W_IDENTIFIER), FCHAINC(W_STRING), FCHAINC(W_DICT_NEXT);
            _rec@(struct WORD)++ -> _rec, _new@(struct WORD)++ -> _new;
            nextloop;

        KEY:
            _key -> _rec!KEY;
            fast_apply(_rec, _key!K_GET_SIZE) -> reclim;
            MOVE_TO_NEW(reclim) -> _item;
            App_key(_new, Fchain);
            goto step_reclim;

        PROPENT_PERM:
        PROPENT_TMPARG:
        PROPENT_TMPVAL:
        PROPENT_TMPBOTH:
        PROPENT_TMPCLR:     ;;; arg & val always dead
            FCHAINC(PTE_ARG), FCHAINC(PTE_VALUE), FCHAINC(PTE_NEXT);
            _rec@(struct PROP_ENTRY)++ -> _rec,
            _new@(struct PROP_ENTRY)++ -> _new;
            nextloop;

        DESTROY_PROPENT:
            FCHAINC(PTE_ARG), FCHAINC(PTE_VALUE), FCHAINC(PTE_NEXT);
            FCHAINC(DPTE_DLINK);
            _rec!DPTE_FLAGS -> _new!DPTE_FLAGS;
            _rec!DPTE_OLD -> _new!DPTE_OLD;
            _rec@(struct DESTROY_PROP_ENTRY)++ -> _rec,
            _new@(struct DESTROY_PROP_ENTRY)++ -> _new;
            nextloop;

        USERNFREC:
        NFULLREC:
            _key -> _rec!KEY;
            MOVE_TO_NEW(_key!K_RECSIZE_R) -> _svnew;
            FCHAIN_USERKEY;
            _rec@(w){_key!K_RECSIZE_R} -> _svrec;
            _key@K_FULL_OFFS_TAB[_0] -> _item;      ;;; full offset table addr
            _item@(-i){_key!K_FULL_OFFS_SIZE} -> reclim;    ;;; lim address
            while _item <@(-i) reclim do
                _new@(w){_item!(-i)++ -> _item} -> _rec;
                if iscompound(_rec!(w) ->> reloc) and reloc >@(w) _new then
                    reloc!KEY -> _rec!(w);
                    _rec -> reloc!KEY
                endif
            endwhile;
            _svrec -> _rec, _svnew -> _new;

        dpad_after:
            if _key!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN and not(_fixedseg) then
                unless _dpad_flags _bitst _2:10 then
                    double_pad_key -> _new!KEY;
                    _new@(struct DOUBLE_PAD)++ -> _new
                endunless;
                unless _dpad_flags _bitst _2:01 then
                    _rec@(struct DOUBLE_PAD)++ -> _rec
                endunless;
                _0 -> _dpad_flags
            endif;
            nextloop;

        USERNFVEC:
            ;;; the K_GET_SIZE procedure uses the key, so must get size first
            ;;; before forward chaining the key
            _key -> _rec!KEY;
            fast_apply(_rec, _key!K_GET_SIZE) -> reclim;
            if _rec == _new then
                _new@(w){reclim}
            else
                MOVE_TO_NEW(reclim)
            endif -> _item;
            FCHAIN_USERKEY;
            _item -> _new, _rec@(w){reclim} -> _rec;
            goto dpad_after;

        DOUBLE_PAD:
            if _new@(w.t->d) /== _new then
                _key -> _new!KEY;
                _new@(struct DOUBLE_PAD)++ -> _new;
                _2:11 -> _dpad_flags
            else
                _2:01 -> _dpad_flags
            endif;
            _rec@(struct DOUBLE_PAD)++ -> _rec, nextloop;

        STACK:
            _key -> _rec!KEY;
            MOVE_TO_NEW(_rec!RAW_SIZE ->> reclim) -> _item;
            App_stack(_new, _rec, _0, Fchain);
            goto step_reclim;

        PROCESS:
            _key -> _rec!KEY;
            MOVE_TO_NEW(@@(struct PROCESS)++) -> _item;
            PSWEAK App_process(_new, Fchain, Fchain_owner, false);
            _item -> _new, _rec@(struct PROCESS)++ -> _rec;
            nextloop;

        PROCSTATE:
            _key -> _rec!KEY;
            MOVE_TO_NEW(_rec!RAW_SIZE ->> reclim) -> _item;
            PSWEAK App_proc_state(_new!PSS_PROCESS, _new, _0,
                                                    Fchain, Fchain_owner);
            goto step_reclim;

        PLOGPROC:
            _key -> _rec!KEY;
            MOVE_TO_NEW(_rec!RAW_SIZE ->> reclim) -> _item;
            PSWEAK App_plog_proc_state(_new, Fchain);
            goto step_reclim;

        DESCRIPTOR:
#_IF DEF VMS
            MOVE_TO_NEW(@@(struct DESCRIPTOR)++) -> _item;
            ;;; have to make a chain of these which we can through afterwards
            ;;; to add back the new DSC_ITEM to the DSPEC_PTR
            _descriptor_chain -> _new!KEY;
            _new -> _descriptor_chain;
            Fchain(_new@DSC_ITEM);
            _item -> _new, _rec@(struct DESCRIPTOR)++ -> _rec;
            nextloop;
#_ENDIF

        DESTROY_MARK:
        CONST_PLOGVAR:
        OBJMOD_PAD:
        ERROR:
            mishap(_rec, 1, 'Pass_2: BAD STRUCTURE');
            setpop();

    endrepeat;

    if _seg!SEG_FLAGS _bitst _M_SEG_FIXED_EXPTRS then
        _new -> _rec;
        _seg!SEG_BASE_PTR@(vpage){_seg!SEG_SIZE} -> _new;
        @@(w){_new, _rec} -> _item;
        if _nonzero(_item) then Make_filler_struct(_rec, _item) endif
    endif;

    ;;; update seg next free address
    _new -> _seg!SEG_FREE_PTR
enddefine;


;;; --- MAIN PROCEDURE --------------------------------------------------

define Setup_noncopy_gc();
    ;;; initialise the position for 'deferred' scan
    _open_seg_free_ptr -> _deferred;

    false -> _copy_gc
enddefine;

define Do_noncopy_gc();

    define lconstant App_segments(pass_p);
        lvars procedure pass_p, _seg = _lowest_heap_seg;
        while _seg <@(struct SEG) _seg_table_next_free do
            unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
                pass_p(_seg)
            endunless;
            _seg@(struct SEG)++ -> _seg
        endwhile
    enddefine;

    ;;; scan and mark from roots
    App_roots(Scan, Scan_clrproptab, Scan_owner, identfn, true); ;;; scan roots
    ;;; finish scan phase
    Finish_scan(Scan, Scan_deferred);

    ;;; Morris compaction phase
    _NULL -> _fdalign_chain;
    ident _fdalign_chain@~FIRST -> _last_fdalign;
    App_segments(Pass_1);

    App_roots(FBchain, FBchain, FBchain_owner, identfn, false); ;;; roots
    App_nonscan_roots(FBchain);                 ;;; roots not scanned

    0 -> _descriptor_chain;         ;;; VMS only
    App_segments(Pass_2);

#_IF DEF VMS
    lvars _desc = _descriptor_chain;
    while iscompound(_descriptor_chain ->> _desc) do
        _desc!KEY -> _descriptor_chain;
        descriptor_key -> _desc!KEY;
        _desc!DSC_ITEM@(w){_desc@DSC_SPEC!DSPEC_PTR} -> _desc@DSC_SPEC!DSPEC_PTR
    endwhile;
#_ENDIF
enddefine;

endsection;     /* $-Sys$-Gc */


/* --- Revision History ---------------------------------------------------
--- John Gibson, May  6 1995
        Changes to processing of tmpclr props
--- John Gibson, Apr 25 1995
        gctype ID*ENT -> FULL2ND_REC2
--- John Gibson, Apr  8 1995
        Change to layout of full-field offset table in not-all-full records
--- John Gibson, Aug 27 1994
        Added testing for pd_table_noscan_region when scanning PD_TABLE in
        a procedure
--- Robert John Duncan, Jun 10 1993
        Added some missing csword type conversions for stack frame pointers
--- John Gibson, Sep  3 1992
        Modified to allow non-garbage fixed free blocks
--- John Gibson, Sep 14 1991
        Changes for new format bytevecs
--- John Gibson, Sep 12 1991
        Added RAWSTRUCT to GCTYPE switches
--- John Gibson, Apr 27 1991
        Changes to Pass_2 to cope with _M_SEG_FIXED_EXPTRS segs
--- John Gibson, Dec  5 1990
        Changes to VMS descriptors
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Aug 11 1990
        Changes to destroy prop entry code
--- John Gibson, Jan 28 1990
        Added code for dealing with fixed-address structures.
--- John Gibson, Dec  5 1989
        Changes for new pop pointers
--- John Gibson, Aug 24 1989
        Removed 3rd arg to -Finish_scan- (no longer needed)
--- John Gibson, Jul 31 1989
        Added "tmpclr" prop entry code
--- John Gibson, May 24 1989
        Added new temp property gc types
--- John Gibson, May 21 1989
        Split off destroy prop entries from _temp_prop_chain into
        separate _destroy_prop_chain
--- John Gibson, May 15 1989
        Included ident.ph
--- Roger Evans, Aug 31 1988
        Fixed bug in Pass2 destroy entry code
--- Roger Evans, Aug 30 1988
        Removed simple2 marking and added DESTROY_MARK
--- John Gibson, Aug 22 1988
        Added OBJMOD_PAD to GCTYPE_GO_ONs
--- Roger Evans, Aug 19 1988
        Added DESTROY_PROPENT code
--- John Gibson, Aug  6 1988
        Changes to support doubleword-aligned structures.
--- John Gibson, Jul  5 1988
        New treatment of return addresses in stack frames, including
        SPARC code.
--- John Gibson, May  1 1988
        Corrected "_rec" to "_desc" in VMS-specific code in Do_noncopy_gc
--- John Gibson, Feb 25 1988
        Sectionised garbage collector, weakref'ed stuff for processes
        and prolog.
--- John Gibson, Dec 18 1987
        Added some missing declarations at top of file
--- John Gibson, Nov  5 1987
        Allowed for W_DICT_NEXT field to contain abitrary structures when
        word not in dictionary.
--- John Gibson, Oct 31 1987
        Replaced callstack.ph macros with csword-type operations
 */
