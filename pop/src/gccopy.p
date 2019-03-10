/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/gccopy.p
 > Purpose:
 > Author:          John Gibson, Sep 26 1987 (see revisions)
 */

;;;------------- GARBAGE COLLECTOR (COPYING VERSION) -----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'gcdefs.ph'
#_INCLUDE 'destroy.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'process.ph'

section $-Sys;

global constant
        procedure (Get_mem_break, Set_mem_break, Pop_seg_count)
    ;

weak global constant
        procedure Make_free_block, free_block_key
    ;

endsection;

section $-Sys$-Gc;

constant
        procedure (App_roots, Finish_scan, App_stack, Clear_proptab),
        const_plogvar_key
    ;

endsection;


;;; -------------------------------------------------------------------

section $-Sys$-Gc;

vars
    _doing_saveincr = false,
    ;

lvars
    _curr_gcseg_test_lim
    ;

;;; --------------------------------------------------------------------------

    /*  Set the current segment variables for the seg containing _rec
    */
define Set_curr_seg(_rec);
    lvars _gcseg = _gcseg_table, _rec;
    ;;; update the copy pointer for the current seg
    _curr_gcseg_copy -> _curr_gcseg!GC_SEG_COPY_PTR;
    ;;; now find the new seg (last entry has -1 in LIM_PTR field)
    until _rec <@(w) _gcseg!GC_SEG_LIM_PTR do
        _gcseg@(struct GC_SEG)++ -> _gcseg
    enduntil;
    if (_gcseg!GC_SEG_BASE_PTR ->> _curr_gcseg_base) <=@(w) _rec then
        _gcseg!GC_SEG_LIM_PTR  ->> _curr_gcseg_lim -> _curr_gcseg_test_lim;
        _gcseg!GC_SEG_COPY_PTR  -> _curr_gcseg_copy;
        _gcseg!GC_SEG_RELOC    -> _curr_gcseg_reloc;
        _gcseg -> _curr_gcseg;
        if _gcseg!GC_SEG_COPY_BASE == _NULL then
            ;;; fixed-address struct segment -- set this _NULL
            ;;; so that the test for a struct being in the current seg
            ;;; in -Copyscan- always fails (saves having an extra test
            ;;; for a fixed-address struct)
            _NULL -> _curr_gcseg_test_lim
        endif
    else
        mishap(_rec, 1, 'Gc$-Set_curr_seg: INVALID STRUCTURE POINTER');
        setpop()
    endif
enddefine;

    /*  Copyscan the owner address in a stack frame and adjust its return
        address. Return the actual address of the owner procedure
    */
define lconstant Copyscan_owner(_sframe, _retaddr_ptr) -> _owner;
    lvars _sframe, _owner = _sframe!SF_OWNER, _retaddr_ptr, _offs;
    Copyscan(_sframe@SF_OWNER@(csword->w)); ;;; do owner address
    unless _retaddr_ptr == _NULL then
        ;;; ordinary callstack frame (not a process) -- adjust return address
        @@(w){_sframe!SF_OWNER, _owner} -> _offs;
        _retaddr_ptr!(csword)@(code){_offs|w} -> _retaddr_ptr!(csword)
    endunless
enddefine;

define lconstant Do_dict_next(_word);
    lvars _word, _addr;
    if iscompound(_word!W_DICT_NEXT ->> _addr) then
        unless (_addr!KEY ->> _addr) == word_key
        or _addr == _mksimple(word_key) then
            ;;; not in dictionary -- scan it
            chain(_word@W_DICT_NEXT, Copyscan)
        elseif _doing_saveincr then
            ;;; need to make a chain of dictionary words for saveincr
            _doing_saveincr -> _word!KEY;
            _word@(w)-{_curr_gcseg_reloc} -> _doing_saveincr
        endunless
    endif
enddefine;

define lconstant Copyscan_clrproptab(_prop);
    lvars _old, _new, _prop;
    _prop!PT_TABLE -> _old;
    if _old <@(w) _lowest_garbageable then
        _old -> _new
    else
        GC_SET_SEG _old;
        if issimple(_old!KEY) then
            _old!FIRST@(w){_curr_gcseg_reloc} -> _new
        else
            _curr_gcseg_copy -> _new;   ;;; can't be fixed-address!
            _old!V_LENGTH -> _new!V_LENGTH;
            _old!KEY -> _new!KEY;
            _new@V_WORDS[_new!V_LENGTH]@~POPBASE -> _curr_gcseg_copy;
            _mksimple(_old!KEY) -> _old!KEY
        endif;
        _new@(w)-{_curr_gcseg_reloc} ->> _old!FIRST -> _prop!PT_TABLE
    endif;
    chain(_prop, _old, _new, Clear_proptab)
enddefine;

    /*  Scan procedure table/frozvals
    */
define :inline lconstant SCAN_PD_TABLE(_new, _ptr, _exec);
    _new!PD_EXECUTE@(code->w){_curr_gcseg_reloc} -> _exec;
    if _new!PD_FLAGS _bitst _:M_PD_CLOSURE then
        Copyscan(_new@PD_CLOS_PDPART);
        _new@PD_CLOS_FROZVALS -> _ptr
    else
        _new@PD_TABLE -> _ptr
    endif;
    if _ptr <@(w) _exec and _exec!(w)[_-1] == pd_table_noscan_region then
        ;;; word before PD_EXECUTE is pd_table_noscan_region -- previous
        ;;; word is offset size to ignore at end of table
        _exec@(w)-{_exec!(w)[_-2]} -> _exec
    endif;
    while _ptr <@(w) _exec do Copyscan(_ptr), _ptr@(w)++ -> _ptr endwhile;
enddefine;

    /*  Copy a record and scan components, if not already done
    */
define Copyscan(_addr);
    lvars _old, _new, _addr;    ;;; minimum possible number of lvars essential
                                ;;; to save system stack space required

    lconstant macro (

        ;;; for code marking and redirecting _old record
        MARK_OLD = [
            _new@(w)-{_curr_gcseg_reloc} -> _old!FIRST; ;;; new addr as it will be
            _old!FIRST -> _addr!(w);                ;;; correct current arg
            _mksimple(_old!KEY) -> _old!KEY;        ;;; flag copied
            ],

        ;;; for checking whether to defer scanning of record
        CHECK_DEFER = [
            if _sp() <@(csword) _callstack_lim then
                ;;; put on deferred chain
                goto DEFER
            endif
            ],

        ;;; copy and mark a property entry
        COPY_MARK_PROPENT = [
            _old!PTE_ARG    -> _new!PTE_ARG;
            _old!KEY        -> _new!KEY;
            _old!PTE_VALUE  -> _new!PTE_VALUE;
            _old!PTE_NEXT   -> _new!PTE_NEXT;
            _new@(struct PROP_ENTRY)++ -> _curr_gcseg_copy;
            MARK_OLD;
            ],

        ;;; copy and mark a tmp property entry and treat as perm
        ;;; when doing saveincr
        COPY_MARK_TMP_PROPENT = [
            COPY_MARK_PROPENT;
            if _doing_saveincr then goto scan_propent_both endif;
            ],
        );

    define :inline lconstant COPY_TO_NEW(_size);
        _moveq(_size, _old@POPBASE, _new@POPBASE)@~POPBASE
    enddefine;

    define :inline lconstant UNTRACED(fld=item);
        (_new!fld -> _old;
            iscompound(_old) and _old >=@(w) _lowest_garbageable
            and iscompound(_old!KEY))
    enddefine;

    define :inline lconstant ADD_TO_CHAIN_GO_SCAN_NEXT(entchain=var);
        entchain -> _new!PTE_LINK, _new -> entchain;
        _new@PTE_NEXT -> _addr, goto SCAN_ADDR;
    enddefine;


SCAN_ADDR:
    ;;; analyse object -- these tests are so arranged for maximum efficiency
    if issimple(_addr!(w) ->> _old) then
        ;;; simple, nothing to do
        return
    elseif _old <@(w) _curr_gcseg_base then
        ;;; below current seg, check garbageable
        if _old <@(w) _lowest_garbageable then
            ;;; not a pointer to a garbageable structure, nothing to do
            return
        elseif issimple(_old!KEY) then
            ;;; already copied -- FIRST field has new address
            _old!FIRST -> _addr!(w);
            return
        else
            ;;; change segment
            Set_curr_seg(_old);
            if _curr_gcseg_test_lim == _NULL then goto FIXED_ADDR endif
        endif
    elseif issimple(_old!KEY) then
        ;;; already copied -- FIRST field has new address
        _old!FIRST -> _addr!(w);
        return
    elseif _old >=@(w) _curr_gcseg_test_lim then
        ;;; test_lim is _NULL if curr seg is fixed-address -- do real test
        if _old >=@(w) _curr_gcseg_lim then
            ;;; above current seg
            Set_curr_seg(_old)
        endif;
        if _curr_gcseg_test_lim == _NULL then goto FIXED_ADDR endif
    endif;

    ;;; else do non-fixed address copy ...
    _curr_gcseg_copy -> _new;                   ;;; next address to copy to
    GCTYPE_GO_ON '' _pint(_old!KEY!K_GC_TYPE);  ;;; see gcdefs.ph


FIXED_ADDR:
    ;;; Fixed-address structure -- copy at the corresponding place
    ;;; in the copy seg so that the structure address won't change.
    ;;; For structures that have _:M_K_NO_FULL_FROM_PTR set only the 1st two
    ;;; words are copied, so that the data in the rest of the struct
    ;;; doesn't need to be copied back.
    _old@(w){_curr_gcseg_reloc} -> _new;
    GCTYPE_GO_ON 'f_' _pint(_old!KEY!K_GC_TYPE);    ;;; see gcdefs.ph


    PROCEDURE:
    f_PROCEDURE:
        COPY_TO_NEW(@@(w)[_old!PD_LENGTH]) -> _curr_gcseg_copy;
        MARK_OLD;
        ;;; correct execute addresses
        Adjust_pdr_exec(@@(w){_new, _old} _sub _curr_gcseg_reloc, _new);
        CHECK_DEFER;
        SCAN_PD_TABLE(_new, _old, _addr);   ;;; scan table/frozvals
        Copyscan(_new@PD_PROPS);
        _new@PD_UPDATER -> _addr, goto SCAN_ADDR;

    FULLREC1:
    f_FULLREC1:
        _old!FIRST -> _new!FIRST;
        _old!KEY -> _new!KEY;
        _new@(struct POPREC1)++ -> _curr_gcseg_copy;
        MARK_OLD;
        _new@FIRST -> _addr, goto SCAN_ADDR;

    FULLREC2:
    f_FULLREC2:
        _old!FIRST -> _new!FIRST;
        _old!KEY -> _new!KEY;
        _old!SECOND -> _new!SECOND;
        _new@(struct POPREC2)++ -> _curr_gcseg_copy;
        MARK_OLD; CHECK_DEFER;
        Copyscan(_new@FIRST);
        _new@SECOND -> _addr, goto SCAN_ADDR;

    FULLREC:
    f_FULLREC:
        COPY_TO_NEW(_old!KEY!K_RECSIZE_R) -> _curr_gcseg_copy;
        MARK_OLD; CHECK_DEFER;
        _curr_gcseg_copy@POPBASE -> _addr;
        _new@SECOND -> _old;
        while _old <@(w) _addr do Copyscan(_old), _old@(w)++ -> _old endwhile;
        _new@FIRST -> _addr, goto SCAN_ADDR;

    BYTEVEC:
        COPY_TO_NEW(_BYTEVEC_SIZE(_old!V_LENGTH)) -> _curr_gcseg_copy;
        MARK_OLD;
        return;
    f_BYTEVEC:
    f_RAWSTRUCT:
    f_FREE_BLOCK:
        _old!FIRST -> _new!FIRST, _old!KEY -> _new!KEY;
        MARK_OLD;
        return;

    VECTOR:
    f_VECTOR:
        COPY_TO_NEW(@@V_WORDS[_old!V_LENGTH] _sub @@POPBASE)
                                                        -> _curr_gcseg_copy;
        MARK_OLD; CHECK_DEFER;
        _curr_gcseg_copy@POPBASE -> _addr;
        _new@V_WORDS -> _old;
        while _old <@(w) _addr do Copyscan(_old), _old@(w)++ -> _old endwhile;
        return;

    FULL2ND_REC2:
    f_FULL2ND_REC2:
        _old!FIRST  -> _new!FIRST;
        _old!KEY    -> _new!KEY;
        _old!SECOND -> _new!SECOND;
        _new@(struct POPREC2)++ -> _curr_gcseg_copy;
        MARK_OLD;
        _new@SECOND -> _addr, goto SCAN_ADDR;

    DDECIMAL:
    f_DDECIMAL:
        _old!DD_1 -> _new!DD_1;
        _old!KEY -> _new!KEY;
#_IF ##(w)[_1|struct DDECIMAL] = _3
        _old!DD_2 -> _new!DD_2;
#_ENDIF
        _new@(struct DDECIMAL)++ -> _curr_gcseg_copy;
        MARK_OLD;
        return;

    WORD:
    f_WORD:
        _old!W_IDENTIFIER -> _new!W_IDENTIFIER;
        _old!KEY -> _new!KEY;
        _old!W_STRING -> _new!W_STRING;
        _old!W_DICT_NEXT -> _new!W_DICT_NEXT;
        _new@(struct WORD)++ -> _curr_gcseg_copy;
        MARK_OLD;
        Copyscan(_new@W_STRING);
        Do_dict_next(_new);
        _new@W_IDENTIFIER -> _addr, goto SCAN_ADDR;

    KEY:
    f_KEY:
        COPY_TO_NEW(fast_apply(_old, _old!KEY!K_GET_SIZE)) -> _curr_gcseg_copy;
        MARK_OLD;
        chain(_new, Copyscan, App_key);

    NFULLREC:
    USERNFREC:
        if _old!KEY!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN then
            ;;; need to double-align record
            if _new@(w.t->d) == _new then
                ;;; already aligned -- add padding after
                COPY_TO_NEW(_old!KEY!K_RECSIZE_R);
                MARK_OLD;
                -> _old;    ;;; next copy addr
                double_pad_key -> _old!KEY;
                _old@(struct DOUBLE_PAD)++ -> _curr_gcseg_copy;
                goto usernfrec_cont         ;;; don't you just hate it
            else
                ;;; not aligned -- add padding before
                double_pad_key -> _new!KEY;
                _new@(struct DOUBLE_PAD)++ -> _new
            endif
        endif;
    usernfrec_no_da:
        COPY_TO_NEW(_old!KEY!K_RECSIZE_R) -> _curr_gcseg_copy;
        MARK_OLD;

    usernfrec_cont:
        CHECK_DEFER;
        if (_new!KEY ->> _old) >=@(w) _lowest_garbageable then
            ;;; full offset table doesn't include the key field
            Copyscan(_new@KEY)
        endif;
        _old@K_FULL_OFFS_TAB[_0] -> _addr;      ;;; full offset table address
        _addr@(-i){_old!K_FULL_OFFS_SIZE} -> _old;  ;;; lim address
        while _addr <@(-i) _old do
            Copyscan( _new@(w){_addr!(-i)++ -> _addr} );
        endwhile;
        return;

    f_NFULLREC:
    f_USERNFREC:
        unless _old!KEY!K_FLAGS _bitst _:M_K_NO_FULL_FROM_PTR then
            ;;; must copy whole rec
            goto usernfrec_no_da
        endunless;
        _old!FIRST -> _new!FIRST, _old!KEY -> _new!KEY;
        MARK_OLD; CHECK_DEFER;
        if (_new!KEY ->> _old) >=@(w) _lowest_garbageable then
            ;;; full offset table doesn't include the key field
            Copyscan(_new@KEY)
        endif;
        returnif(_zero(_old!K_FULL_OFFS_SIZE));     ;;; no full other than key
        ;;; else FIRST must be full
        _new@FIRST -> _addr, goto SCAN_ADDR;

    PROPERTY:
    f_PROPERTY:
        COPY_TO_NEW(@@(struct PROP)++) -> _curr_gcseg_copy;
        MARK_OLD;
        unless _new!PT_REHASH then true -> _new!PT_REHASH endunless;
        Copyscan(_new@PT_ACTIVE);
        Copyscan(_new@PT_DEFAULT);
        Copyscan(_new@PT_EQ_PDR);
        Copyscan(_new@PT_HASH_PDR);
        if _new!PT_ENTRY_KEY!K_GC_TYPE == _:GCTYPE_PROPENT_TMPCLR
        and not(_doing_saveincr) then
            chain(_new, Copyscan_clrproptab)
        else
            _new@PT_TABLE -> _addr;
            goto SCAN_ADDR
        endif;

    PROPENT_PERM:
    f_PROPENT_PERM:
        COPY_MARK_PROPENT;
    scan_propent_both:
        Copyscan(_new@PTE_ARG);
        Copyscan(_new@PTE_VALUE);
        _new@PTE_NEXT -> _addr, goto SCAN_ADDR;

    PROPENT_TMPARG:
    f_PROPENT_TMPARG:
        COPY_MARK_TMP_PROPENT;
        if UNTRACED(PTE_ARG) then
            ADD_TO_CHAIN_GO_SCAN_NEXT(_prop_tmparg_chain)
        else
            goto scan_propent_both
        endif;

    PROPENT_TMPVAL:
    f_PROPENT_TMPVAL:
        COPY_MARK_TMP_PROPENT;
        if UNTRACED(PTE_VALUE) then
            ADD_TO_CHAIN_GO_SCAN_NEXT(_prop_tmpval_chain)
        else
            goto scan_propent_both
        endif;

    PROPENT_TMPBOTH:
    f_PROPENT_TMPBOTH:
        COPY_MARK_TMP_PROPENT;
        if UNTRACED(PTE_ARG) then
            if UNTRACED(PTE_VALUE) then
                ADD_TO_CHAIN_GO_SCAN_NEXT(_prop_tmpboth_chain)
            else
                ADD_TO_CHAIN_GO_SCAN_NEXT(_prop_tmparg_chain)
            endif
        elseif UNTRACED(PTE_VALUE) then
            ADD_TO_CHAIN_GO_SCAN_NEXT(_prop_tmpval_chain)
        else
            goto scan_propent_both
        endif;

    PROPENT_TMPCLR:
    f_PROPENT_TMPCLR:
        COPY_MARK_TMP_PROPENT;
        dead_prop_entry ->> _new!PTE_ARG -> _new!PTE_VALUE;
        _new@PTE_NEXT -> _addr, goto SCAN_ADDR;


    DESTROY_PROPENT:
    f_DESTROY_PROPENT:
        _old!PTE_ARG    ->> _new!PTE_ARG
                        -> _new!DPTE_OLD; ;;; save org arg in OLD slot also
        _old!KEY        -> _new!KEY;
        _old!PTE_VALUE  -> _new!PTE_VALUE;
        _old!PTE_NEXT   -> _new!PTE_NEXT;
        _old!DPTE_FLAGS -> _new!DPTE_FLAGS;
        _new@(struct DESTROY_PROP_ENTRY)++ -> _curr_gcseg_copy;
        MARK_OLD;
        if _new!DPTE_FLAGS _bitst _DPTE_SCHEDULED then
            ;;; already scheduled for execution -- scan next in
            ;;; scheduled chain (DLINK), then treat as perm
            _old!DPTE_DLINK -> _new!DPTE_DLINK;
            Copyscan(_new@DPTE_DLINK);
            goto scan_propent_both
        endif;
        ;;; set DLINK to point to (new) self (only needed if put on
        ;;; destroy_prop_chain, but UNTRACED overwrites _old).
        _old!FIRST -> _new!DPTE_DLINK;
        if not(_doing_saveincr) and UNTRACED(PTE_ARG) then
            ;;; clear VSCANNED and KEYSAV
            _new!DPTE_FLAGS _biclear (_DPTE_VSCANNED _biset _DPTE_KEYSAV)
                                                    -> _new!DPTE_FLAGS;
            ADD_TO_CHAIN_GO_SCAN_NEXT(_destroy_prop_chain)
        else
            goto scan_propent_both
        endif;

    DESTROY_MARK:
    f_DESTROY_MARK:
        ;;; destroy_mark key superimposed on already processed data
        ;;; just insert new address and mark the key
        _old!FIRST -> _addr!(w);
        _mksimple(_old!KEY) -> _old!KEY;
        return;

    USERNFVEC:
        if _old!KEY!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN then
            ;;; need to double-align record
            if _new@(w.t->d) == _new then
                ;;; already aligned -- add padding after
                COPY_TO_NEW(fast_apply(_old, _old!KEY!K_GET_SIZE));
                MARK_OLD;
                -> _old;
                double_pad_key -> _old!KEY;
                _old@(struct DOUBLE_PAD)++ -> _curr_gcseg_copy;
                _new@KEY -> _addr, goto SCAN_ADDR
            else
                ;;; not aligned -- add padding before
                double_pad_key -> _new!KEY;
                _new@(struct DOUBLE_PAD)++ -> _new
            endif
        endif;
        COPY_TO_NEW(fast_apply(_old, _old!KEY!K_GET_SIZE)) -> _curr_gcseg_copy;
        MARK_OLD;
        _new@KEY -> _addr, goto SCAN_ADDR;
    f_USERNFVEC:
        _old!FIRST -> _new!FIRST, _old!KEY -> _new!KEY;
        MARK_OLD;
        _new@KEY -> _addr, goto SCAN_ADDR;

    USERREC:
    f_USERREC:
        COPY_TO_NEW(_old!KEY!K_RECSIZE_R) -> _curr_gcseg_copy;
        MARK_OLD; CHECK_DEFER;
        _curr_gcseg_copy@POPBASE -> _addr;
        _new@POPBASE -> _new;
        while _new <@(w) _addr do Copyscan(_new), _new@(w)++ -> _new endwhile;
        return;

    USERVEC:
    f_USERVEC:
        COPY_TO_NEW(@@V_WORDS[_old!V_LENGTH] _sub @@POPBASE) -> _curr_gcseg_copy;
        MARK_OLD; CHECK_DEFER;
        _curr_gcseg_copy@POPBASE -> _addr;
        _new@V_WORDS -> _old;
        while _old <@(w) _addr do Copyscan(_old), _old@(w)++ -> _old endwhile;
        _new@KEY -> _addr, goto SCAN_ADDR;

    STACK:
    f_STACK:
        COPY_TO_NEW(_old!RAW_SIZE) -> _curr_gcseg_copy;
        MARK_OLD;
        chain(_new, _old, _curr_gcseg_reloc, Copyscan, App_stack);

    PROCESS:
    f_PROCESS:
        COPY_TO_NEW(@@(struct PROCESS)++) -> _curr_gcseg_copy;
        MARK_OLD;  CHECK_DEFER;
        chain(_new, Copyscan, Copyscan_owner, true, PSWEAK App_process);

    PROCSTATE:
    f_PROCSTATE:
        COPY_TO_NEW(_old!RAW_SIZE) -> _curr_gcseg_copy;
        MARK_OLD;
        chain(_new!PSS_PROCESS, _new, _curr_gcseg_reloc,
                            Copyscan, Copyscan_owner, PSWEAK App_proc_state);

    PLOGPROC:
    f_PLOGPROC:
        COPY_TO_NEW(_old!RAW_SIZE) -> _curr_gcseg_copy;
        MARK_OLD;
        chain(_new, Copyscan, PSWEAK App_plog_proc_state);

    CONST_PLOGVAR:
    f_CONST_PLOGVAR:
        _old!PGV_CONT -> _new;          ;;; the var's contents
        ;;; deref any constant plogvars, to make sure that
        ;;; this var doesn't lead thru a chain of them back to itself...
        while iscompound(_new) and _new!KEY == weakref const_plogvar_key do
            _new!PGV_CONT -> _new
        endwhile;
        if _new == _old then
            ;;; what to do - who knows?
            '<CIRCULAR PROLOGVAR CHAIN>' -> _new
        endif;
        _new -> _old!PGV_CONT;          ;;; replace contents for another time
        _new -> _addr!(w);              ;;; also replaces current arg
        goto SCAN_ADDR;                 ;;; now scan that

    RAWSTRUCT:
        COPY_TO_NEW(_old!RAW_SIZE) -> _curr_gcseg_copy;
        MARK_OLD;
        return;

#_IF DEF VMS
    DESCRIPTOR:
    f_DESCRIPTOR:
        COPY_TO_NEW(@@(struct DESCRIPTOR)++) -> _curr_gcseg_copy;
        MARK_OLD;
        @@(w){_new@DSC_SPEC!DSPEC_PTR, _new!DSC_ITEM} -> _old;
        Copyscan(_new@DSC_ITEM);
        _new!DSC_ITEM@(w){_old} -> _new@DSC_SPEC!DSPEC_PTR;
        return;
#_ENDIF

    DOUBLE_PAD:
    f_DOUBLE_PAD:
    OBJMOD_PAD:
    f_OBJMOD_PAD:
    FREE_BLOCK:
#_IF not(DEF VMS)
    DESCRIPTOR:
    f_DESCRIPTOR:
#_ENDIF
    ERROR:
        mishap(_old, 1, 'Copyscan: BAD STRUCTURE');
        setpop();

    DEFER:
        ;;; come here to add structure to deferred chain
        _deferred -> _new!KEY;
        _old -> _deferred
        ;;; then return
enddefine;      /* Copyscan */


    /*  Complete scanning of deferred structures.
    */
define Copyscan_deferred();
    lvars _old, _new, _key;
    while _deferred /== _NULL do
        ;;; _deferred is the old structure
        _deferred -> _old;
        GC_SET_SEG _old;
        _old!FIRST@(w){_curr_gcseg_reloc} -> _new;
        ;;; new structure key field contains next deferred structure
        _new!KEY -> _deferred;
        ;;; recover the key of the new structure from the old
        _mkcompound(_old!KEY) ->> _key -> _new!KEY;

        ;;; then scan the fields of the new structure
        ;;; -- only for types which use DEFER_CHECK in Copyscan
        ;;; and structures made non-writeable by sys_lock_system.

        GCTYPE_GO_ON '' _pint(_key!K_GC_TYPE);  ;;; see gcdefs.ph

        PROCEDURE:
            SCAN_PD_TABLE(_new, _old, _key);    ;;; scan table/frozvals
            Copyscan(_new@PD_PROPS);
            Copyscan(_new@PD_UPDATER);
            nextloop;

        FULLREC1:
            Copyscan(_new@FIRST); nextloop;

        FULLREC2:
            Copyscan(_new@FIRST);
            Copyscan(_new@SECOND);
            nextloop;

        FULLREC:
            Copyscan(_new@FIRST);
            App_range(_new@SECOND, @@POPBASE{_key!K_RECSIZE_R} _sub @@SECOND,
                                                                Copyscan);
            nextloop;

        USERVEC:
            Copyscan(_new@KEY);
        VECTOR:
            App_range(_new@V_WORDS, @@(w)[_new!V_LENGTH], Copyscan);
            nextloop;

        FULL2ND_REC2:
            Copyscan(_new@SECOND); nextloop;

        WORD:
            Copyscan(_new@W_IDENTIFIER);
            Copyscan(_new@W_STRING);
            Do_dict_next(_new);
            nextloop;

        KEY:
            App_key(_new, Copyscan); nextloop;

        NFULLREC:
        USERNFREC:
            if _key >=@(w) _lowest_garbageable then
                ;;; full offset table doesn't include the key field
                Copyscan(_new@KEY)
            endif;
            _key@K_FULL_OFFS_TAB[_0] -> _old;       ;;; full offset table address
            _old@(-i){_key!K_FULL_OFFS_SIZE} -> _key;   ;;; lim address
            while _old <@(-i) _key do
                Copyscan( _new@(w){_old!(-i)++ -> _old} )
            endwhile;
            nextloop;

        USERNFVEC:
            Copyscan(_new@KEY); nextloop;

        USERREC:
            App_range(_new@POPBASE, _key!K_RECSIZE_R, Copyscan);
            nextloop;

        BYTEVEC:
        DDECIMAL:
        RAWSTRUCT:
            nextloop;

        PROCESS:
            PSWEAK App_process(_new, Copyscan, Copyscan_owner, true);
            nextloop;

        DESCRIPTOR:
#_IF DEF VMS
            @@(w){_new@DSC_SPEC!DSPEC_PTR, _new!DSC_ITEM} -> _old;
            Copyscan(_new@DSC_ITEM);
            _new!DSC_ITEM@(w){_old} -> _new@DSC_SPEC!DSPEC_PTR;
            nextloop;
#_ENDIF

        PROPERTY:
        PROPENT_PERM:
        PROPENT_TMPARG:
        PROPENT_TMPVAL:
        PROPENT_TMPBOTH:
        PROPENT_TMPCLR:
        DESTROY_PROPENT:
        DESTROY_MARK:
        STACK:
        PROCSTATE:
        PLOGPROC:
        CONST_PLOGVAR:
            mishap(_old, 1, 'Gc$-Copyscan_deferred: UNEXPECTED STRUCTURE');

        DOUBLE_PAD:
        OBJMOD_PAD:
        FREE_BLOCK:
        ERROR:
            mishap(_old, 1, 'Gc$-Copyscan_deferred: BAD STRUCTURE')
    endwhile;

    ;;; update the copy pointer for the current seg
    _curr_gcseg_copy -> _curr_gcseg!GC_SEG_COPY_PTR
enddefine;      /* Copyscan_deferred */



;;; --- SETUP PROCEDURES -----------------------------------------------------

define Setup_copy_gc(_open_seg_reloc, doing_syslocksys);
    lvars doing_syslocksys, _seg = _lowest_heap_seg, _gcseg, _copy, _base,
        _lim, _open_seg_reloc, _next_free_mem = Get_mem_break(),
        _start_mem = _next_free_mem;

    define lconstant Alloc_workspace(_size);
        lvars _size, _newnext = _next_free_mem@(w){_size};
        ;;; Unix note: after the first of these Set_mem_breaks, _userhi is no
        ;;; longer at the end of of memory, so user stack underflow isn't
        ;;; trapped...
        if Set_mem_break(_newnext, _0) == _-1 then
            Set_mem_break(_start_mem, _0) -> ;  ;;; reset mem allocated
            exitfrom(false, Setup_copy_gc)      ;;; can't do -- return false
        else
            _newnext -> _next_free_mem
        endif
    enddefine;

    _next_free_mem ->> _gcseg -> _gcseg_table;  ;;; gcseg table start
    ;;; Allocate space for gcseg table -- number of entries is number of
    ;;; heap segs plus 1 extra for limit entry at end
    Alloc_workspace(@@(struct GC_SEG)[
                            Pop_seg_count(_lowest_heap_seg,false) _add _1]);
    _next_free_mem@~POPBASE -> _copy;   ;;; start copying space after table

    ;;; enter table entries for segments (includes open seg)
    while _seg <@(struct SEG) _seg_table_next_free do
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
            _seg!SEG_BASE_PTR ->> _base -> _gcseg!GC_SEG_BASE_PTR;
            _seg!SEG_FREE_PTR ->> _lim -> _gcseg!GC_SEG_LIM_PTR;
            _copy@(w.r->d) -> _copy;
            @@(w){_copy, _base} -> _gcseg!GC_SEG_RELOC;
            if _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS then
                ;;; fixed-address (only uses RELOC)
                _NULL
            else
                _copy
            endif ->> _gcseg!GC_SEG_COPY_BASE -> _gcseg!GC_SEG_COPY_PTR;
            _copy@(w){_lim, _base} -> _copy;
            if doing_syslocksys
            and not(_seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS) then
                ;;; allow an extra page of copy space per segment
                _copy@(vpage)++ -> _copy
            endif;
            _gcseg@(struct GC_SEG)++ -> _gcseg
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile;

    ;;; last, a limit entry
    _-1 ->> _gcseg!GC_SEG_BASE_PTR -> _gcseg!GC_SEG_LIM_PTR;
    _copy@(w.r->d) ->> _copy -> _gcseg!GC_SEG_COPY_BASE;

    ;;; allocate copying space
    Alloc_workspace(@@(w){_copy@POPBASE, _next_free_mem});

    ;;; set the globals initially for the open seg
    _gcseg--@(struct GC_SEG) -> _gcseg;
    _gcseg!GC_SEG_BASE_PTR -> _curr_gcseg_base;
    _gcseg!GC_SEG_LIM_PTR  ->> _curr_gcseg_lim -> _curr_gcseg_test_lim;
    _gcseg!GC_SEG_COPY_PTR  -> _curr_gcseg_copy;
    ;;; adjust open seg reloc
    _gcseg!GC_SEG_RELOC _sub _open_seg_reloc ->> _gcseg!GC_SEG_RELOC
                                            -> _curr_gcseg_reloc;
    _gcseg -> _curr_gcseg;

    _NULL -> _deferred;         ;;; clear deferred chain
    true -> _copy_gc;           ;;; say this is a copying gc

    true        ;;; to say done OK
enddefine;

    /*  Process a fixed-address seg: copy back non-garbage structures
        and turn unused space into free blocks.
    */
define lconstant Fixed_seg(_seg, reloc);
    lvars   key, reloc, procedure (fixed_free_p),
            _rec = _seg!SEG_BASE_PTR, _lim = _seg!SEG_FREE_PTR,
            _freeblk = _NULL, _ngrec = _rec, _size, _seg;

    lconstant macro COPY_NG = [
        _moveq(@@(w){_rec, _ngrec}, _ngrec@POPBASE{reloc}, _ngrec@POPBASE)
        ];

    if _seg!SEG_FLAGS _bitst _M_SEG_FIXED_EXPTRS then
        ;;; These contain fixed-address external pointers tied to an
        ;;; external relocation mechanism, and freed space can't be
        ;;; reused (or rather, the XP_PTR fields can't).
        Make_filler_struct
    else
        weakref[free_block_key] Make_free_block
    endif -> fixed_free_p;

    while _rec <@(w) _lim do
        if issimple(_rec!KEY ->> key) then
            ;;; not garbage
            _mkcompound(key) -> key;
            ;;; can't use GET_SIZE procedure on new rec since key will not
            ;;; be correct for user key, so apply to old rec with FIRST
            ;;; field restored
            _rec@(w){reloc}!FIRST -> _rec!FIRST;    ;;; restore old rec FIRST
            fast_apply(_rec, key!K_GET_SIZE) -> _size;

            if _freeblk /== _NULL then
                ;;; finish up last free block
                fixed_free_p(_freeblk, @@(w){_rec, _freeblk});
                _NULL -> _freeblk;
                ;;; start new area to copy back
                _rec -> _ngrec
            endif;
            if key!K_FLAGS _bitst _:M_K_NO_FULL_FROM_PTR then
                ;;; only 1st two words have been copied -- only KEY needs
                ;;; restoring
                if _ngrec /== _rec then COPY_NG -> endif;
                _rec@(w){reloc}!KEY -> _rec!KEY;    ;;; restore key
                if key!K_GC_TYPE == _:GCTYPE_FREE_BLOCK then
                    ;;; a non-garbage fixed free block -- treat this as if it
                    ;;; were garbage, but keep it separate (so pointers to it
                    ;;; remain valid)
                    fixed_free_p(_rec, _size)
                endif;
                _rec@(w){_size} -> _ngrec
            ;;; else accumulate into area to copy back
            endif;
            _rec@(w){_size} -> _rec

        else
            ;;; garbage
            if _freeblk == _NULL then
                ;;; first free after non-garbage -- copy records back
                if _ngrec /== _rec then COPY_NG -> endif;
                ;;; start new free block
                _rec -> _freeblk
            ;;; else accumulate into free block
            endif;
            _rec@(w){fast_apply(_rec, key!K_GET_SIZE)} -> _rec

        endif;
    endwhile;

    if _freeblk == _NULL then
        ;;; copy back last records
        COPY_NG @~POPBASE -> _freeblk
    endif;

    if _seg!SEG_FLAGS _bitst _M_SEG_FIXED_EXPTRS then
        _freeblk -> _rec;
        _seg!SEG_BASE_PTR@(vpage){_seg!SEG_SIZE} -> _freeblk;
        @@(w){_freeblk, _rec} -> _size;
        if _nonzero(_size) then Make_filler_struct(_rec, _size) endif
    endif;

    ;;; truncate used size of seg
    _freeblk -> _seg!SEG_FREE_PTR
enddefine;

define lconstant Update_segs();
    lvars _seg = _lowest_heap_seg, _gcseg = _gcseg_table, _base, _cbase;

    ;;; Do -Fixed_seg- on all fixed segs. (Must do this before copying back
    ;;; any relocatable segs, since that will overwrite the old mem occupied
    ;;; by (relocatable) user keys of fixed structures, invalidating the use
    ;;; of such keys in -Fixed_seg-. It's just as well fixed-address keys
    ;;; can't be created ... )
    while _seg <@(struct SEG) _seg_table_next_free do
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
            if _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS then
                Fixed_seg(_seg, _gcseg!GC_SEG_RELOC)
            endif;
            _gcseg@(struct GC_SEG)++ -> _gcseg
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile;

    ;;; copy back relocatable segs
    _lowest_heap_seg -> _seg; _gcseg_table -> _gcseg;
    while _seg <@(struct SEG) _seg_table_next_free do
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
            unless _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS then
                _gcseg!GC_SEG_COPY_BASE -> _cbase;
                _cbase@(w)-{_gcseg!GC_SEG_RELOC} ->> _base
                                                 -> _seg!SEG_BASE_PTR;
                ;;; copy back
                _moveq(@@(w){_gcseg!GC_SEG_COPY_PTR, _cbase},
                        _cbase@POPBASE, _base@POPBASE)@~POPBASE
                                                -> _seg!SEG_FREE_PTR
            endunless;
            _gcseg@(struct GC_SEG)++ -> _gcseg
        endunless;
        _seg@(struct SEG)++ -> _seg
    endwhile
enddefine;

define Do_copy_gc();

    ;;; scan and copy from the roots
    App_roots(Copyscan, Copyscan_clrproptab, Copyscan_owner, Copyscan_deferred,
                                                                    true);
    ;;; finish scan and copy
    Finish_scan(Copyscan, Copyscan_deferred);

    ;;; move segs back from copy areas and update the seg table
    Update_segs();

    ;;; Unix note: after this, user stack underflow is trapped again...
    Set_mem_break(_gcseg_table, _0) ->      ;;; free workspace
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
--- John Gibson, Feb 19 1993
        Changed accesses to _retaddr_ptr in Copyscan_owner to be "csword"
--- John Gibson, Sep  3 1992
        o Substituted M_K_NO_FULL_FROM_PTR from byte access key flag
        o Modified to allow non-garbage fixed free blocks
--- John Gibson, Sep 14 1991
        Changes for new format bytevecs
--- John Gibson, Sep 12 1991
        Added RAWSTRUCT label to GCTYPE switches
--- John Gibson, Apr 27 1991
        Changed to -Fixed_seg- to cope with _M_SEG_FIXED_EXPTRS segs
--- John Gibson, Feb  5 1991
        Added optimisations for fixed-address M_K_NO_FULL_FROM_PTR structures
--- John Gibson, Dec  5 1990
        Changes to VMS descriptors
--- John Gibson, Sep 20 1990
        Altered -Update_segs- so that -Fixed_seg- is done first on all fixed
        segs, before copying back any relocatable segs (otherwise, the latter
        can overwrite the old mem occupied by a (relocatable) user key of a
        fixed structure).
--- John Gibson, Sep 12 1990
        Fixed bug with user structures in -Fixed_seg-.
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
--- Roger Evans, Aug 30 1988
        removed simple2 marking and added DESTROY_MARK code
--- John Gibson, Aug 22 1988
        Added OBJMOD_PAD to GCTYPE_GO_ONs
--- Roger Evans, Aug 19 1988
        added DESTROY_PROPENT code and 'destroy' marks in Copyscan
--- John Gibson, Aug  6 1988
        Changes to support doubleword-aligned structures.
--- John Gibson, Jul  5 1988
        New treatment of return addresses in stack frames
--- John Gibson, Feb 25 1988
        Sectionised garbage collector, weakref'ed stuff for processes
        and prolog.
 */
