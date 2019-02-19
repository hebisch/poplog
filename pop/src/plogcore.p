/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/plogcore.p
 > Purpose:
 > Author:          John Gibson & Jonathan Laventhol (see revisions)
 > Documentation:   REF *PROLOG
 */

;;; ---------------- PROLOG VARIABLES AND UNIFICATION ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gcdefs.ph'

constant
        procedure (prolog_deref),
        _plog_trail_push, _prolog_save, _prolog_restore, _srchain
    ;

vars
        _plog_area_hi, _plog_contn_barrier, _plog_trail_lim, _plog_trail_sp,
        _plog_trail_barrier, _plog_area_lo, _plog_contn_sp,
        _plog_save_contn_sp, _plog_contn_top
    ;

section $-Sys;

constant
        procedure (Chainto, Eq__Integer, Eq__Float, Plog$-Area_overflow),
        Gc$-dead_prop_entry

    ;

endsection;

    ;;; force inclusion of prolog area -- see plog_area.p
uses (prolog_reset);


;;; ----------------------------------------------------------------------

section $-Sys$-Plog =>
                    prolog_unify prolog_unifyc prolog_type isprologvar
                    prolog_undefvar prolog_newvar prolog_deref prolog_assign
                    prolog_assign_check prolog_var_number prolog_match_constant
                    prolog_is_pair prolog_is_functor prolog_own_invoke
                    prolog_own_exit_on_success prolog_push_continuation
                    prolog_push_continuations prolog_apply_continuation
                    prolog_set_contn_trap prolog_barrier_apply
                    prologvar_key
                    ;

    ;;; the marker ref at the end of plogvar chains
lconstant
    var_end_marker = consref(0);
    ;;; the cont of -var_end_marker- points to itself
    var_end_marker -> cont(var_end_marker);


    ;;; global variables controlling prolog (see also plog_area.p)
vars

    ;;; number of prolog processes inside current process
    barrier_count       = false,

    ;;; next free prologvar in the chain
    $- _plog_next_var   = var_end_marker,
    ;

lvars
    ;;; start of current chain of prologvars
    _var_chain          = false,
    _continuation_cut,      ;;; see prolog_own_exit_on_success
    ;

    ;;; This controls whether functors in prolog terms must be fixed, or are
    ;;; allowed to be variables. The default for the standard system is "fixed"
    ;;; there is another one in plogterms.p
lconstant macro
    PROLOG_FIXED_FUNCTORS = true;

lconstant syntax (
    PLOG_SAVE       = valof("sysPLOG_SAVE"),
    PLOG_RESTORE    = valof("sysPLOG_RESTORE"),
    );


define prolog_unify(term1, term2);
    lvars term1, term2, _work, _tmp;
START:
    prologvar_key -> _work;
    while iscompound(term1) and term1!KEY == _work do
        if (fast_cont(term1) ->> _tmp) == term1 then
            while iscompound(term2) and term2!KEY == _work
            and (fast_cont(term2) ->> _tmp) /== term2 do
                _tmp -> term2
            endwhile;
            unless term1 == term2 then
                term2 -> fast_cont(term1);
                _plog_trail_push(term1);
            endunless;
            return(true);
        else
            _tmp -> term1;
        endif
    endwhile;
    while iscompound(term2) and term2!KEY == _work do
        if (fast_cont(term2) ->> _tmp) == term2 then
            term1 -> fast_cont(term2);
            _plog_trail_push(term2);
            return(true)
        else
            _tmp -> term2
        endif
    endwhile;
    if term1 == term2 then
        return(true)
    elseif issimple(term1) then
        term2, term1;
        return( if isinteger(term1) then
                    Eq__Integer()
                else
                    weakref Eq__Float()
                endif)
    elseif issimple(term2) then
        term1, term2;
        return( if isinteger(term2) then
                    Eq__Integer()
                else
                    weakref Eq__Float()
                endif)
    endif;
    term1!KEY -> _work;
    if term2!KEY /== _work then
        return(false)
    elseif _work == prologterm_key then
        term1!PGT_LENGTH -> _work;
        if term2!PGT_LENGTH == _work then
#_IF PROLOG_FIXED_FUNCTORS
            unless fast_prolog_functor(term1) == fast_prolog_functor(term2)
#_ELSE
            _checkall();
            unless prolog_unify(fast_prolog_functor(term1),
                                fast_prolog_functor(term2))
#_ENDIF
            then
                return(false);
            endunless;
            _pint(_work _sub _1) -> _work;  ;;; arity now
            if _work == 1 then
                _CHECKINTERRUPT;
            else
                _checkall();
                repeat
                    unless prolog_unify(
                        fast_prolog_arg(_work, term1),
                        fast_prolog_arg(_work, term2))
                    then
                        return(false);
                    endunless;
                    _work fi_- 1 -> _work;
                quitif(_work == 1);
                endrepeat;
            endif;
            fast_prolog_arg(1, term1) -> term1;
            fast_prolog_arg(1, term2) -> term2;
            goto START;
        endif;
    elseif _work == pair_key then
        _checkall();
        if prolog_unify(fast_front(term1), fast_front(term2)) then
            fast_back(term1) -> term1;
            fast_back(term2) -> term2;
            goto START;
        endif;
    else
        return(term1 = term2)
    endif;
    false;
enddefine;

    ;;; continuation-passing version
define prolog_unifyc(c) with_nargs 3;
    lvars c;
    PLOG_SAVE;
    if prolog_unify()
        then c()
    endif;
    PLOG_RESTORE;
enddefine;


;;; prolog_type:
;;;     extracts the prolog "gross type" field from a POP item

define prolog_type(/* term */) with_nargs 1;
    _pint(_datakey(/* term */)!K_PLOG_TYPE);
enddefine;


define isprologvar(item);
    lvars item;
    if iscompound(item) and item!KEY == prologvar_key then true
    else false
    endif;
enddefine;

    ;;; = on prolog objects means 'unified' and so we must deref chains and
    ;;; ignore them.  all other Eq__ procedures chain to this one when
    ;;; comparing against a plogvar.
define lconstant Eq__Plogvar(item, plogvar);
    lvars item plogvar;
    prolog_deref(plogvar) -> plogvar;
    prolog_deref(item) -> item;
    if isprologvar(plogvar) then
        item == plogvar     ;;; uninstantiated variables must be sharing
    elseif isprologvar(item) then
        false               ;;; item is dereffed, so must be uninstantiated var
    else
        CHAIN_EQ(item, plogvar) ;;; do it for dereffed objects
    endif;
enddefine;

define prolog_undefvar(item);
    lvars item;
    if iscompound(item) and item!KEY == prologvar_key
    and fast_cont(item) == item then
        true
    else
        false
    endif;
enddefine;

    ;;; come here when the ref at the end of a block of variables is reached
    ;;; (in which case the ref cont points to the next block),
    ;;; or _plog_next_var is the -var_end_marker- ref
define New_var() with_props prolog_newvar;
    lvars plogvar, _count, _end_marker = var_end_marker;
    _plog_next_var!RF_CONT -> plogvar;
    if plogvar /== _end_marker
    or (_plog_next_var == _end_marker and (_var_chain ->> plogvar)) then
        ;;; not the last block, or chain already started
        plogvar@(struct PLOGVAR)++ -> _plog_next_var;      ;;; next in the block
        plogvar -> plogvar!PGV_CONT;            ;;; make it undef
        return(plogvar)                         ;;; and return it
    endif;

    ;;; else this was the last block, or chain not yet (re)started
    ;;; -- allocate some more. N.B. If Get_store causes a GC, then
    ;;; _plog_next_var will become var_end_marker ...
    Get_store(@@(struct PLOGVAR)[_63] _add @@(struct REF)[_1]) -> plogvar;
    if _plog_next_var == _end_marker then
        ;;; (re)start chain
        plogvar -> _var_chain
    else
        ;;; chain this block onto the last
        plogvar -> _plog_next_var!RF_CONT
    endif;

    plogvar;                            ;;; leave the result on the stack
    plogvar@(struct PLOGVAR)++ -> _plog_next_var;   ;;; next one
    _63 -> _count;
    until _zero(_count) do
        plogvar -> plogvar!PGV_CONT;    ;;; make undef
        prologvar_key -> plogvar!KEY;
        plogvar@(struct PLOGVAR)++ -> plogvar;
        _count _sub _1 -> _count
    enduntil;

    ;;; initialise the ref at the end of the block
    var_end_marker -> plogvar!RF_CONT;
    ref_key -> plogvar!KEY
enddefine;

define prolog_newvar() -> plogvar;
    lvars plogvar;
    if (_plog_next_var ->> plogvar)!KEY == ref_key then
        ;;; reached the ref at the end of a block of variables
        chain(New_var)
    endif;
    plogvar@(struct PLOGVAR)++ -> _plog_next_var;      ;;; next in the block
    plogvar -> plogvar!PGV_CONT;            ;;; make it undef
enddefine;

define prolog_deref(item);
    lvars item new _work;
    prologvar_key -> _work;
    while iscompound(item) and item!KEY == _work
    and (fast_cont(item) ->> new) /== item do
        new -> item
    endwhile;
    item
enddefine;

define prolog_assign(plogvar,item);
    lvars plogvar item;
    item -> fast_cont(plogvar);
    _plog_trail_push(plogvar);
enddefine;

define prolog_assign_check(plogvar, item);
    lvars plogvar item;
    if iscompound(plogvar) and plogvar!KEY == prologvar_key
     and plogvar == fast_cont(plogvar) then
        item -> fast_cont(plogvar);
        _plog_trail_push(plogvar);
    else
        mishap(plogvar, 1, 'CAN\'T ASSIGN TO NON-PROLOGVAR')
    endif
enddefine;


lvars
    ;;; chain of prop entries numbering vars
    varnum_chain= 0,
    ;;; next var number to allocate
    next_varnum = 1,
    ;
;;;
define prolog_var_number(plogvar);
    lvars plogvar, last, _entry;

    lconstant
        varnum_entry_key = struct KEY_PROP_ENTRY =>> {%
            _NULL,                  ;;; K_GC_RELOC
            key_key,                ;;; KEY
            _:M_K_PROP_ENTRY _biset _:M_K_PLOG_VARNUM _biset _:M_K_WRITEABLE,
                                    ;;; K_FLAGS
            _:GCTYPE_PROPENT_TMPARG, ;;; K_GC_TYPE
            procedure; ->, @@(struct PROP_ENTRY)++ endprocedure,
                                    ;;; K_GET_SIZE

            "prop_entry",           ;;; K_DATAWORD
            false,                  ;;; K_SPEC
            false,                  ;;; K_RECOGNISER
            WREF Exec_nonpd,        ;;; K_APPLY
            nonop ==,               ;;; K_SYS_EQUALS
            WREF nonop ==,          ;;; K_EQUALS
            Minimal_print,          ;;; K_SYS_PRINT
            WREF Minimal_print,     ;;; K_PRINT
            WREF Fullrec1_hash,     ;;; K_HASH

            _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
            _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
            _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
            _0,                     ;;; K_SPARE_BYTE

            "tmparg"                ;;; K_PROP_TYPE_NAME
            %};

    unless plogvar then
        ;;; false means reset numbering
        1 -> next_varnum;
        0 -> varnum_chain;
        return
    endunless;
    prolog_deref(plogvar) -> plogvar;
    unless isprologvar(plogvar) then
        ;;; not an undefvar
        return(false)
    endunless;
    varnum_chain -> _entry;
    (ident varnum_chain)@~PTE_NEXT -> last; ;;; !

    ;;; find the lowest numbered _entry for a var which derefs to plogvar
    while iscompound(_entry) do
        if _entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
            ;;; killed by garbage collector - remove it from chain
            _entry!PTE_NEXT -> last!PTE_NEXT
        elseif prolog_deref(_entry!PTE_ARG) == plogvar then
            return(_entry!PTE_VALUE)
        else
            _entry -> last
        endif;
        _entry!PTE_NEXT -> _entry
    endwhile;

    ;;; not found - allocate next number to a new _entry added on the end
    Get_store(@@(struct PROP_ENTRY)++) -> _entry;
    varnum_entry_key -> _entry!KEY;
    0 -> _entry!PTE_NEXT;
    plogvar -> _entry!PTE_ARG;
    next_varnum ->> _entry!PTE_VALUE;       ;;; return result
    next_varnum fi_+ 1 -> next_varnum;
    _entry -> last!PTE_NEXT
enddefine;

define Reset_vars();
    false -> _var_chain;
    var_end_marker -> _plog_next_var;
    prolog_var_number(false)            ;;; reset variable numbering
enddefine;

define prolog_match_constant(item, const);
    lvars item const _new;
    while iscompound(item) and item!KEY == prologvar_key do
        fast_cont(item) -> _new;
        if _new == item then
            const -> fast_cont(item);
            _plog_trail_push(item);
            return(true)
        else
            _new -> item
        endif
    endwhile;
    item == const
enddefine;

define prolog_is_pair(item);
    lvars item _new;
    repeat
        unless iscompound(item) then
            return(false, false, item)
        elseif item!KEY == prologvar_key then
            fast_cont(item) -> _new;
            if _new == item then
                return(item, true, item)
            else
                _new -> item
            endif
        elseif item!KEY == pair_key then
            return(true, false, item)
        else
            return(false, false, item)
        endunless
    endrepeat;
enddefine;

define prolog_is_functor(item, word, _len);
    lvars item word _len _new;
    repeat
        unless iscompound(item) then
            return(false, false, item)
        elseif item!KEY == prologvar_key then
            fast_cont(item) -> _new;
            if _new == item then
                return(item, true, item)
            else
                _new -> item
            endif
        elseif item!KEY == prologterm_key and _int(_len) == item!PGT_LENGTH
        and fast_prolog_functor(item) == word then
            return(true, false, item)
        else
            return(false, false, item)
        endunless
    endrepeat;
enddefine;

define prolog_own_invoke() with_nargs 1;
    ()();
    false
enddefine;

define prolog_own_exit_on_success();
    ;;; _continuation_cut has just been set by prolog_apply_continuation
    ;;; truncate the contn stack back to point immediately before
    ;;; the contn for this procedure was pushed
    _continuation_cut -> _plog_contn_sp;
    _plog_contn_barrier@(w){_plog_contn_sp _sub _PLOG_SAFEGAP}
                                                    -> _plog_trail_lim;
    _sp_flush() -> ;
    chain((true, identfn, fast_chain), prolog_own_invoke, Chainto)
enddefine;

;;; --------------------------------------------------------------------

define prolog_push_continuation(_len);
    lvars _len, _end, _lim;
    _plog_contn_barrier@(w){_plog_contn_sp} -> _end;    ;;; current end
    _plog_contn_top -> _end--!(w) -> _end;  ;;; store link to current top
    @@(w)[_int(_len)] -> _len;              ;;; length -> offset size
    _end@(w)-{_len} -> _lim;
    while _end >@(w) _lim do
        -> _end--!(w) -> _end;              ;;; store continuation values
    endwhile;
    _len -> _end--!(w) -> _end;             ;;; store size
    @@(w){_end, _plog_contn_barrier} ->> _plog_contn_top    ;;; now current top
                                  -> _plog_contn_sp;        ;;; and current end
    _end@(w)-{_PLOG_SAFEGAP} ->> _end -> _plog_trail_lim;
    if _end <@(w) _plog_trail_sp then
        ;;; need to increase space in the prolog stack area
        Area_overflow()
    endif
enddefine;

define prolog_push_continuations(_num);
    lvars _num, _end, _lim, _len;
    _plog_contn_barrier@(w){_plog_contn_sp} -> _end;    ;;; current end
    until _num == 0 do
        _plog_contn_top -> _end--!(w) -> _end; ;;; store link to curr top
        @@(w)[_int()] -> _len;              ;;; length from stack -> offset size
        _end@(w)-{_len} -> _lim;
        while _end >@(w) _lim do
            -> _end--!(w) -> _end;          ;;; store continuation values
        endwhile;
        _len -> _end--!(w) -> _end;         ;;; store offset size
        @@(w){_end, _plog_contn_barrier} -> _plog_contn_top;    ;;; now current top
        _end@(w)-{_PLOG_SAFEGAP} -> _lim;
        if _lim <@(w) _plog_trail_sp then
            ;;; need to increase space in the prolog stack area
            _lim -> _plog_trail_lim;
            @@(w){_end, _plog_contn_barrier} -> _plog_contn_sp;    ;;; set new end
            Area_overflow();
            ;;; the continuation stack may have moved
            ;;; so recompute the value of _end
            _plog_contn_barrier@(w){_plog_contn_sp} -> _end;    ;;; current end
        endif;
        _num fi_- 1 -> _num;
    enduntil;
    _lim -> _plog_trail_lim;
    @@(w){_end, _plog_contn_barrier} -> _plog_contn_sp;     ;;; set new end
enddefine;

define prolog_apply_continuation();
    lvars _top, _lim;
    _plog_contn_barrier@(w){_plog_contn_top} -> _top;   ;;; current top contn
    _top!(w)++ -> _top -> _lim;     ;;; contn length
    _top@(w){_lim} -> _lim;         ;;; to get limit
    while _top <@(w) _lim do
        _top!(w)++ -> _top          ;;; stack args and identifier
    endwhile;
    _top!(w)++ -> _top -> _plog_contn_top;  ;;; set new top to link
    @@(w){_top, _plog_contn_barrier} -> _lim;
    if _lim _slteq _plog_save_contn_sp then
        ;;; pushed since last choice point - can reclaim space
        _lim -> _plog_contn_sp;
        _top@(w)-{_PLOG_SAFEGAP} -> _plog_trail_lim
    endif;
    _lim -> _continuation_cut;      ;;; see prolog_own_exit_on_success
    fast_chain(fast_idval(()!W_IDENTIFIER))
enddefine;

    ;;; create prolog barriers without actually changing the globals
define Create_barrier() -> _contn -> _trail;
    lvars _contn, _trail;
    ;;; create a barrier on the continuation stack
    _plog_contn_barrier@(w){_plog_contn_sp} -> _contn;
    ;;; barrier marker is 3 words: a negative length (-2 words) of the
    ;;; rest of the marker, the value of _plog_contn_top,
    ;;; and the contn stack size
    _plog_contn_top -> _contn--!(w) -> _contn;
    @@(w){_plog_contn_barrier, _contn} -> _contn--!(w) -> _contn;
    @@(w)[_-2] -> _contn--!(w) -> _contn;   ;;; negative length block marker

    ;;; create one on the trail
    _plog_trail_sp -> _trail;
    ;;; barrier marker is 2 words: a spare for use in garbage collection
    ;;; (set to popint 0), and the process trail length as a pop integer
    0 -> _trail!(w)++ -> _trail;            ;;; the spare
    _pint(@@(w){_trail, _plog_trail_barrier}) -> _trail!(w)++ -> _trail;
enddefine;

    ;;; set prolog barriers -- create them and change the globals
define Set_barrier();
    Create_barrier() -> _plog_contn_barrier
                            ->> _plog_trail_barrier -> _plog_trail_sp;
    _plog_contn_barrier@(w)-{_PLOG_SAFEGAP} -> _plog_trail_lim;
    _0 -> _plog_contn_sp;
enddefine;

define Unset_barrier();
    lvars _addr, _size;
    ;;; remove the continuation stack barrier
    _plog_contn_barrier@(w)++ -> _addr;         ;;; erase block marker marker
    ;;; restore saved values in block marker
    _addr!(w)++ -> _addr -> _size;
    _addr@(w){_size} -> _plog_contn_barrier;
    _addr!(w)++ -> _addr -> _plog_contn_top;
    @@(w){_addr, _plog_contn_barrier} -> _plog_contn_sp;
    _addr@(w)-{_PLOG_SAFEGAP} -> _plog_trail_lim;

    ;;; remove the trail barrier
    _plog_trail_barrier -> _addr;
    _int(_addr--!(w) -> _addr) -> _size;        ;;; process' trail size
    _addr@(w)-{_size} -> _plog_trail_barrier;
    _addr--@(w) -> _plog_trail_sp;              ;;; remove the spare
enddefine;

define prolog_set_contn_trap(setting);
    lvars setting;
    prolog_push_continuation(false, "prolog_set_contn_trap", 2);
    unless setting then
        mishap(0, 'PROLOG CONTINUATION STACK EMPTY');
    endunless;
enddefine;

define prolog_barrier_apply(pdr);
    lvars pdr;

    /* _var_chain and _plog_next_var must NEVER be local to any other
    procedure, since the garbage collector has to treat their saved values on
    the callstack specially - see GC routines below
    */
    dlocal _var_chain, _plog_next_var, _plog_save_contn_sp;

    define lconstant Barrier() with_nargs 1;
        ;;; do nothing for process suspend/resume
        if (/*context*/) fi_> 2 then return endif;
        ;;; set barriers on the continuation stack and trail
        Set_barrier();
        ;;; increase the count of prolog barriers
        if barrier_count then
            barrier_count fi_+ 1
        else
            1
        endif -> barrier_count
    enddefine;

    define updaterof Barrier() with_nargs 1;
        ;;; do nothing for process suspend/resume
        if (/*context*/) fi_> 2 then return endif;
        ;;; reduce count of barriers
        if barrier_count fi_> 1 then
            barrier_count fi_- 1
        else
            false
        endif -> barrier_count;
        ;;; unbar the trail and continuation stack
        Unset_barrier()
    enddefine;

    dlocal 0 % Barrier(dlocal_context) %;

    ;;; start a new chain of plogvars
    false -> _var_chain;
    var_end_marker -> _plog_next_var;
    ;;; create a continuation to trap contn stack underflow
    prolog_set_contn_trap(true);

    ;;; run procedure
    pdr()
enddefine;


;;; --- KEYS ---------------------------------------------------------------

define lconstant Plogvar_print(plogvar);
    lvars plogvar;
    if pop_pr_level == 0 then return(Minimal_print(plogvar)) endif;
    Print_str('<prologvar ');
    if fast_cont(plogvar) == plogvar then       ;;; uninstantiated
        cucharout(`_`);
        sys_syspr(prolog_var_number(plogvar))
    else
        pr(fast_cont(plogvar))
    endif;
    cucharout(`>`)
enddefine;

constant
    prologvar_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_RECORD _biset _:M_K_COPY _biset
        _:M_K_MATCH_VAR _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_FULLREC1,      ;;; K_GC_TYPE
        Rec1_getsize,           ;;; K_GET_SIZE

        "prologvar",            ;;; K_DATAWORD
        [full],                 ;;; K_SPEC
        isprologvar,            ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Plogvar,            ;;; K_SYS_EQUALS
        WREF Eq__Plogvar,       ;;; K_EQUALS
        Plogvar_print,          ;;; K_SYS_PRINT
        WREF Plogvar_print,     ;;; K_PRINT
        WREF Fullrec1_hash,     ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_VAR,      ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct PLOGVAR)++,   ;;; K_RECSIZE_R
        prolog_newvar,          ;;; K_CONS_R
        fast_cont,              ;;; K_DEST_R
        {^fast_cont},           ;;; K_ACCESS_R
        %};

endsection;     /* $-Sys$-Plog */


;;; --- PROLOG TRAIL GARBAGE COLLECTION --------------------------------------

section $-Sys$-Gc;

    ;;; Special GC key for making prologvars constant
constant
    const_plogvar_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _0,                     ;;; K_FLAGS
        _:GCTYPE_CONST_PLOGVAR, ;;; K_GC_TYPE
        Rec1_getsize,           ;;; K_GET_SIZE
        %};

    ;;; Offsets into prolog_barrier_apply's stack frame
    ;;; of the saved values of _var_chain and _plog_next_var.
    ;;; Used by Gc$-App_calls and Prune_plog_trail.
lvars
    _pba_var_chain_offs = _0,
    _pba_next_var_offs  = _0,

    _in_prolog_calls,
    ;

    ;;; Compute those offset if haven't already done so.
    ;;; (offsets must be non-zero)
define lconstant Setup_pba_offs();
    if _zero(_pba_var_chain_offs) then
        Dlocal_frame_offset(ident _var_chain, prolog_barrier_apply, true)
                                        -> _pba_var_chain_offs;
        Dlocal_frame_offset(ident _plog_next_var, prolog_barrier_apply, true)
                                        -> _pba_next_var_offs

    endif
enddefine;

define Prune_plog_trail();
    lvars   plogvar, startvar, _trail, _lasttrailpoint, _sframe, _barbase,
            _sflim, _startpoint, _vchain
        ;

    false -> _in_prolog_calls;  ;;; see Garbage_plog_trail below
    if (_plog_trail_sp ->> _lasttrailpoint) == _plog_area_lo then return endif;
    Setup_pba_offs();
    ;;; set next trail word false to mark end of variable set
    false -> _plog_trail_sp!(w)++ -> _plog_trail_sp;
    _plog_trail_barrier -> _barbase;
    _var_chain -> _vchain;      ;;; false if not yet (re)started
    _caller_sp_flush() -> _sframe;
    _call_stack_seg_hi -> _sflim;

    repeat
        if _sframe == _sflim then
            quitif(_sflim == _call_stack_hi);
            ;;; skip external calls to next callstack seg
            _sframe!SF_NEXT_SEG_HI -> _sflim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;

        if _vchain then
            if _sframe!SF_OWNER == prolog_barrier_apply then
                var_end_marker -> startvar;
                _barbase -> _trail
            elseunless _sframe!SF_OWNER!PD_FLAGS _bitst _:M_PD_PLOG_CHOICE then
                ;;; next frame
                _nextframe(_sframe) -> _sframe;
                nextloop
            else
                _sframe!SF_PLGSV_NEXT_VAR -> startvar;
                _barbase@(w){_sframe!SF_PLGSV_TRAIL_SP} -> _trail
            endif;
            if startvar == var_end_marker then
                ;;; use the start of the var chain -- only do this once,
                ;;; since no new vars got created before this
                _vchain -> startvar;
                false -> _vchain
            endif;
            _trail -> _startpoint;

            ;;; next test only allows vars in the open segment to be optimised
            ;;; away, since plogvar >=@(w) startvar will be true for any
            ;;; plogvar in a higher segment than startvar (i.e. including
            ;;; a plogvar created before the last GC).
            if startvar >=@(w) _open_seg_base_ptr then
                while _trail <@(w) _lasttrailpoint do
                    _trail!(w)++ -> _trail -> plogvar;
                    if plogvar >=@(w) startvar then
                        ;;; var was instantiated before the choice point
                        ;;; following its creation - can therefore be removed
                        ;;; Give it the const_plogvar_key so that this gc will
                        ;;; replace all references to it with its contents...
                        const_plogvar_key -> plogvar!KEY
                    endif
                endwhile
            endif;
            _startpoint -> _lasttrailpoint
        endif;

        if _sframe!SF_OWNER!PD_FLAGS _bitst _:M_PD_PLOG_CHOICE then
            true -> _in_prolog_calls;       ;;; see Garbage_plog_trail below
            ;;; Morris chaining -- save current value in saved trail slot
            ;;; in PLGSV_TRAIL_SP and then replace former with
            ;;; stack frame address made simple
            _barbase@(w){_sframe!SF_PLGSV_TRAIL_SP} -> _trail;
            _trail!(w) -> _sframe!SF_PLGSV_TRAIL_SP;
            _mksimple(_sframe) -> _trail!(w)
        elseif _sframe!SF_OWNER == prolog_barrier_apply then
            ;;; going thru a barrier, change the barrier trail base
            true -> _in_prolog_calls;       ;;; see Garbage_plog_trail below
            _barbase--@(w) -> _barbase;         ;;; point to popint size
            ;;; limit for this process excludes the spare word before the size
            _barbase--@(w) -> _lasttrailpoint;
            ;;; set spare word false to mark end of variable set
            false -> _lasttrailpoint!(w);
            ;;; move barbase down to next barrier
            _barbase@(w)-{_int(_barbase!(w))} -> _barbase;
            ;;; get the next var chain -- false if not yet (re)started
            _sframe!(csword){_pba_var_chain_offs} -> _vchain
        endif;

        ;;; next stack frame
        _nextframe(_sframe) -> _sframe
    endrepeat;

    ;;; do the stuff outside any prolog procedures
    _plog_area_lo -> _trail;
    while _trail <@(w) _lasttrailpoint do
        _trail!(w)++ -> _trail -> plogvar;
        if plogvar >=@(w) _lowest_garbageable then
            const_plogvar_key -> plogvar!KEY
        endif
    endwhile
enddefine;

    /*  remove garbage and 'constant' variables from the prolog trail
        after the gc. If there are prolog procedure calls,
        -Prune_plog_trail- has subsituted each trail entry with a chain
        of simple stack frame pointers for all calls whose PLGSV_TRAIL_SP
        offset references that trail slot, the chains ending with the plogvars
        (or false at the last position of a variable set). By following thru
        the chains, the PLGSV_TRAIL_SP offset values are relocated.
    */
define Garbage_plog_trail();
    lvars offs, _entry, _trail, _newtrail, _sframe;
    ;;; set current prolog var chain to false, and prolog next var to
    ;;; end marker, forcing a new chain of plogvars to be started
    ;;; (has been done for all stack frames by Gc_app_calls)
    false -> _var_chain;
    var_end_marker -> _plog_next_var;

    _plog_area_lo -> _plog_trail_barrier;

    unless _in_prolog_calls then
        ;;; There are no prolog procedures on the call stack
        ;;; (set true by -Prune_plog_trail- if there are)
        ;;; Can therefore just clear the trail
        _plog_area_lo -> _plog_trail_sp;
        return
    endunless;

    _plog_area_lo ->> _trail -> _newtrail;
    _0 -> offs;         ;;; current offset from barrier

    repeat
        _trail!(w)++ -> _trail -> _entry;       ;;; next entry
        ;;; Work thru chain of simple stack frame pointers,
        ;;; finishing with compound object (plogvar or false)
        while issimple(_entry) do
            _mkcompound(_entry) -> _sframe;
            _sframe!SF_PLGSV_TRAIL_SP -> _entry;    ;;; next in chain
            ;;; insert new offset in PLGSV_TRAIL_SP
            offs -> _sframe!SF_PLGSV_TRAIL_SP
        endwhile;

        ;;; _entry is now a plogvar, or false for a barrier or the end
        if _entry then
            ;;; plogvar -- remove if garbage or a constant
            if _entry >=@(w) _lowest_garbageable then
                nextunless(issimple(_entry!KEY));
                ;;; marked and not constant, so retain it
                if _copy_gc then
                    _entry!FIRST -> _entry  ;;; its new address
                endif
            endif;
            _entry -> _newtrail!(w)++ -> _newtrail;
            @@(w){offs}++ -> offs       ;;; step on offset from barrier
        elseif _trail <@(w) _plog_trail_sp then
            ;;; barrier - 1st word is a spare (for marking false at the end
            ;;; of a variable set).
            ;;; 2nd is plog process trail size as a pop integer.
            _trail@(w)++ -> _trail;     ;;; skip the size
            ;;; put the spare and the updated size in the new trail
            false -> _newtrail!(w)++ -> _newtrail;
            _pint(@@(w){_newtrail, _plog_trail_barrier})
                                            -> _newtrail!(w)++ -> _newtrail;
            ;;; now reset the process trail base
            _newtrail -> _plog_trail_barrier;
            _0 -> offs
        else
            ;;; end of trail
            quitloop
        endif
    endrepeat;

    _newtrail -> _plog_trail_sp
enddefine;

    /*  Apply app_p to an area of prolog continuations
        from  _contn to _cont + _size (for saved bits in processes).
        If _contn == _NULL, do the actual continuation stack.
    */
define App_plog_contns(_contn, _size, app_p);
    lvars procedure app_p, _contn, _size, _lim;
    if _contn == _NULL then
        _plog_contn_barrier@(w){_plog_contn_sp} -> _contn;
        @@(w){_plog_area_hi, _contn} -> _size
    endif;
    _contn@(w){_size} -> _lim;
    while _contn <@(w) _lim do
        _contn!(w)++ -> _contn -> _size;    ;;; contn size
        if _neg(_size) then
            ;;; block marker
            _contn@(w)-{_size} -> _contn
        else
            _contn@(w){_size} -> _size;
            while _contn <@(w) _size do
                app_p(_contn);
                _contn@(w)++ -> _contn
            endwhile;
            _contn@(w)++ -> _contn          ;;; skip link to next
        endif
    endwhile
enddefine;

    /*  Apply app_p to the trail
    */
define App_plog_trail(app_p);
    lvars procedure app_p;
    App_range(_plog_area_lo, @@(w){_plog_trail_sp, _plog_area_lo}, app_p)
enddefine;

    /*  Called by App_calls for each stack frame
    */
define Plog_frame_reset(owner, _sframe);
    lvars owner, _sframe;
    if _zero(_pba_var_chain_offs) then Setup_pba_offs() endif;
    if owner!PD_FLAGS _bitst _:M_PD_PLOG_CHOICE then
        ;;; zap any prolog saved next var
        var_end_marker -> _sframe!SF_PLGSV_NEXT_VAR
    elseif owner == prolog_barrier_apply then
        ;;; set the saved values of _var_chain to false and
        ;;; _plog_next_var to the end marker so that a new chain is started
        false -> _sframe!(csword){_pba_var_chain_offs};
        var_end_marker -> _sframe!(csword){_pba_next_var_offs}
    endif
enddefine;

    /*  Called from Kill_temp_prop_entries for a plog_varnum_entry.
        See if the garbage var derefs to a non-garbage variable;
        if it does, make that var the arg of the entry (ensuring that the
        lowest number gets retained).
    */
define Kill_plog_varnum(_entry);
    lvars _entry, _item, _plogvar = _entry!PTE_ARG, _key;
    while iscompound(_plogvar!PGV_CONT ->> _item) do
        _item!KEY -> _key;
        if _item <@(w) _lowest_garbageable then
            ;;; non-garbageable item -- kill if not prologvar
            quitif(_key /== prologvar_key);
            _item -> _entry!PTE_ARG, return ;;; don't kill
        elseif (_key == prologvar_key or _key == const_plogvar_key) then
            ;;; garbage plogvar
            quitif(_item == _plogvar);      ;;; kill if undefvar
            _item -> _plogvar               ;;; else loop for next
        elseif _key == _mksimple(prologvar_key) then
            ;;; non-garbage var -- don't kill
            if _copy_gc then
                _item!FIRST -> _entry!PTE_ARG
            else
                _item -> _entry!PTE_ARG
            endif;
            return
        else
            ;;; else kill
            quitloop
        endif
    endwhile;
    ;;; kill entry
    dead_prop_entry -> _entry!PTE_ARG
enddefine;

endsection;     /* $-Sys$-Gc */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  4 1996
        Changed to use CHAIN_EQ macro
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar  5 1991
        Put in temporary fix for ghastly shortcoming in -Prune_plog_trail-
        (using address ordering to decide whether a plogvar can be
        made a constant is invalid with more than one heap segment).
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Nov 10 1989
        Changed -Prune_plog_trail- to deal with segmented callstack.
--- Rob Duncan, Jul 21 1989
        Added some calls to -_checkall- and _CHECKINTERRUPT in -prolog_unify-
        to stop it overrunning the callstack limit and to be interruptable
        on cyclic structures. Made it tail recursive on the last argument
        in pairs and prologterms.
--- John Gibson, May 24 1989
        Made -varnum_entry_key- a property entry key.
--- John Gibson, Apr 10 1989
        Stuff not in Sys$-Gc into section Sys$-Plog
--- John Gibson, Jan 22 1989
        Removed use of -apply- from -prolog_own_invoke-
--- John Gibson, Jan 22 1989
        Renamed plog_trail_push as _plog_trail_push
--- John Gibson, Nov 13 1988
        Made -Prolog_newvar- initialise the PGV_CONT of each prolog
        variable in the block it constructs (otherwise, a -sys_lock_heap-
        without a preceding -sysgarbage- can cause the next GC to fall over).
--- John Gibson, Mar  4 1988
        Split off prolog area stuff into plog_area.p
--- John Gibson, Feb 28 1988
        Set M_K_MATCH_VAR in -prologvar_key- flags
--- John Gibson, Feb 25 1988
        Moved sectionised prolog garbage collector routines into this file.
--- John Gibson, Feb 10 1988
        Eq__Integer and Eq__Float into section Sys
 */
