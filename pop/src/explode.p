/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/explode.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *DATA
 */

;;; ----------------- EXPLODE/FILL STRUCTURES ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'
#_INCLUDE 'io.ph'

constant
        procedure isarray
    ;

section $-Sys;

constant
        procedure (Fill_string, Prop$-Dest, Array$-Dest, Array$-Fill),
        procedure_label_key
    ;

vars
        _in_X_call
    ;

weak constant
        procedure (Fill_dstring)
    ;

endsection;


;;; -------------------------------------------------------------------

section $-Sys => explode, fill, datalist;

define explode(item);
    lvars item, key = datakey(item), _work;
    lconstant procedure err = mishap(%1, 'BAD ARGUMENT FOR EXPLODE'%);

    define lconstant Dest_closure(clos);
        lvars clos, _offs, _limit;
        @@PD_CLOS_FROZVALS[_0] -> _offs;
        @@PD_CLOS_FROZVALS[clos!PD_CLOS_NFROZ] -> _limit;
        while _offs _lt _limit do
            _CHECKUSER;
            clos!(w){_offs};
            @@(w){_offs}++ -> _offs;
        endwhile;
    enddefine;

    if key!K_FLAGS _bitst _:M_K_VECTOR then
        fast_apply(item, key!K_DEST_V) ->
    elseif key == word_key then
        deststring(item!W_STRING) ->
    elseif islist(item) then
        dl(item)
    elseif key!K_FLAGS _bitst _:M_K_RECORD then
        fast_apply(item, key!K_DEST_R)
    elseif key == procedure_key then
        item!PD_FLAGS -> _work;
        if _work _bitst _:M_PD_CLOSURE then
            if _work _bitst _:M_PD_CLOS_PROPERTY then
                weakref[property_key] Prop$-Dest(item)
            elseunless _work _bitst _:M_PD_CLOS_UNDEF
            and vm_pas_mode /== "popc" then     ;;; special for Popc
                Dest_closure(item)
            else
                err(item)
            endif
        elseif _work _bitst _:M_PD_ARRAY then
            weakref[isarray] Array$-Dest(item)
        elseif _work _bitst _:M_PD_COMPOSITE then
            ;;; is p1 <> p2
            item!PD_COMPOSITE_P1, item!PD_COMPOSITE_P2
        else
            err(item)
        endif;
    elseif key == key_key and item!K_FLAGS _bitst _:M_K_RECORD then
        explode(item!K_ACCESS_R)
    elseif vm_pas_mode == "popc" then
        ;;; specials to enable Popc to get at fields
        if key == weakref property_key then
            item!PT_TABLE,
            item!PT_ACTIVE,
            item!PT_COUNT,
            item!PT_DEFAULT,
            item!PT_ENTRY_KEY,
            item!PT_EQ_PDR,
            item!PT_EXPAND,
            item!PT_HASH_PDR,
            item!PT_REHASH
        elseif key == weakref procedure_label_key then
            item!PLAB_OWNER,
            item!PLAB_IDENT,
            item!PLAB_LABEL
        elseif key == weakref device_key then
            ;;; only used for user devices
            item!D_READ,
            item!D_WRITE,
            item!D_SEEK,
            item!D_FLUSH,
            item!D_CLOSE,
            item!D_CLEAR_INPUT,
            item!D_TEST_INPUT
        elseif key == weakref exfunc_closure_key then
            item!EFC_FUNC,
            item!EFC_ARG,
            item!EFC_ARG_DEST -> _work;
            if _work == _extern pop_exfunc_arg:data then
                `A`
            elseif _work == ident _in_X_call then
                `X`
            else
                false
            endif
        else
            err(item)
        endif
    else
        err(item)
    endif
enddefine;
;;;
define updaterof explode(item);
    lvars item, key = datakey(item), _work;
    lconstant procedure err = mishap(%1, 'BAD ARGUMENT FOR EXPLODE UPDATER'%);

    define lconstant Fill_fullvec(vec);
        lvars vec, _ptr, _lim;
        vec@V_WORDS -> _lim;
        _lim@(w)[vec!V_LENGTH] -> _ptr;
        while _ptr >@(w) _lim do
            () -> _ptr--!(w) -> _ptr
        endwhile
    enddefine;

    define lconstant Fill_nfvec(vec);
        lvars vec, procedure p, _index;
        vec!KEY!K_FAST_SUBSCR_V!PD_UPDATER -> p;
        _pint(vec!V_LENGTH) -> _index;
        until _index == 0 do
            p((), _index, vec);
            _index fi_- 1 -> _index
        enduntil
    enddefine;

    define lconstant Fill_record(rec);
        lvars access_vec, rec, _offs, _lim;
        rec!KEY!K_ACCESS_R -> access_vec;
        @@V_WORDS -> _lim;
        @@V_WORDS[access_vec!V_LENGTH] -> _offs;
        while _offs _gr _lim do
            --@@(w){_offs} -> _offs;
            fast_apply((), rec, access_vec!(w){_offs}!PD_UPDATER)
        endwhile
    enddefine;

    define lconstant Fill_closure(clos);
        lvars clos, _ptr, _lim;
        clos@PD_CLOS_FROZVALS -> _lim;
        _lim@(w)[clos!PD_CLOS_NFROZ] -> _ptr;
        while _ptr >@(w) _lim do
            () -> _ptr--!(w) -> _ptr
        endwhile
    enddefine;

    key!K_FLAGS -> _work;
    if _work _bitst _:M_K_FULL_VECTOR then
        Fill_fullvec(item)
    elseif _work _bitst _:M_K_STRING then
        if _work _bitst _:M_K_DSTRING then
            weakref[dstring_key] Fill_dstring(item, true) ->
        else
            Fill_string(item, true) ->
        endif
    elseif _work _bitst _:M_K_VECTOR then
        Fill_nfvec(item)
    elseif islist(item) then
        () -> dl(item)
    elseif key == word_key then
        err(item)
    elseif _work _bitst _:M_K_RECORD then
        Fill_record(item)
    elseif key == procedure_key then
        item!PD_FLAGS -> _work;
        if _work _bitst _:M_PD_CLOSURE then
            if _work _bitst (_:M_PD_CLOS_PROPERTY _biset _:M_PD_CLOS_UNDEF)
            then
                err(item)
            else
                Fill_closure(item)
            endif
        elseif _work _bitst _:M_PD_ARRAY then
            weakref[isarray] Array$-Fill(item)
        else
            err(item)
        endif
    else
        err(item)
    endif
enddefine;

define fill(item) -> item;
    lvars item;
    if item == [] then
        mishap([], 1, 'BAD ARGUMENT FOR FILL')
    elseif ispair(item) then
        ;;; so it it matches appdata
        -> (fast_front(item), fast_back(item))
    else
        ;;; on everything else, same as updater(explode)
        -> explode(item)
    endif
enddefine;

define datalist(item);
    lvars item;
    [% explode(item) %]
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 11 1997
        String16 changes
--- John Gibson, Jan 15 1996
        Stopped explode working on undef closures (except for Popc, allows
        base explode on one)
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- John Williams, Jul 14 1993
        Added missing #_INCLUDE for 'external.ph'.
--- John Gibson, May 18 1993
        Added more POPC support to explode
--- John Gibson, Oct  7 1992
        Added more POPC support in explode
--- John Gibson, Jan  6 1992
        Changed updater of -explode- for dstrings
--- John Gibson, May 18 1989
        Made -explode- produce fields in property records and procedure
        label records for use by POPC.
--- John Gibson, Feb 13 1989
        Made -explode- give p1, p2 when applied to composite procedure
        p1 <> p2
--- John Gibson, Apr  5 1988
        Moved out of data.p
 */
