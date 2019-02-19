/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/newanyproperty.p
 > Purpose:
 > Author:          John Williams (see revisions)
 > Documentation:   REF *PROPS
 > Related Files:   C.all/src/property.p, C.all/src/newproperty.p
 */

;;; --------------- PROPERTIES WITH USER HASHING, ETC --------------------

#_INCLUDE 'declare.ph'

global constant
        Sys$-Gc$-dead_prop_entry
    ;

section $-Sys$-Prop;

    ;;; All defined in property.p
constant
        procedure (Round_tabsize, Search, Get, Add_entry, Kill_entry,
        Link_entries, Count, Get_prop_rec, New_table, Init_hash)
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Prop => newanyproperty;


;;; --- REHASHING USER-HASHED PROPERTIES ---------------------------------

define lconstant Hash_store_codes(prop);
    ;;; work out all hash-codes in one go, storing them in PTE_LINK fields
    ;;; used in HASH_REHASH_GC and HASH_EXPAND to avoid problems with
    ;;; interrupts and garbage collections during prop rehashing/expansion
    lvars entry, procedure pdr, prop, table, _end, _offs;
    prop!PT_HASH_PDR -> pdr;
    prop!PT_TABLE -> table;
    false -> prop!PT_REHASH;            ;;; so can tell if a gc occurs
    @@V_WORDS[_0] -> _offs;
    @@V_WORDS[table!V_LENGTH] -> _end;
    while _offs _lt _end do
        table!(w){_offs} -> entry;
        while iscompound(entry) do
            unless entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
                fast_apply(entry!PTE_ARG, pdr) -> entry!PTE_LINK;
                if prop!PT_REHASH then
                    chain(prop, Hash_store_codes)
                endif
            endunless;
            entry!PTE_NEXT -> entry
        endwhile;
        @@(w){_offs}++ -> _offs
    endwhile
enddefine;

define Rehash_hash(prop);
    lvars ptes, prop, _cell, _mask, _table, _deads = 0;
    if prop!PT_REHASH /== true then
        ;;; prop set up by POPC or POPLINK
        Init_hash(prop)
    endif;
    Hash_store_codes(prop);
    prop!PT_TABLE -> _table;
    Link_entries(_table) -> ptes;
    _negate(_table!V_LENGTH) -> _mask;
    _table@V_WORDS[_0] -> _table;
    while iscompound(ptes) do
        ptes!PTE_NEXT;
        if ptes!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
            _deads fi_+ 1 -> _deads
        else
            _table@(w)[##(w){ptes!PTE_LINK} _biclear _mask] -> _cell;
            _cell!(w) -> ptes!PTE_NEXT;
            ptes -> _cell!(w)
        endif;
        -> ptes
    endwhile;
    if prop!PT_EXPAND then
        prop!PT_COUNT fi_+ _deads -> prop!PT_COUNT
    endif;
    false -> prop!PT_REHASH
enddefine;

    /*  Expand & rehash property
        (User & address hashed versions)
    */
define lconstant Expand(prop);
    lvars prop, newtable, table, _cell, _count, _end, _entry, _mask;
    prop!PT_TABLE -> table;
    table!V_LENGTH -> _end;
    ;;; do this first, in case it causes a GC (which may make more deads)
    New_table(_pint(_end) fi_<< prop!PT_EXPAND) -> newtable;
    ;;; no point in rehashing after GC because rehashing here anyway
    false -> prop!PT_REHASH;
    newtable -> prop!PT_TABLE;
    table@V_WORDS[_0] -> table;
    table@(w)[_end] -> _end;
    _negate(newtable!V_LENGTH) -> _mask;
    newtable@V_WORDS[_0] -> newtable;
    0 -> _count;
    while table <@(w) _end do
        table!(w)++ -> table -> _entry;
        while iscompound(_entry) do
            _entry!PTE_NEXT;
            unless _entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry do
                newtable@(w)[##(w){_entry!PTE_ARG} _biclear _mask] -> _cell;
                _cell!(w) -> _entry!PTE_NEXT;
                _entry -> _cell!(w);
                _count fi_+ 1 -> _count
            endunless;
            -> _entry
        endwhile
    endwhile;
    (_count fi_<< prop!PT_EXPAND) fi_- _count -> prop!PT_COUNT
enddefine;

define lconstant Expand_hash(prop);
    lvars table, newtable, prop, _cell, _count, _end, _entry, _mask;
    prop!PT_TABLE -> table;
    table!V_LENGTH -> _end;
    New_table(_pint(_end) fi_<< prop!PT_EXPAND) -> newtable;
    Hash_store_codes(prop);
    newtable -> prop!PT_TABLE;
    table@V_WORDS[_0] -> table;
    table@(w)[_end] -> _end;
    _negate(newtable!V_LENGTH) -> _mask;
    newtable@V_WORDS[_0] -> newtable;
    0 -> _count;
    while table <@(w) _end do
        table!(w)++ -> table -> _entry;
        while iscompound(_entry) do
            _entry!PTE_NEXT;
            unless _entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry do
                newtable@(w)[##(w){_entry!PTE_LINK} _biclear _mask] -> _cell;
                _cell!(w) -> _entry!PTE_NEXT;
                _entry -> _cell!(w);
                _count fi_+ 1 -> _count
            endunless;
            -> _entry
        endwhile;
    endwhile;
    (_count fi_<< prop!PT_EXPAND) fi_- _count -> prop!PT_COUNT
enddefine;


;;; --- SEARCHING PROPERTIES FOR AN ENTRY (all versions) -----------------

define Search_hash(hasharg, arg, prop);
    lvars prop, arg, hasharg, procedure eq_pdr;
    prop!PT_EQ_PDR -> eq_pdr;
    prop!PT_TABLE -> prop;
    prop!V_WORDS[ ##(w){hasharg} _biclear _negate(prop!V_LENGTH) ] -> prop;
    while iscompound(prop) do
        if fast_apply(prop!PTE_ARG, arg, eq_pdr) then
            return(prop)
        else
            prop!PTE_NEXT -> prop
        endif
    endwhile;
    false
enddefine;


;;; --- REMOVING ENTRIES FROM PROPERTIES (all versions) --------------------
;;; these return booleans (TRUE if entry deleted) for counting properties

define Kill_hash(entry, arg, prop);
    ;;; entry = hasharg
    lvars arg, entry, prop, procedure eq_pdr, _offs;
    prop!PT_EQ_PDR -> eq_pdr;
    prop!PT_TABLE -> prop;
    ;;; must be careful in case EQ_PDR causes GC
    @@(w)[ ##(w){entry} _biclear _negate(prop!V_LENGTH) ] -> _offs;
    prop!V_WORDS{_offs} -> entry;
    if iscompound(entry) then
        if fast_apply(entry!PTE_ARG, arg, eq_pdr) then
            entry!PTE_NEXT -> prop!V_WORDS{_offs};
            return(true)
        endif;
        while iscompound(entry!PTE_NEXT ->> prop) do
            if fast_apply(prop!PTE_ARG, arg, eq_pdr) then
                prop!PTE_NEXT -> entry!PTE_NEXT;
                return(true)
            endif;
            prop -> entry
        endwhile
    endif;
    false
enddefine;


;;; --- GET PROPERTY VALUE FOR GIVEN ARG --------------------------------
;;; (n.b. string pdprops are for popc identification)

lconstant macro PROP_CHECK_ENTRY =
    [if entry then
        entry!PTE_VALUE
    elseif prop!PT_ACTIVE then
        fast_apply(arg, prop!PT_ACTIVE)
    else
        prop!PT_DEFAULT
    endif;];

lconstant macro (
    PROP_CHECK_REHASH =
        [if prop!PT_REHASH then Rehash_hash(prop) endif],
    ;;; in case property set up by POPC or POPLINK
    PROP_CHECK_INIT_REHASH =
        [if prop!PT_REHASH and prop!PT_REHASH /== true then Rehash_hash(prop) endif],
    );

define Get_hash(arg, prop) with_props '(Sys$-Prop$-Get_hash)';
    lvars arg, prop, entry;
    if prop!PT_REHASH then
        if prop!PT_REHASH /== true then
            ;;; initial rehash for prop set up by POPC/POPLINK
            Rehash_hash(prop)
        elseunless prop!PT_ENTRY_KEY == perm_prop_entry_key then
            ;;; removes dead entries
            Count(prop) ->
        endif
    endif;
    Search_hash(fast_apply(arg, prop!PT_HASH_PDR), arg, prop) -> entry;
    PROP_CHECK_ENTRY
enddefine;
;;;
define updaterof Get_hash(value, arg, prop)
                                        with_props '(->Sys$-Prop$-Get_hash)';
    lvars arg, entry, hasharg, prop, value;
    PROP_CHECK_INIT_REHASH;
    fast_apply(arg, prop!PT_HASH_PDR) -> hasharg;
    if fast_apply(prop!PT_DEFAULT, value, prop!PT_EQ_PDR) then
        Kill_hash(hasharg, arg, prop) ->
    elseif (Search_hash(hasharg, arg, prop) ->> entry) then
        value -> entry!PTE_VALUE
    else
        Add_entry(prop, hasharg, arg, value)
    endif
enddefine;


define Get_hash_gc(arg, prop) with_props '(Sys$-Prop$-Get_hash_gc)';
    lvars arg, prop, entry;
    fast_apply(arg, prop!PT_HASH_PDR);      ;;; hasharg on stack
    PROP_CHECK_REHASH;
    Search_hash((), arg, prop) -> entry;
    PROP_CHECK_ENTRY
enddefine;
;;;
define updaterof Get_hash_gc(value, arg, prop)
                                with_props '(->Sys$-Prop$-Get_hash_gc)';
    lvars arg, entry, hasharg, prop, value;
    fast_apply(arg, prop!PT_HASH_PDR) -> hasharg;
    if fast_apply(prop!PT_DEFAULT, value, prop!PT_EQ_PDR) then
        PROP_CHECK_REHASH;
        Kill_hash(hasharg, arg, prop) ->
    else
        PROP_CHECK_REHASH;
        if (Search_hash(hasharg, arg, prop) ->> entry) then
            value -> entry!PTE_VALUE
        else
            Add_entry(prop, hasharg, arg, value)
        endif
    endif
enddefine;


;;; --- UPDATE PROPERTY VALUE FOR GIVEN ARG ----------------------------
;;; (User & address hashed versions: expandable versions of each)
;;; (n.b. string pdprops are for popc identification)

define Put_exp(value, arg, prop) with_props '(Sys$-Prop$-Put_exp)';
    lvars arg, entry, prop, value;
    if value == prop!PT_DEFAULT then
        if Kill_entry(arg, prop) then
            prop!PT_COUNT fi_+ 1 -> prop!PT_COUNT
        endif
    elseif (Search(arg, prop) ->> entry) then
        value -> entry!PTE_VALUE
    else
        Add_entry(prop, arg, arg, value);
        if (prop!PT_COUNT fi_- 1 ->> prop!PT_COUNT) == 0 then
            Expand(prop)
        endif
    endif
enddefine;

define Put_exp_hash(value, arg, prop) with_props '(Sys$-Prop$-Put_exp_hash)';
    lvars arg, entry, hasharg, prop, value;
    PROP_CHECK_INIT_REHASH;
    fast_apply(arg, prop!PT_HASH_PDR) -> hasharg;
    if fast_apply(prop!PT_DEFAULT, value, prop!PT_EQ_PDR) then
        if Kill_hash(hasharg, arg, prop) then
            prop!PT_COUNT fi_+ 1 -> prop!PT_COUNT
        endif
    elseif (Search_hash(hasharg, arg, prop) ->> entry) then
        value -> entry!PTE_VALUE
    else
        Add_entry(prop, hasharg, arg, value);
        if (prop!PT_COUNT fi_- 1 ->> prop!PT_COUNT) == 0 then
            Expand_hash(prop)
        endif
    endif
enddefine;

define Put_exp_hash_gc(value, arg, prop)
                                    with_props '(Sys$-Prop$-Put_exp_hash_gc)';
    lvars arg, entry, hasharg, prop, value;
    fast_apply(arg, prop!PT_HASH_PDR) -> hasharg;
    if fast_apply(prop!PT_DEFAULT, value, prop!PT_EQ_PDR) then
        PROP_CHECK_REHASH;
        if Kill_hash(hasharg, arg, prop) then
            prop!PT_COUNT fi_+ 1 -> prop!PT_COUNT
        endif
    else
        PROP_CHECK_REHASH;
        if (Search_hash(hasharg, arg, prop) ->> entry) then
            value -> entry!PTE_VALUE
        else
            Add_entry(prop, hasharg, arg, value);
            if (prop!PT_COUNT fi_- 1 ->> prop!PT_COUNT) == 0 then
                Expand_hash(prop)
            endif
        endif
    endif
enddefine;


;;; --- NEWANYPROPERTY --------------------------------------------------

define newanyproperty(init, size, expand, count, hash_p, eq_p, type,
                                    default, active_default) -> prop_p;
    lvars init, size, expand, count, hash_p, eq_p, type, init,
        default, active_default, prop, gc_rehash, procedure prop_p
        ;

    ;;; round table size to power of 2
    Round_tabsize(size) -> size;

    if expand then
        Check_integer(expand, 1)
    elseif count then
        1 -> expand
    endif;
    if count then
        Check_integer(count, 1)
    elseif expand then
        size -> count
    endif;

    if hash_p then
        if isref(hash_p) then
            true;
            fast_cont(hash_p) -> hash_p
        elseif isword(type) then
            false
        else
            ;;; old-style
            type;
            "perm" -> type
        endif -> gc_rehash;
        Check_procedure(hash_p);

        if eq_p then
            Check_procedure(eq_p)
        else
            nonop == -> eq_p
        endif
    elseif eq_p then
        mishap(eq_p, 1, 'NO HASHING PROCEDURE SPECIFIED WITH EQUALS PROCEDURE')
    endif;

    ;;; now make property
    Get_prop_rec(type, default, size) -> prop;
    count       -> prop!PT_COUNT;
    eq_p        -> prop!PT_EQ_PDR;
    expand      -> prop!PT_EXPAND;
    hash_p      -> prop!PT_HASH_PDR;

    if hash_p then
        if gc_rehash then
            Consclos_protect(Get_hash_gc, prop, 1) -> prop_p;
            if expand then
                Put_exp_hash_gc -> prop_p!PD_UPDATER!PD_CLOS_PDPART
            endif
        else
            Consclos_protect(Get_hash, prop, 1) -> prop_p;
            if expand then
                Put_exp_hash -> prop_p!PD_UPDATER!PD_CLOS_PDPART
            endif
        endif
    else
        Consclos_protect(Get, prop, 1) -> prop_p;
        if expand then
            Put_exp -> prop_p!PD_UPDATER!PD_CLOS_PDPART
        endif
    endif;
    prop_p!PD_FLAGS _biset _:M_PD_CLOS_PROPERTY -> prop_p!PD_FLAGS;

    if active_default then
        Consclos_protect(active_default, prop_p, 1) -> prop!PT_ACTIVE
    endif;

    until null(init) do
        fast_destpair(init) -> init -> default;
        hd(tl(default)) -> prop_p(fast_front(default))
    enduntil

enddefine;

endsection;     /* $-Sys$-Prop */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1992
        Added checks for property set up by POPLINK
--- John Gibson, Jul 31 1989
        Property record now constructed by -Get_prop_rec-
--- John Gibson, Jun 29 1989
        Hashed non-gc-dependent props check for PT_REHASH /== true when
        prop set up by POPC/POPLINK
--- John Gibson, Jun 18 1989
        -New_table- gets a new property table vector
--- John Gibson, Jun  1 1989
        Removed restriction from -newanyproperty- of not allowing
        "tmparg" or "tmpboth" with an equals procedure other than ==.
--- John Gibson, May 24 1989
        Enabled old gc_flag arg to newanyproperty to specify property type,
        plus other changes to support "tmpval" type in properties
        with hash procedure.
--- John Gibson, Jan 16 1989
        Got rid of procedures -Search_hash_gc- and -Kill_hash_gc-.
        Added string pdprops to property closure procedures for
        popc identfiication
--- John Gibson, Mar 24 1988
        Moved out of props.p
 */
