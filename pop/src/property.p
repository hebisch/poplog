/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/property.p
 > Purpose:         Basic run-time support for properties
 > Author:          John Gibson & John Williams (see revisions)
 */


#_INCLUDE 'declare.ph'
#_INCLUDE 'destroy.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure isproperty
    ;

section $-Sys;

global constant
        Gc$-dead_prop_entry,
    ;

weak global constant
        procedure (Prop$-Rehash_hash)
    ;

endsection;


;;; ------------------------------------------------------------------------

section $-Sys$-Prop;

    /*  Relink the entries in property table _table
    */
define Link_entries(_table) -> ptes;
    ;;; link entries in property together using PTE_NEXT field
    ;;; used in re-hashing properties
    lvars ptes, _entry, _lim, _table;
    _table@V_WORDS[_table!V_LENGTH] -> _lim;
    _table@V_WORDS[_0] -> _table;
    0 -> ptes;
    while _table <@(w) _lim do
        _table!(w) -> _entry;
        0 -> _table!(w)++ -> _table;    ;;; set table cell empty
        while iscompound(_entry) do
            _entry!PTE_NEXT;
            ptes -> _entry!PTE_NEXT;
            _entry -> ptes;
            -> _entry
        endwhile
    endwhile
enddefine;

    /*  Called before a prop is to be rehashed by Rehash and Rehash_hash.
        Deals with prop set up by POPLINK which may have an additional
        vector of entries in PT_REHASH -- these are transferred on to the
        first bucket of the normal table
    */
define Init_hash(prop);
    lvars ptes, prop, extra = prop!PT_REHASH, _entry, _lim, _extra, _save;
    true -> prop!PT_REHASH;
    returnunless(isvector(extra));

    prop!PT_TABLE!V_WORDS[_0] -> ptes;
    extra@V_WORDS[extra!V_LENGTH] -> _lim;
    extra@V_WORDS[_0] -> _extra;
    while _extra <@(w) _lim do
        _extra!(w)++ -> (_entry, _extra);
        nextunless(iscompound(_entry));
        _entry -> _save;
        while iscompound(_entry!PTE_NEXT) do
            _entry!PTE_NEXT -> _entry
        endwhile;
        ptes -> _entry!PTE_NEXT;
        _save -> ptes
    endwhile;
    ptes -> prop!PT_TABLE!V_WORDS[_0]
enddefine;

    /*  Rehash a ordinary property
    */
define Rehash(prop);
    lvars ptes, prop, _cell, _deads = 0, _mask, _table;
    if prop!PT_REHASH /== true then
        ;;; prop set up by POPC or POPLINK
        Init_hash(prop)
    endif;
    prop!PT_TABLE -> _table;
    Link_entries(_table) -> ptes;
    _negate(_table!V_LENGTH) -> _mask;
    _table@V_WORDS[_0] -> _table;
    while iscompound(ptes) do
        ptes!PTE_NEXT;
        if ptes!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
            _deads fi_+ 1 -> _deads
        else
            _table@(w)[##(w){ptes!PTE_ARG} _biclear _mask] -> _cell;
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

    /*  Count number of entries, removing dead ones
    */
define Count(prop) -> count;
    lvars count = 0, prop, _entry, _last, _cell, _end, _deads = 0;
    if isvector(prop!PT_REHASH) then Init_hash(prop) endif;
    prop!PT_TABLE -> _cell;
    _cell@V_WORDS[_cell!V_LENGTH] -> _end;
    _cell@V_WORDS -> _cell;
    while _cell <@(w) _end do
        _cell@~PTE_NEXT -> _last;
        _cell!(w)++ -> _cell -> _entry;
        while iscompound(_entry) do
            if _entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
                _deads fi_+ 1 -> _deads
            else
                _entry ->> _last!PTE_NEXT -> _last;
                count fi_+ 1 -> count
            endif;
            _entry!PTE_NEXT -> _entry
        endwhile;
        0 -> _last!PTE_NEXT
    endwhile;
    if prop!PT_EXPAND then
        prop!PT_COUNT fi_+ _deads -> prop!PT_COUNT
    endif
enddefine;

define App_entries_check(prop);
    lvars prop;
    returnunless(prop!PT_REHASH);
    unless prop!PT_HASH_PDR then
        Rehash(prop)
    elseif prop!PT_REHASH /== true then
        ;;; set up by POPC/POPLINK
        weakref Rehash_hash(prop)
    elseif prop!PT_ENTRY_KEY /== perm_prop_entry_key then
        Count(prop) ->
    endunless
enddefine;


    /*  Search for -arg- in -prop-
    */
define Search(arg, prop);
    lvars arg, prop;
    if prop!PT_REHASH then Rehash(prop) endif;
    prop!PT_TABLE -> prop;
    prop!V_WORDS[ ##(w){arg} _biclear _negate(prop!V_LENGTH) ] -> prop;
    while iscompound(prop) do
        if arg == prop!PTE_ARG then
            return(prop)
        else
            prop!PTE_NEXT -> prop
        endif
    endwhile;
    false
enddefine;

    /*  Add a new entry to -prop-
    */
define Add_entry(prop, hasharg, arg, value);
    lvars entry, arg, hasharg, prop, value, _cell, _key;
    prop!PT_ENTRY_KEY -> _key;
    if _key == DPWEAK destroy_prop_entry_key then
        Get_store(@@(struct DESTROY_PROP_ENTRY)++) -> entry;
        if DPWEAK sys_destroy_dependent then _DPTE_DEPEND else _0 endif
                                                -> entry!DPTE_FLAGS;
        0 -> entry!DPTE_DLINK   ;;; for safety
    else
        Get_store(@@(struct PROP_ENTRY)++) -> entry
    endif;
    _key -> entry!KEY;
    arg -> entry!PTE_ARG;
    value -> entry!PTE_VALUE;

    prop!PT_TABLE -> prop;
    prop@V_WORDS[##(w){hasharg} _biclear _negate(prop!V_LENGTH) ] -> _cell;
    _cell!(w) -> entry!PTE_NEXT;
    entry -> _cell!(w)
enddefine;

    /*  Kill entry for -arg- in -prop-
    */
define Kill_entry(arg, prop);
    lvars arg, prop, _cell;
    if prop!PT_REHASH then Rehash(prop) endif;
    prop!PT_TABLE -> prop;
    prop@V_WORDS[ ##(w){arg} _biclear _negate(prop!V_LENGTH) ] -> _cell;
    while iscompound(_cell!(w) ->> prop) do
        if arg == prop!PTE_ARG then
            prop!PTE_NEXT -> _cell!(w);
            return(true)
        else
            prop@PTE_NEXT -> _cell
        endif
    endwhile;
    false
enddefine;

    /*  Access/update the value of -arg- in -prop-
        (n.b. pdprops are for popc identification)
    */
define Get(arg, prop) with_props '(Sys$-Prop$-Get)';
    lvars entry, prop, arg;
    if prop!PT_REHASH then Rehash(prop) endif;
    prop!PT_TABLE -> entry;
    entry!V_WORDS[ ##(w){arg} _biclear _negate(entry!V_LENGTH) ] -> entry;
    while iscompound(entry) do
        returnif(entry!PTE_ARG == arg) (entry!PTE_VALUE);
        entry!PTE_NEXT -> entry
    endwhile;
    if prop!PT_ACTIVE then
        fast_apply(arg, prop!PT_ACTIVE)
    else
        prop!PT_DEFAULT
    endif
enddefine;
;;;
define updaterof Get(value, arg, prop) with_props '(->Sys$-Prop$-Get)';
    lvars arg, entry, prop, value;
    if prop!PT_DEFAULT == value then
        Kill_entry(arg, prop) ->
    elseif (Search(arg, prop) ->> entry) then
        value -> entry!PTE_VALUE;
        if prop!PT_ENTRY_KEY == DPWEAK destroy_prop_entry_key then
            if DPWEAK sys_destroy_dependent then _DPTE_DEPEND else _0 endif
                                            -> entry!DPTE_FLAGS
        endif;
    else
        Add_entry(prop, arg, arg, value)
    endif
enddefine;


;;; --- THINGS USED BY OTHER PROPERTY PROCEDURES -------------------------

define Checkr_prop(prop);
    lvars prop;
    unless isproperty(prop) do
        mishap(prop, 1, 'PROPERTY NEEDED')
    endunless;
    prop!PD_CLOS_FROZVALS[_0]
enddefine;


    /* Copy property record (used by -copy-) */

define Copy(prop) -> prop;
    lvars prop, vec, _i;

    define lconstant Copy_entries(entry) -> entry;
        lvars entry;
        if iscompound(entry) then
            copy(entry) -> entry;
            Copy_entries(entry!PTE_NEXT) -> entry!PTE_NEXT
        endif
    enddefine;

    ;;; Copy property record
    prop -> vec;
    copy(prop) -> prop;
    vec!PT_REHASH -> prop!PT_REHASH;

    ;;; Copy vector of property entries
    writeable copy(prop!PT_TABLE) ->> vec -> prop!PT_TABLE;

    ;;; Copy individual entries
    fast_for _i from 1 to datalength(vec) do
        Copy_entries(fast_subscrv(_i, vec)) -> fast_subscrv(_i, vec)
    endfast_for
enddefine;


endsection;     /* $-Sys$-Prop */


;;; --- PROPERTY KEY ----------------------------------------------------

section $-Sys => isprop_entry;

define lconstant Hash_property(item);
    lvars item;
    _pint(item!PT_TABLE!V_LENGTH) fi_+ syshash(item!PT_DEFAULT)
enddefine;

constant
    property_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_PROPERTY,      ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "property",             ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        false,                  ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Minimal_print,          ;;; K_SYS_PRINT
        WREF Minimal_print,     ;;; K_PRINT
        WREF Hash_property,     ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct PROP)++,      ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};


;;; --- PROPERTY ENTRY KEYS ------------------------------------------------

define isprop_entry(item);
    lvars item;
    iscompound(item) and item!KEY!K_FLAGS _bitst _:M_K_PROP_ENTRY
enddefine;

define lconstant Propent_getsize();
    ->, @@(struct PROP_ENTRY)++
enddefine;

define lconstant Propent_print(entry);
    lvars entry;
    if pop_pr_level == 0 then
        Minimal_print(entry)
    else
        cucharout(`<`);
        spr(entry!KEY!K_DATAWORD);
        spr(entry!PTE_ARG);
        pr(entry!PTE_VALUE);
        cucharout(`>`);
    endif
enddefine;


define lconstant PROPENT_KEY(gc_type, type_name);
    lvars gc_type, type_name;
    struct KEY_PROP_ENTRY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_PROP_ENTRY _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        gc_type,                ;;; K_GC_TYPE
        Propent_getsize,        ;;; K_GET_SIZE

        "prop_entry",           ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isprop_entry,           ;;; K_RECOGNISER
        #_< WREF Exec_nonpd >_#,;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        #_< WREF nonop == >_#,  ;;; K_EQUALS
        Propent_print,          ;;; K_SYS_PRINT
        #_< WREF Propent_print >_#,
                                ;;; K_PRINT
        #_< WREF Fullrec1_hash >_#,
                                ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        type_name               ;;; K_PROP_TYPE_NAME
        %}
enddefine;

constant
    perm_prop_entry_key     = PROPENT_KEY(_:GCTYPE_PROPENT_PERM,    "perm"),
    tmparg_prop_entry_key   = PROPENT_KEY(_:GCTYPE_PROPENT_TMPARG,  "tmparg"),
    tmpval_prop_entry_key   = PROPENT_KEY(_:GCTYPE_PROPENT_TMPVAL,  "tmpval"),
    tmpboth_prop_entry_key  = PROPENT_KEY(_:GCTYPE_PROPENT_TMPBOTH, "tmpboth"),
    tmpclr_prop_entry_key   = PROPENT_KEY(_:GCTYPE_PROPENT_TMPCLR,  "tmpclr"),
    ;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Apr 16 1993
        Made table vector writeable in Copy
--- John Gibson, Sep 30 1992
        Made Count call Init_hash if PT_REHASH is a vector
--- John Gibson, Sep 19 1992
        Added Init_hash to deal with incrementally-linked props set up by
        POPLINK
--- John Gibson, Aug 11 1990
        Changed PTE for destroy prop entries to DPTE
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Jul 31 1989
        Added "tmpclr" property type
--- John Williams, Jun  7 1989
        Added -Propent_print-, -isprop_entry-, changed flags to _: syntax
--- John Gibson, May 24 1989
        Added keys for new property entry types, plus other changes to
        support these.
--- John Gibson, Jan 16 1989
        Speeded up -Get-.
        Added string pdprops to -Get- and updater for popc identfiication
--- Roger Evans, Aug 31 1988
        added code to initialise DPTE_FLAGS for destroy prop entries
--- John Williams, Jul  6 1988
        Added -Copy-
--- John Gibson, Mar 24 1988
        Moved out of props.p
 */
