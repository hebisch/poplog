/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/property_misc.p
 > Purpose:         Various operations on properties
 > Author:          John Williams (see revisions)
 > Documentation:   REF * PROPS
 */

#_INCLUDE 'declare.ph'

global constant
        $-Sys$-Gc$-dead_prop_entry
    ;

section $-Sys$-Prop;

constant
        procedure (Checkr_prop, Count, App_entries_check)
    ;

endsection;


;;; -------------------------------------------------------------------

section $-Sys$-Prop => clearproperty property_default property_size
                        Prop_entry_state_init Prop_entry_state_next
                        clear_prop_entries;

define clearproperty(prop);
    ;;; empty property table
    lvars prop, _tab, _end;
    Checkr_prop(prop) -> prop;
    if prop!PT_EXPAND then
        prop!PT_COUNT fi_+ Count(prop) -> prop!PT_COUNT
    endif;
    prop!PT_TABLE -> _tab;
    _tab@V_WORDS[_tab!V_LENGTH] -> _end;
    _tab@V_WORDS -> _tab;
    while _tab <@(w) _end do
        0 -> _tab!(w)++ -> _tab
    endwhile
enddefine;


/*
 * Empty a property table, clearing all prop entries
 */
define clear_prop_entries(prop);
    lvars prop;
    lvars _bucket, _last_bucket, _pte, _count=0;

    Checkr_prop(prop) -> prop;  ;;; GET REAL PROPERTY FROM CLOSURE

    (prop!PT_TABLE)@V_WORDS -> _bucket;
    _bucket@(w)[((prop!PT_TABLE)!V_LENGTH)] -> _last_bucket;

    ;;; FOR EVERY BUCKET IN VECTOR OF BUCKETS
    while _bucket <@(w) _last_bucket do
        _bucket!(w) -> _pte;
        ;;; AND FOR EVERY PROP TABLE ENTRY IN EACH BUCKET
        while iscompound(_pte) do
            ;;; CLEAR THE PROP TABLE ENTRY
            $-Sys$-Gc$-dead_prop_entry ->> _pte!PTE_ARG -> _pte!PTE_VALUE;
            _count fi_+ 1 -> _count;
            _pte!PTE_NEXT -> _pte;
        endwhile;
        ;;; CLEAR THE BUCKET
        0 -> _bucket!(w)++ -> _bucket;
    endwhile;

    ;;; IF THE PROPERTY IS EXPANDABLE, UPDATE THE COUNTDOWN TO TABLE
    ;;; EXPANSION
    if prop!PT_EXPAND then
        prop!PT_COUNT fi_+ _count -> prop!PT_COUNT;
    endif;

enddefine;


define property_default() with_nargs 1;
    Checkr_prop()!PT_DEFAULT
enddefine;


define property_size() with_nargs 1;
    datalength(Checkr_prop()!PT_TABLE)
enddefine;


define Prop_entry_state_next(_pair);
    lvars _entry, _pair, _table, _subs;
    fast_front(_pair) -> _entry;
    repeat
        while iscompound(_entry) do
            if _entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
                _entry!PTE_NEXT -> _entry
            else
                _entry!PTE_NEXT -> fast_front(_pair);
                return(_entry)
            endif
        endwhile;
        ;;; get cell subscript and table vector
        fast_destpair(fast_back(_pair)) -> (_subs, _table);
        returnunless(_int(_subs) _lt _table!V_LENGTH) (false);
        _subs fi_+ 1 ->> _subs -> fast_front(fast_back(_pair));
        fast_subscrv(_subs, _table) ->> _entry -> fast_front(_pair)
    endrepeat
enddefine;

define Prop_entry_state_init(prop);
    lvars prop, table;
    Checkr_prop(prop) -> prop;
    if prop!PT_REHASH then App_entries_check(prop) endif;
    prop!PT_TABLE -> table;
    conspair(fast_subscrv(1,table), conspair(1, table))
enddefine;


endsection;     /* $-Sys$-Prop */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 17 1992
        Rewrote Prop_entry_ procedures to use pairs for the state so it
        can be given to sys_grbg_list
--- Adrian Howard, Jun  8 1992
        Added -clear_prop_entries-
--- John Williams, May  4 1990
        Added -property_size- (for use by Clisp, and LIB * DATAFILE)
--- John Gibson, May 24 1989
        Replaced -property_vector- with -Prop_entry_state_next- and
        -Prop_entry_state_init-.
--- Ian Rogers, Apr  3 1989
        Added property_vector
--- John Gibson, Mar 24 1988
        Moved out of props.p, appprop.p
 */
