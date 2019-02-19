/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/src/fast_appproperty.p
 > Purpose:
 > Author:          John Gibson & John Williams (see revisions)
 > Documentation:   REF *PROPS
 */

;;; ----- APPLY A PROCEDURE TO EACH ENTRY IN A PROPERTY (FAST VERSION) ------

#_INCLUDE 'declare.ph'

global constant
        $-Sys$-Gc$-dead_prop_entry
    ;

section $-Sys$-Prop;

constant
        procedure (Checkr_prop, App_entries_check)
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys$-Prop => fast_appproperty;

    /*  Fast version that assumes the property isn't changed
        (-prop- is the actually property record)
    */
define lconstant App_property(prop, p);
    lvars entry, procedure p, prop, _cell, _limit;
    if prop!PT_REHASH then App_entries_check(prop) endif;
    prop!PT_TABLE -> prop;
    @@V_WORDS[_0] -> _cell;
    @@V_WORDS[prop!V_LENGTH] -> _limit;
    while _cell _lt _limit do
        prop!(w){_cell} -> entry;
        while iscompound(entry) do
            unless entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
                _CHECKUSER;
                fast_apply(entry!PTE_ARG, entry!PTE_VALUE, p)
            endunless;
            entry!PTE_NEXT -> entry
        endwhile;
        @@(w){_cell}++ -> _cell
    endwhile
enddefine;

    ;;; user version of app_property
define fast_appproperty(prop, pdr);
    lvars prop, pdr;
    Check_procedure(pdr);
    ;;; allow prop to be an actual property record (for popc)
    unless iscompound(prop) and prop!KEY == property_key then
        Checkr_prop(prop) -> prop
    endunless;
    App_property(prop, pdr)
enddefine;

    /*  Called by -explode- on a property
    */
define Dest() with_nargs 1;
    chain(()!PD_CLOS_FROZVALS[_0],
            procedure();
                conspair((), conspair((), []))
            endprocedure,
            App_property)
enddefine;


endsection;     /* $-Sys$-Prop */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 11 1999
        Added missing userstack check in App_property
--- John Gibson, May 24 1989
        Changes to -App_property-
--- John Gibson, Jan 17 1989
        Allowed -fast_appproperty- to take an actual property record
--- Roger Evans, Aug 19 1988
        changed tests == temp_prop_entry_key to be /== perm_prop_entry_key
        (so destroy_prop_entry_key gets treated like temp_prop_entry_key)
--- John Gibson, Mar 24 1988
        Moved out of appprop.p
 */
