/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/fast_prop_entry.p
 > Purpose:
 > Author:          John Williams (see revisions)
 > Documentation:   REF *FASTPROCS
 */

;;; ----------------- FAST ACCESS TO PROPERTIES -------------------------

#_INCLUDE 'declare.ph'

global constant
    procedure newanyproperty
    ;

section $-Sys$-Prop;

constant
        procedure (Search, Kill_entry, Rehash_hash, Search_hash, Kill_hash)
    ;

endsection;

;;; --------------------------------------------------------------------

section $-Sys$-Prop => fast_get_prop_entry fast_kill_prop_entry
                       fast_prop_entry_value fast_prop_entry_arg;
lconstant macro
    NAP_WEAK    = [weakref %"["% newanyproperty %"]"%];

define fast_get_prop_entry(arg,prop);
    ;;; return property entry for arg or false.
    ;;; Do minimal checking - must be fast
    lvars arg, prop;
    prop!PD_CLOS_FROZVALS -> prop;  ;;; first froz val is property
    if prop!PT_HASH_PDR then
        ;;; stack args for search procedure. hash(arg), arg, then prop.
        fast_apply(arg, prop!PT_HASH_PDR), arg, prop;
        if prop!PT_REHASH then
            NAP_WEAK Rehash_hash(prop)
        endif;
        NAP_WEAK Search_hash()
    else
        Search(arg, prop)
    endif
enddefine;

define fast_kill_prop_entry(arg, prop);
    ;;; remove property entry for arg, returning true if there was one
    lvars arg, prop;
    prop!PD_CLOS_FROZVALS -> prop;  ;;; first froz val is property
    if prop!PT_HASH_PDR then
        ;;; stack args for search procedure. hash(arg), arg, then prop.
        fast_apply(arg, prop!PT_HASH_PDR), arg, prop;
        if prop!PT_REHASH then
            NAP_WEAK Rehash_hash(prop)
        endif;
        NAP_WEAK Kill_hash()
    else
        Kill_entry(arg, prop)
    endif;
    if prop!PT_COUNT and dup() then
        prop!PT_COUNT fi_+ 1 -> prop!PT_COUNT
    endif
enddefine;

    /*  The next procedure should never be applied to anything but
        the result of -fast_get_prop_entry-
    */
define fast_prop_entry_value() with_nargs 1;
    lvars entry;
    ()!PTE_VALUE
enddefine;
;;;
define updaterof fast_prop_entry_value() with_nargs 2;
    () -> ()!PTE_VALUE
enddefine;

define fast_prop_entry_arg() with_nargs 1;
    ()!PTE_ARG
enddefine;
;;; IR: no updater for obvious reasons

endsection;     /* $-Sys$-Prop */



/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Apr  5 1989
        Added -fast_prop_entry_arg-
--- John Gibson, Mar 24 1988
        Moved out of props.p
 */
