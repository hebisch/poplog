/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/popcompiler.p
 > Purpose:         Old VED compiler variable
 > Author:          John Gibson, Dec 29 1992 (see revisions)
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

include subsystem.ph;

section;

define active popcompiler;
    lvars ssname = subsystem or "pop11";
    if is_subsystem_loaded(ssname, false) then
        subscr_subsystem(SS_COMPILER, ssname)
    else
        if ssname == "top" then "prolog_toplevel" -> ssname endif;
        ;;; autoload dummy versions of compilers
        valof(ssname <> "_compile")
    endif
enddefine;
;;;
define updaterof active popcompiler p;
    lvars p, w, n, ssname;
    if isword(pdprops(p) ->> w) and (isendstring('_compile', w) ->> n) then
        if (subword(1,n-1,w) ->> ssname) == "prolog_toplevel" then
            "top" -> ssname
        endif;
        unless is_subsystem_loaded(ssname, true) then
            ;;; construct dummy subsystem
            word_string(ssname) -> w;
            subsystem_add_new(ssname, p, '.' <> w, nullstring, [], w)
        endunless;
        ssname -> subsystem
    else
        mishap(p, 1, 'popcompiler: CAN\'T HANDLE ASSIGNMENT')
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 26 1993
        Made updater use new subsystem_add_new if subsystem doesn't exist
 */
