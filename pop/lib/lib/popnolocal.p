/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/popnolocal.p
 > Purpose:         Remove local dirs from popsyslist etc
 > Author:          John Gibson, Jun 18 1993
 > Documentation:   REF * LIBRARY
 > Related Files:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define lconstant remove_local(id);
    lvars id;

    define lconstant get_nonlocal(item);
        lvars item, org_item = item;
        if islist(item) then hd(item) -> item endif;    ;;; annotated
        if isident(item) or isword(item) then
            remove_local(item)
        endif;
        unless isstring(item) and issubstring('local',item) then
            org_item
        endunless
    enddefine;

    if isword(id) then identof(id) -> id endif;
    maplist(idval(id), get_nonlocal) -> idval(id)
enddefine;

remove_local(ident popsyslist);
remove_local(ident popincludelist);

constant popnolocal = true;

endsection;
