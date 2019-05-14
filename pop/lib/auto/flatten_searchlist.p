/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/flatten_searchlist.p
 > Purpose:         "Flattening" search lists
 > Author:          Aaron Sloman, Jul 25 1990 (see revisions)
 > Documentation:   REF * FLATTEN_SEARCHLIST, REF * LIBRARY, HELP * POPUSESLIST
 > Related Files:   LIB * POPUSESLIST, * LOADLIB
 */

compile_mode:pop11 +strict;

section;

define global flatten_searchlist(list);
    lvars list, want_props = false, props = false;

    define lconstant Flatten(item);
        lvars   item;
        dlocal  props;
        if islist(item) then
            ;;; directory + props
            item -> props;
            hd(item) -> item;
        endif;
        if isstring(item) then
            ;;; simple directory name
            if want_props and props then
                if item = hd(props) then props else item :: tl(props) endif;
            else
                item;
            endif;
        elseif isprocedure(item) then
            ;;; ignore
        else
            ;;; must be a recursive search list
            if isword(item) then
                valof(item) -> item;
            elseif isident(item) then
                idval(item) -> item;
            else
                mishap(item, 1, 'ILLEGAL ITEM IN SEARCH LIST');
            endif;
            if islist(item) then
                applist(item, Flatten);
            else
                Flatten(item);
            endif;
        endif;
    enddefine;

    if isboolean(list) then
        list -> want_props -> list;
    endif;
    conslist(#| applist(list, Flatten) |#);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 19 1991
        Changed to ignore procedures rather than mishap. Modified to be
        more like -syssearchpath-
--- John Williams, Nov  9 1990
        Rewritten. Now copes with search lists that contain elements of
        the form [DIR PROPS COMPILER]. New optional arg. "want_props",
        if true, means that [DIR PROPS COMPILER] elements should be left
        unchanged.
--- Aaron Sloman, Jul 28 1990
        Renamed -flatten_liblist- as -flatten_searchlist-
 */
