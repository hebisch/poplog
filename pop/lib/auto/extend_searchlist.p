/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/extend_searchlist.p
 > Purpose:         Adding a directory to a library search list
 > Author:          Roger Evans, Jun  1 1990 (see revisions)
 > Documentation:   REF * LIBRARY
 > Related Files:   C.all/src/syslibcompile.p
 */

section;

define lconstant Member(dir, list);
    lvars dir item list;
    for item in list do
        returnif(item = dir) (true);
        while isword(item) or isident(item) do
            if isword(item) then
                valof(item)
            else
                idval(item)
            endif -> item;
            if islist(item) then
                returnif(Member(dir, item)) (true);
            else
                returnif(item = dir) (true)
            endif
        endwhile
    endfor;
    false
enddefine;


define global extend_searchlist(dir, liblist);
    lvars at_end dir liblist;

    if isboolean(liblist) then
        liblist -> at_end, dir -> liblist, -> dir
    else
        false -> at_end
    endif;

    if isstring(dir) then
        dir dir_>< nullstring -> dir
    endif;

    if islist(dir) then
        if at_end then
            [^^liblist ^^dir]
        else
            [^^dir ^^liblist]
        endif
    else
        if Member(dir, liblist) then
            liblist
        elseif at_end then
            [^^liblist ^dir]
        else
            [^dir ^^liblist]
        endif
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Dec  3 1990
        Now handles [DIR PROPS] style search list entries correctly.
--- Aaron Sloman, Oct 29 1990
        Altered so that first argument can be a list
--- John Williams, Sep 13 1990
        Largely re-written. Now takes optional third "at_end" argument.
        Also ensures directory has a trailing "/".
--- John Williams, Sep 13 1990
        Moved from lib/lib to lib/auto
--- Aaron Sloman, Jul 28 1990
        Renamed -extendsearchlist- as -extend_searchlist-.
        Compare -flatten_searchlist-.
--- Roger Evans, Jul 13 1990
        Added code to filter out vedfileprops words
 */
