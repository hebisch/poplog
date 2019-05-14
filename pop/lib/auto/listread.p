/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/listread.p
 >  Purpose:        read a list, vector or item
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * LISTREAD
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global listread();
    lvars item;
    readitem() -> item;
    if item = "[" then
        [%until (listread() ->> item) == "]" then
            if item == termin or item == "}" then
                mishap(item, 1, 'listread: EXPECTING ] CLOSING BRACKET')
            else
                item
            endif
          enduntil%]
    elseif item == "{" then
        {%until (listread() ->> item) == "}" then
            if item == termin or item == "]"then
                mishap(item, 1, 'listread: EXPECTING } CLOSING BRACKET')
            else
                item
            endif
          enduntil%}
    else
        item
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  3 1989
        Added +strict, tidied up
 */
