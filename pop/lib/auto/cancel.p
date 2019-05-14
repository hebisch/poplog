/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           C.all/lib/auto/cancel.p
 > Purpose:        cancel words
 > Author:         Unknown, ??? (see revisions)
 > Documentation:  HELP * CANCEL
 */
compile_mode :pop11 +strict;

section;

define syntax cancel;
    lvars item;
    until (readitem() ->> item) == ";" or item == termin do
        nextif(item == ",");
        if isprotected(sys_read_path(item, false, false) ->> item) then
            mishap(item, 1, 'ATTEMPT TO CANCEL PROTECTED IDENTIFIER')
        else
            syscancel(item)
        endif
    enduntil;
    ";" :: proglist -> proglist
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 20 1987
        Made a syntax word rather than a macro; made to use -sys_read_path-
        so as to cope with sectioned identifiers.
--- Mark Rubinstein, Apr 22 1986
        Made cancel check for protected words.
*/
