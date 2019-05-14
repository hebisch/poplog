/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/countitem.p
 *  Purpose:        Counts the number of text items in a file
 *  Author:         Aaron Sloman, Feb 1982 (see revisions)
 *  Documentation:  HELP * COUNTITEM
 *  Related Files:
 */

section $-library => countitem;

define global countitem(file) -> x;
lvars c white = false, file x = 0;
    discin(file) -> file;
    until (file() ->> c) == termin do
        if strmember(c, '\s\t\n\r') then
            unless white then
                x fi_+  1 -> x;
                true -> white
            endunless;
        else
            false -> white
        endif;
    enduntil
enddefine;

endsection;
