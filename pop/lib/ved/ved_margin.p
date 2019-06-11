/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 *  File:           C.all/lib/ved/ved_margin.p
 *  Purpose:        for setting value of vedleftmargin
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

compile_mode: pop11 +strict;
section;

define global vars ved_margin();
lvars temp;
    if vedargument == nullstring then
        vedcolumn - 1 -> temp
    elseif strnumber(vedargument)  then
        strnumber(vedargument)  -> temp
    else vederror('NUMBER NEEDED')
    endif;
    if temp < vedlinemax then
        temp -> vedleftmargin;
    else vederror('MARGIN MUST BE LESS THAN VEDLINEMAX')
    endif;
    vedputmessage('LEFT MARGIN IS ' sys_>< vedleftmargin)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        Now uses sys_><
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
 */
