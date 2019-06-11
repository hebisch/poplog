/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 *  File:           C.all/lib/ved/ved_autowrite.p
 *  Purpose:        for setting value of vedautowrite
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * VEDVARS/vedautowrite
 *  Related Files:
 */

compile_mode: pop11 +strict;
section;

define global vars ved_autowrite();
lvars vedtemp;
    if strnumber(vedargument) ->> vedtemp then
        max(0,vedtemp) -> vedautowrite;
    elseunless vedargument = '' then
        vederror('NUMBER NEEDED')
    endif;
    if vedautowrite = 0 then false -> vedautowrite endif;
    vedputmessage('VEDAUTOWRITE: ' sys_>< vedautowrite)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        Made +strict. Now uses sys_><
 */
