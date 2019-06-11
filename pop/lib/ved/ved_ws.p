/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ws.p
 >  Purpose:        for setting value of vedstartwindow: i.e. window size
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:  LIB * VED_WINDOW
 */

section;

define global ved_ws();
    lvars old;
    if vedargument == nullstring then
    elseif strnumber(vedargument) then
        vedstartwindow -> old;
        min(vedscreenlength, strnumber(vedargument)) -> vedstartwindow;
        unless vedstartwindow == old then
            false ->> vedupperfile -> vedlowerfile;
            vedsetonscreen(vedcurrentfile, false);
            if vedstartwindow < vedscreenlength
            and length(vedbufferlist) > 1 then
                vedswapfiles(); vedswapfiles()
            endif;
        endunless;
    else
        vederror('NUMBER NEEDED')
    endif;
    vedputmessage('LOWER WINDOW SIZE IS ' sys_>< vedstartwindow)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  7 1992
        Altered to use sys_>< instead of ><.
 */
