/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_window.p
 >  Purpose:        change the size of the current window
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  REF *VEDCOMMS/ved_window HELP *VED_WINDOW
 >  Related Files:  C.all/x/ved/auto/ved_xved.p
 */

section;

define global ved_window;
    lvars num, arg = vedargument;

    if arg == nullstring then
        vedstartwindow >< nullstring -> vedargument;
        vedstartwindow
    elseif arg = '?' then
        arg;
    else
        strnumber(arg)
    endif -> num;

    if vedusewindows /== "x" then
        ;;; normal version
        if num == arg then
            vedputmessage(vedwindowlength sys_>< nullstring);
            return();
        elseif num then
            num -> vedwindowlength;
            unless vedcurrentfile == vedupperfile then
                vedscreenlength - vedwindowlength -> vedscreenoffset
            endunless;
            vedrefresh();
        else
            vederror('NUMBER NEEDED')
        endif;
    else
        ;;; XVed version
        if num == arg then
            vedputmessage(vedwindowlength sys_>< nullstring);
            return();
        else
            if num then
                'xved window numRows '
            else
                'xved window '
            endif sys_>< vedargument -> vedargument;
            veddo(vedargument);
        endif;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul 15 1991 : Bug fix, <ENTER> window in XVed now sets
        window length to -vedstartwindow- for compatability with VED
--- Jonathan Meyer, Jul  7 1991
        Moved majority of XVed code into C.all/x/ved/auto/ved_xved.p and
        changed to use veddo to call the XVED version
--- Jonathan Meyer, Jun 22 1991
        Revised to work for XVed
 */
