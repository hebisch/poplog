/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_rcol.p
 >  Purpose:        set right hand column (vedlinemax)
 >  Author:         David Roberts, (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_rcol
 >  Related Files:  LIB * VED_LCOL
 */
compile_mode :pop11 +strict;

section;

define vars ved_rcol;
    lvars margin, col;
    unless vedargument = '?' then
        if isprocedure(vedleftmargin) then
            vedleftmargin()
        else
            vedleftmargin
        endif -> margin;
        strnumber(vedargument) or vedcolumn -> col;
        if col > margin then
            col -> vedlinemax
        else
            vederror('\{b}rightmost column must be to right of leftmost')
        endif
    endunless;
    vedputmessage('\{b}rightmost column is ' <> (vedlinemax sys_>< nullstring))
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul  3 1995
        No longer needs to dlocal vedleftmargin.
--- John Williams, Jan 22 1993
        Changed >< to sys_><
 */
