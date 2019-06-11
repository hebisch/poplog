/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedcharnext.p
 >  Purpose:        Like VEDCHARRIGHT, but moves to next line if at line end
 >  Author:         John Williams, Aug 15 1985
 >  Documentation:  HELP *VEDPROCS /vedcharnext
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedcharnext();
    if vedcolumn fi_> vvedlinesize then
        vednextline()
    else
        vedcharright()
    endif
enddefine;

endsection;
