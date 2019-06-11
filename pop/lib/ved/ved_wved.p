/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_wved.p
 >  Purpose:        write files and start editing a new file
 >  Author:         A.Sloman 1982
 >  Documentation:  HELP * VEDCOMMS/ved_wved
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_wved();
    dlocal vedargument;
    lvars vedtext;
    ;;; hide vedargument from ved_w
    vedargument -> vedtext;
    nullstring -> vedargument;
    ved_w();
    vedtext -> vedargument;
    ved_ved();
enddefine;

endsection;
