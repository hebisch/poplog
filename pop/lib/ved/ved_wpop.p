/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_wpop.p
 >  Purpose:        write the file and go to pop
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_wpop();
    ved_w();
    ved_pop()
enddefine;

endsection;
