/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_hprint.p
 >  Purpose:        print file(s) with a header
 >  Author:         Mark Rubinstein, February 1985
 >  Documentation:  HELP * VED_PRINT
 >  Related Files:  LIB * VED_PRINT
 */
compile_mode :pop11 +strict;

section;

define vars ved_hprint;
    '-h ' >< vedargument -> vedargument;
    ved_print();
enddefine;

endsection;
