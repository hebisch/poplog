/*  --- Copyright University of Sussex 1994.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_jjp.p
 >  Purpose:        Full justification of current paragraph
 >  Author:         Chris Slymon (from David Roberts), June 1983 (see revisions)
 >  Documentation:  HELP * FORMAT
 >  Related Files:  LIB * VED_JJ
 */
compile_mode :pop11 +strict;

section;

define vars ved_jjp();
    vedpositionpush();
    vedmarkpush();
    false -> vvedmarkprops;
    vedmarkparagraph();
    ved_jj();
    vedmarkpop();
    vedpositionpop();
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 18 1994
        Changed to use new vedmarkparagraph and so made entirely consistent
        with ved_jp
--- John Gibson, Apr 21 1993
        Tidied up
*/
