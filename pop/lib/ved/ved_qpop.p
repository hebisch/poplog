/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_qpop.p
 >  Purpose:        leave ved to pop, while quitting the current file.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_qpop();
    ved_q();
    ved_pop()
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Robert Smith, Jun 22 1988 - made ved_qpop 'vars procedure'
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
