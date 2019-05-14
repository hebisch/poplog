/* --- Copyright University of Sussex 1987. All rights reserved. ----------
 > File:            C.all/lib/auto/newsparse.p
 > Purpose:         Create a sparse array
 > Author:          Jonathan Laventhol, Oct 18 1984 (see revisions)
 > Documentation:   REF *PROPS, HELP *NEWSPARSE
 > Related Files:   NEWANYSPARSE
 */
compile_mode:pop11 +strict;

section;

define global newsparse = newanysparse(% undef %) enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug  5 1987
        Tidied up
--- John Williams, Jul 12 1985
        added "section" brackets
 */
