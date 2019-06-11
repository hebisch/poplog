/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_top.p
 > Purpose:         Synonym for -ved_prolog-
 > Author:          Simon Nichols, Jun 25 1991
 > Related Files:   C.all/plog/src/plogved.p, C.all/lib/ved/ved_prolog.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_top();
    ved_prolog();
enddefine;

endsection;
