/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_qref.p
 > Purpose:        Quit current file and call ref on another
 > Author:         Aaron Sloman, July 1982 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_qref;
    vedqget(ved_ref)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Robert Smith, Jun 22 1988  - made ved_qref 'vars procedure'
--- John Williams, Dec  1 1986 - made it global
--- Ben Rubinstein, Apr 30 1986 - removed 'uses vedqget' (now in system)
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
