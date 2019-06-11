/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_qdoc.p
 > Purpose:        Quit current file and call doc on another
 > Author:         Aaron Sloman, July 1982 (see revisions)
 > Documentation:
 > Related Files:  LIB * VEDQGET
 */
compile_mode :pop11 +strict;

section;

define vars ved_qdoc;
    vedqget(ved_doc)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Robert Smith, Jun 22 1988 - made ved_qdoc 'vars procedure'
--- Ben Rubinstein, Apr 30 1986 - removed 'uses vedqget' (now in system)
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
