/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_qteach.p
 > Purpose:        Quit current file and call teach on another.
 > Author:         Aaron Sloman, July 1982 (see revisions)
 > Documentation:
 > Related Files:  LIB * VEDQGET
 */
compile_mode :pop11 +strict;

section;

define vars ved_qteach;
    vedqget(ved_teach)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Robert Smith, Jun 22 1988 - made ved_qteach 'vars'
--- Ben Rubinstein, Apr 30 1986 - removed 'uses vedqget' (now in system)
--- Mark Rubinstein, Nov 12 1985 - sectionised.
 */
