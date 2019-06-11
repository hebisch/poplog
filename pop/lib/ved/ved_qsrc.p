/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_qsrc.p
 > Purpose:        Quit current file and call src on another.
 > Author:         Unknown, ??? (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

    ;;; can't be a closure because ved_src is a variable ....
define vars ved_qsrc();
    vedqget(ved_src)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Smith, Jun 22 1988 - made ved_qsrc 'vars procedure'
--- Ben Rubinstein, Apr 30 1986 - removed 'uses vedqget' (now in system)
*/
