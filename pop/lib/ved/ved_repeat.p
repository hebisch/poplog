/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_repeat.p
 >  Purpose:        insert format for repeat statement
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global ved_repeat();
    vedlinebelow();
    vedpositionpush();
    vedinsertstring('\trepeat NUMBER times\n\t\tACTION\n\tendrepeat;');
    vedpositionpop()
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Jun 15 1992
        Moved call to -vedpositionpush- after call to -vedlinebelow- for
        consistency with -ved_while- and -ved_until-.
--- Mark Rubinstein, Oct  4 1985 - sectionised and lvarsed.
 */
