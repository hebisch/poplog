/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_until.p
 >  Purpose:        Insert template for UNTIL statement
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:  LIB * VED_WHILE, *VED_IF etc.
 */

section;

define global ved_until();
    vedlinebelow();
    vedpositionpush();
    vedinsertstring('\tuntil CONDITION do\n\t\tACTION\n\tenduntil;');
    vedpositionpop();
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Jun 15 1992
        Remove intial newline from template string.
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
