/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_while.p
 >  Purpose:        Insert template for WHILE statement
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:  LIB * VED_IF, *VED_UNTIL etc.
 */

section;

define global ved_while();
    vedlinebelow();
    vedpositionpush();
    vedinsertstring('\twhile CONDITION do\n\t\tACTION\n\tendwhile;');
    vedpositionpop();
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Jun 15 1992
        Added call to -vedpositionpush- (see bugreport isl-fr.4396).
        Remove intial newline from template string.
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
