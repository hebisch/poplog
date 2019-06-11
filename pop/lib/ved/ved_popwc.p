/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 *  File:           C.all/lib/ved/ved_popwc.p
 *  Purpose:        Uses wordcount to provide a count for current file
 *  Author:         Roger Evans, April 1983 (see revisions)
 *  Documentation:  HELP * WORDCOUNT
 *  Related Files:  LIB * VED_POPWCMR.P
 */

uses wordcount;

compile_mode: pop11 +strict;
section;

define global vars ved_popwc();
    dlocal vedcolumn vedline vvedlinesize;
    vedtopfile();
    vedputmessage(wordcount(vedrepeater) sys_>< ' WORDS');
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        o Now compiles under +strict
        o Uses sys_>< rather than ><
--- Mark Rubinstein, Nov 12 1985 - sectionised.
 */
