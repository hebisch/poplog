/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 *  File:           C.all/lib/ved/ved_popwcmr.p
 *  Purpose:        uses wordcount to provide a count for marked range
 *  Author:         Roger Evans, April 1983 (see revisions)
 *  Documentation:
 *  Related Files:  LIB * WORDCOUNT
 */

compile_mode: pop11 +strict;
section;

uses wordcount;

define global vars ved_popwcmr();
    dlocal vedcolumn vedline ;
    vedmarkfind();
    vedputmessage(
        wordcount(vedrangerepeater(vvedmarklo,vvedmarkhi)) sys_>< ' WORDS');
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        o Now compiles under +strict
        o Uses sys_>< instead of ><
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
