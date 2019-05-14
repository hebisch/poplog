/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/impseqread.p
 >  Purpose:        procedure for reading an imperative sequence
 >  Author:         Allan Ramsay, Nov 7 1983 (see revisions)
 >  Documentation:  HELP * FORMS
 >  Related Files:  LIB * READIMP   LIB * FORMS  LIB * EXPRREAD
 */
compile_mode :pop11 +strict;

section;

define global impseqread = readimp(% true %) enddefine;

endsection;
