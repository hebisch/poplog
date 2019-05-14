/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/impread.p
 >  Purpose:        procedure for reading an expression sequence (an imperative)
 >  Author:         Allan Ramsay, Nov 7 1983 (see revisions)
 >  Documentation:  HELP FORMS
 >  Related Files:  LIB * READIMP
 */
compile_mode :pop11 +strict;

section;

;;; allow commas but not ";","=>" or "==>" as separators
define global impread = readimp(% false %) enddefine;

endsection;
