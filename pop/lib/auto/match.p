/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/match.p
 *  Purpose:        Compatability with old library programs
 *  Author:         Anonymous
 *  Documentation:  HELP *MATCH
 *  Related Files:  HELP *MATCHES
 */

section;

define global procedure match(pattern, datum);
    lvars datum pattern;
    datum matches pattern
enddefine;

endsection;
