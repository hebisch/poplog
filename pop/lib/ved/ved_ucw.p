/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_ucw.p
 *  Purpose:        converts next N words to upper case. (defaults to 1)
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:  LIB * VED_UCL, * VED_UCR
 */

section;

define global ved_ucw();
    vedconvertword(islowercode, lowertoupper, vedargument, true)
enddefine;

endsection;
