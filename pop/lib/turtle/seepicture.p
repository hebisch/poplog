/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/seepicture.p
 |  Purpose:        does turtle picture analysis
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  HELP * SEEPICTURE
 |  Related Files:  LIB * TURTLE, * DATABASE
 */

section;

uses add;
uses remove;
uses present;
uses foreach;
uses turtle;

global vars Isvturtle; if Isvturtle.isundef then false -> Isvturtle endif;

define global seepicture();
    findlines();
    findjuncs();
    paintpicture();
enddefine;

endsection;
