/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XtR.p
 > Purpose:         Null terminated fixed address string constants
 > Author:          Jonathan Meyer, Nov 14 1990
 > Documentation:
 > Related Files:
 */

section;
uses XtN;

define global constant macro XtR;
    XtNLookup(readitem(), "R");
enddefine;

endsection;
