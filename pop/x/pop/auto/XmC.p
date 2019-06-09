/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XmC.p
 > Purpose:         Macro for fixed-address nt string
 > Author:          John Gibson, Apr 14 1993
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

uses XtN;

define macro XmC;
    XtNLookup(readitem(), 'XmC');
enddefine;

endsection;
