/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/CHECKARGLIST.p
 > Purpose:         Macro for calling arglist check as dlocal expression
 >                  to handle XptArgList call forms
 > Author:          John Gibson, Apr 12 1993
 > Documentation:   REF * XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

constant macro $-CHECKARGLIST =
    [dlocal 0 %"%"% if dlocal_context == 1 then
                    XptCheckArgListAndCardinal()
                endif,%"%"%;
    ];
