/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckString.p
 > Purpose:         Check for String
 > Author:          John Gibson, Apr 13 1993
 */
compile_mode :pop11 +strict;

section;

;;; Checks argument is a String - 19/07/90
define XptCheckString(item);
    lvars item;
    if isstring(item) then
        return(item);
    else
        mishap(item, 1, 'String NEEDED');
    endif;
enddefine;

endsection;
