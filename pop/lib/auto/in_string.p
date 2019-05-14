/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/in_string.p
 > Purpose:         for <vars> in_string <string(s)> do <body> endfor
 > Author:          John Williams, (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * IN_VECTOR, LIB * SUBSCR_LOOP
 */

section;

uses subscr_loop;
uses with_index;


define :for_extension global in_string with_nargs 2;
    subscr_loop(false, fast_subscrs, check_string);
enddefine;


define :with_index_hook in_string with_nargs 3;
    subscr_loop(fast_subscrs, check_string);
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Feb 20 1991
    Removed definition of -check_string- as it's now an autoloadable
    library
 */
