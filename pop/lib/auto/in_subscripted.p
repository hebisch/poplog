/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/in_subscripted.p
 > Purpose:
 > Author:          Ian Rogers, Nov  4 1988 (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * CLASS_SUBSCR_LOOP;
 */


/*
    for <vars> in_subscripted <exprs> do <body> endfor;
 */

section;

uses class_subscr_loop;

define :for_extension global in_subscripted with_nargs 2;
    class_subscr_loop(false, class_apply);
enddefine;

define :with_index_hook in_subscripted with_nargs 3;
    class_subscr_loop(class_apply);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Mar 26 1990
     Added hook for -with_index-
 */
