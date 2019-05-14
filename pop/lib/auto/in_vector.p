/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/in_vector.p
 > Purpose:
 > Author:          Ian Rogers, Nov  4 1988 (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * SUBSCR_LOOP
 */

/*
    for <vars> in_vector <exprs> do <body> endfor;

    Like the in_vectorclass form but more specific and efficient.
 */

section;

uses subscr_loop;

define :for_extension global in_vector with_nargs 2;
    subscr_loop(false, fast_subscrv, check_vector);
enddefine;

define :with_index_hook in_vector with_nargs 3;
    subscr_loop(fast_subscrv, check_vector);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 16 1992
        Made check_vector a separate autoloadable procedure
--- Ian Rogers, Mar 26 1990
    Added hook for -with_index-
 */
