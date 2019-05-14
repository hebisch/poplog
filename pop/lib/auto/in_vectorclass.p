/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/in_vectorclass.p
 > Purpose:
 > Author:          Ian Rogers, Nov  9 1988 (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * CLASS_SUBSCR_LOOP;
 */

section;

uses class_subscr_loop;

define :for_extension global in_vectorclass with_nargs 2;
    class_subscr_loop(false, class_fast_subscr);
enddefine;

define :with_index_hook in_vectorclass with_nargs 3;
    class_subscr_loop(class_fast_subscr);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Mar 26 1990
     Added hook for -with_index-
 */
