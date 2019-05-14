/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/in_dstring.p
 > Purpose:         for <vars> in_dstring <(d)string(s)> do <body> endfor
 > Author:          Adrian Howard (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB *IN_STRING
 */

section;

uses subscr_loop;
uses with_index;


define :for_extension global in_dstring with_nargs 2;
    subscr_loop(false, fast_subscrdstring, check_string);
enddefine;


define :with_index_hook in_dstring with_nargs 3;
    subscr_loop(fast_subscrdstring, check_string);
enddefine;

endsection;
