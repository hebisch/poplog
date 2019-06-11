/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/vedgetsysfile.p
 > Purpose:         Key-mapped interface to ved_do_text_action
 > Author:          John Gibson, Nov  4 1995
 > Documentation:   REF * VEDPROCS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include ved_do_text_action.ph;

define vedgetsysfile();
    ved_do_text_action( vedline - vedlineoffset + vedwlineoffset,
                        vedcolumn - vedcolumnoffset + vedwcolumnoffset,
                        VDTA_TYPE_ANY, VDTA_MODE_EXECUTE) ->
enddefine;

endsection;
