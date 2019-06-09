/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/ved/auto/ved_select.p
 > Purpose:         Makes a selection. Leaves you on the status line
 > Author:          Jonathan Meyer, Aug  2 1991
 > Documentation:
 > Related Files:   REF *VED_SELECTION
 */
compile_mode :pop11 +strict;

section;

define vars ved_select;
    vedselection_select(consword(vedargument));
    vedputcommand('seln');
    true -> ved_on_status;
    vedtextright();  vedcharright();
enddefine;

endsection;
