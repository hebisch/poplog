/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/sysFIELD_VAL.p
 > Purpose:         old procedures for planting record field access code
 > Author:          John Gibson, May  9 1990 (see revisions)
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define lconstant Checkr_spec(spec) -> spec;
    lvars spec;
    if iskey(spec) then
        class_field_spec(spec) -> spec;
    endif;
    unless islist(spec) do
        conspair(spec, false) -> spec
    endunless
enddefine;

define global sysFIELD_VAL(n, spec);
    lvars n spec;
    sysFIELD(n, Checkr_spec(spec), false, false)
enddefine;

define global sysUFIELD_VAL(n, spec);
    lvars n spec;
    sysUFIELD(n, Checkr_spec(spec), false, false)
enddefine;

define global sysSUBSCR(spec);
    lvars spec;
    sysFIELD(false, Checkr_spec(spec), false, false)
enddefine;

define global sysUSUBSCR(spec);
    lvars spec;
    sysUFIELD(false, Checkr_spec(spec), false, false)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul 12 1993
        Made Checkr_spec an lconstant.
--- John Gibson, Jan 18 1991
        Changed c*lass_spec to class_field_spec
--- John Williams, May 10 1990
        Rewritten to fix numerous bugs
 */
