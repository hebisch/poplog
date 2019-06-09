/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptGeometrySpec.p
 > Purpose:         Construct geometry spec args from numeric data
 > Author:          Roger Evans, Nov 11 1990 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define XptGeometrySpec(spec);
    lvars spec, l;

    if (spec.isvector or spec.islist)
    and (length(spec) ->> l) == 2 or l == 4 then
        if l == 4 then spec(3), spec(4) else false, false endif,
                spec(1), spec(2) -> XptParseGeometry()
    else
        mishap(spec, 1, 'INVALID GEOMETRY SPECIFICATION');
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 15 1993
        Changed to use new updater of XptParseGeometry
 */
