/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_br.p
 >  Purpose:        move marked block specified places to the right
 >  Author:         Chris Slymon (after David Roberts), June 1983 (see revisions)
 >  Documentation:  HELP * FORMAT
 >  Related Files:  SHOWLIB * VED_BR, * VED_AR
 */
compile_mode :pop11 +strict;

section;

define ved_br;
    lvars _offset, oldchanged=vedchanged;
    dlocal vedstatic, vedbreak, vedautowrite;
    vedpositionpush();
    false ->> vedstatic ->> vedbreak -> vedautowrite;
    vedargint(vedargument) -> _offset; /* defaults to 1 */
    repeat _offset times
        ` `
    endrepeat;
    consstring( _offset) -> _offset;
    vedmarkfind();
    until vedline > vvedmarkhi do
        if vvedlinesize > 0 then
            vedtextleft();
            vedinsertstring( _offset);
        endif;
        vednextline()
    enduntil;
    vedpositionpop();
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    Changed vedargnum to vedargint
 */
