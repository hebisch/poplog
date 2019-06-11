/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_showlibs.p
 > Purpose:         Display names of LIB files matching a given word
 > Author:          John Williams, Dec  5 1991 (see revisions)
 > Documentation:   HELP * SHOWLIBS
 > Related Files:   LIB * VED_HELPFOR
 */
compile_mode :pop11 +strict;

section;

uses ved_helpfor;

define vars ved_showlibs();
    lvars item list;
    if vedargument = nullstring then
        'lib' -> vedargument
    endif;
    subsystem_searchlist("vedlibname", veddocsubsystem) -> list;
    if null(list) then
        popuseslist -> list
    endif;
    vedhelpfor(vedargument,
                [% for item in flatten_searchlist(list, false) do
                    [% item, "lib" %]
                endfor %])
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 15 1993
        subsystem_searchlist and veddocsubsystem no longer need to be
        weakref'd (they are now in the core system)
 */
