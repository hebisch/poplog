/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/prolog_compile.p
 > Purpose:         Old dummy version of compiler
 > Author:          John Gibson, Jan 12 1993 (see revisions)
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

uses prolog_subsystem;

define vars prolog_compile(stream);
    lvars stream;
    mishap(0, 'PROLOG NOT LOADED');
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 26 1993
        uses prolog_subsystem
 */
