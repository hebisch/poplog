/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/TYPESPEC.p
 > Purpose:         Syntax word to return structure for a typespec
 > Author:          Roger Evans, Feb  3 1991 (see revisions)
 > Documentation:   REF *DEFSTRUCT
 > Related Files:   LIB *TYPESPEC_UTILS
 */
compile_mode :pop11 +strict;

uses typespec_utils;

section $-typespec_utils => TYPESPEC;

sysunprotect("TYPESPEC");

define global syntax TYPESPEC;
    lvars spec;
    dlocal pop_autoload = false;
    need_nextitem([(]) -> ;
    read_typespec(false, false) -> -> -> spec;
    need_nextitem([)]) -> ;
    sysPUSHQ(spec);
enddefine;

sysprotect("TYPESPEC");

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Apr  7 1991 made it global
--- Jonathan Meyer, Feb 14 1991
        Added dlocal pop_autoload = false, which is also the first line
        in x_typespec.
--- Roger Evans, Feb  5 1991 changed to syntax word form
 */
