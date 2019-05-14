/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/useslib.p
 > Purpose:         Load a library (only if its name is undefined)
 > Author:          John Williams, Aug  8 1988 (see revisions)
 > Documentation:   REF * LIBRARY/useslib
 > Related Files:   LIB * USES
 */
compile_mode :pop11 +strict;

section;

define vars useslib(libname);
    uses_lib_idents(libname, false, [], 0)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 14 1992
        Changed to just call uses_lib_idents
--- John Gibson, Oct 24 1991
        Changed to use locchar etc to extract identifier name from pathname
        (allows for Unix pathnames only).
use sys_fname_nam
--- John Williams, Jul 29 1990
        Now localises -pop_default_type- to '.p', to fix buggy interaction
        with redefined -loadlib- in new LIB SUBSYSTEM.
--- James Goodlet, Jan 16 1990
        Accepts filenames which may have file extensions, or be partial
        paths.  Checks root filename (without extension) before loading,
        rather than whole item passed to it.
--- John Williams, Mar  8 1989
        Now uses -isdefined-
 */
