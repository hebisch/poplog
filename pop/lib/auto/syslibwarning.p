/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/syslibwarning.p
 > Purpose:         Procedure for printing LOADING LIB messages
 > Author:          John Williams, Mar 19 1992 (see revisions)
 > Documentation:   REF * LIBRARY
 > Related Files:   LIB * LIBWARNING
 */
compile_mode :pop11 +strict;

section;

define syslibwarning(lib_name);
    printf(';;; LOADING LIB %S\n', [^lib_name])
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 25 1993
        Now uses new %S printf option
--- John Williams, Jan 22 1993
        Sets pop_pr_quotes false.
--- John Gibson, Jun 19 1992
        Undid last change -- sysprmessage is intended for things that are
        errors/warnings, whereas syslibwarning is not really a "warning", it`s
        just information. (The point is that sysprmessage does things like
        raise the basewindow in XVed, which you don't want for this.)
--- John Williams, Jun  4 1992
        Now uses -sysprmessage- instead of -printf- (at JonM's suggestion)
 */
