/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/clisp.p
 > Purpose:         Load LISP subsystem
 > Author:          John Williams, Jul  4 1990 (see revisions)
 > Documentation:   HELP * CLISP
 > Related Files:   C.all/lisp/src/clisp.p
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

unless is_subsystem_loaded("lisp") do
    subsystem_compile('$usepop/pop/lisp/src/clisp.p', "pop11");
    sys_init_subsystem("lisp")
endunless;

syssynonym("clisp", "lisp");        ;;; for uses

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 18 1993
        Removed 'uses subsystem' and replaced compile with pop11_compile
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
