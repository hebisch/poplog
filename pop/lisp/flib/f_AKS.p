/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_AKS.p
 > Purpose:         LIB * FORMAT_PRINT ~A, ~K, and ~S directives
 > Author:          John Williams, Jul  1 1992 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

#_IF not(is_subsystem_loaded("lisp"))
global vars $-lisp$-print_escape;
#_ENDIF

unless isdefined("fbuffer") do
    syslibcompile("fbuffer", f_liblist) ->
endunless;


/* Generic procedure for ~A, ~K and ~S directives */

define lconstant Print_padding(len, mincol, colinc, minpad, padchar);
    lvars len, mincol, colinc, minpad, padchar;
    cuch_chars(minpad, padchar);
    mincol fi_- (len fi_+ minpad) -> mincol;
    until mincol fi_<= 0 do
        cuch_chars(colinc, padchar);
        mincol fi_- colinc -> mincol
    enduntil
enddefine;


define lconstant f_AKS(mincol, colinc, minpad, padchar);
    lvars mincol, colinc, minpad, padchar, arg, len;
    defaults mincol 0, colinc 1, minpad 0, padchar `\s`;

    next_f_arg() -> arg;
    if f_colon
    and f_subsystem == "lisp"
    and arg == [] then
        "'()'" -> arg
    endif;

    if mincol == 0 then
        /* No need to determine printed size of arg - just print directly */
        pr(arg)
    else
        check_positive(mincol);
        check_positive(colinc);
        check_positive(minpad);
        fprint_to_string(arg) -> (arg, len);
        if f_at then                    /* Pad on left */
            Print_padding(len, mincol, colinc, minpad, padchar);
            cuch_string(arg)
        else                            /* Pad on right */
            cuch_string(arg);
            Print_padding(len, mincol, colinc, minpad, padchar)
        endif
    endif
enddefine;


/* Define ~A, ~K, and ~S directives for both Pop-11 and Lisp */

procedure() with_nargs 4;
    dlocal print_escape = nil;
    f_AKS()
endprocedure -> f_proc(`A`);


procedure() with_nargs 4;
    dlocal pr = syspr;
    f_AKS()
endprocedure -> f_proc(`K`);


procedure() with_nargs 4;
    dlocal print_escape, pr;
    if f_subsystem == "pop11" then
        sys_syspr -> pr
    elseif f_subsystem == "lisp" then
        true -> print_escape
    endif;
    f_AKS()
endprocedure -> f_proc(`S`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar  6 1996
        Uses `is_subsystem_loaded("lisp")' to decide whether to declare
        Lisp variables.
--- John Williams, Dec 16 1994
        Re-written using fprint_to_string.
 */
