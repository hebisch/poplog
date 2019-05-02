/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/ved_f.p
 > Purpose:         Lisp version of ved_f (using vedfindpdheader)
 > Author:          John Williams, Jul  6 1990 (see revisions)
 > Documentation:   HELP * LISPVED
 > Related Files:   C.all/lisp/src/lispved.p
 */

lisp_compile_mode;

section $-lisp;


define lconstant Vednextstring();
    lvars char;
    while vedchartype(vedcurrentchar()) == `\s` do
        vedcharnext()
    endwhile;
    consstring
        (#| repeat
                vedcurrentchar() -> char;
                quitunless(vedchartype(char) == `a`);
                lowertoupper(char);
                vedcharnext();
        endrepeat |#);
enddefine;


define lconstant Lisp_findpdheader(name, exact) -> line;
    lvars startline = vedline, wrapped = false;
    dlocal vedline vedcolumn vedlineoffset vvedlinesize;

    lowertoupper(name) -> name;
    repeat
        if vedatend() then
            quitif(wrapped);
            true -> wrapped;
            vedtopfile();
            vedtextleft()
        else
            vednextline();
            quitif(wrapped and (vedline fi_> startline))
        endif;
        if vedcurrentchar() == `(` then
            vedline -> line;
            vedcharright();
            if isstartstring('DEF', Vednextstring()) then
                if exact then
                    returnif(name = Vednextstring())
                else
                    returnif(isstartstring(name, Vednextstring()))
                endif
            endif
        endif
    endrepeat;
    false -> line
enddefine;


define :ved_runtime_action;
    Lisp_findpdheader -> subsystem_valof("vedfindpdheader", "lisp")
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jul 12 1993
        No longer uses cons_with.
--- John Williams, Mar 19 1993
        Now uses ved_runtime_action define form.
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
