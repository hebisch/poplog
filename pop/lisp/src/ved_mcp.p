/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/ved_mcp.p
 > Purpose:         Lisp versions of ved_mbp, ved_mep, ved_jcp, and ved_tidy
 > Author:          John Williams, Jan 30 1987 (see revisions)
 > Documentation:   HELP * LISPVED
 > Related Files:   C.all/lisp/src/lispved.p, LIB * SUBSYSTEM
 */

lisp_compile_mode;

section $-lisp => vedautoindent vedmaxindent;

global vars
    vedautoindent  =  false,
    vedmaxindent   =  vedindentstep,
    ;


define lconstant Lisp_mbp();
    lvars startline;
    dlocal vedline vedcolumn vvedlinesize;
    vedline -> startline;
    ;;; search back to "(DEF" (upper or lower case) at beginning of line
    repeat
        vedscreenleft();
        if  vedcurrentchar() == `(`
        and (vedcharright(), lowertoupper(vedcurrentchar()) == `D`)
        and (vedcharright(), lowertoupper(vedcurrentchar()) == `E`)
        and (vedcharright(), lowertoupper(vedcurrentchar()) == `F`) then
            vedmarklo();
            return
        endif;
        if vedline == 1 then
            vederror('No DEF... before line ' sys_>< startline)
        endif;
        vedcharup()
    endrepeat
enddefine;


define lconstant Vedclearhead();
    dlocal vednocharinsert = true;
    unless vedcolumn == 1 do
        vedclearhead()
    endunless
enddefine;


define lconstant Vedindentto(n);
    lvars line, i;
    dlocal vedcolumn, vedchanged = false;
    ;;; assumes 'vedsetlinesize' done
    returnif(vvedlinesize == 0 or vedline fi_> vvedbuffersize);
    fast_subscrv(vedline, vedbuffer) -> line;
    fast_for vedcolumn from 1 to vvedlinesize do
        quitif(fast_subscrs(vedcolumn, line) fi_> 32)
    endfast_for;
    fast_for i from vedcolumn to vvedlinesize do
        quitunless(fast_subscrs(i, line) == `;`);
        if (i fi_- vedcolumn) == 2 then
            Vedclearhead();
            return
        endif
    endfast_for;
    unless n == (vedcolumn fi_- 1) do
        Vedclearhead();
        n // vedindentstep -> n -> i;
        fast_repeat n times vedcharinsert(`\t`) endfast_repeat;
        fast_repeat i times vedcharinsert(`\s`) endfast_repeat;
        vedtrimline();
    endunless
enddefine;


define lconstant Lisp_mep();
    lvars char nesting n q s ss;
    dlocal vedline vedcolumn vvedlinesize;

    0 ->> nesting -> n;
    [] -> ss;
    false ->> q -> s;
    vedmarkhi();
    repeat
        vedrepeater() -> char;
        if char == termin then
            if q then
                'Missing closing ' <> consstring(q, 1)
            else
                format_string('~@(~[~;~:;~:*~R ~]missing~) closing bracket~:P',
                             {^nesting})
            endif;
            chain(vederror)
        endif;
        if s then
            s fi_+ 1 -> s;
            unless vedchartype(char) == `a` do
                if s == 2 then 1 else min(s, vedmaxindent) endif -> s;
                conspair(s, ss) -> ss;
                s + n -> n;
                false -> s;
            endunless
        endif;
        if char == `\n` then
            if q == `;` then
                false -> q
            endif;
            if vedautoindent and not(q) then
                Vedindentto(n)
            endif
        elseif q then
            if char == q then
                false -> q
            endif
        elseif char == `(` then
            nesting fi_+ 1 -> nesting;
            1 -> s
        elseif char == `)` then
            nesting fi_- 1 -> nesting;
            quitif(nesting == 0);
            n fi_- (fast_destpair(ss) -> ss) -> n
        elseif char == `;` or char == `"` or char == `|` then
            char -> q
        endif
    endrepeat;
    vedmarkhi();
    if vedautoindent then
        if vedchanged then vedchanged + 1 else 1 endif -> vedchanged
    endif
enddefine;


define lconstant Lisp_jcp();
    dlocal vedline vedcolumn vvedlinesize;
    dlocal vedautoindent = true, vvedmarkprops = false;
    vedmarkpush();
    Lisp_mbp();
    vedmarkfind();
    Lisp_mep();
    vedmarkpop()
enddefine;


define lconstant Lisp_tidy();
    dlocal vedline vedcolumn vvedlinesize;
    dlocal vedautoindent = true, vvedmarkprops = false;
    vedmarkpush();
    vedmarkfind();
    ved_mep();          ;;; Why not Lisp_mep? (John Williams, Jan  8 1993)
    vedmarkpop()
enddefine;


define :ved_runtime_action;
    Lisp_mbp -> subsystem_valof("ved_mbp", "lisp");
    Lisp_mep -> subsystem_valof("ved_mep", "lisp");
    Lisp_jcp -> subsystem_valof("ved_jcp", "lisp");
    Lisp_tidy -> subsystem_valof("ved_tidy", "lisp");
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 19 1993
        Now uses ved_runtime_action define form.
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
