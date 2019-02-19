/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/trace.p
 > Purpose:         Tracing procedure execution
 > Author:          Aaron Sloman, John Williams (see revisions)
*/


#_INCLUDE 'declare.ph'

constant
        procedure (isconstant, isprotected, is_system_process, pr_field,
        recursive_valof, subscr_stack, sys_autoload, sys_read_path,
        sysunprotect
        )
    ;

vars
        pop_current_process, cuchartrace
    ;


vars
    tracing             =   true,
    pop_chain_trace     =   true,
    pop_process_trace   =   true,
    poptraceindent      =   0,
    popmaxtraceindent   =   40,
    ;

vars Sys$-chain_trace = true;


/* General tracing mechanism */

define vars systrace_pr(Name, Before, Args, Upd);

    ;;; Default procedure for printing trace information in -systrace_proc-
    ;;; Name is the name of procedure being traced,
    ;;; Before is TRUE for printing before the procedure runs, FALSE after
    ;;; Args is a vector or list of arguments or results

    lvars Name, Before, Args, Upd, _n = poptraceindent - 1;
    dlocal cucharout, tracing = false;

    if isprocedure(cuchartrace) then
        cuchartrace -> cucharout
    endif;
    unless isinteger(Before) then
        if Before then `>` else `<` endif -> Before
    endunless;
    if poptraceindent > popmaxtraceindent then
        cucharout(`[`);
        pr_field(poptraceindent, 4, `\s`, false, sys_syspr);
        cucharout(`]`);
    else
        until _n == 0 do
            cucharout(`!`);
            _n fi_- 1 -> _n;
        enduntil;
    endif;
    cucharout(Before);
    cucharout(`\s`);
    if Upd then spr("updater") endif;
    spr(Name);
    if islist(Args) then applist(Args, spr) else appdata(Args, spr) endif;
    cucharout(`\n`)
enddefine;


define vars systrace_proc(Proc, Name, Upd);

    ;;; If Upd is true then it is the updater that is being traced.
    ;;; This is the usual third argument of -systrace-
    ;;; Takes the procedure traced and its name, and does trace printing

    lvars Args, Name, Proc, Upd, _nargs, _sl;

    lconstant macro PSWEAK = [weakref %"["% process_key %"]"%];

    define lconstant Get_stack(_hi, _len) -> args;
        lvars args _hi _i _len;
        ;;; _hi is offset from top of stack
        ;;; _len is number of items
        initv(_len) -> args;
        fast_for _i from _len by -1 to 1 do
            subscr_stack(_hi fi_+ 1 ->> _hi) -> fast_subscrv(args, _i)
        endfast_for
    enddefine;

    define lconstant Inout_check(context, process);
        lvars context, process;
        if context == 4 and pop_process_trace
        and process == PSWEAK pop_current_process
        and not(PSWEAK is_system_process(process)) then
            systrace_pr(Name, `R`, [], Upd)
        endif
    enddefine;

    define updaterof Inout_check(context, process);
        lvars context, process;
        if context == 3 and pop_process_trace
        and process == PSWEAK pop_current_process
        and not(PSWEAK is_system_process(process)) then
            systrace_pr(Name, `S`, [], Upd)
        elseif context == 2 and Sys$-chain_trace then
            systrace_pr(Name, `X`, [], Upd)
        endif
    enddefine;

    dlocal
        0 % pop_chain_trace -> Sys$-chain_trace, %,
        0 % Inout_check(dlocal_context, dlocal_process) %,
    ;

    stacklength() -> _sl;
    ;;; pdnargs can give funny values for closures of variadic pdrs
    fi_max(0, pdnargs(Proc)) -> _nargs;
    if _sl fi_< _nargs then
        mishap(_sl, 'TOO FEW ARGUMENTS FOR ' sys_>< Name)
    endif;
    consvector(_nargs) -> Args;
    stacklength() -> _sl;
    systrace_pr(Name, true, Args, Upd);
    explode(Args);
    Proc();
    consvector(fi_max(stacklength() fi_- _sl, 0)) -> Args;
    systrace_pr(Name, false, Args, Upd);
    explode(Args)
enddefine;


define updaterof systrace(Proc, Name, Traceproc, Upd);
    lvars Proc Name Traceproc Upd;
    fast_chain(updater(Proc), Name, Traceproc, true, systrace)
enddefine;
;;;
define systrace(Proc, Name, Traceproc, Upd);
    lvars Name Proc Traceproc Upd;
    dlocal poptraceindent;
    if tracing then
        poptraceindent + 1 -> poptraceindent;
        recursive_valof(Traceproc)(Proc, Name, Upd)
    else
        chain(Proc)
    endif
enddefine;


/* TRACE and UNTRACE syntax */

/* Should local occurrences of trace and untrace automatically produce
    local declarations of traced identifiers?
*/

define istraced(pdr);
    ;;; return traced procedure or false
    lvars pdr;
    unless isprocedure(pdr) then
        mishap(pdr, 1 , 'CANNOT (UN)TRACE NON-PROCEDURE')
    endunless;
    if pdpart(pdr) == systrace then
        fast_frozval(1, pdr)
    else
        false
    endif
enddefine;


define lconstant Get_traceargs(untracing);
    lvars props untracing word;
    [%
    repeat
        nextreaditem() -> word;
        quitif(word == ";");    ;;; leave separator on proglist
        quitif(word == termin);
        Sys$-Prglst$-Chop();
        nextif(word == ",");
        sys_read_path(word, false, false) -> word;
        sys_autoload(word) ->;    ;;; mishap if not a word
        if tracing /== 1 and not(untracing) then
            ;;; Don't allow tracing of syntax procedures
            identprops(word) -> props;
            if isword(props) and isstartstring("syntax", props) then
                mishap(word, 1, 'CANNOT TRACE SYNTAX WORD')
            endif
        endif;
        if isconstant(word) or isprotected(word) then
            if untracing then
                printf(word,
                    ';;; WARNING - UNTRACING PROTECTED OR CONSTANT WORD %\n')
            endif;
            sysunprotect(word)
        endif;
        word
    endrepeat %]
enddefine;


define syntax trace;
    lvars lab list word;

    define lconstant Dotrace(pdr, word) -> pdr;
        lvars pdr props word;
        pdprops(pdr) -> props;
        systrace(% pdr, word, "systrace_proc", false %) -> pdr;
        props ->> pdprops(pdr) -> pdprops(updater(pdr))
    enddefine;

    Get_traceargs(false) -> list;
    unless tracing then
        true -> tracing
    endunless;
    fast_for word in list do
        ;;; unless istraced(valof(word)) then
        ;;;     Dotrace(valof(word), word) -> valof(word)
        ;;; endunless
        sysNEW_LABEL() -> lab;
        sysPUSH(word);
        sysCALLQ(istraced);
        sysIFSO(lab);
        sysPUSH(word);
        sysPUSHQ(word);
        sysCALLQ(Dotrace);
        sysPOP(word);
        sysLABEL(lab);
    endfast_for;
enddefine;


define syntax untrace;
    lvars lab1 lab2 list word;
    Get_traceargs(true) -> list;
    fast_for word in list do
        ;;; if dup(istraced(valof(word))) then
        ;;;     -> valof(word)
        ;;; else
        ;;;     ->
        ;;; endif
        sysNEW_LABEL() -> lab1;
        sysNEW_LABEL() -> lab2;
        sysPUSH(word);
        sysCALLQ(istraced);
        sysOR(lab1);
        sysGOTO(lab2);
        sysLABEL(lab1);
        sysPOP(word);
        sysLABEL(lab2);
    endfast_for;
    ;;; switch tracing off if no arguments given, otherwise on.
    if null(list) then
        false -> tracing
    elseunless tracing then
        true -> tracing
    endif
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  3 1993
        Moved initialisation of cuchartrace to charout.p (so other things
        that reference it won't pull in this file).
--- John Gibson, Jun 26 1991
        Changed is_v*ed_im_process to is_system_process
--- John Gibson, Sep 15 1989
        For tracing of process suspend/resume and abnormal exits, took
        out printing of args (for most purposes this is not useful --
        it just leads to lots of irrelevant junk coming out).
            Also, test for -v*edprocesschar- no longer needed because ved im
        uses new -consproc_to- which doesn't require abnormal exits.
--- John Williams, Jun 17 1988
        Added -pop_chain_trace- and -pop_process_trace- (cf BR johnw.118)
--- John Gibson, Feb 24 1988
        Weakref'ed process references. Put -systrace_proc- dlocal code
        in procedures.
--- John Williams, Dec  2 1987
        Added exit code to -systrace_proc-
        Introduced -popmaxtraceindent-
        Exported -istraced-
        Removed -Trace_arg_vectors- (at John Gibson's request)
--- John Gibson, Aug 16 1987
        Lconstant'ed, etc
 */
