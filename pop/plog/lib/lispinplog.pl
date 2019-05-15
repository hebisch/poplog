/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/plog/lib/lispinplog.pl
 > Purpose:        Access to Lisp from Prolog
 > Author:         John Williams, Apr 30 1987 (see revisions)
 > Documentation:  HELP * LISPINPLOG
 > Related Files:
 */

:- prolog_language(pop11).

section;

uses clisp;

global constant procedure (
    prolog_lisp_mv_apply
    prolog_lisp_mv_eval
    prolog_lisp_apply
    prolog_lisp_eval
    prolog_lisp_symbol
    );

endsection;


section $-lisp;

define prolog_lisp_mv_apply(pdr, args, result, contn);
    lvars args contn pdr result;
    dlocal break_on_errors = nil;
    prolog_unifyc(result,
        [% funcall(prolog_deref(pdr), prolog_full_deref(args)) %],
        contn)
enddefine;


define prolog_lisp_mv_eval(expr, result, contn);
    lvars expr result contn;
    dlocal break_on_errors = nil;
    prolog_unifyc(result, [% eval(prolog_full_deref(expr)) %], contn)
enddefine;


define prolog_lisp_apply(pdr, args, result, contn);
    lvars args contn pdr result sl;
    dlocal break_on_errors = nil;
    stacklength() -> sl;
    funcall(prolog_deref(pdr), prolog_full_deref(args));
    setstacklength(sl fi_+ 1);
    -> sl;
    prolog_unifyc(result, sl, contn)
enddefine;


define prolog_lisp_eval(expr, result, contn);
    lvars expr result contn;
    dlocal break_on_errors = nil;
    prolog_unifyc(
        result,
        eval([% @VALUES, prolog_full_deref(expr) %]),
        contn)
enddefine;


define prolog_lisp_symbol(Word, Sym, contn);
    lvars Word Sym contn word sym;
    prolog_deref(Word) -> word;
    prolog_deref(Sym) -> sym;
    if isword(word) then
        prolog_unifyc(string_to_sym(word sys_>< ''), sym, contn);
    elseif issymbol(sym) then
        prolog_unifyc(word, consword(symbol_string(sym)), contn);
    endif
enddefine;


switch_lisp_to(% "prolog" %) -> magic_word_handler(@PROLOG);

lisp_export(magic_word_handler(@PROLOG), @PROLOG, [0 0 0]);


endsection;


section;

applist(['lisp_mv_apply/3'
         'lisp_mv_eval/2'
         'lisp_apply/3'
         'lisp_eval/2'
         'lisp_symbol/2'
        ],
        procedure(s);
            lvars s;
            consword(s) -> s;
            sysSYNTAX(s, "procedure", true);
            sysGLOBAL(s);
        endprocedure);


prolog_lisp_mv_apply    -> prolog_valof("lisp_mv_apply", 3);
prolog_lisp_mv_eval     -> prolog_valof("lisp_mv_eval", 2);
prolog_lisp_apply       -> prolog_valof("lisp_apply", 3);
prolog_lisp_eval        -> prolog_valof("lisp_eval", 2);
prolog_lisp_symbol      -> prolog_valof("lisp_symbol", 2);


nonmac lisp -> prolog_command("LISP");

nonmac @ -> prolog_macro("@");

5 -> item_chartype(`@`, $-prolog$-prolog_standard_repeater);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul 18 1990
        Revised for new LIB SUBSYSTEM
--- John Williams, Mar  4 1988
        Defines macro LISP
--- John Williams, Sep  3 1987
        Now declares the Prolog predicates global (see BR johnw.69)
 */
