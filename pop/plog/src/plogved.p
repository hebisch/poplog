/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/plog/src/plogved.p
 > Purpose:        Prolog: VED procedures
 > Author:         Robert Duncan & Simon Nichols, May 18 1987 (see revisions)
 */


section;

;;; weak declarations for VED identifiers
weak constant
    procedure ( vedchardown, vedcharup, veddo_in_subsystem, vedjumpto,
        vedtestsearch, vedmarkhi, vedmarklo, vedrepeater, vedscreenleft,
        vedtextright, vedthisline, vedcursorset, vedalignscreen, );

weak vars
    procedure ( vedveddefaults, vededitor, ),
    vedcolumn, vedline, ved_char_in_stream, vvedbuffersize, vvedlinesize,
;

endsection;


section prolog =>
    ved_prolog
    ved_source
;

constant
    procedure ( app_named_predicates, predicate_record, pred_isdefined, );

vars
    prolog_abort_on_syntax_errors,
;

;;; ========================================================================

lvars
    (last_fn, last_arity) = (false, false),
        ;;; last predicate spec. searched for by <ENTER> f etc.
;

lvars
    C, Line,
;

lconstant macro
    TEXT_IN_COL_1 =
        [(  VEDWEAK vvedlinesize /== 0
            and (fast_subscrs(1, VEDWEAK vedthisline() ->> Line) ->> C) /== ` `
            and C /== `\t` and C /== `%`
            and not(issubstring_lim(';;;', 1, 1, false, Line))
            and not(issubstring_lim('/*', 1, 1, false, Line))
            and not(issubstring_lim('*/', 1, 1, false, Line))
        )],
;

;;; split_spec:
;;;     splits a predicate specification (a string) into functor and arity
;;;     parts. Arity may be <false> if not given in the spec.

define lconstant split_spec(spec) -> (fn, arity);
    lvars spec, fn, arity = false;
    lvars len = datalength(spec);
    if len fi_>= 3 then
        lvars i;
        if (locchar_back(`/`, len, spec) ->> i)
        and i fi_> 1 and i fi_< len
        then
            consword(substring(1, i fi_- 1, spec)) -> fn;
            0 -> arity;
            until i == len do
                i fi_+ 1 -> i;
                lvars c = fast_subscrs(i, spec);
                if isnumbercode(c) then
                    arity * 10 + (c fi_- `0`) -> arity;
                else
                    false -> arity;
                    quitloop;
                endif;
            enduntil;
        endif;
    endif;
    unless arity then
        consword(spec) -> fn;
    endunless;
enddefine;

;;; join_spec:
;;;     rejoin a predicate specification for error messages etc.

define lconstant join_spec(fn, arity) -> spec;
    lvars fn, arity, spec;
    consstring(#|
        explode(fn);
        if arity then `/`, dest_characters(arity) endif;
    |#) -> spec;
enddefine;

;;; argument_spec:
;;;     interpret vedargument as a predicate specification

define lconstant argument_spec() -> (fn, arity);
    lvars fn, arity;
    unless VEDWEAK vedargument = nullstring then
        split_spec(VEDWEAK vedargument) -> (fn, arity);
    elseunless last_fn then
        VEDWEAK vederror('Nothing to search for');
    else
        (last_fn, last_arity) -> (fn, arity);
    endunless;
enddefine;

;;; prolog_vedreadterm:
;;;     reads a prolog term from a ved buffer, returning <false> if it
;;;     encounters a syntax error.
;;;     The cursor is left at an undefined postion on the last line of
;;;     the clause.

define lconstant prolog_vedreadterm() -> term;
    lvars term, saved_stacklength = stacklength();
    dlocal
        proglist_state = proglist_new_state(VEDWEAK vedrepeater),
        prolog_abort_on_syntax_errors = exitfrom(% false, prolog_readterm %),
    ;
    unless prolog_readterm() ->> term then
        ;;; a syntax error was encountered
        erasenum(stacklength() fi_- saved_stacklength);
    endunless;
    if VEDWEAK vedcolumn == 1 then VEDWEAK vedcharup() endif;
enddefine;

define lconstant prolog_mark(mode);
    lconstant macro
        START_ERROR =
            [VEDWEAK vederror('CAN\'T FIND START OF DEFINITION')]
    ;
    lvars
        fn, arity, term, loline, hiline, tmp, mode,
    ;
    dlocal
        VEDWEAK vedline, VEDWEAK vedcolumn, VEDWEAK vvedlinesize,
    ;

    ;;; Find a clause starting in column 1
    repeat
        until TEXT_IN_COL_1 do
            if VEDWEAK vedline == 1 then
                START_ERROR;
            else
                VEDWEAK vedcharup();
            endif;
        enduntil;
        VEDWEAK vedline -> loline;
        VEDWEAK vedscreenleft();
        unless prolog_vedreadterm() ->> term then START_ERROR endunless;
        prolog_termspec(prolog_head(term)) -> (fn, arity);
        quitunless (fn == "end_of_file" and arity == 0);
        if loline == 1 then
            START_ERROR;
        else
            VEDWEAK vedjumpto(loline fi_- 1, 1);
        endif;
    endrepeat;
    VEDWEAK vedline -> hiline;

    ;;; Find the first clause with functor name -fn- and arity -arity-.
    repeat
        VEDWEAK vedjumpto(loline, 1);
        repeat
            quitif (VEDWEAK vedline == 1)(2);
            VEDWEAK vedcharup();
            quitif (TEXT_IN_COL_1);
        endrepeat;
        VEDWEAK vedline -> tmp;
        VEDWEAK vedscreenleft();
        quitunless ((prolog_vedreadterm() ->> term)
        and prolog_checkspec(prolog_head(term), fn, arity));
        tmp -> loline;
    endrepeat;
    unless mode == "mep" then
        VEDWEAK vedjumpto(loline, 1); VEDWEAK vedmarklo();
    endunless;

    returnif(mode == "mbp");

    ;;; Find the last clause with functor name -fn- and arity -arity-.
    VEDWEAK vedjumpto(hiline, 1);
    repeat
        repeat
            quitif (VEDWEAK vedline == VEDWEAK vvedbuffersize)(2);
            VEDWEAK vedchardown();
            quitif (TEXT_IN_COL_1);
        endrepeat;
        VEDWEAK vedscreenleft();
        quitunless ((prolog_vedreadterm() ->> term)
        and prolog_checkspec(prolog_head(term), fn, arity));
        VEDWEAK vedline -> hiline;
    endrepeat;
    VEDWEAK vedjumpto(hiline, 1); VEDWEAK vedmarkhi();
enddefine;

define lconstant prolog_find(fn, arity);
    lvars fn, arity, fn_string = fast_word_string(fn);
    lvars foundline = 0, first_foundline = false, last_foundline = 0;
    lvars term, fn1, arity1;
    lvars clause_startline;
    dlocal VEDWEAK vedline, VEDWEAK vedcolumn, VEDWEAK vvedlinesize;

    repeat
        VEDWEAK vedtextright();
        returnunless(VEDWEAK vedtestsearch(fn_string, true))(false);
        if first_foundline then
            returnif(VEDWEAK vedline == first_foundline)(false);
        else
            VEDWEAK vedline -> first_foundline;
        endif;
        foundline -> last_foundline; VEDWEAK vedline -> foundline;
        until TEXT_IN_COL_1 do
            if VEDWEAK vedline == 1 or VEDWEAK vedline == last_foundline then
                VEDWEAK vedjumpto(foundline, 1);
                nextloop(2);
            else
                VEDWEAK vedcharup();
            endif;
        enduntil;
        VEDWEAK vedline -> clause_startline;
        VEDWEAK vedscreenleft();
        if (prolog_vedreadterm() ->> term) then
            prolog_termspec(prolog_head(term)) -> arity1 -> fn1;
            if arity and fn1 == fn and arity1 == arity
            or not(arity) and issubstring_lim(fn, 1, 1, false, fn1)
            then
                quitloop;
            endif;
        endif;
        if foundline fi_> VEDWEAK vedline then
            VEDWEAK vedjumpto(foundline, 1);
        elseif first_foundline fi_> foundline
        and first_foundline fi_<= VEDWEAK vedline
        then
            return(false);
        endif;
    endrepeat;

    VEDWEAK vedjumpto(clause_startline, 1);

    ;;; Find the first clause of this predicate.
    repeat
        repeat
            quitif (VEDWEAK vedline == 1)(2);
            VEDWEAK vedcharup();
            quitif (TEXT_IN_COL_1);
        endrepeat;
        lvars tmp = VEDWEAK vedline;
        VEDWEAK vedscreenleft();
        quitunless ((prolog_vedreadterm() ->> term)
        and prolog_checkspec(prolog_head(term), fn1, arity1));
        VEDWEAK vedjumpto(tmp ->> clause_startline, 1);
    endrepeat;

    return(clause_startline);
enddefine;

;;; search_for:
;;;     search for the first line of the definition of predicate fn/arity
;;;     and move the cursor to it or raise an error if not found

define lconstant search_for(fn, arity);
    lvars fn, arity;
    ;;; remember for next time
    (fn, arity) -> (last_fn, last_arity);
    lvars line = prolog_find(fn, arity);
    if line then
        VEDWEAK vedjumpto(line, 1);
        unless VEDWEAK vedcursorset() then VEDWEAK vedalignscreen() endunless;
    else
        VEDWEAK vederror('Can\'t find: ' <> join_spec(fn, arity));
    endif;
enddefine;

;;; enter_f:
;;;     version of <ENTER> f for Prolog

define lconstant enter_f() with_props ved_f;
    search_for(argument_spec());
enddefine;

;;; ved_source:
;;;     edit the definition of a predicate

define vars ved_source;

    returnunless(VEDLOADED);

    define lconstant source(fn, arity) -> (fn, arity, file);
        lvars fn, arity, file, pred;
        unless arity then
            ;;; find the named predicate with the lowest arity
            app_named_predicates(fn,
                procedure(pred);
                    lvars pred;
                    unless arity and arity < pred_arity(pred) then
                        pred_arity(pred) -> arity;
                    endunless;
                endprocedure);
        endunless;
        unless arity
        and (predicate_record(fn, arity, false) ->> pred)
        and pred_isdefined(pred)
        then
            VEDWEAK vederror('No such predicate: ' <> join_spec(fn, arity));
        elseunless isref(pred_tag(pred) ->> file)
        and isstring(cont(file) ->> file)
        then
            VEDWEAK vederror('No source file for predicate: ' <> join_spec(fn, arity));
        endunless;
    enddefine;

    lvars (fn, arity, file) = source(argument_spec());
    search_for(% fn, arity %) :: VEDWEAK ved_char_in_stream
        -> VEDWEAK ved_char_in_stream;
    VEDWEAK vededitor(VEDWEAK vedveddefaults, file);
enddefine;


;;; ved_prolog:
;;;     run a VED command in the Prolog subsystem

define global vars ved_prolog();
    returnunless(VEDLOADED);
    if VEDWEAK vedargument = nullstring then
        VEDWEAK veddo_in_subsystem("top");
    else
        VEDWEAK veddo_in_subsystem("prolog");
    endif;
enddefine;

;;; ved_pophelp:
;;;     get Pop11 help from Prolog

define vars ved_pophelp();
    returnunless(VEDLOADED);
    'help ' sys_>< VEDWEAK vedargument -> VEDWEAK vedargument;
    VEDWEAK veddo_in_subsystem("pop11");
enddefine;


;;; ved_spy:
;;; ved_nospy:
;;;     add or remove spy-points from VED

define lconstant vedspy(goal);  ;;; goal is "spy" or "nospy"
    lvars goal, arg;
    returnunless(VEDLOADED);
    unless VEDWEAK vedargument = nullstring
    or VEDWEAK vedargument = '.'
    then
        procedure;
            dlocal proglist_state;
            proglist_new_state(stringin(VEDWEAK vedargument <> ' .'))
                -> proglist_state;
            prolog_readterm();
        endprocedure() -> arg;
        prolog_maketerm(arg, goal, 1) -> goal;
    endunless;
    prolog_invoke(goal) -> ;
enddefine;

define vars ved_spy =
    vedspy(% "spy" %);
enddefine;

define vars ved_nospy =
    vedspy(% "nospy" %);
enddefine;

;;; prolog_init_ved:
;;;     install Prolog VED procedures

define prolog_init_ved();
    returnunless(VEDLOADED);
    prolog_mark(% "mbp" %)
        ->> subsystem_valof("ved_mbp", "top")
        ->  subsystem_valof("ved_mbp", "prolog");
    prolog_mark(% "mep" %)
        ->> subsystem_valof("ved_mep", "top")
        ->  subsystem_valof("ved_mep", "prolog");
    prolog_mark(% "mcp" %)
        ->> subsystem_valof("ved_mcp", "top")
        ->  subsystem_valof("ved_mcp", "prolog");
    enter_f
        ->> subsystem_valof("ved_f", "top")
        ->  subsystem_valof("ved_f", "prolog");
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  4 1993
        Added ved_source command
--- Robert John Duncan, Jul 15 1993
        Weakened references to veddo_in_subsystem.
--- Robert John Duncan, May 18 1992
        Made compileable in a system without VED.
--- Robert Duncan, Jan 28 1992
        Added -ved_spy- and -ved_nospy-
--- Simon Nichols, Oct 31 1990
        Changed -ved_prolog- to pass "prolog" as an argument to
        -veddo_in_subsystem- if -vedargument- is not -nullstring-.
--- Simon Nichols, Jul 26 1990
        Changed -prolog_vedreadterm- to work with Edinburgh syntax library.
--- Simon Nichols, Jul 17 1990
        Rewritten for use with new subsystem facilities.
--- Rob Duncan, Jun  6 1990
        Moved in documentation search procedures from "compile.p".
        Fixed -ved_pophelp-
--- Rob Duncan, Aug  8 1989
        Sectionised and added #_INCLUDEs for POPC; moved redefinitions of
        VED procedures into a separate initialisation procedure.
--- Rob Duncan, Aug 31 1988
        Replaced -vednullstring- with -nullstring-
 */
