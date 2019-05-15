/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:        C.all/plog/src/write.p
 > Purpose:     Prolog: writing terms and listing clauses
 > Author:      Robert Duncan & Simon Nichols, February 1987 (see revisions)
 */

section prolog;

constant
    procedure ( prolog_standard_repeater, postfix_prec, postfix_lprec,
        prefix_prec, prefix_rprec, infix_prec, infix_lprec, infix_rprec,
        isprolog_variable_tok, prolog_var_no, ),
;

weak constant
    procedure ( prolog_portray, predicate_has_clauses, $-isveddevice, ),
;

weak vars
    $-vedindentstep,
;

;;; =======================================================================


lvars
    displaying  = false,    ;;; <true> if invoked by display
    quoting     = false,    ;;; <true> if invoked by writeq
    portraying  = false,    ;;; <true> if invoked by print
    linemax     = 0,        ;;; maximum line width if printing to charout
    commaprec,              ;;; Precedence of comma operator
;

;;; is_alphanumeric_atom:
;;;     <true> if -a- is valid as an unquoted alphanumeric atom.

define lconstant is_alphanumeric_atom(a);
    lvars a, i, c, len;
    returnunless(isword(a))(false);
    datalength(a) -> len;
    returnunless(len fi_> 0 and islowercode(Subscrw(1, a)))(false);
    For i from 2 to len do
        Subscrw(i, a) -> c;
        returnunless(isalphacode(c) or isnumbercode(c) or c == `_`)(false);
    endfor;
    true;
enddefine;

;;; is_symbolic_atom:
;;;     <true> if -a- is valid as an unquoted symbolic atom.

define lconstant is_symbolic_atom(a);
    lconstant special_atoms = [; ! %"'{}'"%];
    lvars a, i, len, ctype, last_ctype;

    returnunless(isword(a) and (datalength(a) ->> len) fi_> 0)(false);

    returnif(fast_lmember(a, special_atoms))(true);

    ;;; Test whether the atom consists of sign characters (type 3, and
    ;;; types 10 and 11 provided that they are not juxtaposed).
    false -> last_ctype;
    For i to len do
        item_chartype(Subscrw(i, a), prolog_standard_repeater) -> ctype;
        if (ctype == 11 and last_ctype == 10)               ;;;  */
        or (ctype == 10 and last_ctype == 11)               ;;;  /*
        or (ctype /== 3 and ctype /== 10 and ctype /== 11)
        then
            return(false);
        endif;
        ctype -> last_ctype;
    endfor;
    true;
enddefine;

;;; needsquotes:
;;;     <true> if the atom -a- needs to be quoted.

define lconstant needsquotes(a);
    lvars a;
    not(is_alphanumeric_atom(a) or is_symbolic_atom(a)) or prolog_macro(a);
enddefine;

;;; writechar:
;;;     print a character without causing a line-break (poplinemax
;;;     should have been reduced by 1 so that a single call of this
;;;     won't make the line too long)

define lconstant writechar() with_nargs 1;
    dlocal poplinewidth = false;
    cucharout();
enddefine;

;;; writespace:
;;;     print a space, but if it would cause a line break, do the break
;;;     explicitly and suppress the space at the start of the next line

define lconstant writespace();
    if cucharout == charout
    and isinteger(poplinewidth)
    and pop_charout_col fi_>= poplinewidth
    then
        charout(`\n`);
        charout(`\t`);
    else
        cucharout(`\s`);
    endif;
enddefine;

;;; writevar:
;;;     print variable number n

define lconstant writevar(n);
    lvars n;

    define lconstant char_count(item) -> n;
        lvars item;
        dlocal cucharout = identfn;
        lvars n = (#| `_`, syspr(item) |#);
        erasenum(n);
    enddefine;

    if cucharout == charout
    and isinteger(poplinewidth)
    and pop_charout_col + char_count(n) fi_> poplinemax
    then
        ;;; output will break, so do it now
        charout(`\n`);
        charout(`\t`);
    endif;
    cucharout(`_`);
    syspr(n);
enddefine;

;;; writeatom:
;;;     prints an atomic item, adding quotes if required

define lconstant writeatom(item);
    lvars item;

    ;;; no special treatment for non-Prolog atoms
    unless isword(item) then
        syspr(item);
        return;
    endunless;

    define lconstant is_special_char =
        newproperty([
            [`\n`   '\\n']
            [`\r`   '\\r']
            [`\b`   '\\b']
            [`\t`   '\\t']
            [`'`    '\\\'']
            [`\^?`  '\\^?']
            [`\\`   '\\\\']],
        10, false, true);
    enddefine;

    define lconstant put_char(c);
        lvars c, s;
        if is_special_char(c) ->> s then
            appdata(s, cucharout);
        elseif c fi_> 127 then
            cucharout(`\\`), cucharout(`(`), syspr(c), cucharout(`)`);
        elseif c fi_< `\s` then
            cucharout(`\\`), cucharout(`^`), cucharout(c fi_+ 64);
        else
            cucharout(c);
        endif;
    enddefine;

    define lconstant char_count(n, c) -> n;
        lvars n, c, s;
        if is_special_char(c) ->> s then
            datalength(s)
        elseif c fi_> 127 then
            6
        elseif c fi_< `\s` then
            3
        else
            1
        endif fi_+ n -> n;
    enddefine;

    lvars quote = quoting and needsquotes(item);

    if cucharout == charout
    and isinteger(poplinewidth)
    then
        ;;; try to avoid unpleasant line-breaks in the middle of atoms
        lvars tabstop;
        if VEDLOADED and VEDWEAK isveddevice(pop_charout_device) then
            VEDWEAK vedindentstep
        else
            8
        endif -> tabstop;
        lvars len;
        if quote then
            appdata(2, item, char_count)
        else
            datalength(item)
        endif -> len;
        if pop_charout_col fi_> tabstop fi_+ 1  ;;; something useful printed
        and pop_charout_col fi_+ len fi_> poplinemax
        then
            ;;; item will break, so do it now
            charout(`\n`);
            charout(`\t`);
        endif;
    endif;

    if quote then
        cucharout(`'`), appdata(item, put_char), cucharout(`'`);
    else
        appdata(item, cucharout);
    endif;
enddefine;

;;; needsbrackets:
;;;     <true> if the current term has to be bracketed.

define lconstant needsbrackets(prec, prevprec, isarg);
    lvars prec, prevprec, isarg;
    prec fi_> prevprec or isarg and prec fi_>= commaprec;
enddefine;

;;; writeop:
;;;     writes a complex term as an operator expression if possible, and
;;;     returns a boolean to indicate whether it did or not.

lconstant procedure writeprec;  ;;; forward

define lconstant writeop(term, n, p, prevprec, isarg);
    lvars term, n, p, prevprec, isarg, prec, bracketed;

    if n == 1 and (postfix_prec(p) ->> prec) < MAXPREC then
        returnif(quoting and needsquotes(p))(false);
        if (needsbrackets(prec, prevprec, isarg) ->> bracketed) then
            cucharout(`(`);
        endif;
        writeprec(prolog_arg(1, term), postfix_lprec(p), false);
        writespace(); writeatom(p);
        if bracketed then cucharout(`)`) endif;

    elseif n == 1 and (prefix_prec(p) ->> prec) < MAXPREC then
        returnif(quoting and needsquotes(p))(false);
        if (needsbrackets(prec, prevprec, isarg) ->> bracketed) then
            cucharout(`(`);
        endif;
        writeatom(p); writespace();
        writeprec(prolog_arg(1, term), prefix_rprec(p), false);
        if bracketed then cucharout(`)`) endif;

    elseif n == 2 and (infix_prec(p) ->> prec) < MAXPREC then
        returnif(quoting and needsquotes(p) and p /== ",")(false);
        if (needsbrackets(prec, prevprec, isarg) ->> bracketed) then
            cucharout(`(`);
        endif;
        writeprec(prolog_arg(1, term), infix_lprec(p), false);
        if p == "." then
            cucharout(`.`);
        elseif p == "," then
            writechar(`,`); writespace();
        else
            writespace(); writeatom(p); writespace();
        endif;
        writeprec(prolog_arg(2, term), infix_rprec(p), false);
        if bracketed then cucharout(`)`) endif;

    elseif n == 1 and p == "'{}'" then
        cucharout(`{`);
        writeprec(prolog_arg(1, term), MAXPREC, false);
        cucharout(`}`);

    else
        ;;; Not an operator.
        return(false);
    endif;

    true;
enddefine;

;;; writeprec:
;;;     writes a prolog term with a given precedence. If -isarg- is true,
;;;     the term is an argument to a compound term.

define lconstant writeprec(term, prec, isarg);
    lvars term, prec, isarg, n, p, i;

    if isprologvar(term) then
        writevar(prolog_var_number(term));

    elseif isprolog_variable_tok(term) then
        writevar(prolog_var_no(term));

    elseif portraying and weakref prolog_portray(term) then
        return;

    elseif ispair(term) then
        cucharout(`[`);
        term -> p;
        repeat
            writeprec(prolog_args(p) -> p, MAXPREC, true);
        quitunless (ispair(p));
            writechar(`,`); writespace();
        endrepeat;
        unless p == [] then
            writespace(); writechar(`|`); writespace();
            writeprec(p, MAXPREC, true);
        endunless;
        cucharout(`]`);

    elseif isprologterm(term) then
        prolog_termspec(term) -> n -> p;
        if p == "'$VAR'" and n == 1 then
            writevar(prolog_arg(1, term));
        elseif displaying or not(writeop(term, n, p, prec, isarg)) then
            writeatom(p); writechar(`(`);
            writeprec(prolog_arg(1, term), MAXPREC, true);
            for i from 2 to n do
                writechar(`,`); writespace();
                writeprec(prolog_arg(i, term), MAXPREC, true);
            endfor;
            cucharout(`)`);
        endif;

    else
        writeatom(term);
    endif;
enddefine;

define lconstant write(/* term, */ displaying, portraying, quoting)
with_nargs 4;
    dlocal
        displaying, portraying, quoting, linemax,
        commaprec = infix_prec(","),
        pop_pr_quotes = false,
        pop_pr_ratios = false,
        poplinemax,
    ;
    if isinteger(poplinemax) then
        ;;; reduce poplinemax to improve output
        ;;; NB: setting local linemax as well guards against recursive
        ;;; calls invoked via portray
        fi_max(linemax, poplinemax fi_- 1) ->> linemax -> poplinemax;
    endif;
    if portraying then
        ;;; disable portray if there's no definition for it
        testdef prolog_portray
        and weakref[prolog_portray] predicate_has_clauses("portray", 1)
            -> portraying;
    endif;
    writeprec(/* term, */ MAXPREC, false);
enddefine;


;;; prolog_write:
;;;     prints a prolog term in default style

define prolog_write(/* term */) with_nargs 1;
    write(/* term, */ false, false, false);
enddefine;

;;; prolog_writeq:
;;;     prints a prolog term with atoms quoted

define prolog_writeq(/* term */) with_nargs 1;
    write(/* term, */ false, false, true);
enddefine;

;;; prolog_display:
;;;     prints a prolog term in prefix format and with atoms quoted

define prolog_display(/* term */) with_nargs 1;
    write(/* term, */ true, false, true);
enddefine;

;;; prolog_print:
;;;     prints a prolog term trying portray/1 at each level.
;;;     If there is no suitable portray clause, behaves like write/1.

define prolog_print(/* term */) with_nargs 1;
    write(/* term, */ false, true, false);
enddefine;

;;; prolog_printq:
;;;     prints a prolog term trying portray/1 at each level.
;;;     If there is no suitable portray clause, behaves like writeq/1.

define prolog_printq(/* term */) with_nargs 1;
    write(/* term, */ false, true, true);
enddefine;


;;; prolog_list:
;;;     prints a clause in listing format

define prolog_list(/* Clause */) with_nargs 1;
    lvars   Clause = prolog_deref(/* Clause */), term, prevprec, indent;
    dlocal  displaying = false, portraying = false, quoting = true, linemax,
            commaprec = infix_lprec(","),
            pop_pr_quotes = false, pop_pr_ratios = false, poplinemax;

    if isinteger(poplinemax) then
        fi_max(linemax, poplinemax fi_- 1) ->> linemax -> poplinemax;
    endif;

    if VEDLOADED then VEDWEAK vedindentstep else 4 endif -> indent;

    ;;; Print the clause head
    writeprec(prolog_head(Clause), infix_lprec(":-"), false);

    ;;; Print the body (if there is one)
    prolog_body(Clause) -> term;
    unless term == "true" then
        writespace(); writeatom(":-"); cucharout(`\n`);
        infix_rprec(":-") -> prevprec;
        ;;; Each conjunct goes on a separate line
        while prolog_checkspec(term, ",", 2) do
            fast_repeat indent times cucharout(`\s`) endrepeat;
            writeprec(prolog_arg(1, term), commaprec, false);
            writechar(`,`); cucharout(`\n`);
            prolog_arg(2, term) -> term;
            commaprec -> prevprec;
        endwhile;
        fast_repeat indent times cucharout(`\s`) endrepeat;
        writeprec(term, prevprec, false);
    endunless;

    writechar(`.`); cucharout(`\n`);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  3 1993
        Modified to prevent atoms being split across line-breaks when
        writing to charout.
--- Robert John Duncan, Jul 15 1993
        Weakened references to portray/1 (now defined elsewhere).
--- Robert John Duncan, May 15 1992
        Made compileable in a system without VED.
--- Robert John Duncan, Jul  6 1991
        Changed -portray- *not* to use -prolog_base*invoke-: it must use
        lower-level facilities to allow proper resetting of any variable
        bindings on backtracking.
--- Robert John Duncan, Jan 25 1991
        Changed -portray- to use -prolog_base*invoke-: this should only be
        called from inside Prolog.
--- Simon Nichols, Oct 24 1990
        Added -prolog_printq- to support the use of print/1 in printing
        culprits in error messages.
        Changed the printing procedures to ensure that the print control
        flags -displaying-, -portraying- and -quoting- are all localised
        and set appropriately.
--- Simon Nichols, Oct 17 1990
        Numerous changes and additions to support a correct implementation
        of print/1.
--- Simon Nichols, Mar  9 1990
    Changed -needsquotes- to return <true> if its argument is a prolog
    macro, ensuring that atoms such as 'help' are quoted by -prolog_writeq-.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - made all atom printing go through -writeatom-, so removing the
        need for localising pr = quotedpr;
    - changed -listclause- to -prolog_list-, and moved the other
        definitions for -listing- out to "dbpreds.p";
    - localised -pop_pr_ratios- inside -prolog_write- instead of having
        it set globally;
    - tidied up.
--- Rob Duncan, Mar 16 1988
    Renamed from plogwrite.p
--- Robert Duncan, Aug 18 1987
    fixed a bug reported by UMIST where the predicate 'listing' was not
    printing quoted atoms correctly. This required: changing -prolog_list-
    to dlocal -pr- to be -quotedpr-; changing -writeop- to treat conjunctions
    specially and making -issymbolic_atom- refer to -prolog_standard_repeater-
    when checking the -item_chartype- of characters.
 */
