/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_exceptions.p
 > Purpose:         Prolog: exception handler
 > Author:          Robert Duncan, Apr 27 1993 (see revisions)
 */


section prolog;

constant
    procedure ( prolog_printq, prolog_writeq, predicate_isdefined, ),
;

;;; ========================================================================

vars prolog_exceptions = false;

;;; is_predicate_name:
;;;     tests (roughly) whether -name- is a word of the form fn/arity.

define lconstant is_predicate_name(name);
    lvars name, posn;
    isword(name)
    and (locchar(`/`, 1, name) ->> posn)
    and posn fi_< datalength(name)
    and isnumbercode(fast_subscrw(posn fi_+ 1, name));
enddefine;

;;; print_culprit:
;;;     prints an item in the INVOLVING line of a mishap message:
;;;     uses printq/1 to start with, but switches to writeq/1 if that
;;;     generates further errors,

define lvars print_culprit(item);
    lvars   item;
    dlocal  print_culprit = prolog_writeq;
    prolog_printq(item);
enddefine;

;;; prolog_backtrace:
;;;     prints the DOING part of a mishap message, when popsyscall is
;;;     <false>

define lconstant prolog_backtrace();
    dlocal cucharout = cucharerr, pop_pr_quotes = false, poplineprefix;

    ;;; adapted from the version in errors.p
    define lconstant Conceal_call(props);
        lconstant
            conceal_names =
                [mishap setpop converter_to_contn error\/2 \;\/2],
            conceal_prefixes =
                ['sys' 'ved' 'wved' 'xved' 'pop11_' 'prolog_' 'subsys'];
        lvars props, prefix, l;
        if isword(props) then
            returnif(fast_lmember(props, conceal_names))(true);
            fast_for prefix in conceal_prefixes do
                returnif(isstartstring(prefix, props))(true);
            endfor;
        elseif isstring(props) then
            ;;; for system procedures whose pdprops are a string of the form
            ;;; '( pathname )' for identification by popc
            returnif( (datalength(props) ->> l) fi_> 2
                        and props(1) == `(` and props(l) == `)`) (true)
        endif;
        false;
    enddefine;

    returnif(pop_mishap_doing_lim == 0);
    #_< conspair(';;;\s', false) >_# -> poplineprefix;
    printf('DOING\s\s\s\s:\s');
    lvars pdr, i = 1, nprinted = 0, last_pdr = false, count = 1;
    lvars print_all = true;
    while (caller(i) ->> pdr) and nprinted /== pop_mishap_doing_lim do
        lvars props = recursive_front(pdprops(pdr));
        if props and not(Conceal_call(props))
        and (print_all or is_predicate_name(props))
        then
            if pdr == last_pdr then
                count fi_+ 1 -> count;
            else
                pdr -> last_pdr;
                unless count == 1 then
                    cucharout(`(`);
                    cucharout(`*`);
                    sys_syspr(count);
                    cucharout(`)`);
                    1 -> count;
                endunless;
                cucharout(`\s`);
                pr(props);
                nprinted fi_+ 1 -> nprinted;
                if print_all and is_predicate_name(props) then
                    ;;; print only predicate names after the first
                    false -> print_all;
                endif;
            endif;
        endif;
        i fi_+ 1 -> i;
    endwhile;
    if pdr and nprinted /== 0 then printf('\s...') endif;
    cucharout(`\n`);
enddefine;

;;; prolog_pr_exception:
;;;     print an error occurring in Prolog

define prolog_pr_exception(n, msg, idstring, severity);

    lvars is_error = (severity == `E` or severity == `R`);

    ;;; this indicates whether we do our own backtrace printing
    lvars backtrace = (is_error or pop_message_min_detail > 4)
                      and not(popsyscall);

    ;;; check for non-default header string
    lvars header = false;
    if isvector(msg) and datalength(msg) >= 2 and isstring(msg(2)) then
        msg(2) -> header;
    elseif is_error then
        'PROLOG ERROR -' -> header;
    endif;

    ;;; check for non-default detail level
    lvars detail = false;
    if isvector(msg) and datalength(msg) >= 2 and isinteger(last(msg)) then
        last(msg) -> detail;
        if (detail && 16:F) > 4 and not(popsyscall) then
            (detail &&~~ 16:F) || 4 -> detail;
            true -> backtrace;
        endif;
    elseif backtrace then
        16:14 -> detail;
    endif;

    ;;; convert message
    if isvector(msg) then
        msg(1) -> msg;
    elseunless isstring(msg) then
        ;;; presumably something from Prolog: change to a printf string
        lvars args = consvector(n);
        msg, explode(args), n + 1, '%%p' -> (n, msg);
    endif;
    if header or detail then
        {% msg, if header then header endif, if detail then detail endif %}
            -> msg;
    endif;

    ;;; redefine pr to print culprits using printq/1 but everything else
    ;;; with write/1; relies on sys_pr_message setting pop_pr_quotes
    ;;; true only while printing culprits
    define dlocal pr(item);
        if pop_pr_quotes then
            print_culprit(item);
        else
            prolog_write(item);
        endif;
    enddefine;

    ;;; print it
    if backtrace and pop_message_min_detail > 4 then
        dlocal pop_message_min_detail = 4;
    endif;
    sys_pr_message(n, msg, idstring, severity);
    if backtrace then
        chain(prolog_backtrace);
    endif;
enddefine;

;;; prolog_error_handler:
;;;     calls prolog_error/2 with error handling disabled

define lconstant prolog_error_handler(message, culprits);
    lvars contn =
        procedure(prolog_exceptions);
            dlocal prolog_exceptions;
            prolog_apply_continuation();
        endprocedure(%prolog_exceptions%);
    dlocal prolog_exceptions = false;
    prolog_valof("prolog_error", 2)(message, culprits, contn);
enddefine;

;;; prolog_exception_final:
;;;     calls -prolog_error/2- if there's a matching clause for this
;;;     exception

define prolog_exception_final(n, msg, idstring, severity);
    if severity == `E` or severity == `R` then
        if prolog_exceptions and predicate_isdefined("clause", 2) then
            lvars i = 1, props, last_pred;
            while caller(i) ->> last_pred do
                recursive_front(pdprops(last_pred)) -> props;
                quitif(is_predicate_name(props));
                i fi_+ 1 -> i;
            endwhile;
            if last_pred then
                ;;; see if there's a matching clause for prolog_error/2:
                ;;; if there is, chain back to -last_pred- and call it
                if isvector(msg) then msg(1) -> msg endif;
                if isstring(msg) then consword(msg) -> msg endif;
                lvars args = conslist(n);
                prolog_valof("clause", 2)(
                    prolog_maketerm(msg, args, "prolog_error", 2),
                    prolog_newvar(),
                    chainto(%msg, args, last_pred, prolog_error_handler%),
                );
                ;;; no matching clause -- re-stack args
                dl(args);
            endif;
        endif;
    endif;
    ;;; no handler
    false;
enddefine;

;;; prolog_mishap:
;;;     direct call to mishap, bypassing Prolog error handling

define prolog_mishap(msg, culprits);
    dlocal prolog_exceptions = false;
    mishap(msg, culprits);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 13 1996
        Renamed from "prolog_prm*ishap.p" with changes for new exception
        handling
 */
