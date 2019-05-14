/* --- Copyright University of Birmingham 2000. All rights reserved. ------
 > File:            $poplocal/local/auto/doesmatch.p
 > File:            $poplocal/local/newkit/prb/auto/doesmatch.p
 > Purpose:         Full implementation of a matcher, with state saving
 >                  Also works with sections and dlvars
 >                  (Based on FMATCHES, and John Gibson's new "equals")
 > Author:          Aaron Sloman, Nov 26 1995 (see revisions)
 >              (based partly on work by Jon Cunningham and Tom Khabaza)
 > Documentation: HELP * DOESMATCH, HELP * FMATCHES
 > Related Files: LIB * READPATTERN, LIB * FMATCHES, SRC * SYSMATCH.P
 > Poplog Version 15.02 includes LIB * EQUALS Dec 21 1995
 */


/*
LIB * READPATTERN defines a new syntax word "!" which can precede a list
expression and has the effect of replacing its pattern variables, and
words following the restriction colon ":", with identifiers that are active
when the code is compiled.

*/



/*
LIB DOESMATCH

This version of the matcher, is based loosely on a combination
of ideas from:

(a) LIB FMATCHES (which originally came from work done by Jonathan
    Cunningham for the matcher in Alphapop), and
(b) the new LIB EQUALS by John Gibson which reduces GC overhead by
    (1) using the Pop-11 stack for continuations, and (2) reclaiming
    temporary lists as much as possible.

The new LIB EQUALS is based on the use of a new "matchvar" facility
not yet available in distributed versions of Poplog. It will not work
with the `old' pattern syntax, as it uses "=*" "=**" "=?" and "=??"
in place of "=" "==" "?" and "??". Lib doesmatch preserves the old
syntax, i.e.

    =               match one item
    ==              match an arbitrary number of items
    ?<variable>     match one item and bind it to the variable
    ??<variable>    match any number of items (possibly none), make
                    a list of them, and bind the list (possibly [])
                    to the variable.
        The last two can be followed by a restriction preceded by a
        colon, the restriction being a number or a procedure. If the
        restriction procedure returns something other than <true>
        then the result is assigned as the value of the variable,
        rather than the corresponding portion(s) of the datum.

For further details see HELP * DOESMATCH

Implementation:

The continuations all consist of three items on the stack, the next
datum, the next pattern, and a stack-pointer (result of stacklength)
to the top of the previous continuation.

The global lvars variable contn_top is either false, if there are no
continuations, or else an integer recording stacklength after the
last continuation was stacked.

Continuations are stacked essentially only in one case, namely where the
matcher is about to recurse on datum and pattern elements that are lists.
In that case the tail of the datum and of the pattern are stored so that
matching can continue with them later if matching on the two current
elements is successful. This whenever the matching of a datum and a pattern
has been successfully completed it is simply necessary to check whether
contn_top is non-false, and if so restore the last saved continuation.

When dealing with segment pattern elements a different kind of continuation
is stored which is used for restoring the state between failed attempts
at matching different partial segments. This also uses the stack.

This version, unlike John Gibson's, always completes all continuations
inside sub_doesmatch. so the procedures matches and newsysmatch
never need to run whereclause.

There is probably some simplification still possible.

John Gibson's version of equals has to interact with the generalisation
of the pop-11 system operator "=" which handles matchvars, which can
be included in arbitrary data structures.

*/

compile_mode: pop11 +strict;

section;

applist([doesmatch sub_doesmatch newsysmatch], sysunprotect);

global vars popmatchvars , pop_match_vals;

    ;;; Global variables for sub_doesmatch
lvars
    save_contn_len,
    contn_top,
    procedure whereclause,
;



    /*  This does the main matching of datum against pattern.
        It uses the procedure whereclause nonlocally to test each
        match.
    */

;;;;define lconstant sub_doesmatch(datum, pattern);
define vars procedure sub_doesmatch(datum, pattern);

    lvars
        datum, pattern,
        next_p, id,         ;;; next pattern, and variable id
        x, n, l,            ;;; some temporary variables
        restr,              ;;; false or a restriction (integer or procedure)
        save_top,           ;;; saved contn_top
        save_matchvars, save_conv,  ;;; saved popmatchvars and
                                    ;;; pop_match_vals
        ;

    dlocal  save_contn_len;

    define :inline lconstant FAIL_IFNOT(expr);
        unless expr then goto FAIL endunless
    enddefine;

    define :inline lconstant TRY_CONTINUATION();
        if contn_top then
            ;;; more matching to do
            goto APPLY_CONTINUATION
            ;;; otherwise finished, so run the check procedure
        elseif whereclause() then
            ;;; success.
            return(true)
        else
            ;;; No good. Exit, possibly back to a choice point.
            goto FAIL
        endif;
    enddefine;


    define :inline lconstant SAVE();
        ;;; use before exploring non-deterministic segment matches
        stacklength() -> save_contn_len;    ;;; choice point
        contn_top -> save_top;
    enddefine;

    define :inline lconstant RESTORE();
        ;;; Get rid of any continuations added while exploring
        ;;; segment matches
        save_top -> contn_top;
        erasenum(stacklength() fi_- save_contn_len);
    enddefine;

    define :inline lconstant STACK_NEXT();
        ;;; create continuation including next datum and next pattern
        ;;; and stacklength of previous continuation
        fast_back(datum), next_p, contn_top,
        stacklength() -> contn_top;
    enddefine;

    define :inline lconstant GETRESTRICTION();
        ;;; Get restriction if one exists
        if ispair(next_p) and fast_front(next_p) == ":"
        and ispair(fast_back(next_p) ->> next_p) then
            fast_destpair(next_p) -> (restr, next_p);
            if isident(restr) then idval(restr) -> restr
            elseif isword(restr) then valof(restr) -> restr
            endif;
        else
            false -> restr
        endif;
    enddefine;


    define lconstant check_restriction(/*item*/, restr);
        lvars restr;
        if isinteger(restr) then
            length(/*item*/) == restr
        else
            ;;; it must be a procedure
            restr(/*item*/)
        endif;
    enddefine;

    repeat

        if ispair(pattern) then
            pop_match_vals -> save_conv;
            popmatchvars -> save_matchvars;

    PAIR_CONTINUE:
            ;;; pattern is a pair
            fast_destpair(pattern) -> (x, next_p);
            ;;; Now next_p is back(pattern), x the head
            if next_p == [] then
                ;;; last element of pattern
                if x == "==" then
                    if datum == [] or ispair(datum) then
                        ;;; success
                        TRY_CONTINUATION();
                    else
                        mishap('List needed to match "=="', [^datum])
                    endif;
                    ;;; Datum must be a pair. Otherwise fail.
                elseunless ispair(datum) then
                    goto FAIL
                elseif x == "=" then
                    FAIL_IFNOT(fast_back(datum) == []);
                    TRY_CONTINUATION();
                else
                    ;;; datum must have only one item left
                    FAIL_IFNOT(fast_back(datum) == []);
                    ;;; last element in both pattern and datum
                    if ispair(x) then
                        ;;; No extra continuation needed.
                        x -> pattern;
                        fast_front(datum) -> datum;
                        goto PAIR_CONTINUE;
                    else
                        FAIL_IFNOT(fast_front(datum) = x);
                        TRY_CONTINUATION();
                    endif
                endif;  ;;; end of case next_p == []
            elseif x == "=" then
                FAIL_IFNOT(ispair(datum));
                GETRESTRICTION();   ;;; sets restr
                if restr then
                    FAIL_IFNOT(
                        check_restriction(fast_destpair(datum)->datum, restr));
                else
                    fast_back(datum) -> datum;
                endif;
                next_p -> pattern;
                nextloop
            elseif x == "?" then    ;;; we know ispair(next_p)
                if datum == [] then goto FAIL endif;
                ;;; non-segment variable

                fast_destpair(next_p) -> (id, next_p);

                GETRESTRICTION();   ;;; sets restr

                ;;; Handle non-segment variables here
                if fast_lmember(id, popmatchvars) then
                    ;;; Already bound
                    valof(id) -> x;
                    FAIL_IFNOT(ispair(datum) and fast_front(datum) = x);
                    if restr then
                        if isinteger(restr) then
                            FAIL_IFNOT(length(x) == restr)
                        else
                            FAIL_IFNOT(restr(x) ->> x);
                            ;;; if flags &&/=_0 MV_CONV_P then
                            if x /== true then
                                ;;; x will become value of id
                                conspair(x, conspair(id, pop_match_vals))
                                        -> pop_match_vals;
                            endif
                        endif
                    endif;
                    ;;; finished dealing with already bound non-segment var
                    goto NEXT_PAIR
                endif;

                ;;; id was not on popmatchvars, so add it
                conspair(id, popmatchvars) -> popmatchvars;

                unless restr then
                    ;;; No restriction
                    fast_front(datum) -> valof(id);
                    goto NEXT_PAIR
                else
                    ;;; deal with restriction
                    ;;; apply restriction for non segment case
                    fast_front(datum) -> x;
                    if isinteger(restr) then
                        FAIL_IFNOT(length(x) == restr);
                        x -> valof(id);
                    else
                        ;;; restriction procedure
                        FAIL_IFNOT(restr(x) ->> n);
                        x -> valof(id);
                        if n /== true then
                            conspair(n, conspair(id, pop_match_vals))
                                    -> pop_match_vals;
                        endif;
                    endif;
                    goto NEXT_PAIR;
                endunless
            elseif x == "??" then   ;;; we know ispair(next_p)
                ;;; segment variable

                fast_destpair(next_p) -> (id, next_p);

                GETRESTRICTION();   ;;; sets restr

                goto VAR;

            elseif x == "==" then
                ;;; "==" in middle of list. next_p == [] handled above
                GETRESTRICTION();   ;;; sets restr
                if next_p == [] then
                    ;;; Final segment.
                    if restr then
                        FAIL_IFNOT(check_restriction(datum, restr))
                    endif;
                    ;;; Ignore rest of datum.
                    TRY_CONTINUATION();
                else
                    ;;; not final segment
                    SAVE();
                    [] -> l;
                    repeat
                        if not(restr) or check_restriction(l, restr) then
                        quitif(sub_doesmatch(datum, next_p));
                        endif;
                        RESTORE();
                        unless ispair(datum) then
                            sys_grbg_list(l);
                            goto FAIL
                        endunless;
                        if restr then
                            fast_destpair(datum) -> (x,datum);
                            l nc_<> conspair(x,[]) -> l;
                        else
                            fast_back(datum) -> datum;
                        endif;
                    endrepeat;
                    sys_grbg_list(l);
                    return(true);
                    ;;; any extra continuations will have been done??
                    ;;; TRY_CONTINUATION();
                endif;
            else
                ;;; Can't be a variable
                ;;; matching non-variable elements of datum & pattern
                ;;; Not last element of pattern
                FAIL_IFNOT(ispair(datum));
                ;;; x is first item of pattern, tail is next_p
                if ispair(x) then
                    ;;; hd(pattern) is a list -- make continuation proper
                    ;;; Push continuation. Datum must be a pair at this point
                    STACK_NEXT();
                    x -> pattern;
                    fast_front(datum) -> datum;
                    goto PAIR_CONTINUE
                else
                    FAIL_IFNOT(x = (fast_destpair(datum) -> datum));
                    next_p -> pattern;
                    goto PAIR_CONTINUE
                endif;
                nextloop
            endif
        else
            ;;; pattern not a list -- just use =
            FAIL_IFNOT( datum = pattern );
            TRY_CONTINUATION();
        endif;

        ;;; A check, for debugging
        mishap('FELL THROUGH TO END OF REPEAT LOOP', []);

        NEXT_PAIR:
        next_p -> pattern;
        ;;; Should check datum? (Must be a pair)
        fast_back(datum) -> datum;
        if ispair(pattern) then
            goto PAIR_CONTINUE
        elseif datum = pattern then
            TRY_CONTINUATION();
        else
            goto FAIL
        endif;
    endrepeat;

    ;;; Found a segment matchvar. May be with or without restriction
VAR:
    if fast_lmember(id, popmatchvars) then
        ;;; Already bound
        ;;; Must be a list, cos it`s a segment var
        valof(id) ->> x -> l;
        FAIL_IFNOT(ispair(l) or l == []);
        while ispair(l) do
            unless ispair(datum) and
                ((fast_destpair(datum) -> datum)
                    = (fast_destpair(l) -> l))
            then
                goto FAIL
            endunless;
        endwhile;
        if restr then
            if isinteger(restr) then
                FAIL_IFNOT(listlength(x) == restr);
            else
                FAIL_IFNOT(restr(x) ->> x);
                ;;; if flags &&/=_0 MV_CONV_P then
                if x /== true then
                    ;;; x will become value of id
                    conspair(x, conspair(id, pop_match_vals))
                        -> pop_match_vals;
                endif
            endif;
        endif;
        if next_p == [] then
            FAIL_IFNOT(datum == []);
            TRY_CONTINUATION();
        else
            next_p ->pattern;
            goto PAIR_CONTINUE;
        endif;

    endif; ;;; end of case where id already bound

    ;;; id was not on popmatchvars, so add it
    conspair(id, popmatchvars) -> popmatchvars;

BINDVAR:
    ;;; Variable, now on popmatchvars
    unless restr then
        ;;; No restriction
        if next_p == [] then
            ;;; Final segment. Use rest of datum.
            datum ->> l -> valof(id);
            TRY_CONTINUATION();
        else
                [] ->> l -> valof(id);
            SAVE();
            until sub_doesmatch(datum, next_p) do
                RESTORE();
                unless ispair(datum) then
                    sys_grbg_list(l);
                    [] ->> l -> valof(id);
                    goto FAIL
                endunless;
                fast_destpair(datum) -> (x, datum);
                ;;; add one more item onto list corresponding to ?? variable
                l nc_<> conspair(x,[]) ->> l -> valof(id);
            enduntil;
            return(true);   ;;; can't be any continuation after success
        endif
    else
        ;;; deal with restriction
        ;;; Find a new segment, and bind a variable not already bound
        if next_p == [] then
            datum -> l;
            [] -> datum;
                l -> valof(id);
            if isinteger(restr) then
                FAIL_IFNOT(listlength(l) == restr)
            else
                FAIL_IFNOT(restr(l) ->> n);
                if n /== true then
                    conspair(n, conspair(id, pop_match_vals))
                                -> pop_match_vals;
                endif;
            endif;
            TRY_CONTINUATION();
        endif;
        ;;; next_p /== []
        [] -> l;
        SAVE();
        if isinteger(restr) then
            while restr fi_> 0 do
                    RESTORE();
                unless ispair(datum) then
                    ;;; not enough items left. Remove continuation
                    sys_grbg_list(l);
                    goto FAIL
                endunless;
                fast_destpair(datum) -> (x, datum);
                l nc_<> conspair(x,[]) -> l;
                restr fi_- 1 -> restr;
            endwhile;
                l -> valof(id);
            FAIL_IFNOT(sub_doesmatch(datum, next_p) );
            return(true);
        else
            ;;; Restriction procedure. SAVE done above
            ;;; l is []
            repeat
                if restr(l) ->> n then
                        l -> valof(id);
                    if sub_doesmatch(datum, next_p) then
                        if n /== true then
                            conspair(n, conspair(id, pop_match_vals))
                                -> pop_match_vals;
                        endif;
                        return(true);
                    endif;
                endif;
                RESTORE();
                unless ispair(datum) then
                    [] -> valof(id);
                    sys_grbg_list(l);
                    goto FAIL
                endunless;
                l nc_<> conspair(fast_destpair(datum) -> datum,[]) -> l;
            endrepeat;
        endif; ;;; end of restriction case

        goto NEXT_PAIR;
    endunless;



APPLY_CONTINUATION:
    ;;; Here only if contn_top is non-false after completing a match.
    ;;; There should be at least one continuation left on the stack at
    ;;; this level (current value of contn_top).
    ;;; Restore the three items at that part of the stack,
    ;;; and continue matching.

;;; run next continuation
    stacklength() fi_- (contn_top fi_- 3 ->> l )  -> x;
    subscr_stack(x), subscr_stack(x), subscr_stack(x)
        -> (datum, pattern, contn_top);
    ;;; try to reclaim stack space;
    if l fi_>= save_contn_len then
        ;;; pushed since last choice point - can reclaim space
        erasenum(x)
    endif;
    goto PAIR_CONTINUE;

FAIL:
    ;;; Failed. Tidy up and reclaim garbage.
    if (popmatchvars ) /== [] then
        until popmatchvars == save_matchvars do
            sys_grbg_destpair(popmatchvars) -> (, popmatchvars)
        enduntil;
        if popmatchvars == [] then
            sys_grbg_list(pop_match_vals);
            [] -> pop_match_vals;
        else
            until pop_match_vals == save_conv do
                sys_grbg_destpair(pop_match_vals) -> (, pop_match_vals)
            enduntil;
        endif
    endif;
    false
enddefine;

define lconstant finish_matchvars(/* succeeded */);
    lvars pmvb = popmatchvars, conv_assign = pop_match_vals;
    [] ->> pop_match_vals -> popmatchvars;
    if conv_assign /== [] then
        if dup(/*succeeded*/) then
            ;;; matches succeeded, do assignments
            until conv_assign == [] do
                sys_grbg_destpair(sys_grbg_destpair(conv_assign))
                    -> conv_assign;
                (/*item*/) -> valof(/*WID*/)
            enduntil
        endif;
    endif;
    sys_grbg_list(pmvb)
enddefine;


define vars 8 datum doesmatch pattern;
    lvars datum, pattern, result;

    dlocal  popmatchvars = [], pop_match_vals = [],
            contn_top = false, save_contn_len, whereclause;

    ;;; Check whether optional procedure argument supplied
    if isprocedure(pattern) then
        ((), datum, pattern) -> (datum, pattern, whereclause)
    else
        procedure; true endprocedure -> whereclause
    endif;

    stacklength() -> save_contn_len;
    sub_doesmatch(datum, pattern) -> result;
    erasenum(stacklength() fi_- save_contn_len);

    result;
    if popmatchvars /== [] then finish_matchvars(/*result*/) endif

enddefine;


    /*  Version that allows a nonlocal popmatchvars (analogous to
        sysmatch in old matcher, and applying the class_= procedure for =
        in the new one).
    */

define newsysmatch(pattern, datum);
    lvars datum, pattern, result;

    dlocal  contn_top = false, save_contn_len, whereclause;

    ;;; Check whether procedure argument supplied
    if isprocedure(pattern) then
        ((), datum, pattern) -> (datum, pattern, whereclause)
    else
        procedure; true endprocedure -> whereclause
    endif;

    stacklength() -> save_contn_len;
    sub_doesmatch(datum, pattern) -> result;
    erasenum(stacklength() fi_- save_contn_len);
    result
enddefine;

applist([doesmatch sub_doesmatch newsysmatch], sysprotect);
endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec 29 1995
    Extended to allow ":" for restrictions after "=" and "=="
--- Aaron Sloman, Dec 28 1995
    Further re-writes, to improve speed in simplest cases.
--- Aaron Sloman, Dec 27 1995
    Completely re-written, using ideas from John Gibson's new LIB EQUALS
    (which, in turn was partly based on an older version of this).
    This version prevents garbage due to popmatchvars, continuations, and
    temporary lists.
--- Aaron Sloman, Dec  2 1995
    Following discussion on comp.lang.pop relaced nonactive_valof with valof
--- Aaron Sloman 25 Nov 1995
    Based on LIB * FMATCHES, this is a new library, taking advantage of
    the fact that from Poplog V15.0 "valof" works on both words and identifiers
    Also created while_matching for loops.
 */
