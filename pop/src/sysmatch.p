/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/sysmatch.p
 > Purpose:
 > Author:          Aaron Sloman (see revisions)
 */

;;;-------------------- THE MATCHER ----------------------------------------

#_INCLUDE 'declare.ph'


;;; -----------------------------------------------------------------------

vars
    popmatchvars = [];      ;;; records variable bindings


    /*  -sysmatch- takes as argument a pattern and a data list.
        It returns TRUE iff the data list matches the pattern.
        As a side effect the pattern may specify the binding of
        variables to parts of the data list.
    */
define sysmatch(pattern, datum);
    lvars pattern, datum, ttt, nnn, vvv, sss = popmatchvars, savepmvars;
loop:
    _CHECKINTERRUPT;
    unless ispair(pattern) then
        if datum = pattern then return(true) endif;
        goto mfalse;
    endunless;
    unless ispair(datum) or datum == [] then
        goto mfalse;
    endunless;
    fast_front(pattern) -> ttt;
    fast_back(pattern) -> pattern;
    if ttt == "==" then
        if pattern == [] then return(true) endif;
        until sysmatch(pattern, datum) do
            if datum == [] then goto mfalse endif;
            back(datum) -> datum;
        enduntil;
        return(true);
    endif;
    if ttt == "??" and pattern /== [] then
        destpair(pattern) -> pattern -> nnn;
        if fast_lmember(nnn, popmatchvars) then
            ;;; the variable was previously found in the pattern
            valof(nnn) -> vvv;
            until vvv == [] do
                unless ispair(datum) then goto mfalse endunless;
                if (fast_destpair(datum) -> datum) /= (destpair(vvv) -> vvv) then
                    goto mfalse
                endif;
            enduntil;
            if pattern == [] or front(pattern) /== ":" then goto loop endif;
            ;;; found a colon after the variable
            destpair(fast_back(pattern)) -> pattern -> ttt;
            if isinteger(ttt) then
                ;;; integer followed colon. Check length
                unless length(valof(nnn)) == ttt then
                    goto mfalse endunless;
                goto loop;
            else
                ;;; restriction procedure followed colon. Apply it
                if isword(ttt) then valof(ttt) -> ttt endif;
                ttt(valof(nnn)) -> vvv;
                unless vvv then goto mfalse endunless;
                if vvv == true then goto loop endif;
                unless sysmatch(pattern, datum) then goto mfalse endunless;
                vvv -> valof(nnn);
                return(true);
            endif
        endif;
        ;;; the variable was not previously found in the pattern
        conspair(nnn, popmatchvars) -> popmatchvars;
        if pattern == [] then datum -> valof(nnn); return(true) endif;
        [] -> valof(nnn);
        ;;; check that if there's an atom following the variable, it occurs in
        ;;; the datum too
        if front(pattern) == ":"
        then
            if ispair(back(back(pattern)) ->> vvv)
            then front(vvv)
            else false endif;
        else front(pattern)
        endif -> vvv;
        if vvv and atom(vvv) and vvv /== "??" and vvv /== "?"
        and vvv /== "=" and vvv /== "==" and not(lmember_=(vvv,datum))
        then goto mfalse
        endif;

        if front(pattern) /== ":" then
            until sysmatch(pattern, datum) do
                unless ispair(datum) then goto mfalse endunless;
                valof(nnn) nc_<> conspair(fast_destpair(datum) -> datum,[])
                    -> valof(nnn);
            enduntil;
            return(true);
        endif;
        ;;; found ":" after a ?? variable. Get the restrictor ttt (integer or procedure name)
        destpair(fast_back(pattern)) -> pattern -> ttt;
        if isinteger(ttt) then
            ;;; if there's nothing else in pattern, rest of datum must be right length
            if pattern == [] then
                if length(datum) == ttt then datum -> valof(nnn); return(true)
                else    goto mfalse
                endif;
            endif;
            until length(valof(nnn)) == ttt do
                unless ispair(datum) then goto mfalse endunless;
                valof(nnn) nc_<> conspair(fast_destpair(datum) -> datum,[])
                        -> valof(nnn);
            enduntil;
            goto loop;
        endif;
        ;;; found a restriction procedure after the ?? variable
        if isword(ttt) then valof(ttt) -> ttt endif;
        ;;; if there's nothing else in pattern, rest of datum must be right
        if pattern == [] then
            ttt(datum) -> vvv;
            if vvv == true then datum -> valof(nnn); return(true)
            elseif vvv then vvv -> valof(nnn); return(true)
            else    goto mfalse
            endif;
        endif;

        popmatchvars -> savepmvars;   ;;; may be reset in recursive call
        until sysmatch(pattern,datum) and (ttt(valof(nnn)) ->> vvv) do
            unless ispair(datum) then goto mfalse endunless;
            valof(nnn) nc_<> conspair(fast_destpair(datum) -> datum,[]) -> valof(nnn);
            savepmvars -> popmatchvars;   ;;; in case reset in sysmatch
        enduntil;
        ;;; if restriction procedure produces non-truth-value, give that to the value of the variable
        unless vvv == true then vvv -> valof(nnn) endunless;
        return(true);
    endif;
    unless ispair(datum) then goto mfalse endunless;
    if ttt == "=" then fast_back(datum) -> datum; goto loop endif;
    if ttt == "?" and pattern /== [] then
        destpair(pattern) -> pattern -> nnn;
        fast_front(datum) -> ttt;
        fast_back(datum) -> datum;
        if fast_lmember(nnn, popmatchvars) then
            unless valof(nnn) = ttt then goto mfalse endunless
        else
            ttt -> valof(nnn);      ;;; check nnn a word etc
            conspair(nnn, popmatchvars) -> popmatchvars;
        endif;
        if pattern == [] or front(pattern) /== ":" then goto loop endif;
        ;;; colon found after pattern variable
        destpair(fast_back(pattern)) -> pattern -> ttt;
        if isinteger(ttt) then
            ;;; integer found after colon
            if length(valof(nnn)) == ttt then
                goto loop
            else
                goto mfalse
            endif
        endif;
        ;;; Restriction procedure found after colon. Apply it.
        if isword(ttt) then valof(ttt) -> ttt endif;
        ttt(valof(nnn)) -> vvv;
        unless vvv then goto mfalse endunless;
        if vvv == true then goto loop endif;
        unless sysmatch(pattern, datum) then goto mfalse endunless;
        vvv -> valof(nnn);
        return(true);
    endif;
    unless sysmatch(ttt, front(datum)) then goto mfalse endunless;
    fast_back(datum) -> datum;
    goto loop;
mfalse:
    sss -> popmatchvars;
    return(false);
enddefine;

define vars 8 datum matches pattern;
    lvars datum, pattern;
    dlocal popmatchvars;
    [] -> popmatchvars;
    sysmatch(pattern,datum)
enddefine;

define 8 datum --> pattern;
    lvars datum, pattern;
    unless datum matches pattern then
        mishap(datum,pattern,2,'NON MATCHING ARGUMENTS FOR -->')
    endunless
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 14 1995
        # Uses fast_lmember instead of lmember on popmatchvars.
        # Uses nc_<> to add elements to the end of a matched segment instead
          of <>
--- John Gibson, Dec  4 1995
        Uses lmember_= instead of mem*ber
--- John Gibson, Jan 23 1989
        Got rid of unnecessary use of -apply-
--- John Gibson, Aug 16 1987
        Tidied up
 */
