/* --- Copyright University of Birmingham 1996. All rights reserved. ------
 > File:            $poplocal/local/auto/forall_match.p
 > Purpose:         See HELP * FORALL_MATCH
 > Author:          Originally Rudi Lutz, April 1983 (see revisions)
 > Documentation:
 > Related Files:
 */

compile_mode :pop11 +strict;

section $-forall_match =>   nonmac forall_match
                            nonmac endforall_match
                            startfindall;

;;; make the key words known to ved_jcp - Mark Rubinstein 18.3.85
    "forall_match" :: vedopeners -> vedopeners;
    "endforall_match" :: vedclosers -> vedclosers;

vars mymatchvars;  nil -> mymatchvars; ;;; records variable bindings

define mymatch(pattern, datum);
    lvars ttt nnn vvv sss savepmvars newproc;
    mymatchvars -> sss;
loop:
    unless ispair(pattern) then
        if datum = pattern then suspend(true,1);goto mfalse endif;
        goto mfalse;
    endunless;
    unless ispair(datum) or datum == [] then
        goto mfalse;
    endunless;
    fast_front(pattern) -> ttt;
    fast_back(pattern) -> pattern;
    if ttt == "==" then
l1:        if pattern == [] then suspend(true,1);goto mfalse endif;
        until mymatch(pattern,datum) do
            if datum == nil then goto mfalse endif;
            back(datum) -> datum;
        enduntil;
        suspend(true,1);goto l1;
    endif;
    if ttt == "??" and pattern /== [] then
        destpair(pattern) -> pattern -> nnn;
        if lmember(nnn, mymatchvars) then
            ;;; the variable was previously found in the pattern
            valof(nnn) -> vvv;
            until vvv == nil do
                unless ispair(datum)  then goto mfalse endunless;
                if (fast_destpair(datum) -> datum) /= (destpair(vvv) -> vvv) then
                    goto mfalse
                endif;
            enduntil;
            if pattern == nil or front(pattern) /== ":" then goto loop endif;
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
                unless mymatch(pattern,datum) then goto mfalse endunless;
                vvv -> valof(nnn);
                suspend(true,1);goto mfalse;
            endif
        endif;
        ;;; the variable was not previously found in the pattern
        conspair(nnn, mymatchvars) -> mymatchvars;
        if pattern == nil then datum -> valof(nnn); suspend(true,1);goto mfalse endif;
        nil -> valof(nnn);
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
        and vvv /== "=" and vvv /== "==" and not(member(vvv,datum))
        then goto mfalse
        endif;

        if front(pattern) /== ":" then
            until mymatch(pattern,datum) do
                unless ispair(datum) then goto mfalse endunless;
                valof(nnn) <> conspair(fast_destpair(datum) -> datum,nil)
                    -> valof(nnn);
            enduntil;
            suspend(true,1);goto mfalse;
        endif;
        ;;; found ":" after a ?? variable. Get the restrictor ttt (integer or procedure name)
        destpair(fast_back(pattern)) -> pattern -> ttt;
        if isinteger(ttt) then
            ;;; if there's nothing else in pattern, rest of datum must be right length
            if pattern == [] then
                if length(datum) == ttt then datum ->valof(nnn);suspend(true,1);goto mfalse;
                else    goto mfalse
                endif;
            endif;
            until length(valof(nnn)) == ttt do
                unless ispair(datum) then goto mfalse endunless;
                valof(nnn) <> conspair(fast_destpair(datum) -> datum,nil)
                        -> valof(nnn);
            enduntil;
            goto loop;
        endif;
        ;;; found a restriction procedure after the ?? variable
        if isword(ttt) then valof(ttt) -> ttt endif;
        ;;; if there's nothing else in pattern, rest of datum must be right
        if pattern == [] then
            ttt(datum) -> vvv;
            if vvv == true then datum -> valof(nnn);suspend(true,1);goto mfalse;
            elseif vvv then vvv -> valof(nnn); suspend(true,1);goto mfalse;
            else    goto mfalse
            endif;
        endif;

        mymatchvars -> savepmvars;   ;;; may be reset in recursive call
;;; unlike usual matcher this applies restriction procedures before matching
;;; rest of datum
        until (apply(valof(nnn), ttt) ->> vvv) and mymatch(pattern,datum)  do
                unless ispair(datum) then goto mfalse endunless;
            valof(nnn) <> conspair(fast_destpair(datum) -> datum,nil) -> valof(nnn);
            savepmvars -> mymatchvars;   ;;; in case reset in mymatch
        enduntil;
        ;;; if restriction procedure produces non-truth-value, give that to the value of the variable
        unless vvv == true then vvv -> valof(nnn) endunless;
        suspend(true,1);goto mfalse;
    endif;
    unless ispair(datum) then goto mfalse endunless;
    if ttt == "=" then fast_back(datum) -> datum; goto loop endif;
    if ttt == "?" and pattern /== [] then
        destpair(pattern) -> pattern -> nnn;
        fast_front(datum) -> ttt;
        fast_back(datum) -> datum;
        if lmember(nnn, mymatchvars) then
            unless valof(nnn) = ttt then goto mfalse endunless
        else
            ttt -> valof(nnn);      ;;; check nnn a word etc
            conspair(nnn, mymatchvars) -> mymatchvars;
        endif;
        if pattern == nil or front(pattern) /== ":" then goto loop endif;
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
        unless mymatch(pattern,datum) then goto mfalse endunless;
        vvv -> valof(nnn);
        suspend(true,1);goto mfalse;
    endif;
        consproc(ttt,front(datum),2,mymatch)->newproc;
        mymatchvars->savepmvars;
        while runproc(0,newproc) do
            mymatch(pattern,fast_back(datum)) ->;
            savepmvars->mymatchvars;
        endwhile;
mfalse:
    sss -> mymatchvars;
    return(false);
enddefine;


define findnext_match(FORALL_PLIST,LIST);
    dlocal mymatchvars;
    [] -> mymatchvars;
    mymatch(FORALL_PLIST,LIST);
    ksuspend(false,1)
enddefine;

;;; STARTFINDALL creates a process, with a pattern list, a list and
;;; FINDNEXT_MATCH  as the procedure

define global startfindall(FORALL_PL,LIST);
    consproc(FORALL_PL,LIST,2,findnext_match)
enddefine;

;;;     FORALL_MATCH [........] DO <actions> ENDFORALL_MATCH
;;; becomes, roughly:
;;;
;;;     VARS %V;
;;;     STARTTRYALL([.........]) -> %V;
;;;     WHILE RUNPROC(V, 0) DO <actions> ENDWHILE
;;;
global vars syntax endforall_match ;


define global syntax forall_match;
    lvars FORALL_ENDLAB FORALL_LAB FORALL_VAR ;
    sysnlabel() -> FORALL_LAB; sysloop(FORALL_LAB);
    sysnlabel() -> FORALL_ENDLAB; sysloopend(FORALL_ENDLAB);
    sysnvariable() -> FORALL_VAR;
    sysVARS(FORALL_VAR,0);
    erase(systxcomp([in]));
    erase(systxcomp([do]));
    sysCALL("startfindall");
    sysPOP(FORALL_VAR);
    sysLABEL(FORALL_LAB);
    sysPUSHQ(0);
    sysPUSH(FORALL_VAR);
    sysCALL("runproc");
    sysIFNOT(FORALL_ENDLAB);
    erase(systxsqcomp([endforall_match]));
    sysGOTO(FORALL_LAB);
    sysLABEL(FORALL_ENDLAB)
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 15 1996
    Updated to compile properly in V15.0 with compile_mode set.
    Also updated help file
 */
