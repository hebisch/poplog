/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/equals.p
 > Purpose:         Version of = with full backtracking over list-segment
 >                  matchvars
 > Author:          John Gibson, Dec 21 1995 (see revisions)
 >                  (based partly on work by Aaron Sloman et al)
 > Documentation:   HELP * EQUAL
 */
compile_mode: pop11 +strict;

section;

sysunprotect("equals");

    ;;; Global variables for sub_equals
lvars
    save_contn_len,
    contn_top,
    procedure whereclause,
;

lconstant macro (
    ;;; matchvar flags
    MV_SEG_MATCH    = 2:1e0,
    MV_CONV_P       = 2:1e1,

    ;;; success result from sub_equals when continuation not applied
    CONTN_NOT_DONE  = 0,
);



    /*  This does the main matching of datum against pattern.
        It uses the procedure whereclause nonlocally to test each
        match.
    */
define lconstant sub_equals(datum, pattern, part_contn);
    lvars   datum, pattern, x, n, l, id, save_top, flags, restr,
            pmvb, part_contn, save_conv, save_matchvars;

    dlocal  save_contn_len;

    define :inline lconstant SAVE();
        stacklength() -> save_contn_len;    ;;; choice point
        contn_top -> save_top;
    enddefine;

    define :inline lconstant RESTORE();
        save_top -> contn_top;
        erasenum(stacklength() fi_- save_contn_len);
    enddefine;

    define :inline lconstant SAVE_CONTN_&_PMVB();
        if part_contn then contn_top, stacklength() -> contn_top endif;
        fast_destpair(pop_matchvars_bound) -> (save_conv, save_matchvars)   ;;; OK with []
    enddefine;

    define :inline lconstant FAIL_IFNOT(expr);
        unless expr then goto FAIL endunless
    enddefine;

    define :inline lconstant RETURNIF_DONE(res);
        returnunless(res == CONTN_NOT_DONE) (true)
    enddefine;


    datakey(pattern) -> x;

    if x == pair_key then
        SAVE_CONTN_&_PMVB();

PAIR_CONTINUE:
        ;;; pattern is a pair
        until datakey(fast_front(pattern) ->> x) == matchvar_key
        and (destmatchvar(x) -> (, id, restr, flags),
            flags &&/=_0 MV_SEG_MATCH)
        do
            FAIL_IFNOT(ispair(datum));
            if (fast_back(pattern) ->> n) == [] then
                FAIL_IFNOT(fast_back(datum) == []);
                unless ispair(x) then
                    FAIL_IFNOT(sub_equals(fast_front(datum), x, false) ->> n);
                    RETURNIF_DONE(n);
                    goto APPLY_CONTINUATION
                endunless
            else
                ;;; push partial continuation
                (fast_back(datum), n, false);
                unless ispair(x) then
                    FAIL_IFNOT(sub_equals(fast_front(datum), x, true) ->> n);
                    RETURNIF_DONE(n);
                    -> (datum, pattern, );
                    goto NEXT_PAIR
                endunless;
                ;;; hd(pattern) is a list -- make continuation proper
                contn_top, stacklength() -> contn_top
            endif;
            fast_front(datum) -> datum;
            x -> pattern
        enduntil;

        ;;; found a list segment matchvar

        pop_matchvars_bound -> pmvb;
        if isident(restr) then idval(restr) -> restr endif;
        if id then
            if fast_lmember(id, pmvb) then
                ;;; already bound
                valof(id) ->> x -> l;
                FAIL_IFNOT(ispair(l) or l == []);
                while ispair(l) do
                    FAIL_IFNOT(ispair(datum)
                                and (fast_destpair(l) -> l)
                                    = (fast_destpair(datum) -> datum))
                endwhile;
                if restr then
                    if isinteger(restr) then
                        FAIL_IFNOT(listlength(x) == restr)
                    else
                        FAIL_IFNOT(restr(x) ->> x);
                        if flags &&/=_0 MV_CONV_P then
                            conspair(x, conspair(id, fast_front(pmvb)))
                                            -> fast_front(pmvb)
                        endif
                    endif
                endif;
                fast_back(pattern) -> pattern;
                goto NEXT_PAIR
            endif;
            if pmvb == [] then
                conspair([], conspair(id,[])) ->> pmvb -> pop_matchvars_bound
            else
                conspair(id, fast_back(pmvb)) -> fast_back(pmvb)
            endif
        endif;
        fast_back(pattern) -> pattern;

        unless restr then
            SAVE();
            if id then [] ->> l -> valof(id) endif;
            until sub_equals(datum, pattern, false) ->> x do
                RESTORE();
                unless ispair(datum) then
                    if id then
                        [] -> valof(id);
                        sys_grbg_list(l)
                    endif;
                    goto FAIL
                endunless;
                fast_destpair(datum) -> (x, datum);
                if id then
                    l nc_<> conspair(x,[]) ->> l -> valof(id)
                endif
            enduntil;
            RETURNIF_DONE(x);
            goto APPLY_CONTINUATION
        endunless;

        ;;; deal with restriction
        [] -> l;
        if isinteger(restr) then
            while restr fi_> 0 do
                unless ispair(datum) then
                    if id then sys_grbg_list(l) endif;
                    goto FAIL
                endunless;
                fast_destpair(datum) -> (x, datum);
                if id then l nc_<> conspair(x,[]) -> l endif;
                restr fi_- 1 -> restr
            endwhile;
            if id then l -> valof(id) endif;
            goto NEXT_PAIR
        else
            ;;; restriction procedure
            SAVE();
            repeat
                if restr(l) ->> n then
                    if id then l -> valof(id) endif;
                    if sub_equals(datum, pattern, false) ->> x then
                        if flags &&/=_0 MV_CONV_P then
                            conspair(n, conspair(id, fast_front(pmvb)))
                                                    -> fast_front(pmvb)
                        endif;
                        RETURNIF_DONE(x);
                        goto APPLY_CONTINUATION
                    endif;
                    RESTORE()
                endif;
                unless ispair(datum) then
                    if id then [] -> valof(id) endif;
                    sys_grbg_list(l);
                    goto FAIL
                endunless;
                l nc_<> conspair(fast_destpair(datum) -> datum,[]) -> l
            endrepeat
        endif;

NEXT_PAIR:
        if ispair(pattern) then
            goto PAIR_CONTINUE
        elseif pattern == [] then
            FAIL_IFNOT(datum == [])
        else
            FAIL_IFNOT(sub_equals(datum, pattern, false) ->> x);
            RETURNIF_DONE(x)
        endif;
        goto APPLY_CONTINUATION;


    elseunless class_field_spec(x) ->> n then
DO_CLASS_EQ:
        ;;; not a recordclass or a full vectorclass -- just use class_=
        returnif( fast_apply(datum, pattern, class_=(x)) ) (CONTN_NOT_DONE);
        if part_contn then () -> (,,) endif;
        return(false)


    elseif n == "full" or isprocedure(n) then
        ;;; full (or possibly full) vectorclass
        unless datakey(datum) == x
        and (datalength(datum)->>l) == datalength(pattern) then
            if part_contn then () -> (,,) endif;
            return(false)
        endunless;
        returnif(l == 0) (CONTN_NOT_DONE);
        SAVE_CONTN_&_PMVB();
        class_fast_subscr(x) -> restr;
        1 -> n;
VECTOR_CONTINUE:
        fast_apply(n, datum, restr) -> id;
        fast_apply(n, pattern, restr) -> x;
        if n == l then
            FAIL_IFNOT(sub_equals(id, x, false) ->> x);
            RETURNIF_DONE(x);
            goto APPLY_CONTINUATION
        else
            (datum, pattern, n fi_+ 1);     ;;; push partial continuation
            FAIL_IFNOT(sub_equals(id, x, true) ->> x);
            RETURNIF_DONE(x);
            () -> (, , n);
            goto VECTOR_CONTINUE
        endif;


    elseif ispair(n) then
        ;;; recordclass
        unless datakey(datum) == x then
            if part_contn then () -> (,,) endif;
            return(false)
        endunless;
        datalength(x) -> l;
        returnif(l == 0) (CONTN_NOT_DONE);
        SAVE_CONTN_&_PMVB();
        1 -> n;
RECORD_CONTINUE:
        class_access(n, x) -> restr;
        fast_apply(datum, restr) -> id;
        fast_apply(pattern, restr) -> restr;
        if n == l then
            FAIL_IFNOT(sub_equals(id, restr, false) ->> id);
            RETURNIF_DONE(id);
            goto APPLY_CONTINUATION
        else
            (datum, pattern, n fi_+ 1);     ;;; push partial continuation
            FAIL_IFNOT(sub_equals(id, restr, true) ->> id);
            RETURNIF_DONE(id);
            () -> (, , n);
            goto RECORD_CONTINUE
        endif;


    else
        ;;; integer vector etc
        goto DO_CLASS_EQ
    endif;


APPLY_CONTINUATION:
    ;;; get next continuation
    unless contn_top then
        ;;; finished
        FAIL_IFNOT(whereclause());
        return(true)
    endunless;

    contn_top fi_- 4 -> l;
    stacklength() fi_- l -> x;
    subscr_stack(x), subscr_stack(x), subscr_stack(x), subscr_stack(x)
                        -> (datum, pattern, n, contn_top);
    if l fi_>= save_contn_len then
        ;;; pushed since last choice point - can reclaim space
        erasenum(x)
    endif;
    unless n then
        ;;; pattern was back of a pair
        goto NEXT_PAIR
    elseif isvectorclass(pattern) then
        datalength(pattern) -> l;
        class_fast_subscr(datakey(pattern)) -> restr;
        goto VECTOR_CONTINUE
    else
        datakey(pattern) -> x;
        datalength(x) -> l;
        goto RECORD_CONTINUE
    endunless;


FAIL:
    ;;; failed (n.b. this code is replicated in sys_restore_matchvars)
    if (pop_matchvars_bound ->> pmvb) /== [] then
        fast_destpair(pmvb) -> (pattern, datum);
        until datum == save_matchvars do
            sys_grbg_destpair(datum) -> (, datum)
        enduntil;
        if datum == [] then
            sys_grbg_list(pattern);
            sys_grbg_destpair(pmvb) -> (,);
            [] -> pop_matchvars_bound
        else
            datum -> fast_back(pmvb);
            until pattern == save_conv do
                sys_grbg_destpair(pattern) -> (, pattern)
            enduntil;
            pattern -> fast_front(pmvb)
        endif
    endif;

    false
enddefine;


define 7 datum equals pattern;
    lvars datum, pattern, result;

    dlocal  pop_matchvars_bound = [], contn_top = false,
            save_contn_len, whereclause;

    ;;; Check whether optional procedure argument supplied
    if isprocedure(pattern) then
        ((), datum, pattern) -> (datum, pattern, whereclause)
    else
        procedure; true endprocedure -> whereclause
    endif;

    stacklength() -> save_contn_len;
    sub_equals(datum, pattern, false) -> result;
    erasenum(stacklength() fi_- save_contn_len);
    if result == CONTN_NOT_DONE then whereclause() else result endif;
    if pop_matchvars_bound /== [] then sys_finish_matchvars() endif
enddefine;

    /*  Version that allows a nonlocal pop_matchvars_bound (analagous to
        applying the class_= procedure for =).
    */
define sys_nonlocal_equals(datum, pattern);
    lvars datum, pattern, result;

    dlocal  contn_top = false, save_contn_len, whereclause;

    ;;; Check whether procedure argument supplied
    if isprocedure(pattern) then
        ((), datum, pattern) -> (datum, pattern, whereclause)
    else
        procedure; true endprocedure -> whereclause
    endif;

    stacklength() -> save_contn_len;
    sub_equals(datum, pattern, false) -> result;
    erasenum(stacklength() fi_- save_contn_len);
    if result == CONTN_NOT_DONE then whereclause() else result endif
enddefine;

sysprotect("equals");

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 22 1995
        Now uses sys_finish_matchvars from core system instead of private
        procedure
 */
