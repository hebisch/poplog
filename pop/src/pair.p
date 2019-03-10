/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/pair.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *LISTS
*/

;;; --------------------- PAIRS AND LISTS -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'matchvar.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (Sys$-Unique_hash),
        matchvar_key
    ;

vars
        pop_matchvars_bound, Sys$- _pop_hash_lim
    ;


;;; --------------------------------------------------------------------

section $-Sys => ispair, conspair, front, back, destpair,
                 fast_front, fast_back, fast_destpair, recursive_front,
                 sys_grbg_list, sys_grbg_destpair, null, islist, ::,
                 sysconslist, sysconslist_onto, rev, applist,
                 listlength, length, fast_lmember, nil, popstackmark,
                 pair_key, nil_key
                 ;


;;; --- PAIRS -----------------------------------------------------------

    ;;; free pair list
vars
    _free_pairs = 0;

define ispair(_item);
    lvars _item;
    iscompound(_item) and _item!KEY == pair_key
enddefine;

define Conspair() -> _pair with_props conspair;
    lvars _pair, _next, key = pair_key;
    Get_store(@@(struct PAIR)[_16]) -> _pair;
    _pair -> _free_pairs;

    #_< repeat 14 times [
          0 -> _pair!P_FRONT; key -> _pair!KEY; _pair@(struct PAIR)++ -> _next;
          _next -> _pair!P_BACK; _next -> _pair;
        ].dl endrepeat
    >_#

    0 -> _pair!P_FRONT; key -> _pair!KEY; 0 -> _pair!P_BACK;
    _pair@(struct PAIR)++ -> _pair;

    key -> _pair!KEY;
    () -> _pair!P_BACK -> _pair!P_FRONT
enddefine;

define conspair() with_nargs 2;
    lvars _pair = _free_pairs;
    if iscompound(_pair) then
        _pair!P_BACK -> _free_pairs;
        () -> _pair!P_BACK -> _pair!P_FRONT;
        _pair
    else
        chain(Conspair)
    endif
enddefine;

define lconstant Pair_needed = mishap(% 1, 'PAIR NEEDED' %) enddefine;

define front() with_nargs 1;
    if ispair(dup()) then fast_front() else Pair_needed() endif
enddefine;
;;;
define updaterof front() with_nargs 2;
    if ispair(dup()) then () -> fast_front() else Pair_needed() endif
enddefine;

define back() with_nargs 1;
    if ispair(dup()) then fast_back() else Pair_needed() endif
enddefine;
;;;
define updaterof back() with_nargs 2;
    if ispair(dup()) then () -> fast_back() else Pair_needed() endif
enddefine;

define destpair() with_nargs 1;
    if ispair(dup()) then fast_destpair() else Pair_needed() endif
enddefine;

;;; Now fast, non-type-checking versions

define fast_front() with_nargs 1;
    ()!P_FRONT
enddefine;
;;;
define updaterof fast_front() with_nargs 2;
    -> ()!P_FRONT
enddefine;

define fast_back() with_nargs 1;
    !P_BACK
enddefine;
;;;
define updaterof fast_back() with_nargs 2;
    -> ()!P_BACK
enddefine;

define fast_destpair(pair);
    lvars pair;
    fast_front(pair), fast_back(pair)
enddefine;

define recursive_front(item)->item;
    ;;; used for digging name out of pdprops
    lvars item;
    while ispair(item) do fast_front(item) -> item endwhile
enddefine;

define sys_grbg_list(garbage_list);
    lvars list = garbage_list, garbage_list, _item;
    returnunless(ispair(list));
    repeat
        returnif(list <@(w) _system_end);
        quitunless(ispair(fast_back(list) ->> _item));
        _item -> list
    endrepeat;
    _free_pairs -> fast_back(list);
    garbage_list -> _free_pairs
enddefine;

    ;;; like fast_destpair, but put original pair on free list
define sys_grbg_destpair(pair);
    lvars pair;
    fast_front(pair), fast_back(pair);
    if pair >=@(w) _system_end then
        _free_pairs -> fast_back(pair);
        pair -> _free_pairs
    endif
enddefine;


;;; --- LISTS ------------------------------------------------------------

define Dynamic_add_end(item, pair);
    lvars pair, next, item;
    while ispair(fast_back(pair) ->> next) do next -> pair endwhile;
    if item == termin then
        false -> fast_front(pair)
    else
        item -> fast_front(pair);
        conspair(true, next) -> fast_back(pair)
    endif
enddefine;

define null(item);
    lvars item, work, _savelen;
    _CHECKINTERRUPT;
    if ispair(item) and iscompound(fast_back(item) ->> work) then
        if work!KEY == pair_key or work == [] then
            return(false)
        elseif work!KEY == procedure_key then
            ;;; dynamic list
            if fast_front(item) then      ;;; not (yet) dead
                _stklength() -> _savelen;
                ;;; N.B. Applying this procedure can recursively call null
                ;;; or hd etc on item, causing the list to be expanded
                ;;; when it returns. (This is awful, but we have to allow for
                ;;; it since XVed can be called inside sysread etc.)
                fast_apply(work);
                _stklength() _sub _savelen -> _savelen;
                if _savelen /== @@(w)[_1] then
                    mishap(work,
                        if _savelen _slteq _0 then
                            1, 'NO RESULT FROM DYNAMIC LIST PROCEDURE'
                        else
                            _pint(##(w){_savelen} _add _1),
                            'MORE THAN 1 RESULT FROM DYNAMIC LIST PROCEDURE'
                        endif)
                endif;
                ;;; result on stack
                if fast_back(item) /== work then
                    ;;; back got replaced by recursive expansion
                    Dynamic_add_end((), item);
                    return(false)
                elseif dup() == termin then
                    ->, false -> fast_front(item)
                else
                    () -> fast_front(item);
                    conspair(true, work) -> fast_back(item);
                    return(false)
                endif
            endif;
            ;;; return true if dead dynamic list
            return(true)
        endif
    elseif item == [] then
        return(true)
    endif;
    mishap(item, 1, 'LIST NEEDED')
enddefine;

define Checkr_list_tl(item) -> item;
    lvars item;
    unless item == [] or ispair(item) then
        mishap(item, 1, 'LIST NEEDED')
    endunless
enddefine;

define islist(item);
    lvars item;
    if ispair(item) then
        fast_back(item) -> item;
        iscompound(item)
        and (item!KEY == pair_key or item == [] or item!KEY == procedure_key)
    else
        item == []
    endif
enddefine;

define 4 :: with_nargs 2;
    conspair(Checkr_list_tl())
enddefine;

define rev(list) -> newlist;
    lvars list, newlist = [];
    until null(list) do
        fast_front(list), fast_back(list) -> list;
        conspair((), newlist) -> newlist
    enduntil
enddefine;

define applist(list, p);
    lvars list, procedure p;
    Check_procedure(p);
    until null(list) do
        _CHECKUSER;
        p(fast_front(list));
        fast_back(list) -> list
    enduntil
enddefine;
;;;
define updaterof applist(list, p);
    lvars list, procedure p;
    Check_procedure(p);
    rev(list) -> list;
    until null(list) do
        -> p(fast_front(list));
        fast_back(list) -> list
    enduntil;
    sys_grbg_list(list)
enddefine;

define listlength(list) -> count;
    lvars list, count = 0;
    until null(list) do
        count fi_+ 1 -> count;
        fast_back(list) -> list
    enduntil
enddefine;

define length(item);
    lvars item;
    if item == [] then
        0
    elseif ispair(item) then
        listlength(item)
    else
        datalength(item)
    endif
enddefine;

define fast_lmember(item, list);
    lvars list, item;
    until list == [] do
        returnif(fast_front(list) == item) (list);
        fast_back(list) -> list
    enduntil;
    false
enddefine;

define Check_lsubscr(_num, list) -> list;
    lvars list, savenum, savelist, _num;
    _num -> savenum; list -> savelist;
    if isinteger(_num) and _num fi_> 0 then
        until null(list) do
            _num fi_- 1 -> _num;
            if _num == 0 then return endif;
            fast_back(list) -> list
        enduntil
    endif;
    mishap(savenum, savelist, 2, 'BAD ARGUMENTS FOR INDEXED LIST ACCESS')
enddefine;


;;; --- PAIR KEY ----------------------------------------------------------

define lconstant Pair_print(pair);
    lvars list = fast_back(pair), item = fast_front(pair), pair;
    if pop_pr_level == 0 then
        Print_str(  if list == [] or ispair(list) then
                        '<list>'
                    elseif isprocedure(list) and isboolean(item) then
                        if item then '[...]' else '[]' endif
                    else
                        '<pair>'
                    endif);
        return
    endif;

    cucharout(`[`);
    while ispair(list) do
        pr(item);
        fast_back(list ->> pair) -> list;
        fast_front(pair) -> item;
        if item or not(isprocedure(list)) then
            cucharout(`\s`)
        endif
    endwhile;

    if list == [] then
        pr(item)
    elseif isprocedure(list) and isboolean(item) then
        if item then Print_str('...') endif
    else
        pr(item), cucharout(`|`), pr(list)
    endif;

    cucharout(`]`)
enddefine;

define lconstant Pair_apply(list);
    lvars list;
    unless _nonzero(_stklength()) and isinteger(dup()) then
        ;;; assume should be executing non-procedure
        Exec_nonpd(list)
    else
        fast_front(Check_lsubscr((), list))
    endunless
enddefine;
;;;
define updaterof Pair_apply(list);
    lvars list;
    unless _nonzero(_stklength()) and isinteger(dup()) then
        ;;; assume should be executing non-procedure
        -> Exec_nonpd(list)
    else
        -> fast_front(Check_lsubscr((), list))
    endunless
enddefine;

define lconstant Pair_hash(list) -> _num;
    lvars item, list, _loc = 0, _num = 3674;    ;;; 3674 = syshash("pair")
    dlocal _pop_hash_lim;
    if _pop_hash_lim /== 0 then
        _pop_hash_lim fi_- 1 -> _pop_hash_lim;
        while ispair(list) do
            list!P_FRONT -> item;
            (fast_apply(item, fast_cont(datakey(item)!K_HASH)) fi_<< _loc)
                    fi_+ _num -> _num;
            if _loc == _pop_hash_lim then
                return
            endif;
            _loc fi_+ 1 -> _loc;
            list!P_BACK -> list
        endwhile;
        ;;; in case it wasn't a list, hash on the back
        if list /== [] then
            _num fi_+ syshash(list) -> _num
        endif
    endif
enddefine;


define :inline lconstant HAS_SEQ_MATCH_HD(l);
    lblock lvars _x;
        iscompound(fast_front(l)->> _x) and _x!KEY == matchvar_key
        and _int(_x!MV_FLAGS) _bitst _:M_MV_SEQ_MATCH
    endlblock
enddefine;


    /*  Come here when list1 is a list pair or [] and list2 is a list pair
        whose hd is a sequence matchvar
    */
define lconstant Match_seq(list1, list2);
    lvars   list1, list2, mv, id, l, val, restr,
            pmvb = pop_matchvars_bound,
            (save_conv, save_matchvars) = fast_destpair(pmvb);  ;;; OK with []

    repeat
        _CHECKINTERRUPT;
        fast_destpair(list2) -> (mv, list2);
        mv!MV_IDENT -> id;
        if id and fast_lmember(id, pmvb) then
            ;;; already bound
            quitunless(islist(valof(id) ->> val));
            until null(val) do
                quitif(    null(list1)
                        or not(EQ(fast_destpair(list1) -> list1,
                                                fast_destpair(val) -> val))
                      ) (2)
            enduntil;
            pop_matchvars_bound -> pmvb;
            if mv!MV_RESTRICTION ->> restr then
                ;;; deal with restriction
                valof(id) -> val;
                if isident(restr) then idval(restr) -> restr endif;
                if isinteger(restr) then
                    ;;; check length
                    quitunless(listlength(val) == restr)
                else
                    ;;; restriction procedure
                    quitunless(restr(val) ->> val);
                    if _int(mv!MV_FLAGS) _bitst _:M_MV_CONV_P then
                        cons_assoc(val, id, fast_front(pmvb))
                                                    -> fast_front(pmvb)
                    endif
                endif
            endif

        else
            ;;; the variable was not previously bound
            if id then
                if pmvb == [] then
                    conspair([], conspair(id,[])) ->> pmvb
                                            -> pop_matchvars_bound
                else
                    conspair(id, fast_back(pmvb)) -> fast_back(pmvb)
                endif
            endif;

            mv!MV_RESTRICTION -> restr;
            if list2 == [] then
                unless restr then
                    if id then list1 -> valof(id) endif;
                    return(true)
                endunless
            else
                ;;; check that if there's an atom following the matchvar, it
                ;;; occurs in list1 too
                fast_front(list2) -> val;
                quitif((isnumber(val) or isstring(val) or isword(val))
                        and not(lmember_=(val,list1)));

                unless restr then
                    if id then [] ->> l -> valof(id) endif;
                    until EQ(list1, list2) do
                        if null(list1) then
                            if id then [] -> valof(id) endif;
                            sys_grbg_list(l);
                            quitloop(2)
                        endif;
                        fast_destpair(list1) -> (val, list1);
                        if id then
                            l nc_<> conspair(val,[]) ->> l -> valof(id)
                        endif
                    enduntil;
                    return(true)
                endunless;
            endif;

            ;;; deal with restriction
            if isident(restr) then idval(restr) -> restr endif;
            ;;; if there's nothing else in list2, rest of list1 must be right
            if list2 == [] then
                if isinteger(restr) then
                    quitunless(listlength(list1) == restr)
                else
                    quitunless(restr(list1) ->> val);
                    if _int(mv!MV_FLAGS) _bitst _:M_MV_CONV_P then
                        cons_assoc(val, id, fast_front(pmvb))
                                                    -> fast_front(pmvb)
                    endif
                endif;
                if id then list1 -> valof(id) endif;
                return(true)
            endif;

            [] -> l;
            if isinteger(restr) then
                while restr fi_> 0 do
                    if null(list1) then
                        if id then sys_grbg_list(l) endif;
                        quitloop(2)
                    endif;
                    fast_destpair(list1) -> (val, list1);
                    if id then l nc_<> conspair(val,[]) -> l endif;
                    restr fi_- 1 -> restr
                endwhile;
                if id then l -> valof(id) endif
            else
                ;;; restriction procedure
                repeat
                    if restr(l) ->> val then
                        if id then l -> valof(id) endif;
                        if EQ(list1, list2) then
                            if _int(mv!MV_FLAGS) _bitst _:M_MV_CONV_P then
                                cons_assoc(val, id, fast_front(pmvb))
                                                        -> fast_front(pmvb)
                            endif;
                            return(true)
                        endif
                    endif;
                    if null(list1) then
                        if id then [] -> valof(id) endif;
                        sys_grbg_list(l);
                        quitloop(2)
                    endif;
                    l nc_<> conspair(fast_destpair(list1) -> list1,[]) -> l
                endrepeat
            endif
        endif;

        ;;; matched a fixed number of elements
        unless ispair(list2) and HAS_SEQ_MATCH_HD(list2) then
            returnif(EQ(list1, list2)) (true);
            quitloop
        endunless;
        quitunless(islist(list1))
    endrepeat;

    ;;; failed
    if (pop_matchvars_bound ->> pmvb) /== [] then
        fast_destpair(pmvb) -> (list2, list1);
        until list1 == save_matchvars do
            sys_grbg_destpair(list1) -> (, list1)
        enduntil;
        if list1 == [] then
            sys_grbg_list(list2);
            sys_grbg_destpair(pmvb) -> (,);
            [] -> pop_matchvars_bound
        else
            list1 -> fast_back(pmvb);
            until list2 == save_conv do
                sys_grbg_destpair(list2) -> (, list2)
            enduntil;
            list2 -> fast_front(pmvb)
        endif
    endif;
    false
enddefine;      /* Match_seq */

define Eq__Pair(item, pair);
    lvars item, pair, ibk, pbk;

    if issimple(item) then
        false
    elseif item!KEY == pair_key then
        repeat
            returnif(item == pair) (true);
            _checkall();            ;;; check for interrupt/recursion overflow

            ;;; this would be a lot simpler if list pairs were a distinct
            ;;; data type!
            ;;; priority case is where testing two lists for equality
            item!P_BACK -> ibk;
            pair!P_BACK -> pbk;

            if ispair(ibk) and ispair(pbk) then
                ;;; straight lists
                returnunless(dup(EQ(item!P_FRONT, pair!P_FRONT)));
                unless () == SEQ_MATCH_RETURN then
                    ibk -> item, pbk -> pair;
                    nextloop
                endunless
            elseif isprocedure(ibk) and isboolean(item!P_FRONT) then
                ;;; item is dynamic
                if isprocedure(pbk) and isboolean(pair!P_FRONT) then
                    ;;; both dynamic
                    returnif(null(pair)) (null(item));
                    unless HAS_SEQ_MATCH_HD(pair) then
                        returnif(null(item)) (false);
                        returnunless(EQ(item!P_FRONT, pair!P_FRONT)) (false);
                        item!P_BACK -> item, pair!P_BACK -> pair;
                        nextloop
                    endunless
                else
                    ;;; item is dynamic, pair not
                    returnunless(pbk == [] or ispair(pbk)) (false);
                    unless HAS_SEQ_MATCH_HD(pair) then
                        returnif(null(item)) (false);
                        returnunless(EQ(item!P_FRONT, pair!P_FRONT)) (false);
                        if pbk == [] then
                            CHAIN_EQ(item!P_BACK, pbk)
                        else
                            item!P_BACK -> item, pbk -> pair;
                            nextloop
                        endif
                    endunless
                endif
            elseif isprocedure(pbk) and isboolean(pair!P_FRONT) then
                ;;; pair is dynamic, item not
                returnunless((ibk == [] or ispair(ibk))) (false);
                returnif(null(pair)) (false);
                returnunless(dup(EQ(item!P_FRONT, pair!P_FRONT)));
                unless () == SEQ_MATCH_RETURN then
                    chain(ibk, pair!P_BACK, Eq__Pair)
                endunless
            else
                ;;; no dynamic lists -- matching pairs
                returnunless(dup(EQ(item!P_FRONT, pair!P_FRONT)));
                if () == SEQ_MATCH_RETURN then
                    returnunless((ibk == [] or ispair(ibk))
                                 and (pbk == [] or ispair(pbk))) (false);
                else
                    CHAIN_EQ(ibk, pbk)
                endif
            endif;

            chain(item, pair, Match_seq)
        endrepeat

    elseif item == [] then
        if isprocedure(pair!P_BACK) and isboolean(pair!P_FRONT) then
            ;;; pair is dynamic
            returnif(null(pair)) (true);
            returnunless(HAS_SEQ_MATCH_HD(pair)) (false)
        else
            returnunless(HAS_SEQ_MATCH_HD(pair)
                         and ((pair!P_BACK->>pbk) == [] or ispair(pbk))) (false)
        endif;
        chain(item, pair, Match_seq)
    elseif item!KEY!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(pair, item, item!KEY!K_SYS_EQUALS)
    else
        false
    endif
enddefine;      /* Eq__Pair */


constant
    pair_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_RECORD _biset _:M_K_COPY,
                                ;;; K_FLAGS
        _:GCTYPE_FULLREC2,      ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "pair",                 ;;; K_DATAWORD
        [full full],            ;;; K_SPEC
        ispair,                 ;;; K_RECOGNISER
        WREF Pair_apply,        ;;; K_APPLY
        Eq__Pair,               ;;; K_SYS_EQUALS
        WREF Eq__Pair,          ;;; K_EQUALS
        Pair_print,             ;;; K_SYS_PRINT
        WREF Pair_print,        ;;; K_PRINT
        WREF Pair_hash,         ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_PAIR,     ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct PAIR)++,      ;;; K_RECSIZE_R
        conspair,               ;;; K_CONS_R
        destpair,               ;;; K_DEST_R
        {^front ^back},         ;;; K_ACCESS_R
        %};


;;; --- NIL KEY ----------------------------------------------------------

define lconstant Nil_print() with_nargs 1;
    ->, cucharout(`[`), cucharout(`]`)
enddefine;

define lconstant Eq__Nil() with_nargs 2;
    lvars item, _key;
    () -> (item, );             ;;; erase the []
    if item == [] then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _key) == pair_key and isprocedure(item!P_BACK)
    and isboolean(item!P_FRONT) then
        null(item)
    elseif _key!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(item, [], _key!K_SYS_EQUALS)
    else
        false
    endif
enddefine;

constant
    nil_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL,          ;;; K_FLAGS
        _:GCTYPE_NONE,          ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "nil",                  ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        nonop ==(%[]%),         ;;; K_RECOGNISER
        WREF Pair_apply,        ;;; K_APPLY
        Eq__Nil,                ;;; K_SYS_EQUALS
        WREF Eq__Nil,           ;;; K_EQUALS
        Nil_print,              ;;; K_SYS_PRINT
        WREF Nil_print,         ;;; K_PRINT
        WREF Unique_hash,       ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_NIL,      ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %};


;;; --- SPECIAL THINGS AT THE END --------------------------------------------
;;; N.B. Execute-level code that references these things afterwards
;;; will not get the normal values (i.e. so leave them as the last things
;;; in the file).

define sysconslist() -> list;
    lvars list = [], item;
    until (->> item) == popstackmark do
        conspair(item, list) -> list
    enduntil
enddefine;

define sysconslist_onto() -> list with_nargs 1;
    lvars list = Checkr_list_tl(), item;
    until (->> item) == popstackmark do
        conspair(item, list) -> list
    enduntil
enddefine;


constant
    nil = [],       ;;; set up by poplink
    popstackmark = struct STACKMARK =>> {% _0, stackmark_key%},
    ;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  4 1996
        Changed to use EQ macro
--- John Gibson, Dec 11 1995
        Changes to Eq__Pair for dealing with sequence matchvars.
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs
--- John Gibson, Aug  4 1993
        Changed 'MORE THAN 1 RESULT FROM DYNAMIC LIST PROCEDURE' mishap in
        null so that all results returned are given in INVOLVING list
--- John Gibson, Sep  4 1991
        Fixed -null- to cope with recursive expansion of dynamic list
        through call to dynamic list procedure
--- John Gibson, Sep  2 1991
        Fixed problem in -null-
--- John Gibson, May  6 1991
        [] now set up by poplink (so constants can be initialised to it)
--- John Gibson, Sep 12 1990
        Some tidying up
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Aug 30 1989
        Improved non-fast versions of -front-, -back- and -destpair-
--- John Gibson, Jun 17 1989
        Pairs only get added to -free*pairs- if not in system
--- John Gibson, Jan 25 1989
        Moved some things to end of file so it compiles with new version
        of popc (see comment above).
--- John Gibson, Aug 27 1988
        Added -sysconslist_onto-.
--- John Gibson, Apr  9 1988
        Basic stuff into this file from old lists.p
--- John Gibson, Apr  7 1988
        Moved -conslist- to conslist.p, -subscrl- to subscrl.p, etc
--- John Gibson, Mar 27 1988
        Moved list assoc stuff to list_assoc.p
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
 */
