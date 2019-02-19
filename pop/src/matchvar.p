/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/matchvar.p
 > Purpose:         Matchvar records
 > Author:          John Gibson, Dec  9 1995
 > Documentation:   REF * RECORDS
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'matchvar.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (Sys$-Eq__Pair),
        matchvar_key
    ;

vars
        pop_matchvars_bound
    ;

;;; --------------------------------------------------------------------

section $-Sys => consmatchvar, destmatchvar, matchvar_key;

define consmatchvar(name, id, restriction, flags) -> mv;
    lvars mv, name, id, restriction, flags;

    Check_integer(flags, 0);
    if id then
        unless isident(id) or isword(id) then
            mishap(id, 1, 'IDENTIFIER OR WORD NEEDED')
        endunless
    else
        flags fi_&&~~ M_MV_CONV_P -> flags
    endif;
    if restriction
    and not(   isident(restriction)
            or isprocedure(restriction)
            or isinteger(restriction) and restriction fi_>= 0)
    then
        mishap(restriction, 1, 'INVALID MATCHVAR RESTRICTION ARG')
    endif;

    Get_record(matchvar_key) -> mv;
    name        -> mv!MV_NAME;
    id          -> mv!MV_IDENT;
    restriction -> mv!MV_RESTRICTION;
    flags       -> mv!MV_FLAGS
enddefine;

define destmatchvar(mv);
    lvars mv;
    unless iscompound(mv) and mv!KEY == matchvar_key then
        mishap(mv, 1, 'MATCHVAR NEEDED')
    endunless;
    mv!MV_NAME, mv!MV_IDENT, mv!MV_RESTRICTION, mv!MV_FLAGS
enddefine;

define lconstant Eq__Matchvar(item, mv);
    lvars item, mv, id, restr, val, pmvb;
    if _int(mv!MV_FLAGS) _bitst _:M_MV_SEQ_MATCH then
        ;;; can only match inside a list
        _caller_sp_flush()!SF_OWNER == Eq__Pair and SEQ_MATCH_RETURN
    else
        pop_matchvars_bound -> pmvb;
        mv!MV_IDENT -> id;
        if id and fast_lmember(id, pmvb) then
            ;;; already bound -- check value equal
            returnunless(item = valof(id)) (false);
            false -> id
        endif;
        ;;; check restriction
        if mv!MV_RESTRICTION ->> restr then
            if isident(restr) then idval(restr) -> restr endif;
            if isinteger(restr) then
                returnunless(length(item) == restr) (false)
            else
                returnunless(restr(item) ->> val) (false);
                if _int(mv!MV_FLAGS) _bitst _:M_MV_CONV_P then
                    if pmvb == [] then
                        conspair([], []) ->> pmvb -> pop_matchvars_bound
                    endif;
                    cons_assoc(val, mv!MV_IDENT, fast_front(pmvb))
                                                    -> fast_front(pmvb)
                endif
            endif
        endif;
        if id then
            item -> valof(id);
            if pmvb == [] then
                conspair([], []) ->> pmvb -> pop_matchvars_bound
            endif;
            conspair(id, fast_back(pmvb)) -> fast_back(pmvb)
        endif;
        true
    endif
enddefine;

define lconstant Matchvar_print(mv);
    lvars   mv, id, name, r, _flags = _int(mv!MV_FLAGS),
            seq_match = _flags _bitst _:M_MV_SEQ_MATCH;
    if mv!MV_IDENT ->> id then
        mv!MV_NAME -> name;
        printf(name,if seq_match then
                        if isword(id) then '=??"%p"' else '=??%p' endif
                    else
                        if isword(id) then '=?"%p"' else '=?%p' endif
                    endif);
    else
        printf(if seq_match then '=**' else '=*' endif)
    endif;
    if mv!MV_RESTRICTION ->> r then
        if isident(r) and isprocedure(idval(r)) then
            idval(r) -> r
        endif;
        if isprocedure(r) and pdprops(r) then pdprops(r) -> r endif;
        printf(r, if _flags _bitst _:M_MV_CONV_P then '#%p' else ':%p' endif)
    endif
enddefine;

constant
    matchvar_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD,   ;;; K_FLAGS
        _:GCTYPE_FULLREC,       ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "matchvar",             ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        procedure() with_nargs 1;
            datakey() == matchvar_key
        endprocedure,           ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Matchvar,           ;;; K_SYS_EQUALS
        WREF Eq__Matchvar,      ;;; K_EQUALS
        Matchvar_print,         ;;; K_SYS_PRINT
        WREF Matchvar_print,    ;;; K_PRINT
        WREF Fullrec1_hash,     ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct MATCH_VAR)++, ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};

endsection;
