/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/float_fields.p
 > Purpose:
 > Author:          John Gibson, Feb 28 1988 (see revisions)
 */

;;; ------- PROCEDURES TO ACCESS FLOAT FIELDS IN RECORDS & VECTORS -----------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        _pf_extend_s_to_d, _pf_check_d, _pf_round_d_to_s
    ;

global vars
        Sys$- _inhibit_gc
    ;


;;; -------------------------------------------------------------------------

section $-Sys$-Fld;

lconstant macro (
    REAL_NUM_MESS = 'REAL NUMBER NEEDED',
    EX_S_INF_MESS = ['externally-generated "sfloat" field or function result is Inf/NaN',
                     'field-sftoreal'],
    EX_D_INF_MESS = ['externally-generated "dfloat" field or function result is Inf/NaN',
                     'field-dftoreal'],
    );

    /*  Access/update single float field (pdprops is the value spec)
    */
define Float_val_s() with_nargs 1 with_props decimal;
    lvars _df = _dfresult;
    ;;; arg is the address of the field
    ()!(sfloat) -> _df!(sfloat);        ;;; move field to _dfresult
    if _pf_extend_s_to_d(_df) then      ;;; extend to double
        chain(_:ARGTYPE_DECIMAL, _df, Consdecimal)
    else
        ;;; IEEE infinity or Not a Number
        Float_overflow(0, EX_S_INF_MESS)
    endif
enddefine;
;;;
define updaterof Float_val_s(n, _addr);
    lvars n, _addr;
    dlocal _inhibit_gc = true;  ;;; Dfloating a ratio creates garbage...
    if _neg(Dfloat(n, _dfresult)) then
        mishap(n, 1, REAL_NUM_MESS, 'field-realtosf:type-real')
    elseunless _pf_round_d_to_s(_dfresult) then
        Float_overflow(n, 1, 'converting double into "sfloat" field',
                                    'field-realtosf')
    else
        ;;; assign field into _addr
        _dfresult!(sfloat) -> _addr!(sfloat)
    endif
enddefine;

    /*  Access/update double float field (pdprops is the value spec)
    */
define Float_val_d(_addr) with_props ddecimal;
    lvars _df = _dfresult, _addr;
    ;;; arg is the address of the field
    _addr!(int)++ -> _addr -> _df!(int)[_0];    ;;; move first to _dfresult
    _addr!(int)            -> _df!(int)[_1];    ;;; and second
    if _pf_check_d(_df) then
        chain(_:ARGTYPE_DDECIMAL, _df, Consdecimal)
    else
        ;;; IEEE infinity or Not a Number
        Float_overflow(0, EX_D_INF_MESS)
    endif
enddefine;
;;;
define updaterof Float_val_d(n, _addr);
    lvars n, _df = _dfresult, _addr;
    dlocal _inhibit_gc = true;  ;;; Dfloating a ratio creates garbage...
    if _neg(Dfloat(n, _df)) then
        mishap(n, 1, REAL_NUM_MESS, 'field-realtodf:type-real')
    else
        ;;; assign field into _addr
        _df!(int)++ -> _df -> _addr!(int)++ -> _addr;
        _df!(int)          -> _addr!(int)
    endif
enddefine;

    /*  Access C-language "float" result
    */
#_IF DEF C_FLOAT_RESULT_SINGLE

    /*  Single returned as single
    */
constant procedure Float_val_s_C = Float_val_s;

#_ELSE

    /*  Single returned as double (pdprops is the value spec)
    */
define Float_val_s_C(_addr) with_props decimal;
    lvars _addr;
    ;;; arg is the address of the field
    if _pf_check_d(_addr) then
        chain(_:ARGTYPE_DECIMAL, _addr, Consdecimal)
    else
        ;;; IEEE infinity or Not a Number
        Float_overflow(0, EX_S_INF_MESS)
    endif
enddefine;

#_ENDIF

endsection;     /* $-Sys$-Fld */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  1 1996
        Added mishap id-strings
--- John Gibson, Mar 22 1995
        Corrected access types and added pdprops "decimal" or "ddecimal"
        to indicate access procedure value type
--- John Gibson, Apr  2 1991
        Changed name of procedure to access C "float" result and name of
        flag to indicate that it's single to C_FLOAT_RESULT_SINGLE.
--- John Gibson, Mar 25 1991
        Accessors changed to check for externally-generated bad values
--- John Gibson, May 29 1990
        Added -Float_val_s_C-
--- John Gibson, Mar 14 1989
        Reorganised these procedures with names so that POPC can refer to
        them.
--- John Gibson, Aug  4 1988
        Put _inhibit_gc true when updating a float field (since
        flaoting a ratio creates garbage).
--- John Gibson, Feb 29 1988
        Moved from fields.p
 */
