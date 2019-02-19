/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/integer.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988 (see revisions)
 */


;;; --------------------- INTEGER KEY ----------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'gctypes.ph'


;;; ------------------------------------------------------------------------

section $-Sys => isinteger, isintegral, integer_key;

lconstant
    integer_ms          = 'INTEGER NEEDED',
    integer_idstring    = ':type-integer',
    intrange_idstring   = ':type-intrange',
    ;

define Check_integer(item, _minvalue);
    lvars item, _minvalue;
    unless isinteger(item) then
        mishap(item, 1, integer_ms, integer_idstring)
    elseif _minvalue and item fi_< _minvalue then
        mishap(item, 1, ('INTEGER >= ' sys_>< _minvalue) sys_>< ' NEEDED',
                                                intrange_idstring)
    endunless;
enddefine;

define Check_integer_range(item, _minvalue, _maxvalue);
    lvars item, _minvalue, _maxvalue;
    unless isinteger(item) then
        mishap(item, 1, integer_ms, integer_idstring)
    elseif item fi_< _minvalue or item fi_> _maxvalue then
        mishap(item, 1, 'INTEGER BETWEEN ' sys_>< _minvalue sys_>< ' AND '
                    sys_>< _maxvalue sys_>< ' NEEDED', intrange_idstring)
    endunless;
enddefine;

define Check_integral(item);
    lvars item;
    unless isinteger(item)
    or (iscompound(item) and item!KEY == weakref biginteger_key) then
        mishap(item, 1, '(BIG)INTEGER NEEDED', ':type-integral')
    endunless
enddefine;

define isinteger() with_nargs 1;
    _isinteger()
enddefine;

define isintegral(item);
    lvars item;
    if iscompound(item) then
        item!KEY == weakref biginteger_key
    else
        isinteger(item)
    endif
enddefine;

define Eq__Integer(item, integer);
    lvars item, integer, _key;
    if isinteger(item) then
        return(item == integer)
    elseif issimple(item) then
        FLWEAK _pf_dfloat_dec(item, FLWEAK _dfop1)
    elseif (item!KEY ->> _key) == weakref ddecimal_key then
        FLWEAK _pf_dfloat_ddec(item, FLWEAK _dfop1)
    elseif _key!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(integer, item, _key!K_SYS_EQUALS)
    elseunless _key == weakref complex_key
    and CXWEAK Complex_is_float_real(item, FLWEAK _dfop1) then
        return(false)
    endif;

    ;;; drop thru for float comparison
    FLWEAK _pf_dfloat_int(_int(integer), FLWEAK _dfop2);
    FLWEAK _pfeq(FLWEAK _dfop1, FLWEAK _dfop2)
enddefine;

define Print_mcint(_n, _radix);
    lvars _n, _dig, _radix;
    if _neg(_n) then
        cucharout(`-`);
        ;;; this avoids overflow on largest -ve int
        _negate(_negate(_n _div _radix) -> _n) -> _dig
    else
        _n _div _radix -> _n -> _dig
    endif;
    if _nonzero(_n) then Print_mcint(_n, _radix) endif;
    _pint(_dig) -> _dig;
    cucharout(if _dig fi_> 9 then `7` else `0` endif fi_+ _dig)
enddefine;

define Print_int() with_nargs 1;
    chain(_int(), _int(pop_pr_radix), Print_mcint)
enddefine;

constant
    integer_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL,          ;;; K_FLAGS
        _:GCTYPE_NONE,          ;;; K_GC_TYPE
        0,                      ;;; K_GET_SIZE

        "integer",              ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isinteger,              ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Integer,            ;;; K_SYS_EQUALS
        WREF Eq__Integer,       ;;; K_EQUALS
        Print_int,              ;;; K_SYS_PRINT
        WREF Print_int,         ;;; K_PRINT
        WREF identfn,           ;;; K_HASH

        _:NUMTYPE_INTEGER,      ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_CONST,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_SIMPLE,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %};

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 12 1996
        Added some mishap id-strings
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Sep  4 1992
        Made extern type of integer_key be EXTERN_TYPE_SIMPLE
--- John Gibson, Jan  2 1992
        Replaced M_K_S*IMPLE flag in key with M_K_SPECIAL
--- John Gibson, Oct 19 1990
        Rewrote integer printing routines so as to make -Print_mcint-
        available for use in -printf-.
--- John Gibson, Mar 14 1990
        Change to key layout.
 */
