/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/random.p
 > Purpose:
 > Author:          John Gibson, Jun 11 1987 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;--------------------- RANDOM NUMBERS --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        procedure (sys_real_time, Sys$-Float_random, Sys$-Bigint_random),
        _posword_mul_high,
    ;

;;; -----------------------------------------------------------------------

section $-Sys => ranseed, random0, random;

global vars
    ranseed = false;            ;;; initialised first time used or by user


    ;;; generate next seed from ranseed, and return as a positive integer
define Random_genseed();
    lvars _seed;
    _int(ranseed) _mult _524269 _add _32749 -> _seed;
    _pint(_seed) -> ranseed;
#_IF ##(i)[_1|w] /= _1
    ;;; truncate to a signed int for actual use
    lstackmem -i _tmp;
    _seed -> _tmp!(-i);
    _tmp!(-i) -> _seed;
#_ENDIF
    if _neg(_seed) then _logcom(_seed) else _seed endif
enddefine;

define lconstant Random(n, want_0);
    lvars n, want_0, _n;
    lconstant _SIMPLE_LIM = _shift(_1, _:RANSEED_BITS _sub _7);

    ;;; ensure ranseed set up
    unless ranseed then
        unless isinteger(sys_real_time() ->> ranseed) then
            ranseed && #_< 1<<POPINT_BITS-1 >_# -> ranseed
        endunless
    endunless;

    if isinteger(n) then
        if (_int(n) ->> _n) _slteq _0 then
            goto ERR
        elseif _n _sgreq _SIMPLE_LIM then
            ;;; generate it as a bigint
            if testdef biginteger_key then
                BGWEAK Bigint_op_bgint_int(n, BGWEAK Bigint_random)
            else
                mishap(n, 1, 'RANDOM NUMBER GENERATION REQUIRES BIGINTEGERS (not loaded)')
            endif
        else
            ;;; return the overflow of seed * _n from RANSEED_BITS
            _posword_mul_high(Random_genseed(), _n) -> _n ;
            _pint(_n)
        endif
        ;;; goto RETURN_INT
    elseif issimple(n) then
        goto DEC
    else
        go_on _pint(n!KEY!K_NUMBER_TYPE) to
        ;;; int  bigint  ratio  dec  ddec
            ERR  BGINT    ERR   ERR  DEC
        else ERR;

        BGINT:
            if BGWEAK Bigint_neg(n) then goto ERR endif;
            BGWEAK Bigint_random(n);
            goto RETURN_INT;

        DEC:
            return(FLWEAK Float_random(n, want_0));

        ERR:
            mishap(n, 1, 'INTEGER OR (D)DECIMAL > 0 NEEDED')
    endif;

RETURN_INT:
    unless want_0 then
        if dup() == 0 then ->, n endif      ;;; return n instead of 0
    endunless
enddefine;

define random0() with_nargs 1;
    Random(true)        ;;; want 0 <= result < arg
enddefine;

define random() with_nargs 1;
    Random(false)       ;;; want 0 < result <= arg
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  1 1995
        Changed Random_genseed to return _logcom of neg seed instead of
        transferring sign bit to bit 0
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
 */
