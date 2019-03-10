/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/move_bytes.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF * DATA
 */

;;; ------------ MANIPULATING BYTES OF NON-FULL VECTORS -------------------

#_INCLUDE 'declare.ph'


;;; ----------------------------------------------------------------------

section $-Sys => move_bytes, set_bytes;

    /*  Move a range of bytes from one structure to another
    */
define lconstant Check_byterange(_bsub, struc, _nbytes);
    lvars struc, _key, _len, _bsub, _nbytes;
    lconstant invms = 'INVALID STRUCTURE FOR set/move_bytes';

    Check_integer(_bsub, 1);
    _int(_bsub) _sub _1 -> _bsub;

    unless iscompound(struc) then
        mishap(struc, 1, invms)
    elseunless (struc!KEY ->> _key)!K_FLAGS _bitst _:M_K_BYTE_ACCESS then
        ;;; allow external pointer (but can't check size)
        returnif(_key!K_FLAGS _bitst _:M_K_EXTERN_PTR)
                                    (struc!XP_PTR@(b)[_bsub]);
        mishap(struc, 1, invms)
    endunless;

    ;;; Byte accessible structure

    if _key!K_FLAGS _bitst _:M_K_VECTOR then
        struc!V_LENGTH -> _len;
        go_on _pint(_key!K_FIELD_CODE_V) to
        ;;;   1       2       3      4
            CHECK  l_SHORT l_WORD l_DOUBLE
        else l_BIT;     ;;; i.e. if negative bit size

        l_SHORT:    ##(b)[_len|s] -> _len;  goto CHECK;
        l_WORD:     ##(b)[_len|w] -> _len;  goto CHECK;
        l_DOUBLE:   ##(b)[_len|d] -> _len;  goto CHECK;
        l_BIT:      ##(b){ _negate(_key!K_FIELD_CODE_V) _mult _len | 1.r}
                                  -> _len;  goto CHECK;

    else
        ##(b){ fast_apply(struc, _key!K_GET_SIZE)
                        _sub @@(struct POPREC1)++ | w} -> _len
    endif;

CHECK:
    unless _bsub _add _nbytes _lteq _len then
        mishap(_pint(_bsub _add _1), struc, _pint(_nbytes), 3,
                            'BYTE RANGE EXCEEDS STRUCTURE SIZE')
    endunless;
    struc@V_BYTES[_bsub]
enddefine;

define move_bytes(_bsub1, struc1, _bsub2, struc2, _nbytes);
    lvars struc1, struc2, _bsub1, _bsub2, _nbytes;
    Check_integer(_nbytes, 0);
    _int(_nbytes) -> _nbytes;
    Check_byterange(_bsub1, struc1, _nbytes) -> _bsub1;
    Check_byterange(_bsub2, struc2, _nbytes) -> _bsub2;
    _bmove(@@(b)[_nbytes], _bsub1, _bsub2) ->
enddefine;

define set_bytes(_bsub, struc, _nbytes) with_nargs 4;
    lvars struc, _bsub, _nbytes;
    Check_integer(_nbytes, 0);
    _int(_nbytes) -> _nbytes;
    Check_byterange(_bsub, struc, _nbytes) -> _bsub;
    _bfill(_int(Checkr_byte()), @@(b)[_nbytes], _bsub)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 26 1994
        Changed both procedures to allow struc arg(s) to be external pointers
--- John Gibson, Nov  5 1992
        Fixed Check_byterange to check _nbytes arg against vector length
        instead of total structure size
--- John Gibson, Mar 21 1988
        Moved out of data.p
 */
