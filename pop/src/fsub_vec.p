/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/fsub_vec.p
 > Purpose:
 > Author:          Aled Morris Aug 13 1987 (see revisions)
 > Documentation:   REF *FASTPROCS
 */

;;; ---------------- FAST VECTOR ACCESS PROCEDURES -------------------------

#_INCLUDE 'declare.ph'


;;; -----------------------------------------------------------------------

define fsub_sb(_i, bvec);
    lvars _i, bvec;
    _pint(bvec!V_SBYTES[_int(_i) _sub _1]);
enddefine;
;;;
define updaterof fsub_sb(_b, _i, bvec);
    lvars _b, _i, bvec, _v;
    unless isinteger(_b) and (_int(_b) ->> _v) _sub _-128 _lteq _255 then
        mishap(_b, 1, 'INTEGER -128 TO 127 NEEDED');
    endunless;
    _v -> bvec!V_SBYTES[_int(_i) _sub _1];
enddefine;

define fsub_b() with_nargs 2;
    _subss();
enddefine;
;;;
define updaterof fsub_b(_i, bvec) with_nargs 3;
    lvars _i, bvec;
    Sys$-Checkr_byte() -> _subss(_i, bvec);
enddefine;

define fsub_ss(_i, svec);
    lvars _i, svec;
    _pint(svec!V_SSHORTS[_int(_i) _sub _1]);
enddefine;
;;;
define updaterof fsub_ss(_s, _i, svec);
    lvars _s, _i, svec, _v;
    unless isinteger(_s) and (_int(_s) ->> _v) _sub _MOST_NEGATIVE_SIGNED(s)
    _lteq _MOST_POSITIVE_UNSIGNED(s) then
        mishap(_s, 1, 'INTEGER -32768 TO 32767 NEEDED');
    endunless;
    _v -> svec!V_SSHORTS[_int(_i) _sub _1];
enddefine;

define fsub_s(_i, svec);
    lvars _i, svec;
    _pint(svec!V_SHORTS[_int(_i) _sub _1]);
enddefine;
;;;
define updaterof fsub_s(_s, _i, svec);
    lvars _s, _i, svec, _v;
    unless isinteger(_s) and _int(_s) _lteq _MOST_POSITIVE_UNSIGNED(s) then
        mishap(_s, 1, 'INTEGER 0 TO 65535 NEEDED');
    endunless;
    _int(_s) -> svec!V_SHORTS[_int(_i) _sub _1];
enddefine;

define fsub_si(_i, ivec);
    lvars _i, ivec;
#_IF EXCEEDS_POPINT_SIGNED(i)
    Sys$-Sint_->_pint
#_ELSE
    _pint
#_ENDIF (ivec!V_SINTS[_int(_i) _sub _1])
enddefine;
;;;
define updaterof fsub_si(_I, _i, ivec) with_nargs 3;
    lvars _I, _i, ivec, _v;
#_IF EXCEEDS_POPINT_SIGNED(i)
    Sys$-Pint_->_sint
#_ELSE
    Sys$-Simpint_->_sint
#_ENDIF (_I, _MOST_POSITIVE_SIGNED(i)) -> ivec!V_SINTS[_int(_i) _sub _1]
enddefine;

define fsub_i(_i, ivec) with_nargs 2;
    lvars _i, ivec;
#_IF EXCEEDS_POPINT_UNSIGNED(i)
    Sys$-Uint_->_pint
#_ELSE
    _pint
#_ENDIF (ivec!V_INTS[_int(_i) _sub _1])
enddefine;
;;;
define updaterof fsub_i(_I, _i, ivec);
    lvars _I, _i, ivec, _v;
#_IF EXCEEDS_POPINT_UNSIGNED(i)
    Sys$-Pint_->_uint
#_ELSE
    Sys$-Simpint_->_uint
#_ENDIF (_I, _MOST_POSITIVE_UNSIGNED(i)) -> ivec!V_INTS[_int(_i) _sub _1]
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  5 1995
        Changed int procedures to work in a 64-bit environment
--- Roger Evans, Apr 27 1988
        fixed incorrect upper bound in updaterof fsub_ss
--- John Gibson, Mar 28 1988
        Moved out of vectors.p
 */
