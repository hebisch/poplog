/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/mathlib.ph
 > Purpose:
 > Author:          John Gibson, Jan 21 1988 (see revisions)
 */

;;; ----------------- MATH, TRIG LIBRARY FUNCTIONS ---------------------------

define lconstant macro _EXTERN_MATH_NAME;
    lconstant prefix =  #_IF DEF VMS and DEF ALPHA      "'mth$g'"
                        #_ELSEIF DEF VMS                "'mth$d'"
                        #_ELSEIF DEF ANSI_C or DEF UNIX "''"
                        #_ELSE_ERROR
                        #_ENDIF;

    [ _extern % prefix<>readitem() %]
enddefine;

lconstant macro (

    ;;; math and trig library functions
    _f_SQRT     = _EXTERN_MATH_NAME sqrt,
    _f_LOG      = _EXTERN_MATH_NAME log,
    _f_EXP      = _EXTERN_MATH_NAME exp,
    _f_SIN      = _EXTERN_MATH_NAME sin,
    _f_COS      = _EXTERN_MATH_NAME cos,
    _f_TAN      = _EXTERN_MATH_NAME tan,
    _f_ASIN     = _EXTERN_MATH_NAME asin,
    _f_ACOS     = _EXTERN_MATH_NAME acos,
    _f_ATAN     = _EXTERN_MATH_NAME atan,
    _f_ATAN2    = _EXTERN_MATH_NAME atan2,
    _f_SINH     = _EXTERN_MATH_NAME sinh,
    _f_COSH     = _EXTERN_MATH_NAME cosh,
    _f_TANH     = _EXTERN_MATH_NAME tanh,

    ;;; subscripts within complex trig operation vectors
    TRIGOP_sin  = 1,
    TRIGOP_cos  = 2,
    TRIGOP_tan  = 3,
    TRIGOP_sinh = 4,
    TRIGOP_cosh = 5,
    TRIGOP_tanh = 6,
    TRIGOP_VEC_LEN  = 6,

    IFNOT_ERR   = [unless () then goto ERR endunless],
);


define :inline lconstant _MATH1(_dfaddr, _func);
    _nonzero(_extern __pop_math_1(_dfaddr, _func))
enddefine;

define :inline lconstant _MATH2(_dfaddr, _dfaddr2, _func);
    _nonzero(_extern __pop_math_2(_dfaddr, _dfaddr2, _func))
enddefine;

constant
        Sys$- _pi_/_180
    ;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Added _MATH1/2 macros to replace _m*ath1/2 subroutines
--- John Gibson, Oct 14 1994
        Revised definitions of _f_name functions to accomodate VMS ALPHA
        use of 'g' floats rather than 'd'.
--- Robert John Duncan, Apr 11 1994
        Unix-style maths functions are available from any ANSI C compiler
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
 */
