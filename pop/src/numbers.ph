/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/numbers.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;---------------- DECLARATIONS FOR NUMBERS ----------------------------------



;;; --- GENERAL -------------------------------------------------------

lconstant macro (

    ;;; number types
    NUMTYPE_INTEGER     = 1,
    NUMTYPE_BIGINTEGER  = 2,
    NUMTYPE_RATIO       = 3,
    NUMTYPE_DECIMAL     = 4,
    NUMTYPE_DDECIMAL    = 5,
    NUMTYPE_COMPLEX     = 6,
;;; NUMTYPE_NON_NUMBER  = 7,    ;;; defined in declare.ph

    ;;; argument specifier bits for Consdecimal
    ARGTYPE_DECIMAL     = 2:001,
    ARGTYPE_RATIONAL    = 2:010,
    ARGTYPE_DDECIMAL    = 2:100,

    ;;; subscripts within arith operation vectors
    OP_+        = 1,
    OP_-        = 2,
    OP_*        = 3,
    OP_/        = 4,
    OP_//       = 5,
    OP_negate   = 6,
    OP_abs      = 7,
    OP_fracof   = 8,
    OP_intof    = 9,
    OP_round    = 10,
    OP_sign     = 11,

    OP_VEC_LEN  = 11,

    );


;;; --- BIGINTEGERS ---------------------------------------------------------

deftype SL = BIGINT_SPEC;   ;;; biginteger unsigned slice

struct BIGINT
  { word    BGI_LENGTH;
    full    KEY;
>-> -SL     BGI_SLICES[];
  };


lconstant macro (
    SLICE_BITS      = _pint(##(1)[_1|SL]), ;;; - 1,
    SLICE_MASK      = - 1,

    ;;; for random number generation
    RANSEED_BITS    = _pint(##(1)[_1|int]) - 1,

    ;;; number of slices required to hold a pop integer
    POPINT_SLICES   = (POPINT_BITS+SLICE_BITS-1) div SLICE_BITS,
    );

define :inline lconstant SAVEWORKBGI(work, _save1, _save2);
#_IF POPINT_SLICES == 1 \n
    lvars _save1 = work!BGI_SLICES;
#_ELSE
    lvars _save1 = work!V_WORDS, _save2 = work!BGI_LENGTH;
#_ENDIF
enddefine;

define :inline lconstant RESTWORKBGI(work, _save1, _save2);
#_IF POPINT_SLICES == 1 \n
    _save1 -> work!BGI_SLICES;
#_ELSE
    _save1 -> work!V_WORDS; _save2 -> work!BGI_LENGTH;
#_ENDIF
enddefine;


;;; --- RATIOS ---------------------------------------------------------

struct RATIO
  { full    RAT_NUMER,
            KEY,
>->         RAT_DENOM;
  };


;;; --- COMPLEX ---------------------------------------------------------

struct COMPLEX
  { full    CX_REAL,
            KEY,
>->         CX_IMAG;
  };


;;; --- WEAKREF MACROS ON NUMBER KEYS ---------------------------------

lconstant macro (
    BGWEAK      = [weakref[biginteger_key]],
    RTWEAK      = [weakref[ratio_key]],
    CXWEAK      = [weakref[complex_key]],
    FLWEAK      = [weakref[decimal_key]],
    );

;;; --- CONSTANTS, VARS ------------------------------------------------

weak global constant
        procedure (intof, fracof, round, isrational, isdecimal),

        3 **,

        ratio_key, biginteger_key, complex_key,

        _pmult_testovf,
        _pf_sfloat_dec, _pf_dfloat_int, _pf_dfloat_dec, _pf_dfloat_ddec,
        _pfadd, _pfsub,  _pfmult, _pfdiv, _pfqrem, _pfnegate, _pfabs,
        _pf_intof, _pfmodf, _pf_expof, _pfcopy,
        _pfeq, _pfzero, _pfneg, _pfsgr, _pfsgreq,
    ;

weak global vars
        popradians, popdprecision, pop_pr_radix
    ;


section $-Sys;

weak global constant
        procedure (Check_integral, Clawback_num, Dfloat, Consdecimal,
        Complex_is_float_real, Complex_op_cmplx_real,
        Cxf_op_cmplx, Cxf_op_real, Get_complex, Get_df_complex
        Cxf_add, Cxf_sub, Cxf_mult, Cxf_div, Cxf_sqrt, Cxf_log, Cxf_exp,
        Cxf_copy, Float_overflow, Get_bigint,
        Bigint_op_int_int, Bigint_op_int_bgint, Bigint_op_bgint_int,
        Bigint_neg, Bigint_logical, Bigint_<<,
        Bigint_>>, Bigint_>>_into, Bigint_leastbit,
        Pint_to_bigint, Bigint_copy, Bigint_copy_len, Bigint_return,
        ),

        Bigint_ops, work_bigint1, work_bigint2, double_float_0,

        ;;; working double floats
        _dfop1, _dfop2, _dfresult, _dfradix, _dffrac,
    ;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1995
        Added SAVEWORKBGI and RESTWORKBGI
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- Ian Rogers, Jan 10 1990 - added _pf_sfloat_dec
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Mar 23 1989
        Number keys out of section Sys.
--- John Gibson, Feb  3 1989
        Made perm declarations 'weak ... '
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
--- John Gibson, Sep 20 1987
        Removed declaration for _pf_cvt_to_decimal
--- John Gibson, Sep 15 1987
        Replaced macro SL with deftype SL = BIGINT_SPEC, replaced sSL
        with -SL.
 */
