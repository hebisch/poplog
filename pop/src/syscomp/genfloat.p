/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.68000/src/syscomp/genfloat.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                  FLOATING-POINT GENERATION (IEEE FORMATS)

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

lconstant macro (
    S_EXP_BITS  = 8,
    S_EXCESS    = 126,
    S_SIG_BITS  = 24,
    D_EXP_BITS  = 11,
    D_EXCESS    = 1022,
    D_SIG_BITS  = 53,
);

    /*  Generate int value for a machine single-float = signif * 2**expo
    */
define s_float_val(signif, expo);
    lvars signif, expo, signbit, len, lo16, hi7;
    returnif(signif == 0) (syspop\:_int(0));
    if signif < 0 then 1 else 0 endif -> signbit;
    abs(signif) -> signif;
    integer_length(signif) -> len;
    expo + len + S_EXCESS -> expo;
    (signif << (S_SIG_BITS-len)) -> signif;
    syspop\:_int( (signbit << 31) || (expo << 23) || (signif && 16:7FFFFF) )
enddefine;

    /*  Generate int values for a machine double-float = signif * 2**expo
    */
define lconstant d_float_ints(signif, expo) -> (hi32, lo32);
    lvars signif, expo, signbit, lo32, hi32, len;
    returnif(signif == 0) (dup(0) -> (hi32, lo32));
    if signif < 0 then 1 else 0 endif -> signbit;
    abs(signif) -> signif;
    integer_length(signif) -> len;
    expo + len + D_EXCESS -> expo;
    signif << (D_SIG_BITS-len) -> signif;
    signif && 16:FFFFFFFF -> lo32;
    signif >> 32 -> signif;
    (signbit << 31) || (expo << 20) || (signif && 16:FFFFF) -> hi32
enddefine;

define d_float_vals(/*signif, expo*/) /* -> (lo_addr_int, hi_addr_int) */;
    lvars (hi32, lo32) = d_float_ints();
    syspop\:_int(hi32) -> hi32;
    syspop\:_int(lo32) -> lo32;
#_IF DEF ALPHA
    lo32, hi32      ;;; lo bits precede hi bits in mem for Alpha T format
#_ELSE
    hi32, lo32
#_ENDIF
enddefine;

    /*  Generate word value for a pop decimal = signif * 2**expo
    */
define pd_float_val(signif, expo);
    lvars signif, expo;
#_IF WORD_BITS==DOUBLE_BITS
    lvars (hi32, lo32) = d_float_ints(signif, expo);
    (hi32 << 32) || lo32 &&~~ 2:11;
#_ELSE
    syspop\:_pint(s_float_val(signif, expo)) &&~~ 2:11;
#_ENDIF
    ;;; set pop decimal in tag bits
    syspop\:_int(() || 2:01)
enddefine;


;;; --- PARAMETERS -----------------------------------------------------


constant macro (
    $- IEEE_FLOAT       = true,
    ;;; number of sig bits in a pop decimal
    $- DECIMAL_SIG_BITS = #_IF WORD_BITS==DOUBLE_BITS   D_SIG_BITS-3
                          #_ELSE                        S_SIG_BITS-2
                          #_ENDIF,
    ;;; number of sig bits in pop ddecimal
    $- DDECIMAL_SIG_BITS = D_SIG_BITS,
    ;;; number of sig bits in a double float
    $- DFLOAT_SIG_BITS  = D_SIG_BITS,
    );


define lconstant param_vec(SB, EX, pd);
    lvars SB, EX, procedure pd;
    {%  SB,                     ;;; number of sig bits
        pd(1<<SB-1, EX+2-SB),   ;;; most +ve
        pd(1, -EX),             ;;; least +ve
        pd(-1, -EX),            ;;; least -ve
        pd(-1<<SB+1, EX+2-SB),  ;;; most  -ve
        pd(1, 1-SB),            ;;; plus epsilon
        pd(1, -SB),             ;;; minus epsilon
    %}
enddefine;

    /*  Used in float_params.p to generate pop_float_parameters
    */
define macro $- GEN_DEC_PARAM_VEC;
    perm_const_lab([decimal_key]) -> ;  ;;; mark this used
    param_vec(DECIMAL_SIG_BITS, #_IF WORD_BITS==DOUBLE_BITS D_EXCESS
                                #_ELSE                      S_EXCESS
                                #_ENDIF, pd_float_val)
enddefine;

    /*  Used in float_params.p to generate pop_float_parameters
    */
define macro $- GEN_DDEC_PARAM_VEC;

    define lconstant pdd(signif, expo);
        lvars signif, expo, v;
#_IF WORD_BITS==DOUBLE_BITS
        lvars (hi32, lo32) = d_float_ints(signif, expo);
        {% syspop\:_int( (hi32 << 32) || lo32 ), ddecimal_key %} -> v;
#_ELSE
        lvars (_loaddr, _hiaddr) = d_float_vals(signif, expo);
        {% _loaddr, ddecimal_key, _hiaddr %} -> v;
#_ENDIF
        cons_free_struct("DDECIMAL", v)
    enddefine;

    param_vec(DDECIMAL_SIG_BITS, D_EXCESS, pdd)
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 10 1995
        Changes for 64-bit decimals
--- John Gibson, Feb 18 1995
        Made d_float_vals return values other way round for Alpha T format
--- John Gibson, Jun  7 1989
        Included common.ph and changed _int to syspop\:_int
--- John Gibson, Mar 23 1989
        Keys to top level
--- John Gibson, Aug 24 1988
        Altered plus epsilon for decimals to 2 ** -21 instead of
        2 ** -22.
            (The previous value was right in the sense that, given
        the way double results were being rounded to decimals, it was the
        smallest value which when added to 1.0s0 gave something /= 1.0s0.
        However, the rounding method used by _pf_cvt_to_dec in afloat.s on
        Sun3 and Bobcat didn't conform to the IEEE standard, which has the
        'tie' case rounded so as to make the next bit even -- this is how
        ddecimals are done and decimals should be the same.)
--- John Gibson, Feb  9 1988
        Revised macros for generating float parameters
 */
