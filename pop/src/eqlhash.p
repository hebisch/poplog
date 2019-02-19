/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/eqlhash.p
 > Purpose:
 > Author:          John Williams
 > Documentation:   REF *NUMBERS
 */

;;; --------- COMMON LISP eql NUMERIC EQUALITY OPERATOR -----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (Sys$- Eq__Bigint, Sys$- Eq__Ratio)
    ;

;;; ------------------------------------------------------------------------

section $-Sys =>  ==# ;

define 7 x ==# y;
    lvars x, y;
    if x == y then
        true
    elseif issimple(x) or issimple(y) then
        false
    elseif x!KEY == y!KEY then
        go_on _pint(x!KEY!K_NUMBER_TYPE) to INT BGI RAT DEC DDEC CMPLX
        else NO;

        BGI:
            return(BGWEAK Eq__Bigint(x, y));

        RAT:
            return(RTWEAK Eq__Ratio(x, y));

        DDEC:
            FLWEAK _pf_dfloat_ddec(x, FLWEAK _dfop1);
            FLWEAK _pf_dfloat_ddec(y, FLWEAK _dfop2);
            return(FLWEAK _pfeq(FLWEAK _dfop1, FLWEAK _dfop2));

        CMPLX:
            return(x!CX_REAL ==# y!CX_REAL and x!CX_IMAG ==# y!CX_IMAG);

    else

        INT:
        DEC:
        NO:
            false
    endif
enddefine;


endsection;     /* $-Sys */
