/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/quot_rem.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; --------------- QUOTIENT & REMAINDER OPERATIONS -------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure Sys$-Arith_2
    ;

;;; -----------------------------------------------------------------------

section $-Sys =>  // div rem mod;

define 4 x // y;
    lvars x, y;
    if isinteger(y) then
        if y == 0 then
            mishap(x, y, 2, 'DIVIDING BY ZERO', 'arith-2arg:arith-div0')
        elseif isinteger(x) then
            ;;; dividing largest -ve integer by -1 can overflow!
            return(if y == -1 then 0, negate(x) else x _pdiv y endif)
        endif
    endif;
    Arith_2(x, y, OP_//)
enddefine;

define 2 div divisor with_nargs 2;
    lvars divisor;
    _CLAWBACK_SAVE;
    () // divisor -> divisor -> ;
    Clawback_num(divisor)
enddefine;

define 2 rem with_nargs 2;
    _CLAWBACK_SAVE;
    Clawback_num(() // () ->)
enddefine;

define 2 x mod y;
    lvars x, y;
    _CLAWBACK_SAVE;
    x // y -> -> x;
    if x < 0 then
        if y >= 0 then y + x else x endif
    elseif y < 0 then
        y + x
    else
        x
    endif;
    Clawback_num()
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Added mishap id-string
--- John Gibson, Jul 16 1988
        Corrected erroneous result from dividing largest -ve popint by -1
 */
