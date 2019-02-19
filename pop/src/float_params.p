/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/float_params.p
 > Purpose:
 > Author:          John Gibson, Feb  4 1988
 > Documentation:   REF *NUMBERS
 */


;;;---------- PARAMETERS OF FLOATING-POINT IMPLEMENTATION ---------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
    pop_float_parameters =
        {   2                       ;;; radix of floating-point representation
            /*  The following two macros generate vectors of parameters, the
                first for decimals and the second for ddecimals.
                Each one contains

                    significant bits
                    most  positive value
                    least positive value
                    least negative value
                    most  negative value
                    plus epsilon
                    minus epsilon

                See syscomp/genfloat.p
            */

            ^ GEN_DEC_PARAM_VEC
            ^ GEN_DDEC_PARAM_VEC
        };
