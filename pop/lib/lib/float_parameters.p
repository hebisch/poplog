/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/lib/float_parameters.p
 *  Purpose:        Define floating-point parameter constants
 *  Author:         John Gibson, Jan  3 1986 (see revisions)
 *  Documentation:  REF NUMBERS
 *  Related Files:
 */

    /*  -pop_float_parameters- is also used by the autoloadable procedure
        -float_digits- and the autoloadable constant -pop_float_radix-.
    */

section;

lconstant
    decimal_params  = pop_float_parameters(2),
    ddecimal_params = pop_float_parameters(3);


global constant

    pop_most_positive_decimal   = decimal_params(2),
    pop_least_positive_decimal  = decimal_params(3),
    pop_least_negative_decimal  = decimal_params(4),
    pop_most_negative_decimal   = decimal_params(5),

    pop_most_positive_ddecimal  = ddecimal_params(2),
    pop_least_positive_ddecimal = ddecimal_params(3),
    pop_least_negative_ddecimal = ddecimal_params(4),
    pop_most_negative_ddecimal  = ddecimal_params(5),

    pop_plus_epsilon_decimal    = decimal_params(6),
    pop_minus_epsilon_decimal   = decimal_params(7),

    pop_plus_epsilon_ddecimal   = ddecimal_params(6),
    pop_minus_epsilon_ddecimal  = ddecimal_params(7),

    ;

endsection;

/*  --- Revision History ---------------------------------------------------
 */
