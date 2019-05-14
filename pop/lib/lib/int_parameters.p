/* --- Copyright University of Sussex 1999.  All rights reserved. ---------
 > File:           C.all/lib/lib/int_parameters.p
 > Purpose:        Define some useful (?) integer constants
 > Author:         John Williams, Jan 19 1987 (see revisions)
 > Documentation:  REF *NUMBERS
 > Related Files:  LIB *FLOAT_PARAMETERS
 */
compile_mode :pop11 +strict;

section;

lvars n = 1;

until isbiginteger(n) do n << 1 -> n enduntil;

constant
    pop_max_int =  (n - 1),
    pop_min_int =  (0 - n),
    ;

;;; for uses
global vars int_parameters = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 15 Jul 1999
        Put in global vars declaration for "uses"
--- John Gibson, Oct 15 1998
        Tidied up
 */
