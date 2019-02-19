/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/pop_malloc_min_alloc.p
 > Purpose:         Minimum amount of mem that (external-use) malloc should
 >                  request from O/S
 > Author:          John Gibson, May 19 1995
 > Documentation:   REF * SYSTEM
 */

#_INCLUDE 'declare.ph'

;;; ---------------------------------------------------------------------

section $-Sys => pop_malloc_min_alloc;

lconstant MAXVAL = 1 << min(POPINT_BITS, _pint(##(1)[_1|int])) - 1;

define active pop_malloc_min_alloc;
    _pint(_extern __pop_malloc_min_alloc:data!(int))
enddefine;

define updaterof active pop_malloc_min_alloc(nbytes);
    lvars nbytes;
    Check_integer_range(nbytes, 0, MAXVAL);
    _int(nbytes) -> _extern __pop_malloc_min_alloc:data!(int)
enddefine;

endsection;
