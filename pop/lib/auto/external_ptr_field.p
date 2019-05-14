/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/external_ptr_field.p
 > Purpose:         Old procedure for getting external pointer value
 >                  (Don't use -- use exacc directly instead)
 > Author:          John Gibson, Nov 13 1990 (see revisions)
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

define global external_ptr_field() with_nargs 1;
    exacc ^uint ()
enddefine;
;;;
define updaterof external_ptr_field() with_nargs 2;
    () -> exacc ^uint ()
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1992
        Made -external_ptr_field- global
 */
