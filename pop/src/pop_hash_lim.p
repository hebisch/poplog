/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/pop_hash_lim.p
 > Purpose:
 > Author:          John Williams (see revisions)
 > Documentation:   REF *PROPS
 */

;;; -------- LIMIT FOR HASHING ON STRUCTURES BY syshash ------------------

#_INCLUDE 'declare.ph'

vars
        Sys$- _pop_hash_lim
    ;

;;; ---------------------------------------------------------------------

section $-Sys => pop_hash_lim;

define active pop_hash_lim;
    _pop_hash_lim
enddefine;
;;;
define updaterof active pop_hash_lim(_num);
    lvars _num;
    Check_integer(_num, 0);
    _num -> _pop_hash_lim
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 24 1988
        Moved out of props.p
 */
