/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/underscore.p
 > Purpose:         Anonymous variable
 > Author:          John Gibson, Jan 13 1996
 > Documentation:   REF * IDENT
 */

#_INCLUDE 'declare.ph'

;;; -----------------------------------------------------------------------

define active _ ;
    pop_undef
enddefine;
;;;
define updaterof active _ ;
    ->
enddefine;
