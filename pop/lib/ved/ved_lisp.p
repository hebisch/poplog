/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_lisp.p
 > Purpose:         Execute VED commands in "lisp" subsystem context
 > Author:          John Williams, Oct 15 1990 (see revisions)
 > Documentation:   REF *ved_lisp
 > Related Files:   LIB * LISP_SUBSYSTEM
 */

section;

uses lisp_subsystem;

define global ved_lisp();
    veddo_in_subsystem("lisp")
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 15 1993
        Revised for new subsystem implementation
--- John Williams, Dec 18 1990
        Added -Lispteachlist-
 */
