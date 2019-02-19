/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/sect_name.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ------------------ SECTION NAME/PATHNAME -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'

;;; ----------------------------------------------------------------------

section $-Sys$-Sect => section_name section_pathname;

define section_name() with_nargs 1;
    Checkr()!SEC_NAME
enddefine;

define section_pathname() with_nargs 1;
    copy(Checkr()!SEC_PATH_NAME)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
