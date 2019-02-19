/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/sections.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; --------------- DECLARATIONS FOR SECTIONS ----------------------------

struct SECTION
  { full    SEC_NAME,
            KEY,
>->         SEC_PATH_NAME,
            SEC_SUPERSECT,
            SEC_SUBSECTS,
            SEC_IDENTS,
            SEC_WORD_IDENTS,
            SEC_IMPORTS,
            SEC_EXPORTS,
            SEC_RESTORE,
            SEC_LEVEL;
  };



global constant
        pop_section
    ;

section $-Sys;

global constant
        procedure (del_assoc_val)
    ;

endsection;

section $-Sys$-Sect;

constant
        procedure (Checkr, Set_1word, word_idents),
    ;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
