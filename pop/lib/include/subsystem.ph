/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/include/subsystem.ph
 > Purpose:         Macros for subsystem field subscripts
 > Author:          John Gibson, Dec 21 1992
 > Documentation:   REF *SUBSYSTEM
 */

#_TERMIN_IF DEF SUBSYSTEM_INCLUDED

section;

iconstant macro (

    ;;; Subscripts for the outer list fields
    SS_NAME         = 1,            ;;; word
    SS_PROCEDURES   = 2,            ;;; vector/ident/word
    SS_FILE_EXTN    = 3,            ;;; string
    SS_PROMPT       = 4,            ;;; string
    SS_SEARCH_LISTS = 5,            ;;; association list
    SS_TITLE        = 6,            ;;; string

    ;;; Subscripts for the vector of procedures in valof(SS_PROCEDURES).
    ;;; These are shifted left 8 to distinguish them from the outer list
    ;;; subscripts (ie subscr_subsystem assumes that a subscript >= 16:100
    ;;; refers to the SS_PROCEDURES vector.
    ;;; The vector must be at least length 1 (ie only the compiler is
    ;;; mandatory).

    SS_COMPILER     = 1 << 8,       ;;; procedure
    SS_RESET        = 2 << 8,       ;;; procedure/ident/word
    SS_SETUP        = 3 << 8,       ;;; procedure/ident/word
    SS_BANNER       = 4 << 8,       ;;; procedure/ident/word
    SS_INITCOMP     = 5 << 8,       ;;; procedure/ident/word
    SS_POPARG1      = 6 << 8,       ;;; procedure/ident/word
    SS_VEDSETUP     = 7 << 8,       ;;; procedure/ident/word
    SS_XSETUP       = 8 << 8,       ;;; procedure/ident/word
);

iconstant SUBSYSTEM_INCLUDED = true;

endsection;
