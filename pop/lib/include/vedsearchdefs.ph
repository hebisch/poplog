/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/include/vedsearchdefs.ph
 > Purpose:         defines for vdregexp.p
 > Author:          Jonathan Meyer, Sep 28 1993
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF VEDSEARCHDEFS_INCLUDED

section;

iconstant
    macro (
        ;;; used to redo a search.
        VEDSRCH_SEARCH_P            = 1,    ;;; the computed search procedure
        VEDSRCH_CONSTRAIN           = 2,    ;;; the constrain region name
        VEDSRCH_WRAP                = 3,    ;;; should search wrap-around ?
        VEDSRCH_FROM_HERE           = 4,    ;;; start search from cursor ?
        VEDSRCH_COUNT               = 5,    ;;; number of times to search

        ;;; used to redisplay the search strings
        VEDSRCH_SEARCH_STRING       = 6,    ;;; the search string
        VEDSRCH_SUBSTITUTE_STRING   = 7,    ;;; the substitution string

        ;;; the result of the last search
        VEDSRCH_RESULT              = 8,    ;;; four element vector of coordinates

        VEDSRCH_ANYWHERE            = 9,    ;;; vvedanywhere compatibility

        VEDSRCH_LENGTH            = 9, ;;; length of the search vector
    ),
;

iconstant VEDSEARCHDEFS_INCLUDED = true;

endsection;
