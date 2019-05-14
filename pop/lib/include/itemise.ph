/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/include/itemise.ph
 > Purpose:         Pop-11 itemiser character types
 > Author:          John Gibson, Feb 25 1992 (see revisions)
 > Documentation:   REF *ITEMISE
 */

#_TERMIN_IF DEF ITEMISE_INCLUDED

section;

iconstant macro (
        ITM_TERMIN      = 0,    ;;; <termin>
        ITM_ALPHA       = 1,    ;;; alphabetic
        ITM_DIGIT       = 2,    ;;; digit
        ITM_SIGN        = 3,    ;;; sign char
        ITM_UNDERSCORE  = 4,    ;;; underscore
        ITM_SEPARATOR   = 5,    ;;; separator
        ITM_WHITESPACE  = 6,    ;;; white space
        ITM_STRQUOTE    = 7,    ;;; string quote
        ITM_CHARQUOTE   = 8,    ;;; character quote

        ITM_EOLCOMMENT  = 9,    ;;; end-of-line comment
        ITM_BRCOMMENT1  = 10,   ;;; bracketed comment, 1st char
        ITM_BRCOMMENT2  = 11,   ;;; bracketed comment, 2nd char
        ITM_ABC         = 12,   ;;; alphabeticiser

        ITM_FRACTION    = 13,   ;;; fraction character
        ITM_EXTENDING   = 14,   ;;; extends alpha

        ITM_COMBINING   = 15,   ;;; combining
        ITM_ENCLOSING   = 16,   ;;; enclosing (= combining + basetype->sign)
        ITM_IGNORE      = 17,   ;;; ignored
        ITM_NOTCHAR     = 18,   ;;; character code is not a defined character

        ITM_LAST        = 18,
    );

iconstant ITEMISE_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 29 1997
        New types for Unicode
 */
