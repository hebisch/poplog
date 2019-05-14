/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/include/locale.ph
 > Purpose:         Locale categories
 > Author:          John Gibson, Apr 15 1997
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF LOCALE_INCLUDED

section;

iconstant macro (
                            ;;; Alternate ASCII char for sys_locale
    LC_CTYPE    = 0,        ;;; `C`
    LC_NUMERIC  = 1,        ;;; `N`
    LC_TIME     = 2,        ;;; `T`
    LC_COLLATE  = 3,        ;;; `S`
    LC_MONETARY = 4,        ;;; `M`
    LC_MESSAGES = 5,        ;;; `I`
    LC_ALL      = 6,        ;;; `A`

    LC_LAST     = 6,
);

iconstant LOCALE_INCLUDED = true;

endsection;
