/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/include/external_flags.ph
 > Purpose:         Flag bits in the external var -pop_external_flags-
 > Author:          John Gibson, Jul  9 1991 (see revisions)
 > Documentation:   REF *EXTERNAL
 > Related Files:   $popexternlib/callback.h
 */

#_TERMIN_IF DEF EXTERNAL_FLAGS_INCLUDED

section;

    /*  Flag bits in pop_external_flags, which is local to every block of
        external calls, and set to zero when in pop.
        These are repeated in $popexternlib/callback.h
    */
iconstant macro (
    ;;; status flags
    PEF_SYS_MALLOC_USER     = 2:1e0,    ;;; for internal system use
    PEF_DOING_ABEXIT        = 2:1e1,    ;;; set for abnormal exit back to pop

    ;;; user control over abnormal exits
    PEF_RETURN_ABEXIT_NEXT  = 2:1e8,    ;;; return abexit on next callback
    PEF_RETURN_ABEXIT_ANY   = 2:1e9,    ;;; return abexit on any callback
    PEF_CATCH_ABEXIT_NEXT   = 2:1e10,   ;;; catch abexit on next callback
    PEF_CATCH_ABEXIT_ANY    = 2:1e11,   ;;; catch abexit on any callback

    ;;; async callback
    PEF_ASYNC_CALLBACK      = 2:1e12,   ;;; allow asynchronous callback
    PEF_ASYNC_RETURN_ABEXIT = 2:1e13,   ;;; set RETURN_ABEXIT_NEXT for async cb
    PEF_ASYNC_CATCH_ABEXIT  = 2:1e14,   ;;; set CATCH_ABEXIT_NEXT  for async cb

    ;;; for system use
    PEF_DO_USER_MALLOC      = 2:1e15,
    );


iconstant EXTERNAL_FLAGS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 15 1993
        Added PEF_DO_USER_MALLOC
--- John Gibson, Jul 13 1991
        Added new ASYNC flags
 */
