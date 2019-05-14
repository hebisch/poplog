/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/sysdefs.p
 > Purpose:         Defining system attributes as globals
 > Author:          Jonathan Meyer, Apr 18 1991 (see revisions)
 > Documentation:   HELP *SYSDEFS
 > Related Files:   INCLUDE *SYSDEFS LIB *POPHOST
 */
/*
    All SYSDEFS DEFINITIONS HAVE MOVED TO INCLUDE *SYSDEFS.PH
    This library remains for compatibility

*/

loadinclude sysdefs.ph;
global vars $-sysdefs = true;       ;;; for uses


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 22 1992
        Added definition of sysdefs
 */
