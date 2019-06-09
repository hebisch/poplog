/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XSizeHints.p
 > Purpose:
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XProperties XWindowManager
 */


external declare xpop in c;
    (external_import_procedure XptImportProcedure)

    typedef struct {
        long flags; ;;; marks which fields in this structure are defined
        int x, y;
        int width, height;
        int min_width, min_height;
        int max_width, max_height;
        int width_inc, height_inc;
        struct {
            int x;  ;;; numerator
            int y;  ;;; denominator
        } min_aspect, max_aspect;
    } XSizeHints;

endexternal;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
