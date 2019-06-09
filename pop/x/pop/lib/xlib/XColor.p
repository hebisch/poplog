/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XColor.p
 > Purpose:
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:   REF * XColor
 > Related Files:   LIB * XCursors XColorcells XTile
 */

compile_mode: pop11 +strict;
section;

external declare xpop in c;
    (external_import_procedure XptImportProcedure)

    ;;; Data structure used by color operations

    typedef struct {
        unsigned long pixel;
        unsigned short red, green, blue;
        char flags;  ;;; do_red, do_green, do_blue */
        char pad;
    } XColor;

endexternal;

global constant macro(
    /* QueryBestSize Class */
    CursorShape     = 0,   /* largest size that can be displayed */
    TileShape       = 1,   /* size tiled fastest */
    StippleShape    = 2,   /* size stippled fastest */
);

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun  9 1993
        Tidied, sectioned, made +strict
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
