/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XCoords.p
 > Purpose:
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XGraphicsContext XDrawingPrimitives XRegions
 */

section;
loadinclude XCoords.ph;

external declare xpop in c;
    (external_import_procedure XptImportProcedure)

    /*
     * opaque reference to Region data type
     */
    typedef struct _XRegion *Region;

    /*
     * Data structures for graphics operations.  On most machines, these are
     * congruent with the wire protocol structures, so reformatting the data
     * can be avoided on these architectures.
     */
    typedef struct {
        short x, y;
    } XPoint;

    typedef struct {
        short x, y;
        unsigned short width, height;
    } XRectangle;

endexternal;

global vars XCoords = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed include to loadinclude
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
--- Aaron Sloman, May 23 1990
    Moved macro definitions to .ph file
 */
