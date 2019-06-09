/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_xtypes.p
 > Purpose:         typespecs for some common Xlib types
 > Author:          Roger Evans, Nov 19 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

loadinclude xpt_xtypes.ph;

global vars xpt_xtypes = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1991
        typespec definitions moved to xpt_xtypes.ph
--- Adrian Howard, Sep 17 1991 : Added -XptXrmQuark-
--- Jonathan Meyer, Mar 11 1991
        Added Pixel, GC, Color, Cursor, Bitmap, Atom, Colormap, Visual.
        Moved Cardinal to xpt_coretypes.
--- Roger Evans, Nov 19 1990 added cardinal and opaque
 */
