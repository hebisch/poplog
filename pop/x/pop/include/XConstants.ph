/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XConstants.ph
 > Purpose:         Basic X constants for Xlib libraries
 > Author:          Aaron Sloman May 23rd 1990 (see revisions)
 > Documentation:   REF * XConstants
 > Related Files:   XConstants.p
 */

#_TERMIN_IF DEF XCONSTANTS_INCLUDED

compile_mode: pop11 +strict;
section;

iconstant macro (

    None            = 0,    /* universal null resource or null atom */
    CurrentTime     = 0,    /* special Time */
    NoSymbol        = 0,    /* special KeySym */
    Success         = 0,    /* everything's okay */
    NULL            = 0,

    AnyKey          = 0,       /* special Key Code, passed to GrabKey */
    AnyButton       = 0,       /* special Button Code, passed to GrabButton */
    AnyModifier     = 1<<15,   /* used in GrabButton, GrabKey */

    /*
     * Button names. Used as arguments to GrabButton and as detail in
     * ButtonPress and ButtonRelease events. Not to be confused with button
     * masks above. Note that 0 is already defined above as "AnyButton".
     */
    Button1         = 1,
    Button2         = 2,
    Button3         = 3,
    Button4         = 4,
    Button5         = 5,

    /* GrabPointer, GrabButton, GrabKeyboard, GrabKey Modes */
    GrabModeSync    =   0,
    GrabModeAsync   =   1,

    /* GrabPointer, GrabKeyboard reply status */
    GrabSuccess     =   0,
    AlreadyGrabbed  =   1,
    GrabInvalidTime =   2,
    GrabNotViewable =   3,
    GrabFrozen      =   4,


    /*****************************************************************
     * ERROR CODES                                                   *
     *****************************************************************/

    BadRequest     = 1,    /* bad request code */
    BadValue       = 2,    /* int parameter out of range */
    BadWindow      = 3,    /* parameter not a Window */
    BadPixmap      = 4,    /* parameter not a Pixmap */
    BadAtom        = 5,    /* parameter not an Atom */
    BadCursor      = 6,    /* parameter not a Cursor */
    BadFont        = 7,    /* parameter not a Font */
    BadMatch       = 8,    /* parameter mismatch */
    BadDrawable    = 9,    /* parameter not a Pixmap or Window */
    BadAccess     = 10,    /* depending on context:
                            *    #  Key/button already grabbed
                            *    #  Attempt to free an illegal cmap entry
                            *    #  Attempt to store into a read-only color
                            *       map entry.
                            *    #  Attempt to modify the access control list
                            *       from other than the local host.
                            */
    BadAlloc      = 11,    /* insufficient resources */
    BadColor      = 12,    /* no such colormap */
    BadGC         = 13,    /* parameter not a GC */
    BadIDChoice   = 14,    /* choice not in range or already used */
    BadName       = 15,    /* font or color name doesn't exist */
    BadLength     = 16,    /* Request length incorrect */
    BadImplementation = 17,/* server is defective */

    FirstExtensionError = 128,
    LastExtensionError  = 255,

);

iconstant XCONSTANTS_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 30 1993
        Tidied, sectioned, made +strict
--- Adrian Howard, Feb 10 1992 : Moved in constants from -XGrabbing.p-
--- Adrian Howard, Feb  7 1992 : Added -AnyKey-
--- Jonathan Meyer, Jan 25 1991
        moved from x/pop/lib/xlib to x/pop/include.
        Made it use iconstant
 */
