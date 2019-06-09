/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XClientConnections.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:   REF * XClientConnections
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

uses XConstants;
include xdefs.ph;

global constant macro (
    /* special Resource ID passed to KillClient */
    AllTemporary    = 0,

    /* Used in ChangeCloseDownMode */
    DestroyAll              = 0,
    RetainPermanent         = 1,
    RetainTemporary         = 2,
);


external declare XClientConnections in c;
    (external_import_procedure XptImportProcedure)

    void XKillClient(display, resource)
        Display *display;
        XID resource;
    {}

    void XSetCloseDownMode(display, close_mode)
        Display *display;
        int close_mode;
    {}

endexternal;

xlib_external_require XClientConnections;

global vars XClientConnections = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun  8 1993
        Tidied, sectioned, made +strict
--- Jonathan Meyer, Jan 25 1991
        Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
