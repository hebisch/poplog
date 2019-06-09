/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XBuffers.p
 > Purpose:         X Cut & Paste Buffers
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:   REF * XBuffers
 > Related Files:
 */


compile_mode: pop11 +strict;
section;

uses XConstants;

external declare XBuffers in c;
    (external_import_procedure XptImportProcedure)

    void XStoreBuffer(display, bytes, nbytes, buffer)
        Display *display;
        char bytes[];
        int nbytes;
        int buffer;
    {}

    void XStoreBytes(display, bytes, nbytes)
        Display *display;
        char bytes[];
        int nbytes;
    {}

    char *XFetchBuffer(display, nbytes, buffer)
        Display *display;
        int *nbytes;                    ;;; RETURN
        int buffer;
    {}

    char *XFetchBytes(display, nbytes)
        Display *display;
        int *nbytes;                    ;;; RETURN
    {}

    void XRotateBuffers(display, rotate)
        Display *display;
        int rotate;
    {}

endexternal;

xlib_external_require XBuffers;

global vars XBuffers = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun  7 1993
        Tidied, sectioned, made +strict
--- Jonathan Meyer, Jan 25 1991
        Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
