/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/XmVendorE.p
 > Purpose:         Vendor extensions and other object classes
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses
    xmDesktopObject,
    xmWorldObject,
    xmDisplayObject,
    xmScreenObject,
    xmShellExtObject,
    xmVendorShellExtObject,
;

global constant XmVendorE = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Split up into separate files
 */
