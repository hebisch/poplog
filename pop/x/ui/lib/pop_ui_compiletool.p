/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_compiletool.p
 > Purpose:         Poplog UI compile file tool
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;
uses pop_ui_filetool;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 10 1995
        Now defined with pop_ui_filetool
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Compiling a file now sets busy cursor on.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 21/8/91
    Merged OpenLook/Motif compile file into this file.
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
Julian Clinton,  15/7/91
    Changed to use XOPENLOOK instead of XOPENWINDOWS.
 */
