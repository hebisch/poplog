/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_edittool.p
 > Purpose:         Poplog UI edit file tool
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
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Adrian Howard, Sep 17 1992
        Now places VED command on the status line
--- Simon Nichols, Feb 10 1992
        Inserted Julian's fix from bugreport isl-fr.4407.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 21/8/91
    Merged OpenLook/Motif edit file into this file.
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
Julian Clinton,  15/7/91
    Changed to use XOPENLOOK instead of XOPENWINDOWS.
 */
