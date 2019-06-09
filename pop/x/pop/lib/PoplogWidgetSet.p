/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/PoplogWidgetSet.p
 > Purpose:         Minimal information for the Poplog widget-set
 > Author:          Jonathan Meyer, Aug 29 1990 (see revisions)
 > Documentation:   HELP * Xpw
 > Related Files:   LIB * XptWidgetSet
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

uses-now popxlib, Xpw, XptWidgetSet;
uses XpwCallMethod;

loadinclude XpwCore.ph;
loadinclude XpwGraphic.ph;
loadinclude XpwPixmap.ph;
loadinclude XpwScrollText.ph;

lconstant ws = XptNewWidgetSet("Poplog");

XPW_EXLIBS -> ws("WidgetSetLibList");

"Xpw" -> ws("WidgetSetPrefix");

[
    CoreWidget
    GraphicWidget
    PixmapWidget
    ScrollTextWidget
    CompositeWidget
    TransparentWidget
] -> ws("WidgetSetMembers");


procedure(wsname, wcname);
    lvars wsname, wcname, index, s;
    if wcname == "CompositeWidget" or wcname == "TransparentWidget" then
        'xpw'
    else
        issubstring('Widget', wcname) -> index;
        substring(1, index - 1, wcname) -> wcname;
        'Xpw'
    endif -> s;
    consword(s sys_>< wcname)   ;;; to make it use useslib
endprocedure -> ws("WidgetSetFileMapping");

procedure(wcname);
    lvars wcname;
    ;;; "CoreWidget" -> "xpwCoreWidgetClass"
    consword(#| `x,`p,`w, wcname.explode, `C,`l,`a,`s,`s |#);
endprocedure -> ws("WidgetSetNameMapping");

global constant PoplogWidgetSet = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Julian Clinton, Jan 17 1994
        Added some loadincludes so code using XptWidgetSet compiles okay.
--- John Gibson, Apr  2 1993
        o Moved XpwCallMethod to Xpw/XpwCallMethod.p.
        o Changed WidgetSetFileMapping to load libraries
        o Also now uses XPW_EXLIBS.
--- Adrian Howard, Mar 30 1992
        Added -include- for "xpt_coretypes.ph" (needed for XptInt)
--- Ian Rogers, Feb  4 1992
        Added TransparentWidget
--- John Gibson, Feb  2 1992
        Added result type XptInt
--- John Gibson, Nov 21 1991
        VMS corrections
--- Simon Nichols, Oct 23 1991
        Added constant -PoplogWidgetSet- for -uses-.
--- Adrian Howard, Sep  2 1991 : "WidgetSetFileMapping" altered to return
        filenames so we don't have to alter -popuseslist-
--- Jonathan Meyer, Jul 31 1991
        libXpw now lives in $popexternlib and is called libXpw.olb.
        Name defined by include constant XPWLIBFILE.
--- Jonathan Meyer, Jul 30 1991
        Removed XpwFailureImport, XpwImportXID. Changed XpwCallMethod to
        take return result types "XptPixel", "XptXID", "exptr".
        Removed xpw_check_live_widget (redundant).
        Made FileMapping and NameMapping procs produce less garbage.
--- Jonathan Meyer, Jul  6 1991
        Changed to use is_null_external_ptr
--- Robert John Duncan, Jun 11 1991
        Changed WidgetSetLibList and tidied up.
--- Ian Rogers, Apr 17 1991
        Added -XpwImportXID-. cf. BR rogere.50
--- Ian Rogers, Jan 18 1991
        Changed the call form of raw_XpwCallMethod from (N):int to
        (N):exptr
        Added -XpwFailureImport-
--- Jonathan Meyer, Oct 22 1990 Fixed for new version of XptWidgetSet
--- Jonathan Meyer, Oct 20 1990 commented out oldX - no longer needed
--- James Goodlet, Sep 12 1990 - added the missing server extensions and
        miscellaneous utilities libraries to the WidgetSetLibList.
--- Jonathan Meyer, Sep 10 1990
    Added section, endsection, fixed xpw_check_live_widget.
 */
