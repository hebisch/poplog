/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_coretypes.p
 > Purpose:         Type specifications for core Poplog X types
 > Author:          Roger Evans, Nov 16 1990 (see revisions)
 > Documentation:   REF *xtoolkit
 > Related Files:
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

loadinclude xpt_coretypes.ph;

;;; So uses works OK
global vars xpt_coretypes = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1991
        All procedures made autoloadable, and all typespec definitions
        moved to xpt_coretypes.ph
--- John Gibson, Sep 26 1991
        Corrected definition of XptPosition from :ushort to :short
--- Adrian Howard, Sep 13 1991 : Added -XptObject- typespec
--- Roger Evans, Jul  2 1991 added XptIsValidCallback
--- Roger Evans, Jun 28 1991
        added XptInputId and XptIntervalId
        fixed XptProcedure to work for exfunc_closures
--- Jonathan Meyer, Mar 11 1991 Added the following types:
        XptCardinal XptDimension XptPosition XptInt XptFile
        XptShort    XptFloat XptLongBoolean XptUnsignedChar XptChar
    (these are defined by the X toolkit, not by Xlib, otherwise they'd be
     in xpt_xtypes)
--- Roger Evans, Feb 15 1991 made XtPointer use string cache - this really
        needs proper fixing for 14.1
--- Roger Evans, Feb 10 1991 changed XptString to retern false instead
        of termin for null string pointers, gave better defn of XptPointer
--- Roger Evans, Feb  7 1991 made the new XptString actually work!
--- Roger Evans, Feb  6 1991 changed XptString to be smarter
--- Roger Evans, Nov 18 1990 added XptPopObj etc.
 */
