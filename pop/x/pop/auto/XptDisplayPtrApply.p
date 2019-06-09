/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptDisplayPtrApply.p
 > Purpose:         Class apply for DisplayPtr descriptors
 > Author:          Jonathan Meyer, Jun 27 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses xt_display;

XptLoadProcedures XptDisplayPtrApply lvars XServerVendor;

define XptDisplayPtrApply(name, desc);
    lvars name, desc;
    XptCheckDisplayPtr(desc)->;
    if name == "appcontext" then
        fast_XtDisplayToApplicationContext(desc)
    elseif name == "name" then
        XptDataProps(desc);
    elseif name == "vendor" then
        exacc (1):exptr#exacc_ntstring raw_XServerVendor(desc)
    else
        mishap(name, 1, 'UNKNOWN DisplayPtr FIELD');
    endif;
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 29 1992
        Removed use of XptD*isplayDevice (no longer supported)
--- John Gibson, Nov  3 1991
        Removed uses xpt_coretypes
 */
