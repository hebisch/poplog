/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XptGCValuesPtr.p
 > Purpose:         shadowclass support for GCValues struct
 > Author:          Roger Evans, Nov 15 1990 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

include xpt_constants.ph;
include xpt_xtypes.ph;

shadowclass XptGCValuesPtr #_< [props ^XDT_GCVALUESPTR] >_# {:XptGCValues};

constant XptGCValuesPtr = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov  5 1991 : Moved *back* to lib
--- John Gibson, Nov  3 1991
        XptGCValues typespec moved to xpt_xtypes.ph
--- Adrian Howard, Nov  1 1991 :
    o Added -XptGCValues- typespec
    o Renamed fields from XptGCVP... to XptGCV...
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Jonathan Meyer, Jan 17 1991 Moved to auto
 */
