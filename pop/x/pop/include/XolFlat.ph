/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolFlat.ph
 > Purpose:         Xol flat widget management
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLFLAT_INCLUDED

include XolConstants.ph;

section;

i_typespec
    OlFlatCallData {
        item_index: ulong,
        items: exptr,
        num_items: ulong,
        item_fields: ntstring_ptr,
        num_item_fields: ulong,
        num_fields: ulong,
    },
;

iconstant XOLFLAT_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1993
        Made an include file
--- Andreas Schoter, Jul 15 1991
    Added global constant XolFlat for compatibility with uses
 */
