/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xcolor.ph
 > Purpose:         typespec definitions of X Color structures
 > Author:          Jonathan Meyer, Jan 31 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XPT_XCOLOR_INCLUDED

section;

iconstant macro (
    DoRed           = (1<<0),
    DoGreen         = (1<<1),
    DoBlue          = (1<<2),
);

i_typespec XColor {
    pixel: ulong,
    red: ushort,
    green: ushort,
    blue: ushort,
    flags: byte,
    pad: byte,
};

iconstant XPT_XCOLOR_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 30 1991 Added iconstant's
 */
