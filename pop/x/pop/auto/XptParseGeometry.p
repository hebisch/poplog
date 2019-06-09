/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptParseGeometry.p
 > Purpose:         Conversion between X geometry string and integers/false
 > Author:          John Gibson, Oct  1 1991 (see revisions)
 > Documentation:   REF *XT_LIBS
 */
compile_mode :pop11 +strict;

section;

XptLoadProcedures XptParseGeometry
lvars
    XParseGeometry(gstring,x,y,width,height) :int
;

lconstant macro (
    XValue      = 2:1e0,
    YValue      = 2:1e1,
    WidthValue  = 2:1e2,
    HeightValue = 2:1e3,
);

define XptParseGeometry(gstring) /* -> (x, y, width, height) */;
    lvars gstring, mask;
    lconstant
        x_p     = EXPTRINITSTR(:int),
        y_p     = EXPTRINITSTR(:int),
        width_p = EXPTRINITSTR(:uint),
        height_p= EXPTRINITSTR(:uint),
    ;

    returnunless(gstring) (false, false, false, false);
    check_string(gstring);
    exacc raw_XParseGeometry(gstring, x_p, y_p, width_p, height_p) -> mask;
    mask &&/=_0 XValue      and exacc :int x_p,
    mask &&/=_0 YValue      and exacc :int y_p,
    mask &&/=_0 WidthValue  and exacc :uint width_p,
    mask &&/=_0 HeightValue and exacc :uint height_p
enddefine;
;;;
define updaterof XptParseGeometry(x, y, width, height) /* -> gstring */;
    lvars x, y, width, height, s;

    define lconstant dest_int(/*i, low*/) with_props false;
        chain(fi_check((), false), dest_characters)
    enddefine;

    consstring(#|
        if width then dest_int(width, 0) endif;
        if height then `x`, dest_int(height, 0) endif;
        if x then
            `+`, dest_int(x, false);
            if y then `+`, dest_int(y, false) endif
        elseif y then
            ;;; this case doesn't work
            mishap(x, y, 2, 'GEOMETRY STRING CAN\'T CONTAIN y VALUE WITHOUT x VALUE')
        endif
    |#) -> s;
    s /= nullstring and s
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  1 1995
        Replaced use of int*vecs with EXPTRINITSTRs for correct types
--- John Gibson, Dec 15 1993
        Added updater
 */
