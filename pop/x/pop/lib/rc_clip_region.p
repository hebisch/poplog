/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_clip_region.p
 > Purpose:         Set clip limits relative to user coordinates
 > Author:          David S Young, Nov 26 1992 (see revisions)
 > Documentation:   HELP *RC_GRAPHIC
 > Related Files:   LIB *RC_GRAPHIC
 */
compile_mode :pop11 +strict;

section;

uses rc_graphic;

define rc_clip_region(region);
    ;;; sets the clipping limits to the region specified in user coordinates
    lvars region;
    lvars xmin xmax ymin ymax;
    if region.isreal then
        region
    else
        explode(region)
    endif -> (xmin, xmax, ymin, ymax);
    rc_transxyout(xmin,ymin) -> (rc_xmin, rc_ymin);
    min(rc_xmin,rc_xmax), max(rc_xmin,rc_xmax) -> (rc_xmin,rc_xmax);
    rc_transxyout(xmax,ymax) -> (rc_xmax, rc_ymax);
    min(rc_ymin,rc_ymax), max(rc_ymin,rc_ymax) -> (rc_ymin,rc_ymax);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- David S Young, Nov 26 1992
        Added section and endsection
 */
