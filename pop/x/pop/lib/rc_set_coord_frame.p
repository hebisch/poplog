/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_set_coord_frame.p
 > Purpose:         Simplifies setting of coordinate systems
 > Author:          David S Young, Nov 26 1992
 > Documentation:   HELP *RC_GRAPHIC
 > Related Files:   LIB *RC_GRAPHIC
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses rc_graphic, rc_set_scale;

define rc_set_coord_frame(type, window_region, user_region);
    lvars type window_region user_region;
    lvars sx0 sx1 sy0 sy1 ux0 ux1 uy0 uy1 xrat yrat;

    ;;; Fix arguments
    if user_region.isreal then
        type, window_region, user_region
            -> (type, window_region, ux0, ux1, uy0, uy1)
    else
        if user_region.isarray then
            boundslist(user_region) -> user_region
        endif;
        explode(user_region) -> (ux0, ux1, uy0, uy1)
    endif;
    if window_region.isreal then
        type, window_region
            -> (type, sx0, sx1, sy0, sy1)
    else
        if window_region.isarray then
            boundslist(window_region) -> window_region
        endif;
        explode(window_region) -> (sx0, sx1, sy0, sy1)
    endif;

    ;;; Get scale ratios
    (sx1 - sx0) / (ux1 - ux0) -> xrat;
    (sy1 - sy0) / (uy1 - uy0) -> yrat;
    if type == "existing" then
        ;;; Set scale relative to existing coords
        rc_xscale * xrat -> rc_xscale;
        rc_yscale * yrat -> rc_yscale
    else
        rc_set_scale(type, xrat, -yrat);
        0 ->> rc_xorigin -> rc_yorigin;
    endif;
    rc_shift_frame_by(sx0/xrat - ux0, sy0/yrat - uy0)
enddefine;

endexload_batch;
endsection;
