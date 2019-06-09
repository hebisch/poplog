/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_rotate_xy.p
 > Purpose:         Produce rotation procedure for use with rc_graphic
 > Author:          Aaron Sloman, May 19 1990 (see revisions)
 > Documentation:   HELP * RC_GRAPHIC, TEACH * RC_GRAPHIC
 > Related Files:   LIB * RC_GRAPHIC, * RC_MOUSE, LIB * RC_DRAWGRAPH
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses rc_graphic;

/*

Define  three  more  global  variables   for  use  by  the   co-ordinate
transformation system: the angle, the cosine  and the sine of the  angle
through which the  user-coordinate system  is rotated  counter-clockwise
about its origin (if xscale  is positive and yscale negative),  relative
to the window co-ordinates. The variables are accessed via the  "active"
variable rc_frame_angle, and the procedure rc_rotate_frame. The sine and
cosine are computed only when the angle is updated, and used  repeatedly
by the transformation procedure rc_rotate_xy,  which can be assigned  to
rc_transxyout

Similarly, rc_rotate_xyin can be assigned to rc_transxyin
*/

;;; Hidden from user
lvars
    rc_actual_frame_angle = 0,
    rc_rotate_cos = 1.0,
    rc_rotate_sin = 0.0,
;

define active rc_frame_angle();
    ;;; This active variable is used to access the current frame angle
    rc_actual_frame_angle;
enddefine;

define updaterof active rc_frame_angle(angle);
    ;;; Update rc_actual_frame_angle, and set the values of rc_rotate_cos and
    ;;; rc_rotate_sin to correspond to a rotation of angle degrees.
    lvars angle;
    dlocal popradians = false;
    ;;; normalise the angle
    while angle >= 360.0 do
        angle - 360.0 -> angle
    endwhile;
    while angle < 0 do
        angle + 360.0 -> angle
    endwhile;
    cos(angle) -> rc_rotate_cos;
    sin(angle) -> rc_rotate_sin;
    angle -> rc_actual_frame_angle;
enddefine;

define rc_rotate_frame(angle);
    lvars angle;
    ;;; add the angle to the current frame angle, then set
    ;;; rc_rotate_cos and rc_rotate_sin
    rc_actual_frame_angle + angle -> rc_frame_angle
enddefine;

define rc_rotate_xy(x, y) -> y -> x;
    ;;; Takes two numbers (user co-ordinates) and returns two
    ;;; numbers - window co-ordinates. Can be assigned to rc_transxyin
    ;;; It uses rc_rotate_cos and rc_rotate_sin
    lvars x,y;

    ;;; First rotate about user origin
    x * rc_rotate_cos - y * rc_rotate_sin,
        x * rc_rotate_sin + y * rc_rotate_cos
            -> y -> x;
    ;;; Do the rest of the transformation
    round(if rc_xscale == 1 then x else x * rc_xscale endif + rc_xorigin) -> x;
    round(if rc_yscale == 1 then y else y * rc_yscale endif + rc_yorigin) -> y;
enddefine;

define rc_rotate_xyin(x, y) -> y -> x;
    ;;; Takes two numbers (window co-ordinates) and returns two
    ;;; numbers - user co-ordinates.
    ;;; It uses rc_rotate_cos and rc_rotate_sin
    lvars x,y;

    ;;; first translate to user origin and scale
    if rc_xscale == 1 or rc_xscale = 1.0 then
        x - rc_xorigin
    else (x - rc_xorigin) / rc_xscale
    endif -> x;

    if rc_yscale == -1 then
        rc_yorigin - y
    elseif rc_yscale == 1 then
        y - rc_yorigin
    elseif rc_yscale = -1.0 then
        rc_yorigin - y
    elseif rc_yscale = 1.0 then
        y - rc_yorigin
    else
        (y - rc_yorigin) / rc_yscale
    endif -> y;

    ;;; Now rotate about user origin
    x * rc_rotate_cos + y * rc_rotate_sin,
        y * rc_rotate_cos - x * rc_rotate_sin
            -> y -> x;
enddefine;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, May 23 1990
    Put rounding into rc_rotate_xy
 */
