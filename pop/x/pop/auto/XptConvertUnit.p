/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptConvertUnit.p
 > Purpose:         Routines for converting between units
 > Author:          Jonathan Meyer, Nov 3 1990 (see revisions)
 > Documentation:   REF *XPT_SCREENINFO
 */

compile_mode :pop11 +strict;

section;

include xpt_xscreen.ph;

define :inline lconstant XResOfScreen(XScreen);
    (exacc XScreen.mwidth / exacc XScreen.width)
enddefine;

define :inline lconstant YResOfScreen(XScreen);
    (exacc XScreen.mheight / exacc XScreen.height)
enddefine;

define lconstant axis_to_mm_per_pixel(axis, XScreen);
    lvars axis XScreen;
    if axis == "x" or axis == "X" then
        XResOfScreen(XScreen)
    elseif axis == "y" or axis == "Y" then
        YResOfScreen(XScreen)
    elseunless axis then
        ;;; average
        (XResOfScreen(XScreen) + YResOfScreen(XScreen)) / 2
    else
        mishap(axis,1,'INVALID AXIS: must specify "x", "y" or -false-');
    endif;
enddefine;

define lconstant pixel_to_mm(p, axis, screen);
    lvars p axis screen;
    1.0 * p * axis_to_mm_per_pixel(axis, screen)
enddefine;

define lconstant mm_to_pixel(p, axis, screen);
    lvars p axis screen;
    round(p / axis_to_mm_per_pixel(axis, screen));
enddefine;

lconstant
    unit_to_mm_scale =
        [   mm 1
            cm 10
            inch  25.4
            point 2.834645669
            pica %12 * 2.834645669%
            pixel ^pixel_to_mm
        ],
    mm_to_unit_scale =
        [   mm 1
            cm 0.1
            inch %1/25.4%
            point %1/2.834645669%
            pica %1/(12 * 2.834645669)%
            pixel ^mm_to_pixel
        ],
;


define global XptConvertUnit(num, from_unit, to_unit, axis, screen) -> num;
    lvars num, from_unit, to_unit, axis, screen, from_scale, to_scale;
    unless from_unit.isword and
            fast_lmember(from_unit, unit_to_mm_scale) ->> from_scale then
        mishap(from_unit,1,'UNKNOWN UNIT');
    elseunless to_unit.isword and
            fast_lmember(to_unit, mm_to_unit_scale) ->> to_scale then
        mishap(to_unit,1,'UNKNOWN UNIT');
    endunless;

    fast_front(fast_back(from_scale)) -> from_scale;
    fast_front(fast_back(to_scale)) -> to_scale;

    num,
    if from_scale.isprocedure then
        from_scale(axis, screen)
    else
        * from_scale
    endif; ;;; -> num_scaled_into_mm

    if to_scale.isprocedure then
        to_scale(axis, screen)
    else
        * to_scale
    endif -> num; ;;; -> num_scaled_into_to_unit
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 17 1991 moved to auto
--- Jonathan Meyer, Nov 23 1990 made use of include xpt_xscreen.ph
--- Roger Evans, Nov 19 1990 made proc global
 */
