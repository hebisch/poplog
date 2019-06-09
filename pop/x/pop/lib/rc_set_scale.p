/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_set_scale.p
 > Purpose:         Easy setting of units in inches etc.
 > Author:          Poplog System, May 24 1990
 > Documentation:
 > Related Files:   LIB * RC_GRAPHIC
 */
compile_mode :pop11 +strict;

/*
rc_set_scale(type, xscale, yscale)

Type can be
    "inches"
        Then xscale is the number of inches per user unit to right
        Then yscale is the number of inches per user unit up screen

    "cm"
        Then xscale is the number of centimetres per user unit to right
        Then yscale is the number of centimetres per user unit up screen
    "frame"
        Then xscale is the number of window widths per user unit to right
        Then yscale is the number of window heights per user unit up screen
    false
        Just use xscale and yscale as given (y positive upwards)

*/

vars rc_pixels_per_inch;
unless isinteger(rc_pixels_per_inch) then
    90 -> rc_pixels_per_inch ;;; default should be what???
endunless;

define rc_set_scale(type, xscale, yscale);
    lvars type, xscale, yscale;
    if type == "inches" or type == "in" then
        ;;; set to xscale inches per user unit, etc.
        rc_pixels_per_inch * xscale,
        -rc_pixels_per_inch * yscale

    elseif type == "cm" or type == "c" then
        ;;; set to xscale centimetres per user unit, etc.
        rc_pixels_per_inch * xscale / 2.54,
        -rc_pixels_per_inch * yscale / 2.54

    elseif type = "frame" then
        ;;; set to xscale frames per user unit etc.
        rc_setsize();
        rc_window_xsize * xscale,
        -rc_window_ysize * yscale

    else
        xscale,
        -yscale
    endif     -> rc_yscale -> rc_xscale;
enddefine;
