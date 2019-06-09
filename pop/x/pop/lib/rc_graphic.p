/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_graphic.p
 > Purpose:         Graphics interface Relative to Co-ordinate frame of user
 > Author:          Aaron Sloman, May 15 1990 (see revisions)
 > Documentation:   HELP * RC_GRAPHIC  TEACH * RC_GRAPHIC
 > Related Files:   LIB * RC_DRAWGRAPH, LIB * RC_ROTATE_XY, * RC_MOUSE
                    LIB * RC_CONTEXT
 */
compile_mode :pop11 +strict;

/*

This library provides a collection of graphical utilities making it easy
to do things relative to a user co-ordinate frame instead of window
co-ordinates.

The programs are interfaced to the X Windows facilities in Poplog, but are
capable of being modified to use a different interface, as explained in
the HELP file.

 */

uses-now popxlib, Xpw;

section;
exload_batch;

#_IF DEF POPC_COMPILING
#_INCLUDE 'Xpw/popc_declare.ph'
#_ENDIF

include xpt_coretypes.ph;

;;; This is special to make GX functions etc available in the ordinary
;;; system (N.B. behaves as include for POPC)
loadinclude xpt_xgcvalues.ph;

uses XpwGraphic;


/* BEGINNING OF INTERFACE DEFINITIONS */

/*

This package is designed for use with the X Windows system, via the
Poplog X Windows interface.

The identifiers starting "xt_" or "Xpw" will need to be re-defined if
the package is ever re-implemented in terms of some other graphics
system than LIB xt_windows or LIB XpwGraphic.

In particular, the following procedures will have to be provided, or
re-defined, if this package is ever ported to a different kind of window
manager. For further details see HELP * RC_GRAPHIC

Most of the interface facilities are defined in
    HELP * XpwGraphic, or LIB * xt_windows

XpwDrawPoint(window,x,y);

XpwDrawLine(window,x1,x2,y1,y2)

XpwDrawRectangle(widget, x, y, width, height).

XpwDrawArc(widget,x,y,width,height,start_angle,angle_incr)

XpwDrawString(window, x, y, string)

XpwClearWindow(window);

XptDestroyWindow(window);

XptNewWindow(namestring, size-position-vector, arglist, class) -> w
    Used in xt_new_window, defined below

XpwGraphic
    Not a procedure but the name of a window class

THE NEXT THREE IDENTIFIERS ARE DEFINED BELOW
xt_islivewindow(window);
    Defined below, but will have to be re-defined for a different windowing
    system.

xt_new_window(string,xsize,ysize,xloc,yloc) -> window;
    Defined below, but will have to be re-defined for a different window
    system.

*/

define xt_islivewindow(/*w*/) /* -> boolean */ ;
    XptIsLiveType("Widget");
enddefine;

global vars rc_wm_input = false;

define lconstant xt_new_window(string,xsize,ysize,xloc,yloc) -> widget;
    lvars string, xsize, ysize, xloc, yloc, widget;
    lconstant arg_vector = initv(4); ;;; re-usable vector for XptNewWindow
    lconstant input_arg_vector = {input ^(not(not(rc_wm_input)))};  ;;;thanks to jonm

    check_string(string);
    fi_check(xsize,0,false) ->;
    fi_check(ysize,0,false) ->;
    fi_check(xloc,false,false) ->;
    fi_check(yloc,false,false) ->;

    XptNewWindow(
        string,
        fill(xsize, ysize, xloc, yloc, arg_vector),
        [],
        xpwGraphicWidget,
        [^input_arg_vector]
        ) ->widget;
enddefine;


/* END OF INTERFACE DEFINITIONS */


/* MACRO FOR ASSIGNING DEFAULTS */

/*
Use RC_DEFAULT to assign a default value to name if the test produces
the result indicated. The default value is an arbitrary expression
terminated by semi-colon, e.g. to give a default value to -xposition-
if isinteger(xposition) is false, do.
RC_DEFAULT xposition isinteger false round((xmax + ymin)/2);
*/

define lconstant macro RC_DEFAULT name test result /* default */;
    lvars name, test, result, item;
    ;;; start code for conditional
    dl([if ^test(^name) == ^result then]);

    ;;; read code to semi-colon
    repeat
        readitem() -> item;
    quitif(item == ";");
        item
    endrepeat;

    ;;; end code for conditional
    dl([-> ^name endif;])
enddefine;


/* POINT CONSTRUCTING AND ACCESSING PROCEDURES */

/*
User definable procedures for manipulating points, assuming that
a point is a two-element data structure.
*/

global vars procedure(rc_conspoint, rc_destpoint);

RC_DEFAULT rc_conspoint isundef true conspair;
RC_DEFAULT rc_destpoint isundef true destpair;


define procedure rc_getxy(/* <point> or x, y */) /* -> y -> x */;
    ;;; Utility for rc_jumpto etc. Takes either a point data-structure
    ;;; or two numbers.
    ;;; Given two numbers on stack leave them. Otherwise call rc_destpoint
    unless isnumber(dup()) then rc_destpoint() endunless
enddefine;

/* PREPARE FOR LIB * RC_MOUSE */

weak constant procedure rc_mouse_setup;


/* TURTLE STATE: it has a current position, and rc_heading */

global vars

    ;;; current "turtle" position
    rc_xposition , rc_yposition,

    ;;; The angle the turtle would have to rc_turn, in a clockwise direction,
    ;;; to be facing along the x - axis, to the right. Altered by -rc_turn-
    rc_heading ;

;;; Set up default values: Turtle at origin facing to right
RC_DEFAULT rc_xposition isnumber false 0;
RC_DEFAULT rc_yposition isnumber false 0;
RC_DEFAULT rc_heading isnumber false 0;

/* WINDOW ATTRIBUTES - location and size, coordinate bounds, etc */

;;; Default values are set below.
global vars

    ;;; The window to be used for drawing
    rc_window,

    ;;; Information about window location on screen, and size.
    ;;; in screen units (pixels)
    rc_window_x, rc_window_y,   ;;; top left hand corner
    rc_window_xsize, rc_window_ysize,

    ;;; A variable to determine whether lines should be clipped
    rc_clipping

    ;;; Window bounds used for clipping. In window coodinates, not user
    ;;; coordinates. They must be non-negative integers
    rc_xmin, rc_ymin, rc_xmax, rc_ymax,

;


/* DEFAULT VALUES FOR WINDOW ATTRIBUTES */
;;; Size of window
RC_DEFAULT rc_window_xsize isinteger false 500;
RC_DEFAULT rc_window_ysize isinteger false 500;

;;; Location of top left corner
RC_DEFAULT rc_window_x isinteger false 520;
RC_DEFAULT rc_window_y isinteger false 300;

;;; Variable to determine if clipping should be done
RC_DEFAULT rc_clipping isboolean false true;

;;; Clipping boundary, used if rc_clipping is true.
;;; Default is the window boundary
RC_DEFAULT rc_xmin isinteger false 0;
RC_DEFAULT rc_ymin isinteger false 0;
RC_DEFAULT rc_xmax isinteger false rc_window_xsize;
RC_DEFAULT rc_ymax isinteger false rc_window_ysize;


;;; Define an active variables rc_linewidth, rc_linestyle, rc_linefunction
define active rc_linewidth() /*-> int */;
    ;;; return current linewidth
    XptVal[fast] rc_window(XtN lineWidth:int);
enddefine;

define updaterof active rc_linewidth(/*int*/);
    ;;; change current linewidth
    fi_check(/* int */, 0, false) -> XptVal[fast] rc_window(XtN lineWidth:int);
enddefine;

define active rc_linestyle() /*-> int */;
    ;;; return current linestyle
    XptVal[fast] rc_window(XtN lineStyle:int);
enddefine;

define updaterof active rc_linestyle(/*int*/);
    ;;; change current linestyle
    fi_check(/* int */, 0, false) -> XptVal[fast] rc_window(XtN lineStyle:int);
enddefine;

define active rc_linefunction() /*-> int */;
    ;;; return current linefunction
    XptVal[fast] rc_window(XtN function:int);
enddefine;

define updaterof active rc_linefunction(/*int*/);
    ;;; change current linefunction
    fi_check(/* int */, 0, false) -> XptVal[fast] rc_window(XtN function:int);
enddefine;



/* TRANSFORMATION UTILITIES */

/*
A collection of facilities for transforming user-coordinates to screen
co-ordinates or vice versa, via user definable procedures

    rc_transxyout      rc_transxyin

(The latter is defined in LIB * rc_mouse)

The variables rc_xscale and rc_yscale, positive or negative numbers,
determine scale change in horizontal and vertical directions. User
co-ordinates will be multiplied by these factors to produce screen
co-ordinates.

The variables rc_xorigin  and rc_yorigin determine the location of
the user's coordinate frame in the window, using window coordinates.
*/

global vars
    rc_xscale, rc_yscale,
    rc_xorigin, rc_yorigin;


/* DEFAULT COORDINATE TRANSFORMATION VALUES */

;;; Put the origin in the middle of the screen
RC_DEFAULT rc_xorigin isnumber false rc_window_xsize >> 1 ;
RC_DEFAULT rc_yorigin isnumber false rc_window_ysize >> 1 ;

;;; Make the scales 1, with y going up the screen
RC_DEFAULT rc_xscale isnumber false  1 ;
RC_DEFAULT rc_yscale isnumber false -1 ;

;;; Function for transforming between user co-ordinates and screen
;;; co-ordinates.

define vars procedure rc_transxyout(x, y) /* -> y -> x */;
    ;;; User definable. Make it identfn to have no effect
    ;;; Takes two numbers (user co-ordinates) or a point data-structure
    ;;; and returns two numbers - window co-ordinates.
    ;;; Must leave numbers on stack in same order
    lvars x,y;
    ;;; Do the transformation
    round(if rc_xscale == 1 then x else x * rc_xscale endif + rc_xorigin),
    round(if rc_yscale == 1 then y else y * rc_yscale endif + rc_yorigin),
enddefine;

vars procedure rc_transxyin;    ;;; defined in LIB * RC_MOUSE


define rc_set_coordinates(xorigin, yorigin, xscale, yscale);
    lvars xorigin, yorigin, xscale, yscale;
    xorigin -> rc_xorigin;
    yorigin -> rc_yorigin;
    xscale -> rc_xscale;
    yscale -> rc_yscale;
enddefine;

define rc_shift_frame_by(/*x, y*/);
    ;;; Move the users frame by x, y in user coordinates.
    lvars x, y;
    rc_getxy() -> y -> x;
    rc_xorigin + x * rc_xscale -> rc_xorigin;
    rc_yorigin + y * rc_yscale -> rc_yorigin;
enddefine;

define rc_stretch_frame_by(scale);
    lvars scale;
    scale * rc_xscale -> rc_xscale;
    scale * rc_yscale -> rc_yscale;
enddefine;


/* GETTING INFORMATION ABOUT SIZE OF WINDOW */

define rc_setsize();
    ;;; set the global variables
    XptVal[fast] rc_window(XtN width:XptDimension, XtN height:XptDimension)
                    -> (rc_window_xsize, rc_window_ysize);
enddefine;


/* CREATING AND CLEARING THE WINDOW */

define rc_new_window(width, height, xloc, yloc, setframe);
    ;;; Start a new window with the specified attributes.
    ;;; If setframe is true, setup turtle location and co-ordinate frame etc.
    lvars width, height, xloc, yloc, setframe;
    lvars old = false;

    unless width >= 0 and height >= 0 then
        mishap(width, height, 2, 'WIDTH AND HEIGHT OF WINDOW MUST BE POSITIVE')
    endunless;

    if xt_islivewindow(rc_window) then
        rc_window -> old;
    endif;

    width -> rc_window_xsize;
    height ->rc_window_ysize;
    xloc -> rc_window_x;
    yloc -> rc_window_y;

    if setframe then
        ;;; set clipping boundary to frame
        0 ->> rc_xmin -> rc_ymin;
        width -> rc_xmax; height -> rc_ymax;

        ;;; set origin in middle of window, y increasing upwards
        rc_set_coordinates(
            rc_window_xsize >> 1, rc_window_ysize >> 1, 1, -1);

        0 ->> rc_xposition ->> rc_yposition -> rc_heading;
    endif;

    xt_new_window(
        'Xgraphic',
        rc_window_xsize, rc_window_ysize,
            rc_window_x, rc_window_y) -> rc_window;
    if testdef rc_mouse_setup then weakref rc_mouse_setup(rc_window) endif;
    if old then
        XptDestroyWindow(old);
    endif;
enddefine;

define rc_clear_window();
    ;;; Clear current turtle window
    XpwClearWindow(rc_window);
    ;;; update information about size, in case altered.
    rc_setsize();
enddefine;


/* INITIALISE OR RE_INITIALISE TURTLE AND WINDOW */

define rc_start();
    ;;; Create or clear window, and set turtle at origin
    if xt_islivewindow(rc_window) then
        ;;; clear the window
        rc_clear_window();
        if testdef rc_mouse_setup then weakref rc_mouse_setup(rc_window) endif;
    else
        rc_new_window(rc_window_xsize, rc_window_ysize, rc_window_x, rc_window_y, true)
    endif;
    0 ->> rc_xposition ->> rc_yposition -> rc_heading;
    ;;;; 'Window ready' =>  ??? should this be in?
enddefine;


/* TURTLE UTILITIES - RELATIVE TO CURRENT LOCATION AND HEADING */

;;; The global state variables were defined above

define procedure rc_thispoint() /* -> point */;
    ;;; Return a point record of the current location
    rc_conspoint(rc_xposition, rc_yposition)  /* -> point */
enddefine;

;;; Two procedures for saving the turtle state (in a vector) and
;;; restoring the state. Useful for certain kinds of drawing.

define rc_save_state() /* -> state */;
    ;;; return a 3-element state vector for the turtle
    {%rc_xposition, rc_yposition, rc_heading%}
enddefine;

define rc_restore_state(state);
    lvars state;
    unless isvector(state) and datalength(state) == 3 then
        mishap(state, 1, 'TURTLE STATE VECTOR REQUIRED')
    endunless;
    explode(state) -> rc_heading -> rc_yposition -> rc_xposition
enddefine;

define rc_turn(angle);
    ;;; Alter rc_heading by angle degrees (counter clockwise
    ;;;  if rc_yscale < 0)
    lvars angle;
    rc_heading + angle -> rc_heading;
    while rc_heading >= 360 do rc_heading - 360 -> rc_heading endwhile;
    while rc_heading < 0 do rc_heading + 360 -> rc_heading endwhile;
enddefine;

define rc_turnto(/*x, y*/);
    ;;; rotate turtle to point towards x, y;
    lvars dx, dy;
    (rc_getxy(/*x, y*/) - rc_yposition -> dy) - rc_xposition -> dx;
    if dx = 0.0 and dy = 0.0 then
        mishap(dx + rc_xposition,dy + rc_yposition,2,'POSITION TOO CLOSE TO CURRENT LOCATION')
    endif;
    arctan2(dx,dy) -> rc_heading
enddefine;


/* DRAWING AND JUMPING PROCEDURES */

/* DRAW AT A POINT */

define procedure rc_drawpoint(/*x,y*/);
    ;;; Takes two numbers or a point. Draw a point at location given
    lvars x, y;
    rc_transxyout(rc_getxy()) -> y -> x;
    XpwDrawPoint(rc_window,x,y);
enddefine;


define procedure rc_point_here();
    ;;; Draw a point at current location
    rc_drawpoint(rc_xposition, rc_yposition)
enddefine;


/* CALCULATE NEW POSITION FOR RELATIVE DRAW OR JUMP */

define rc_newposition(amount) -> newy -> newx;
    ;;; given current position and rc_heading, compute new position
    lvars amount, newx, newy;
    dlocal popradians = false;  ;;; always use degrees

    (rc_xposition + amount * cos(rc_heading)) -> newx;
    (rc_yposition + amount * sin(rc_heading)) -> newy;
enddefine;


/* JUMPING UTILITIES ABSOLUTE AND RELATIVE */

define rc_jump(amount);
    ;;; relative to current location and heading
    lvars amount;
    rc_newposition(amount) -> rc_yposition -> rc_xposition;
enddefine;

define rc_jumpby(dx, dy);
    ;;; relative to current location
    lvars dx, dy;
    rc_xposition + dx -> rc_xposition;
    rc_yposition + dy -> rc_yposition;
enddefine;

define rc_jumpto(/* x, y */);
    ;;; Takes either a point or two numbers as arguments.
    ;;; unpack the data-structure if necessary
    rc_getxy() -> rc_yposition -> rc_xposition
enddefine;

/* DRAWING UTILITIES */

/*
rc_drawline is used for drawing lines with optional clipping.

rc_clipping  can be made true to enable clipping in rc_drawline

With LIB XpwGraphic the clipping at window boundaries is done automatically,
but this mechanism allows clipping within the window.
*/


define lconstant procedure rc_clip(x1, y1, x2, y2, xlim, pred) -> bothout;
    ;;; Returns a boolen, true if both points are beyond the limit.
    ;;; If bothout is false, it also returns x1,y1,x2,y2

    ;;; This procedure can also be used with x and y co-ordinates interchanged.
    ;;; See rc_drawline

    ;;; rc_clip tests if either point is beyond the boundary, and
    ;;; if so replaces either p1 or p2 with a point on the boundary.
    ;;; If both are beyond the boundary, bothout is true and returned
    ;;; alone, and nothing is transformed.
    ;;; Otherwise the new co-ordinates and false(bothout) are returned.

    ;;; pred is either > or < and xlim is the co-ordinate of the boundary.
    lvars p1out, p2out, x1, x2, y1, y2, xlim, procedure pred, bothout = false ;
    pred(x1,xlim) -> p1out;      ;;; first point out of bounds.
    pred(x2,xlim) -> p2out;      ;;; target point out of bounds.

    if p1out and p2out then
        true -> bothout;
    else
        if p1out or p2out then
            ;;; find coordinates of intersection point.
            xlim,
            round(y2
                - (y2 - y1)*(number_coerce((x2 - xlim),0.0s+1))/(x2 - x1));

            ;;; assign them to initial or final point:
            if p1out then   ->y1 ->x1
            else            ->y2 ->x2
            endif;
            ;;; bothout is false, so return co-ordinates
        endif;
        x1, y1, x2, y2
    endif
enddefine;

define rc_drawline(/*x1, y1, x2, y2*/);
    ;;; See the man XDrawLine file
    ;;; Draws line from x1,y1 to x2,y2, clipping relative to
    ;;; rc_xmin, rc_xmax, rc_ymin, rc_ymax AFTER the coordinates
    ;;; have been transformed using rc_transxyout

    lvars x1, y1, x2, y2;
    rc_getxy() -> y2 -> x2;
    rc_getxy() -> y1 -> x1;

    ;;; Transform from user to window coordinates
    rc_transxyout(x1, y1) -> y1 -> x1;
    rc_transxyout(x2, y2) -> y2 -> x2;

    if rc_clipping then
        ;;; Test for crossing each boundary in turn.

        ;;; Use rc_clip to test if line crosses x = rc_xmin boundary.
        ;;; If result is true both points are outside the boundary, so draw
        ;;; nothing
        returnif(rc_clip(x1,y1,x2,y2, rc_xmin, nonop <));
           -> (x1, y1, x2, y2);
        ;;; test if line crosses x = rc_xmax boundary.
        returnif(rc_clip(x1, y1, x2, y2, rc_xmax, nonop >));
           -> (x1, y1, x2, y2);
        ;;; now do tests for line crossing rc_ymin or rc_ymax
        returnif(rc_clip(y1, x1, y2, x2,  rc_ymin,  nonop <));
           -> (y1, x1, y2, x2);
        returnif(rc_clip(y1, x1, y2, x2,  rc_ymax,  nonop >));
           -> (y1, x1, y2, x2)
    endif;

    ;;; Draw the line (XpwDrawLine calls -intof- on the numbers
    XpwDrawLine(rc_window, x1, y1, x2, y2);
enddefine;

define rc_drawto(/* x, y */);
    ;;; Draw from to current location
    ;;; Argument may be a point or two numbers.
    lvars  x, y;
    rc_getxy() -> y -> x;
    rc_drawline(rc_xposition, rc_yposition, x, y);
    x -> rc_xposition; y -> rc_yposition;
enddefine;

define rc_drawby(dx, dy);
    ;;; Draw relative to current location
    lvars dx, dy;
    rc_drawto(rc_xposition + dx, rc_yposition + dy)
enddefine;

define rc_draw(amount);
    ;;; Relative to current location and heading
    lvars amount;
    rc_drawto(rc_newposition(amount));
enddefine;

/* DRAWING RECTANGLES AND ARCS */

define rc_draw_rectangle(width, height);
    ;;; See the man XDrawRect file
    ;;; Draws a rectangle of given (user) dimensions with top left corner at
    ;;; current location, and lines horizontal and vertical.
    lvars width, height;

    XpwDrawRectangle(
        rc_window,
        rc_transxyout(rc_xposition, rc_yposition),
        round(abs(width * rc_xscale)), round(abs(height * rc_yscale)))
enddefine;

define rc_draw_oblong(width, height, radius);
lvars width, height, radius;
    XpwDrawRoundedRectangle(
        rc_window,
        rc_transxyout(rc_xposition, rc_yposition),
        round(abs(width * rc_xscale)), round(abs(height * rc_yscale)),
        round(abs(radius * rc_xscale)), round(abs(radius * rc_yscale)))
enddefine;

;;; There appears to be a bug in XDrawArc that makes this sometimes
;;; do strange things with angle combinations of 45 and 90 or multiples
;;; thereof.
define rc_draw_arc(/*x, y*/, width, height, angle1, angleinc);
    lvars x, y, width, height, angle1, angleinc;
    ;;; See the man XDrawArc file
    ;;; Draws an arc on a circle or ellipse bounded by the rectangle whose top
    ;;; left corner is (x, y), with given width and height, all in user
    ;;; co-ordinates
    ;;; The arc is defined by the start angle of the radius angle1 and
    ;;; the amount it is to be increased angleinc, measured from the
    ;;; three O'clock position, in 64ths of a degree, counterclockwise
    ;;; (negative angles are measured clockwise).
    rc_getxy() -> y -> x;
    XpwDrawArc(rc_window,
        rc_transxyout(x,y),
        round(abs(width * rc_xscale)), round(abs(height * rc_yscale)),
        round(angle1), round(angleinc))
enddefine;

define rc_arc_around(radius, degrees);
    ;;; Draw a circular arc from the current location with radius given
    ;;; (in user co-ordinates) in such a way that it starts at current
    ;;; location. -degrees- is the angle of the arc in degrees. If
    ;;; it is positive, curve round to left, otherwise to right
    lvars xcentre, ycentre, radius, degrees, dx1, dy1, dx2, dy2, alpha;
    dlocal popradians = false;

    ;;; compute displacement from current location to centre of circle.
    radius * sin(rc_heading) -> dx1;
    radius * cos(rc_heading) -> dy1;
    ;;; compute displacement from centre to new location
    rc_heading + degrees -> alpha;
    radius*sin(alpha) -> dx2;
    radius*cos(alpha) -> dy2;
    ;;; calculate centre of circle and new location
    if degrees >= 0 then
        (rc_xposition - dx1 ->> xcentre) + dx2 -> rc_xposition;
        (rc_yposition + dy1 ->> ycentre) - dy2 -> rc_yposition;
    else
        (rc_xposition + dx1 ->> xcentre) - dx2 -> rc_xposition;
        (rc_yposition - dy1 ->> ycentre) + dy2 -> rc_yposition;
    endif;

    ;;; The use of rc_xscale and rc_yscale means that this will be wrong
    ;;; if rc_transxyout has been redefined not to use them.  However,
    ;;; without this it is wrong when rc_transxyout is unchanged but
    ;;; the sign of either scale has been changed.  Sometime it
    ;;; needs doing properly - which means using rc_transxyout to
    ;;; get the positions, and then converting them back? --- DSY

    rc_draw_arc(
        xcentre - sign(rc_xscale) * radius,
        ycentre - sign(rc_yscale) * radius,
        dup(radius+radius),
        if degrees >= 0 then
            (rc_heading - 90) * 64
        else
            (90 + rc_heading) * 64
        endif,
        round(degrees * 64));
    ;;; New heading
    rc_turn(degrees)
enddefine;


/* PRINTING STRINGS IN THE WINDOW */

define rc_print_at (/*x, y*/ string);
    ;;; takes a point, or two numbers, and something to print at the location
    ;;; specified
    lvars x, y, string;
    check_string(string);
    rc_transxyout(rc_getxy(/*x, y*/)) -> y ->x;
    XpwDrawString(rc_window, x, y, string)
enddefine;


define rc_print_here(string);
    lvars string;
    rc_print_at(rc_xposition, rc_yposition, string)
enddefine;


constant rc_graphic = true;  ;;; for "uses"

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 15 1993
        Added comment above loadinclude xpt_xgcvalues.ph
--- David S Young, Apr 22 1993
        Fixed bug in rc_arc_around which made it fail whenever the
        sign of rc_xscale or rc_yscale had been changed from the default.
--- John Gibson, Apr  6 1993
        Replaced use of XptW*idgetSet with uses Xpw/XpwGraphic. Put file
        in batch load.
--- Ian Rogers, Dec  2 1992
        Added loadinclude xpt_xgcvalues.ph; to fix BR davidy.75
--- John Gibson, Sep 12 1992
        Changed to use XptVal
--- Adrian Howard, Feb 28 1992 : Added -rc_wm_input- to allow control of the
        "input" resource of the shell of XpwGraphic window.
--- Adrian Howard, Feb 24 1992 : Installed changes made by Jonathan
        Cunningham (changed to make rc_window accept input, 23 Jan 1992
        added an rc_draw_oblong)
--- Aaron Sloman, Jul 15 1991
    Put in missing declaration of xlim
--- Jonathan Meyer, Jul  6 1991
        Changed to use XptIsLiveType
--- Aaron Sloman, Jan  7 1991
    Removed check for integer args in rc_draw_rectangle (Bugreport Davidy.30)
--- David Young, Dec  5 1990 Fixed clipping bug in rc_drawline
--- Adrian Howard, Dec  3 1990 : Changed to use XptNewWindow
--- Roger Evans, Nov 22 1990 fixed rc_set_size to use XtN
--- Roger Evans, Nov 19 1990 removed uses of - after XtN
--- Roger Evans, Nov 11 1990 removed use of XptSimpleStartup
--- Roger Evans, Oct 22 1990 reinstalled in masters
--- Aaron Sloman, Oct 15 1990
    removed spurious local in rc_setsize
--- Jonathan Meyer, Sep 19 1990
    Made use of X*ptValue
--- Andreas Schoter, ???
    Fixed for popversion xpop.
--- Aaron Sloman, Aug 14 1990
    Added "uses xt_compat"
--- Andreas Schoter, July 16 1990
    Changed all variable names from Pop* to Xpw*
--- Aaron Sloman, Jun 22 1990 fixed stuff left on stack by rc_new_window
--- Aaron Sloman, Jun 22 1990 added declaration for rc_transxyin
--- Aaron Sloman, Jun 22 1990 added lib rc_context
--- Aaron Sloman, May 23 1990 Inserted new checking and rounding
--- Aaron Sloman, May 23 1990 Created LIB * RC_MOUSE
--- Aaron Sloman, May 19 1990
    Added rc_turnto to this file, created LIB RC_ROTATEXY, extended
    HELP * RC_GRAPHIC
 */
