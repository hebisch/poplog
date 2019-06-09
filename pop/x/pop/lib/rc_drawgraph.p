/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_drawgraph.p
 > Purpose:         Procedure for drawing a graph of a function
 > Author:          Aaron Sloman, May 1990
 > Documentation:   HELP * RC_GRAPHIC, TEACH * RC_GRAPHIC
 > Related Files:   LIB * RC_GRAPHIC, LIB * RC_ROTATE_XY, * RC_MOUSE
 */
compile_mode :pop11 +strict;

/*

The procedure rc_drawgraph takes the following arguments:
        xmin,xmax,ymin,ymax,    ;;; limits of graph
        xstep,ystep,            ;;; steps between marks on axes
        xline, yline,           ;;; lengths of marks on axes
        xincr,                  ;;; amount by which to increment x
        procedure fun           ;;; function for graph

And produces two results
    -> minyvalue
    -> maxyvalue


Example of use

define testproc(x, x0, r);
    ;;; a test procedure - for drawing a parabola
    ;;; x0 and r will be partially applied, to give different parabolas
    ;;; y = r*x*(x0 - x)
    lvars x,r;
    r*x*(x0 - x)
enddefine;

;;; Set co-ordinate frame with origin at bottom left of window,
;;; with window bounds each corresponding to 1 unit.
rc_set_coordinates(0, rc_window_ysize, rc_window_xsize, -rc_window_ysize);

rc_clear_window();
rc_drawgraph(0,1,0,1,0.1,0.1,0.02,0.02,0.05, testproc(%1,2.3%)) =>

;;; Change origin to middle, and halve the scale.
rc_set_coordinates
    (rc_window_xsize / 2.0, rc_window_ysize/2.0,
        rc_window_xsize/2.0, -rc_window_ysize/2.0);

rc_clear_window();
rc_drawgraph(-1,1,-1,1,0.1,0.1,0.02,0.02,0.05, testproc(%0.5,2.3%)) =>

rc_drawgraph(-1,1,-1,1,0.05,0.05,0.02,0.02,0.05, testproc(%0.6, 3%)) =>
*/

section;

uses rc_graphic;

define global rc_drawgraph(
        xmin,xmax,ymin,ymax,    ;;; limits of graph
        xstep,ystep,            ;;; steps between marks
        xline, yline,           ;;; lengths of lines on axes
        xincr,                  ;;; amount by which to increment x
        fun)            ;;; function for graph
            -> minyvalue -> maxyvalue;

    ;;; Draw a graph of the function fun with axes and scale as specified

    ;;; incrementing x by the amount xincr
    ;;; Mark positions on x and y axes correponding to xstep and ystep
    ;;; using lines of length xline and yline
    ;;; Return maximum value of y achieved
    lvars val, x, y, xmin,xmax,ymin,ymax,xstep,ystep,xline, yline,
        xincr, procedure fun, , minyvalue, maxyvalue;

    ;;; half xline and yline
    xline / 2.0 -> xline;
    yline / 2.0 -> yline;

    ;;; draw axes
    rc_drawline(xmin,0,xmax,0);
    rc_drawline(0,ymin,0,ymax);

    ;;; draw marks on axes
    for x from xmin by xstep to xmax do
        rc_jumpto(x, -xline);
        rc_drawto(x, xline);
    endfor;
    for y from ymin by ystep to ymax do
        rc_jumpto(-yline, y);
        rc_drawto(yline, y);
    endfor;

     10000000 -> minyvalue;
    -10000000 -> maxyvalue;

    rc_jumpto(xmin, fun(xmin));
    for x from xmin + xincr by xincr to xmax do
        max(rc_yposition, maxyvalue) -> maxyvalue;
        min(rc_yposition, minyvalue) -> minyvalue;
        rc_drawto(x, fun(x));
    endfor;
    max(rc_yposition, maxyvalue) -> maxyvalue;
    min(rc_yposition, minyvalue) -> minyvalue;
enddefine;

endsection;
