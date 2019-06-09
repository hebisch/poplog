/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rcg_utils.p
 > Purpose:         Backup for RC_GRAPHPLOT and RC_GRAPHPLOT2
 > Author:          David Young, Dec  7 1990 (see revisions)
 > Documentation:   TEACH * RC_GRAPHPLOT, HELP * RC_GRAPHPLOT
 > Related Files:   LIB * RC_GRAPHPLOT, LIB * RC_GRAPHPLOT2, LIB RC_GRAPHIC
 */
compile_mode :pop11 +strict;

uses-now popxlib;

section;
exload_batch;

include xpt_xgcvalues.ph;

uses
    rc_graphic,
    rc_set_scale,
    rc_set_coord_frame,
    rc_clip_region,
;

/* GLOBAL VARIABLES */

vars rcg_char_width = 7, rcg_char_height = 12; ;;; need to be set intelligently

;;; Global variables to control what region to use
vars rcg_win_reg = 0.1,
     rcg_usr_reg = undef;

;;; to control plotting style
vars rcg_pt_type = "line";

;;; to control axis drawing
vars rcg_ax_type = "axes";

;;; to determine whether to clear the window
vars rcg_newgraph = true;

;;; to control axis annotation
;;; First 2 are fractions of graph size. 3rd is target, not actual no
vars
    rcg_mk_len = 0.02,    ;;; length of an annotated mark
    rcg_tk_len = 0.01,    ;;; length of a tick
    rcg_mk_no = 5,     ;;; the number of annotated marks to aim for on each axis
    rcg_tk_no = 10;     ;;; the number of ticks per mark

;;; to control how much space to leave round
;;; the dependent variable's region, as a fraction of its range.
vars rcg_ax_space = 0.2;

;;; to control line type at different stages
vars
    rcg_pt_lw = 0,
    rcg_pt_lf = GXcopy,
    rcg_pt_ls = LineSolid,
    rcg_ax_lw = 0,
    rcg_ax_lf = GXcopy,
    rcg_ax_ls = LineSolid,
    rcg_mk_lw = 0,
    rcg_mk_lf = GXcopy,
    rcg_mk_ls = LineSolid,
    rcg_tk_lw = 0,
    rcg_tk_lf = GXcopy,
    rcg_tk_ls = LineSolid;

;;; to control size of plot symbols
vars rcg_pt_cs = rcg_char_width;

/* HANDY STUFF */

define active:3 rcg_setline;
    rc_linewidth, rc_linefunction, rc_linestyle
enddefine;

define updaterof active:3 rcg_setline;
        -> (rc_linewidth, rc_linefunction, rc_linestyle)
enddefine;

/* DATA STRUCTURING */

/* Next few procedures convert different kinds of things into
repeaters - useful to have a unified data structure to pass to the
basic plotting procedure */

define rcg_isrealproc(p) /* -> boolean */;
    lvars p;
    p.isprocedure and not(p.isarray)
enddefine;

define rcg_lin_rep(tmin,tinc,tmax) -> (valrepeater, resetter);
    ;;; Returns a value repeater that returns tmin the first time
    ;;; it is called, then increments by tinc until it reaches
    ;;; tmax, returning <termin> thereafter.
    ;;; Calling resetter sets if off from tmin again.
    lvars tmin tinc tmax valrepeater resetter;
    lvars t = tmin;
    procedure;
        if t > tmax then
            termin
        else
            t;  /* -> result */
            t + tinc -> t
        endif
    endprocedure -> valrepeater;
    procedure; tmin -> t endprocedure -> resetter;
enddefine;

define rcg_dat_rep(data) -> (datarepeater, resetter);
    ;;; Returns a repeater that gives each successive value from a list,
    ;;; array or vector, returning <termin> thereafter.
    ;;; Also returns a resetter that makes the next call of
    ;;; the repeater start at the beginning.
    ;;; Arrays are just replaced by the relevant bit of their arrayvectors.
    lvars data datarepeater resetter;
    lvars ldata point accessor firstpoint lastpoint;

    if data.rcg_isrealproc then
        data -> datarepeater;
        false -> resetter

    elseif data.islist then
        data -> ldata;

        procedure;
            if ldata == [] then
                termin
            else
                fast_front(ldata);   ;;; result
                fast_back(ldata) -> ldata
            endif
        endprocedure -> datarepeater;

        procedure; data -> ldata endprocedure -> resetter;

    else
        if data.isarray then
            arrayvector_bounds(data) -> firstpoint -> lastpoint;
            arrayvector(data) -> data
        else
            1 -> firstpoint;
            datalength(data) -> lastpoint
        endif;
        data.datakey.class_fast_subscr -> accessor;
        firstpoint -> point;

        procedure;
            if point fi_> lastpoint then
                termin
            else
                accessor(point,data);   ;;; result
                point fi_+ 1 -> point
            endif
        endprocedure -> datarepeater;

        procedure; firstpoint -> point endprocedure -> resetter;

    endif
enddefine;

define rcg_fun_rep(func,reset) -> (func, reset);
    ;;; If the function does not have a resetter (passed in reset)
    ;;; then create one by getting all the values into a vector
    ;;; and returning a repeater and resetter for it.
    lvars f;
    lvars func reset;
    unless reset then
        rcg_dat_rep(
            {%
                until (func() ->> f) == termin do
                    f
                enduntil
                %}
        ) -> (func, reset)
    endunless
enddefine;

/* UTILITIES FOR AXIS DRAWING AND REGION SETTING */

define rcg_roundup(val) /* -> roundval */;
    ;;; round up a value to 1,2 or 5 times a power of 10
    lvars val roundval;
    lvars ilog pten ared;
    intof(log10(val)) -> ilog;
    if val < 1 then ilog-1 -> ilog endif;
    10 ** ilog -> pten;
    val/pten -> ared;
    if ared > 5 then 10
    elseif ared > 2 then 5
    elseif ared > 1 then 2
    else 1
    endif * pten /* -> roundval */
enddefine;

define rcg_upnext(x,xincr) -> result;
    ;;; Round up x to the next multiple of xincr
    lvars x xincr result;
    (x div xincr) * xincr -> result;
    if result < x then result + xincr -> result endif
enddefine;

define rcg_downnext(x,xincr) -> result;
    ;;; Round down x to the next multiple of xincr
    lvars x xincr result;
    (x div xincr) * xincr -> result;
    if result > x then result - xincr -> result endif
enddefine;

define rcg_stretch(ymin,ymax) -> (ylo,yhi);
    ;;; Stretches the limits for a variable using rcg_ax_space.
    ;;; Will not stretch past zero.
    ;;; A messy procedure that also (somewhat unpleasantly) raises
    ;;; ymax a touch in the hope of getting in the last mark or whatever,
    ;;; and similarly lowers ymin.
    ;;; If the two limits are equal, then "fixes" this
    ;;; regardless of rcg_ax_space by making the range go from 0 to
    ;;; twice the value, or -1 to 1.
    lconstant frac = 0.0001;    ;;; extend by this much of range at end
    lvars ymin ymax, ylo = ymin, yhi = ymax;
    lvars yextra;
    if ymin.isreal and ymax.isreal then
        if ymin = ymax then
            if ymin = 0 then
                -1 ->> ymin -> ylo;
                1 ->> ymax -> yhi
            else
                min(0,2*ymin) ->> ymin -> ylo;
                max(0,2*ymax) ->> ymax -> yhi
            endif
        endif;
        if rcg_ax_space then
            rcg_ax_space * (ymax-ymin) / 2 -> yextra;
            ;;; stretch a bit first
            ymax + yextra -> yhi;
            ymin - yextra -> ylo;
            ;;; then round up to nearest sensible point
            rcg_roundup(yextra) -> yextra;
            rcg_upnext(yhi,yextra) -> yhi;
            rcg_downnext(ylo,yextra) -> ylo;
            ;;; Don't cross zero, however
            if ymin >= 0 and ylo < 0 then 0 -> ylo endif;
            if ymax <= 0 and yhi > 0 then 0 -> yhi endif;
            ;;; Finally, twitch limits a little (less than a pixel, one trusts)
            frac * (yhi - ylo) -> yextra;
            if yhi.isdecimal and yhi /= 0 then
                yhi + yextra -> yhi
            endif;
            if ylo.isdecimal and ylo /= 0 then
                ylo - yextra -> ylo
                endif
            endif
        endif
enddefine;

/* Procedure to position the graph */

define rcg_set_reg(region) -> region;
    ;;; Takes the actual extremes of x and y for the graph to be plotted,
    ;;; as a list going xmin xmax ymin ymax.  This may be  overriden by
    ;;; the values that are defined in the global rcg_usr_reg.
    ;;; Returns the same region or the override values.
    ;;; If rcg_win_reg is defined, will as a side effect map the
    ;;; user region onto the screen region, and set the clipping limits
    ;;; to this region.
    lvars region;
    lvars targ tglob;
    dlocal rcg_win_reg;

    ;;; first establish the user region
    if rcg_usr_reg.islist then ;;; override the arguments
        [% for tglob, targ in rcg_usr_reg, region do
                if tglob == undef then targ else tglob endif
            endfor %] -> region
    endif;

    ;;; If user region defined, establish screen region
    unless lmember(undef,region) then
        ;;; Special case: just a number specifies a border as a fraction of
        ;;; window size
        if rcg_win_reg.isnumber then
            [frame % rcg_win_reg, 1-rcg_win_reg,
                1-rcg_win_reg, rcg_win_reg %]
                -> rcg_win_reg
        endif;
        ;;; If the screen region is now a list, have to map the user region
        if rcg_win_reg.islist then
            if length(rcg_win_reg) == 5 then
                rc_set_coord_frame(hd(rcg_win_reg),
                    tl(rcg_win_reg), region)
            else
                rc_set_coord_frame(false,rcg_win_reg,region)
            endif;
            ;;; and set the clipping boundaries
            rc_clip_region(region);
        endif
    endunless
enddefine;

/* AXIS DRAWING */

define rcg_annot(xory,u,t,val);
    ;;; Prints the value. It is put to the left of the point specified
    ;;; if xory is "y" and below it if xory is "x",
    ;;; using rcg_char_width and rcg_char_height to decide the
    ;;; position (messy). If the value is not a string, it's converted to
    ;;; one using pr.
    lvars xory u t val;
    dlocal pop_pr_ratios = false;

    ;;; CONVERT ITEM TO STRING USING -pr-
    define lconstant mk_string(item);
        lvars item;
        dlocal cucharout = identfn;
        consstring(#| pr(item) |#);
    enddefine;

    unless val.isstring then val.mk_string -> val endunless;

    if xory == "y" then
        t - (length(val)+1) * rcg_char_width / rc_xscale -> t;
        u + 0.3 * rcg_char_height / rc_yscale -> u;
        rc_print_at(t,u,val)
    elseif xory == "x" then
        u - 0.5 * length(val) * rcg_char_width / rc_xscale -> u;
        t + rcg_char_height / rc_yscale -> t;
        rc_print_at(u,t,val)
    endif;

enddefine;

define rcg_ladder(xory,tstart,tend,tinc,umin,umax);
    ;;; Draw a set of vertical parallel lines.  Useful
    ;;; for axis marks and making a grid.
    ;;; If xory is "x" then the lines are vertical; if "y" then
    ;;; they are horizontal; could be generalised to any angle.
    ;;; tstart, tend and tinc refer to the limits on the coordinate
    ;;; along the length of the ladder and the distance between rungs;
    ;;; umin and umax refer to the ends of the rungs.
    ;;; Rungs always occur at multiples of tinc from the origin of t.
    lvars t;
    lvars xory tstart tend tinc umin umax;
    ;;; Avoid problems with signs
    min(tstart,tend), max(tstart,tend), abs(tinc) -> (tstart,tend,tinc);
    rcg_upnext(tstart,tinc) -> tstart;
    if xory == "x" then
        for t from tstart by tinc to tend do
            rc_jumpto(t,umin); rc_drawto(t,umax)
        endfor
    elseif xory == "y" then
        for t from tstart by tinc to tend do
            rc_jumpto(umin,t); rc_drawto(umax,t)
        endfor
    endif
enddefine;

define rcg_axis(xory,               ;;; whether x or y axis
        tstart,tend,u,              ;;;  where to put the main line
        markup, markdown, markinc,  ;;; large axis marks
        tickup, tickdown, tickinc,  ;;; small axis ticks
        labl,                       ;;; axis label
        no0);                       ;;; suppress zeroest annotation
    ;;; General axis drawing procedure
    dlocal rcg_setline;
    lvars xory tstart tend u markup markdown markinc tickup
        tickdown tickinc labl no0;
    lvars t;
    if labl then        ;;; can switch off by setting labl false
        ;;; draw the main line
        rcg_ax_lw, rcg_ax_lf, rcg_ax_ls -> rcg_setline;
        if xory == "x" then
            rc_jumpto(tstart,u); rc_drawto(tend,u)
        elseif xory == "y" then
            rc_jumpto(u,tstart); rc_drawto(u,tend)
        endif;
        min(tstart,tend), max(tstart,tend) -> (tstart,tend);
        if markinc then
            abs(markinc) -> markinc;
            ;;; Do large marks
            rcg_mk_lw, rcg_mk_lf, rcg_mk_ls -> rcg_setline;
            rcg_ladder(xory,tstart,tend,markinc,u-markdown,u+markup);
            ;;; Add annotations
            for t from rcg_upnext(tstart,markinc) by markinc to tend do
                unless no0 and (t = 0 or (t = tstart and tstart > 0)) then
                    rcg_annot(xory,t,u-markdown,t)
                endunless
            endfor
        endif;
        ;;; Do small marks
        if tickinc then
            rcg_tk_lw, rcg_tk_lf, rcg_tk_ls -> rcg_setline;
            rcg_ladder(xory,tstart,tend,tickinc,u-tickdown,u+tickup)
        endif;
        ;;; Add a label
        unless labl == nullstring then
            if xory == "x" then
                rcg_annot(xory, tend, u-markdown+1.2*rcg_char_height/rc_yscale, labl)
            elseif xory == "y" then
                rcg_annot(xory, tend-1.1*rcg_char_height/rc_yscale, u-markdown, labl)
            endif
        endunless
    endif
enddefine;

define rcg_def_ax(xmin,xmax,xlabl,ymin,ymax,ylabl);
    ;;; Draws an axis pair using sensible settings
    ;;; for things like tick size.
    dlocal pr;
    lvars xmin xmax xlabl ymin ymax ylabl;
    ;;; constants are mainly fractions of graph size
    lvars xsize = xmax-xmin, ysize = ymax-ymin,
        xmark = rcg_mk_len*ysize, xtick = rcg_tk_len*ysize,
        ymark = rcg_mk_len*xsize, ytick = rcg_tk_len*xsize,
        xmarkinc = rcg_mk_no and rcg_roundup(xsize/rcg_mk_no),
        xtickinc = xmarkinc and rcg_tk_no and xmarkinc/rcg_tk_no,
        ymarkinc = rcg_mk_no and rcg_roundup(ysize/rcg_mk_no),
        ytickinc = ymarkinc and rcg_tk_no and ymarkinc/rcg_tk_no;

    if rcg_ax_type == "axes" then
        if ymin < 0 and ymax > 0 then   ;;; x axis is in middle
            rcg_axis("x",xmin,xmax,0, xmark,xmark,xmarkinc,
                xtick,xtick,xtickinc, xlabl, true)
        else        ;;; draw x axis at bottom
            rcg_axis("x",xmin,xmax,ymin, xmark,0,xmarkinc,
                xtick,0,xtickinc, xlabl, false)
        endif;
        if xmin < 0 and xmax > 0 then   ;;; y axis is in middle
            rcg_axis("y",ymin,ymax,0, ymark,ymark,ymarkinc,
                ytick,ytick,ytickinc, ylabl, true)
        else        ;;; draw y axis at left
            rcg_axis("y",ymin,ymax,xmin, ymark,0,ymarkinc,
                ytick,0,ytickinc, ylabl, false)
        endif
    elseif rcg_ax_type == "box" then
        rcg_axis("x",xmin,xmax,ymin, xmark,0,xmarkinc,
            xtick,0,xtickinc, xlabl, false);
        rcg_axis("y",ymin,ymax,xmin, ymark,0,ymarkinc,
            ytick,0,ytickinc, ylabl, false);
        erase -> pr;    ;;; switch off annotations
        rcg_axis("x",xmin,xmax,ymax, 0,xmark,xmarkinc,
            0,xtick,xtickinc, xlabl and nullstring, false);
        rcg_axis("y",ymin,ymax,xmax, 0,ymark,ymarkinc,
            0,ytick,ytickinc, ylabl and nullstring, false)
    endif
enddefine;

/* INDIVIDUAL POINT PLOTTING PROCEDURES */

/* Procedure for plotting line graphs */

define active rcg_plt_line /* -> procedure */;
    ;;; Needs to be active so that drawing can be initialised and
    ;;; remembered.
    lvars drawing = false;

    procedure(x,y); lvars x y;
        unless x.isreal and y.isreal then
            false -> drawing
        elseif drawing then
            rc_drawto(x,y)
        else
            rc_jumpto(x,y);
            true -> drawing
        endunless
    endprocedure
enddefine;

/* Procedures for drawing points for point graphs */

define rcg_plt_square(x,y);
    ;;; Draw a square of size rcg_pt_cs in screen coords,
    ;;; centred on x,y in user coords.
    ;;; Goes through rc_graphic stuff instead of calling XpwDrawRectangle
    ;;; so that clipping can be applied consistently.
    lvars x y;
    lvars xsize = rcg_pt_cs/rc_xscale, ysize = rcg_pt_cs/rc_yscale;
    if x.isreal and y.isreal then
        rc_jumpto(x-xsize/2, y-ysize/2);
        rc_drawby(xsize,0);
        rc_drawby(0,ysize);
        rc_drawby(-xsize,0);
        rc_drawby(0,-ysize);
    endif;
enddefine;

define rcg_plt_cross(x,y);
    ;;; Draw a cross of size rcg_pt_cs in screen coords
    ;;; centred on x,y in user coords.
    lvars x y;
    lvars xsize = rcg_pt_cs/rc_xscale, ysize = rcg_pt_cs/rc_yscale;
    if x.isreal and y.isreal then
        rc_jumpto(x-xsize/2, y-ysize/2);
        rc_drawby(xsize,ysize);
        rc_jumpby(-xsize,0);
        rc_drawby(xsize,-ysize)
    endif
enddefine;

define rcg_plt_plus(x,y);
    ;;; Draw a plus, as above
    lvars x y;
    lvars xsize = rcg_pt_cs/rc_xscale, ysize = rcg_pt_cs/rc_yscale;
    if x.isreal and y.isreal then
        rc_jumpto(x-xsize/2, y);
        rc_drawby(xsize,0);
        rc_jumpto(x,y-ysize/2);
        rc_drawby(0,ysize)
    endif
enddefine;

define rcg_plt_circle(x,y);
    ;;; Draw a circle, as above
    lvars x y;
    lconstant circle = 360*64;  ;;; a circle in rc_draw_arc's units
    lvars xd = rcg_pt_cs/rc_xscale, yd = rcg_pt_cs/rc_yscale;
    if x.isreal and y.isreal then
        rc_draw_arc(x-xd/2, y-yd/2, xd, yd, 0, circle)
    endif
enddefine;

;;; for uses
constant rcg_utils = true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- David S Young, Sep 23 1996
        Added rcg_plt_circle
--- David S Young, Jun 21 1994
        Fixed bug in rcg_def_ax that prevented rcg_tk_no and rcg_mk_no
        being <false>, as described in the help file.
--- Adrian Howard, Jul  6 1993
        Axis notation now works properly with pop_pr_quotes set to true
--- John Gibson, Jun 15 1993
        Added mssing include for xpt_xgcvalues.ph, put in batch load
--- David S Young, Nov 26 1992
        Removed rc_clip_region and rc_set_coord_frame to separate libraries
--- David S Young, Nov 13 1991
    Changed sys_>< back to ><, since this is necessary to allow redefinition
    of pr to have an effect.
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
 */
