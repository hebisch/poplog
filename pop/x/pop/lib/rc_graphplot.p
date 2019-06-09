/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_graphplot.p
 > Purpose:         Package for drawing graphs of functions and data
 > Author:          David Young, Dec  7 1990 (see revisions)
 > Documentation:   TEACH * RC_GRAPHPLOT, HELP * RC_GRAPHPLOT
 > Related Files:   LIB * RCG_UTILS, LIB * RC_GRAPHIC, LIB * RC_GRAPHPLOT2
 */
compile_mode :pop11 +strict;

/* Library to draw graphs of functions or data using * RC_GRAPHIC */

uses-now popxlib;

section;
exload_batch;

uses rcg_utils;

/* BASIC PLOTTING PROCEDURE */

define rcg_basic(tfunc,xfunc,yfunc,plotfunc);
    lvars t x y;
    lvars tfunc xfunc yfunc plotfunc;
    until
        (tfunc() ->> t) == termin or
        (xfunc(t) ->> x) == termin or
        (yfunc(t) ->> y) == termin
    do
        plotfunc(x,y)
    enduntil
enddefine;

/* LIMIT FINDING */

define rcg_maxmin(values) -> (mx, mn);
    ;;; Find max and min values, skipping undefined ones
    ;;; The argument should be a repeater that returns
    ;;; termin when finished.  Anything that isn't a real number
    ;;; just gets ignored.
    lvars val;
    lvars values, mx = undef,  mn = undef;
    ;;; Find the first defined values
    until ((values() ->> val) == termin) or val.isreal do enduntil;
    unless val == termin then
        val ->> mx -> mn
    endunless;
    ;;; And compare with the rest
    until (values() ->> val) == termin do
        if val.isreal then
            if val > mx then
                val -> mx
            elseif val < mn then
                val -> mn
            endif
        endif
    enduntil
enddefine;

/* DATA STRUCTURING */

define 5 rcg_rep_<> (tfunc,func) /* -> result */;
    ;;; concatenate a repeater and a function,
    ;;; result returns termin if the repeater does so
    lvars tfunc func;
    procedure();
        lvars t;
        if (tfunc() ->> t) == termin then
            termin
        else
            func(t)
        endif
    endprocedure
enddefine;

/* TOP-LEVEL CALLER */

define rc_graphplot(xfunc, xlabel, yfunc, ylabel) -> region;
    dlocal rcg_setline, rcg_pt_type;
    lvars xfunc xlabel yfunc ylabel;
    lvars tmin tinc tmax,
        xmin = undef, xinc, xmax = undef, xreset = false, nx = false,
        ymin = undef, yinc, ymax = undef, yreset = false, ny = false,
        tfunc = false, treset = false,
        region;

    ;;; sort out the arguments
    if yfunc.isnumber then
        xfunc, xlabel, yfunc -> (xfunc, xlabel, ymin, yinc, ymax);
        identfn -> yfunc;
        rcg_lin_rep(ymin,yinc,ymax) -> (tfunc, treset);
    elseif not(yfunc.rcg_isrealproc) then   ;;; yfunc is data structure
        length(yfunc) -> ny;
        rcg_dat_rep(yfunc) -> (yfunc, yreset);
    endif;

    if xfunc.isnumber then
        xfunc -> (xmin, xinc, xmax);
        identfn -> xfunc;
        rcg_lin_rep(xmin,xinc,xmax) -> (tfunc, treset);
    elseif not(xfunc.rcg_isrealproc) then   ;;; xfunc is a data structure
        length(xfunc) -> nx;
        rcg_dat_rep(xfunc) -> (xfunc, xreset);
    endif;

    unless tfunc then   ;;; x and y both data or procedures
        if nx and ny then    ;;; both are data
            rcg_lin_rep(1,1,min(nx,ny)) -> (tfunc,treset)
        else    ;;; at least one is a real procedure, so get parameter
                -> tfunc;
            if tfunc.isnumber then
                tfunc -> (tmin, tinc, tmax);
                rcg_lin_rep(tmin,tinc,tmax) -> (tfunc, treset)
            elseif not(tfunc.rcg_isrealproc) then   ;;; tfunc is a data structure
                rcg_dat_rep(tfunc) -> (tfunc, treset);
            endif
        endif
    endunless;

    ;;; sort out the plotting region - first attempt
    rcg_set_reg([% xmin,xmax,ymin,ymax %]) -> region;
    explode(region) -> (xmin,xmax,ymin,ymax);

    ;;; If unsuccessful, get some data limits
    if lmember(undef,region) then       ;;; need some limits
        if xmin == undef or xmax == undef then  ;;; need to know x limits
            rcg_fun_rep(tfunc,treset) -> (tfunc,treset);
            unless xreset then
                rcg_fun_rep(tfunc rcg_rep_<> xfunc, xreset) -> (xfunc, xreset);
                treset();
            endunless;
            rcg_maxmin(xfunc) -> (xmax, xmin);
            xreset();
            rcg_stretch(xmin,xmax) -> (xmin, xmax);
        endif;
        if ymin == undef or ymax == undef then  ;;; need to know y limits
            rcg_fun_rep(tfunc,treset) -> (tfunc,treset);
            unless yreset then
                rcg_fun_rep(tfunc rcg_rep_<> yfunc, yreset) -> (yfunc, yreset);
                treset();
            endunless;
            rcg_maxmin(yfunc) -> (ymax, ymin);
            yreset();
            rcg_stretch(ymin,ymax) -> (ymin, ymax);
        endif;

        ;;; again try to set plotting region
        rcg_set_reg([% xmin,xmax,ymin,ymax %]) -> region;
        if lmember(undef,region) then mishap('Insufficient data', []) endif;
        explode(region) -> (xmin,xmax,ymin,ymax);
    endif;

    ;;; start a new graph if rcg_newgraph is trey
    if rcg_newgraph then rc_start() endif;

    ;;; Draw the axes
    rcg_def_ax(xmin,xmax,xlabel,ymin,ymax,ylabel);

    ;;; Make x and y functions take arguments
    if xreset then  ;;; xfunc is a repeater but needs to take an arg
        erase <> xfunc -> xfunc
    endif;
    if yreset then
        erase <> yfunc -> yfunc
    endif;

    ;;; Draw the curve
    if rcg_pt_type then
        rcg_pt_lw, rcg_pt_lf, rcg_pt_ls -> rcg_setline;
        if rcg_pt_type.isword then
            valof("rcg_plt_" <> rcg_pt_type) -> rcg_pt_type
        endif;
        rcg_basic(tfunc,xfunc,yfunc,rcg_pt_type)
    endif

enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 15 1993
        Put in batch load
 */
