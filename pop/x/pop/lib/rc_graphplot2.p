/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_graphplot2.p
 > Purpose:         Package for drawing graphs of functions and data
 > Author:          David Young, Dec  7 1990
 > Documentation:   TEACH * RC_GRAPHPLOT, HELP * RC_GRAPHPLOT
 > Related Files:   LIB * RCG_UTILS, LIB * RC_GRAPHIC, LIB * RC_GRAPHPLOT
 */
compile_mode :pop11 +strict;

/* Library to draw graphs of functions or data using * RC_GRAPHIC */

uses-now popxlib;

section;
exload_batch;

uses rcg_utils;

/* BASIC PLOTTING PROCEDURE */

define rcg_basic2(xyfunc,plotfunc);
    lvars x y;
    lvars xyfunc plotfunc;
    until (xyfunc() -> (x,y), x) == termin or y == termin
    do
        plotfunc(x,y)
    enduntil
enddefine;

/* DATA STRUCTURING */

define rcg_needsexplode(thing) /* -> boolean */;
    lvars thing;
    thing.isrecordclass or thing.isvectorclass or thing.isarray
enddefine;

define rcg_dat_rep2(data) -> (datarepeater, resetter);
    ;;; Returns a repeater that gives each successive
    ;;; pair of values from a list,
    ;;; array or vector, returning (termin,termin) thereafter.
    ;;; Also returns a resetter that makes the next call of
    ;;; the repeater start at the beginning.
    ;;; Arrays are just replaced by the relevant bit of their arrayvectors.
    ;;; If the first entry in the structure is a structure itself
    ;;; then the x and y values are assumed to be stored together in
    ;;; sub-structures which need to be exploded.  Otherwise, they
    ;;; are assumed to be stored alternately.

    lvars data datarepeater resetter;
    lvars ldata point accessor firstpoint lastpoint;

    if data.rcg_isrealproc then
        data -> datarepeater;
        false -> resetter

    elseif data.islist then
        data -> ldata;

        if rcg_needsexplode(hd(data)) then
            procedure /* -> (x,y) */;
                if ldata == [] then
                    termin, termin
                else
                    explode(fast_front(ldata));   ;;; result
                    fast_back(ldata) -> ldata
                endif
            endprocedure
        else
            procedure /* -> (x,y) */;
                repeat 2 times
                    if ldata == [] then
                        termin
                    else
                        fast_front(ldata);
                        fast_back(ldata) -> ldata
                    endif
                endrepeat
            endprocedure
        endif -> datarepeater;

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

        if rcg_needsexplode(data(firstpoint)) then
            procedure /* -> (x,y) */;
                if point fi_> lastpoint then
                    termin, termin
                else
                    explode(accessor(point,data));   ;;; result
                    point fi_+ 1 -> point
                endif
            endprocedure
        else
            procedure /* -> (x,y) */;
                repeat 2 times
                    if point fi_> lastpoint then
                        termin
                    else
                        accessor(point,data);   ;;; result
                        point fi_+ 1 -> point
                    endif
                endrepeat
            endprocedure
        endif-> datarepeater;

        procedure; firstpoint -> point endprocedure -> resetter;

    endif
enddefine;

define rcg_fun_rep2(func,reset) -> (func, reset);
    ;;; If the function does not have a resetter (passed in reset)
    ;;; then create one by getting all the values into a vector
    ;;; and returning a repeater and resetter for it. Expects 2
    ;;; results for each function call.
    lvars x y;
    lvars func reset;
    unless reset then
        rcg_dat_rep2(
            {%
                until (func() -> (x,y), x) == termin or y == termin do
                    x, y
                enduntil
                %}
        ) -> (func, reset)
    endunless
enddefine;

define 5 rcg_rep_<>_2 (tfunc,func) /* -> (x,y) */;
    ;;; concatenate a repeater and a function,
    ;;; result returns termin if the repeater does so
    lvars tfunc func;
    procedure();
        lvars t;
        if (tfunc() ->> t) == termin then
            termin, termin
        else
            func(t)
        endif
    endprocedure
enddefine;

/* LIMIT FINDING */

define rcg_minmax2(values) -> (xmin, xmax, ymin, ymax);
    ;;; Find max and min values, skipping undefined ones
    ;;; The argument should be a repeater that returns
    ;;; (x,y) values, one of which should be <termin> when
    ;;; finished. If on any call either x or y is not a real
    ;;; number, then both x and y are ignored.
    lvars x y;
    lvars values, xmin = undef, xmax = undef, ymin = undef, ymax = undef;
    ;;; Find the first defined values
    until ((values() -> (x,y), x) == termin) or y == termin
    or (x.isreal and y.isreal) do enduntil;
    unless x == termin or y == termin then
        x ->> xmin -> xmax;
        y ->> ymin -> ymax
    endunless;
    ;;; And compare with the rest
    until (values() -> (x,y), x) == termin or y == termin do
        if x.isreal and y.isreal then
            if x > xmax then
                x -> xmax
            elseif x < xmin then
                x -> xmin
            endif;
            if y > ymax then
                y -> ymax
            elseif y < ymin then
                y -> ymin
            endif
        endif
    enduntil
enddefine;

/* TOP-LEVEL CALLER */

define rc_graphplot2(xyfunc, xlabel, ylabel) -> region;
    dlocal rcg_setline, rcg_pt_type;
    lvars xyfunc xlabel ylabel;
    lvars tmin tinc tmax, tfunc = false, treset,
        xmin = undef, xmax = undef,
        ymin = undef, ymax = undef,
        xyreset = false,
        region;

    ;;; sort out the arguments
    if xyfunc.rcg_isrealproc then   ;;; need parametric input, presumably
            -> tfunc;
        if tfunc.isnumber then
            tfunc -> (tmin, tinc, tmax);
            rcg_lin_rep(tmin,tinc,tmax) -> (tfunc, treset)
        elseif not(tfunc.rcg_isrealproc) then   ;;; tfunc is a data structure
            rcg_dat_rep(tfunc) -> (tfunc, treset);
        endif
    else
        rcg_dat_rep2(xyfunc) -> (xyfunc, xyreset)
    endif;

    ;;; sort out the plotting region - first attempt
    rcg_set_reg([% xmin,xmax,ymin,ymax %]) -> region;
    explode(region) -> (xmin,xmax,ymin,ymax);

    ;;; If unsuccessful, get some data limits
    if lmember(undef,region) then       ;;; yes - need some limits
        if tfunc then
            rcg_fun_rep2(tfunc rcg_rep_<>_2 xyfunc, xyreset) -> (xyfunc, xyreset);
            false -> tfunc
        else
            rcg_fun_rep2(xyfunc, xyreset) -> (xyfunc, xyreset)
        endif;
        rcg_minmax2(xyfunc) -> (xmin, xmax, ymin, ymax);
        xyreset();
        rcg_stretch(xmin,xmax) -> (xmin, xmax);
        rcg_stretch(ymin,ymax) -> (ymin, ymax);

        ;;; again try to set plotting region
        rcg_set_reg([% xmin,xmax,ymin,ymax %]) -> region;
        if lmember(undef,region) then mishap('Insufficient data', []) endif;
        explode(region) -> (xmin,xmax,ymin,ymax);
    endif;

    ;;; start a new graph if rcg_newgraph is true
    if rcg_newgraph then rc_start() endif;

    ;;; Draw the axes
    rcg_def_ax(xmin,xmax,xlabel,ymin,ymax,ylabel);

    ;;; Draw the curve
    if rcg_pt_type then
        rcg_pt_lw, rcg_pt_lf, rcg_pt_ls -> rcg_setline;
        if rcg_pt_type.isword then
            valof("rcg_plt_" <> rcg_pt_type) -> rcg_pt_type
        endif;
        if tfunc then tfunc rcg_rep_<>_2 xyfunc -> xyfunc endif;
        rcg_basic2(xyfunc,rcg_pt_type)
    endif

enddefine;

endexload_batch;
endsection;
