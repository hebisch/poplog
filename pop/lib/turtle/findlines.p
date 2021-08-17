/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 |  File:           C.all/lib/turtle/findlines.p
 |  Purpose:        find lines in a turtle picture, adding them to the database
 |  Author:         Steven Hardy, 18 Jan 1978 (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

compile_mode :pop11 +oldvar;

;;; The programme assumes that the TURTLE picture is composed of line-like
;;; patterns and gives odd results if applied to filled-in shapes whose size
;;; is >= minlinelength. (MBC Nov 1980)

section ;

uses turtle;
uses present;
uses add;
uses remove;
uses flush;

global vars
    Isvturtle
    vturtle_origin      /* Used if vturtle */
    vturtle_max         /* Used if vturtle */
    it
    database
    minlinelength
    breaklines
    ;

if isundef(Isvturtle) then false -> Isvturtle endif;
if isundef(database) then [] -> database; endif;
unless isinteger(minlinelength) then 4 -> minlinelength endunless;
unless breaklines == false do true -> breaklines endunless;

section findlines => findlines;

define global findlines();
    ;;; variable declarations
    lvars  p,x,y, type, xsize, ysize, hrzs, vrts, lfts, rhts ;
    vars breaklines, minlinelength, vedediting, vedautowrite, vedchanged;
    ;;; remove existing lines from DATABASE
    flush([line ==]);
    ;;; Calculate size of picture
    if      Isvturtle
    then    vturtle_max();
            false ->> vedautowrite -> vedchanged;
    else    pdprops(picture)(3), pdprops(picture)(5);
    endif   -> ysize -> xsize;
    ;;; set up buckets for line types
    {%repeat ysize times [] endrepeat%} -> hrzs;
    {%repeat xsize times [] endrepeat%} -> vrts;
    {%repeat xsize  fi_+  ysize times [] endrepeat%} -> lfts;
    {%repeat xsize  fi_+  ysize times [] endrepeat%} -> rhts;
    ;;; scan picture for marked points
    for y from 1 to ysize do
        if      Isvturtle
        then    jumpto(1,y); vvedlinesize -> xsize;
        endif;
        for x from 1 to xsize
        do  unless  picture(x, y) == space
            then    conspair(x, y) -> p;
                    conspair(p, hrzs(y)) -> hrzs(y);
                    conspair(p, vrts(x)) -> vrts(x);
                    conspair(p, lfts(x fi_+ y)) -> lfts(x fi_+ y);
                    conspair(p, rhts(x fi_- y fi_+ ysize)) -> rhts(x fi_- y fi_+ ysize);
                    0 -> picture(x, y);
            endunless;
        endfor;
    endfor;
    ;;; the following procedure takes a list of points which may form a line
    define follow (list);
    lvars list line xs ys yf xf fx fy;
    until   null(list)
    do      list -> line;
            fast_destpair(fast_front(list)) -> ys -> xs;

    followline:
            fast_destpair(fast_destpair(list) -> list) -> yf -> xf;
            unless  list == []
            then    fast_destpair(fast_front(list)) -> fy -> fx;
                    if      abs(fx fi_- xf) <= 1 and abs(fy fi_- yf) <= 1
                    then    goto followline /* !!!! */
                    endif;
            endunless;
            if      abs(xf fi_- xs) >= minlinelength
                    or abs(yf fi_- ys) >= minlinelength
            then    if Isvturtle then jumpto(fx, fy) endif;
                    if      breaklines
                    then    until   line == list
                            do      fast_destpair(fast_destpair(line) -> line)
                                                                    -> fy -> fx;
                                    picture(fx, fy) fi_+ 1 -> picture(fx, fy)
                            enduntil
                    else    fast_destpair(fast_destpair(fast_back(line)) -> line)
                                                                    -> fy -> fx;
                            until   line == list
                            do      if      picture(fx, fy) fi_> 1
                                    then    add([line ^type [^fx ^fy] [^xs ^ys]]);
                                            fx -> xs; fy -> ys
                                    endif;
                                    fast_destpair(fast_destpair(line) -> line)
                                                                    -> fy -> fx;
                            enduntil;
                            add([line ^type [^xf ^yf] [^xs ^ys]]);
                            conspair(conspair(xf, yf), list) -> list;
                    endif;
            endif;
    enduntil;
    enddefine;

    ;;; the operation of the above procedure requires we decrement
    ;;; minlinelength
    minlinelength fi_- 1 -> minlinelength;

    ;;; examine buckets for decent lines
findlines:
    ;;; hrz type lines
    "hrz" -> type;
    appdata(hrzs, follow);
    ;;; vrt type lines
    "vrt" -> type;
    appdata(vrts, follow);
    ;;; lft diagonal lines
    "lft" -> type;
    appdata(lfts, follow);
    ;;; rht diagonal lines
    "rht" -> type;
    appdata(rhts, follow);

    ;;; if that was a line breaking pass do line adding pass
    if breaklines then
        false -> breaklines;
        goto findlines;
    endif;
    ;;; now to reset picture to paint
    appdata(hrzs, procedure (list);
                    lvars y  y p list;
                    for p in list
                    do  fast_destpair(p) -> y -> x;
                        if      isinteger(picture(x, y))
                        then    paint -> picture(x, y)
                        endif
                    endfor;
                  endprocedure);
enddefine;

section_cancel(current_section);
endsection;
endsection;

/*  --- Revision History ---------------------------------------------------
--- John Williams, Aug  3 1995 - now sets compile_mode +oldvar.
--- Aaron Sloman, Oct 14 1985 - more modifications (also by MR).
--- Allan Ramsay, July 1984 - Modified to work with vturtle as well as turtle
--- Aaron Sloman, Nov 1980 - Modified to allow globals to be predefined.
 */
