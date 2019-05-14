/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/showarray.p
 > Purpose:         Print arrays on ordinary terminals
 > Author:          David Young, Jan 31 1988 (see revisions)
 > Documentation:   TEACH * SHOWARRAY
 > Related Files:   LIB READPICTURE
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

global vars sa_greys = ' .,:x%IOM#',
            sa_yup,
            sa_aspect_ratio = 2.15,
            sa_sigfigs = 6,
            sa_chardata = false,
            sa_print_axes = false;

if isundef(sa_yup) then false -> sa_yup endif;

global constant procedure (sa_print sa_chars sa_limit sa_resize
                           showarray sa_prnums
                           sa_simple_fitscreen sa_simple_asis
                           sa_simple_nums);


/* For use in sa_print */
lvars sa_yaxis_sep_char = ` `;

define lconstant bounds(array,region) -> x0 -> x1 -> y0 -> y1;
    ;;; Get separate array or region bounds
    lvars array region x0 x1 y0 y1;
    lvars x0a x1a y0a y1a;
    if isarray(array) then
        boundslist(array) -> array
    endif;
    if length(array) /== 4 then
        mishap('array must be 2-dimensional',[^array])
    endif;
    if islist(region) then
        if length(region) /== 4 then
            mishap('region list must have length 4',[^region])
        endif;
        explode(region) -> y1 -> y0 -> x1 -> x0;
        explode(array) -> y1a -> y0a -> x1a -> x0a;
        if x0 < x0a or y0 < y0a or x1 > x1a or y1 > y1a then
            mishap('region not within array bounds',[^region ^array])
        endif
    else
        explode(array) -> y1 -> y0 -> x1 -> x0
    endif
enddefine;

define lconstant appregion(array,region,proc);
    ;;; apply a procedure to every element of an array in a given region
    lvars x y;
    lvars array region proc;
    lvars x0 x1 y0 y1;
    bounds(array,region) -> x0 -> x1 -> y0 -> y1;
    fast_for x from x0 to x1 do
        fast_for y from y0 to y1 do
            proc(array(x,y))
        endfor
    endfor
enddefine;

define lconstant rounddown(x) -> r;
    lvars x r;
    intof(x) -> r;
    if x < 0 and x /= r then
        r - 1 -> r
    endif
enddefine;

define lconstant arrmxmn(array,region) -> mx -> mn;
    dlvars mx = "undef", mn = "undef";
    lvars array region x0 y0;
    appregion(array,region,
        procedure(val); lvars val;
            if isreal(val) then
                if mx == "undef" then
                    val ->> mx -> mn
                endif;
                if val > mx then
                    val -> mx
                elseif val < mn then
                    val -> mn
                endif
            endif
        endprocedure)
enddefine;

define lconstant lennums(n1,n2) /* -> l */;
    lvars n1 n2;
    max(length(round(n1) >< ''), length(round(n2) >< ''))
enddefine;

define lconstant chartoprint(x0,x1,c0,col,c1,charno) /* -> c */;
    lvars x0 x1 c0 col c1 charno;
    lvars x s l;
    x0 + ((x1 - x0) / (c1 - c0 + 1)) * (0.5 + col - c0) -> x;
    round(x) >< '' -> s;
    length(s) -> l;
    if l < charno then ` ` else s(l - charno + 1) endif
enddefine;

define lconstant sa_xaxis_nums(x0,x1,c0,c1);
    ;;; Prints the numbers for an x-axis that runs from x0 to x1 in
    ;;; columns c0 to c1 (x0 is just to left of c0, x1 just to right
    ;;; of c1)
    lvars x0 x1 c0 c1;
    lvars nline col;
    for nline from lennums(x0,x1) by -1 to 1 do
        repeat c0 - 1 times
            cucharout(` `)
        endrepeat;
        for col from c0 to c1 do
            chartoprint(x0,x1,c0,col,c1,nline).cucharout
        endfor;
        cucharout(`\n`);
    endfor
enddefine;

define lconstant sa_yaxis_num(y0,y1,row0,row,row1);
    ;;; Prints a number for a y axis
    lvars y0 y1 row0 row row1;
    lvars col;
    for col from lennums(y0,y1) by -1 to 1 do
        chartoprint(y0,y1,row0,row,row1,col).cucharout
    endfor
enddefine;

define sa_print(array,region,levchars);
    ;;; For making grey level images of arrays - tries to draw
    ;;; the given region of the array on the screen - if region is not
    ;;; a list then the whole of the array is used.
    ;;; levchars is a list of alternating characters
    ;;; and levels, starting and finishing with a character and with the
    ;;; levels in increasing order, which determines which character is
    ;;; to be used for any value found in the array.
    ;;; If a word or string is stored in any element of the array then
    ;;; the first character is printed regardless of levchars.
    ;;; If the global sa_chardata is true, then the value is treated as
    ;;; a character code, and levchars is ignored.
    lvars c l;
    lvars array region levchars;
    lvars val x y x0 x1 y0 y1 ax0 ax1 ay0 ay1, ydir = 1, lyaxisnum = 1,
        linemax;
    dlocal poplinemax poplinewidth;

    bounds(array,region) -> x0 -> x1 -> y0 -> y1;

    if sa_print_axes == true then
        x0-0.5 -> ax0; x1+0.5 -> ax1; y0-0.5 -> ay0; y1+0.5 -> ay1
    elseif sa_print_axes then
        unless length(sa_print_axes) == 4 then
            mishap('Wrong length for sa_print_axes',[^sa_print_axes])
        endunless;
        bounds(sa_print_axes,"whole") -> ax0 -> ax1 -> ay0 -> ay1;
    endif;

    if sa_yup then
        -1 -> ydir;             ;;; otherwise 1
        y0, y1 -> y0 -> y1
    endif;

    if sa_print_axes then
        lennums(ay0,ay1) + 2 -> lyaxisnum   ;;; otherwise 1
    endif;

    x1 - x0 + lyaxisnum ->> poplinemax ->> poplinewidth -> linemax;

    if sa_print_axes and not(sa_yup) then
        cucharout(`\n`);
        sa_xaxis_nums(ax0,ax1,lyaxisnum,linemax);   ;;; PW*M changes poplinemax
    endif;

    for y from y0 by ydir to y1 do
        cucharout(`\n`);
        if sa_print_axes then
            sa_yaxis_num(ay0,ay1,min(y0,y1),y,max(y0,y1)); cucharout(sa_yaxis_sep_char);
        endif;
        for x from x0 to x1 do
            array(x,y) -> val;
            if sa_chardata then
                val -> c
            elseif isreal(val) then
                dest(levchars) -> l -> c;
                until l == [] do
                quitif ((dest(l) -> l) > val);
                    dest(l) -> l -> c;
                enduntil
            elseif isword(val) or isstring(val) then
                val(1) -> c
            else
                mishap('Unprintable value for array element',[^val at ^x ^y])
            endif;
            cucharout(c);
        endfor;
    endfor;

    cucharout(`\n`);
    if sa_print_axes and sa_yup then
        cucharout(`\n`);
        sa_xaxis_nums(ax0,ax1,lyaxisnum,linemax);
        cucharout(`\n`);
    endif;
enddefine;

define lconstant sa_evenchars(array,region,greystring) /* -> levchars */;
    ;;; Produces a list, suitable for showarray to use, from array and
    ;;; a string of characters arranged in increasing order of brightness.
    ;;; Just divides up the range between the largest and smallest values.
    lvars array region greystring;
    lvars val x valinc valmax valmin;
    arrmxmn(array,region) -> valmax -> valmin;
    if valmax == "undef" then
        []
    else
        (valmax-valmin)/length(greystring) -> valinc;
        valmin ->  val;
        [%   greystring(1),
             for x from 2 to length(greystring) do
                 val + valinc ->> val, greystring(x)
             endfor %]
    endif
enddefine;

define lconstant sa_logchars(array,region,greystring) /* -> levchars */;
    ;;; Like sa_evenchars but spaced as if the logarithm of the image had been
    ;;; taken.
    lvars array region greystring;
    lvars val x valinc valmax valmin;
    arrmxmn(array,region) -> valmax -> valmin;
    if valmax == "undef" then
        []
    else
        if valmin <= 0 then
            mishap('Can\'t cope with non-positive values',[^array with min ^valmin])
        endif;
        (valmax/valmin) ** (1.0/length(greystring)) -> valinc;
        valmin -> val;
        [%   greystring(1),
             for x from 2 to length(greystring) do
                 val * valinc ->> val, greystring(x)
             endfor %]
    endif
enddefine;

define lconstant sa_histochars(array,region,greystring) /* -> levchars */;
    ;;; Like sa_evenchars but the characters are adjusted so that an equal area
    ;;; is given to each as far as possible. This is only suitable for small
    ;;; images or small regions - for larger ones a random sampling technique should be
    ;;; used, as the method of forming a list and sorting it is a bit wasteful.
    lvars array region greystring;
    lvars x y n neach vallist;
    syssort(
        [% appregion(array,region,
                 procedure(x);
                     lvars x; if isreal(x) then x endif endprocedure) %],
        false,nonop < ) -> vallist;
    length(vallist)/length(greystring) -> neach;
    0.5 -> n;
    [%   greystring(1),
         if length(vallist) > 1 then
             for x from 2 to length(greystring) do
                 n + neach -> n;
                 intof(n) -> y;
                 (vallist(y+1) + vallist(y))*0.5, greystring(x)
             endfor
         endif %]
enddefine;

define lconstant sa_figchars(array,region) /* -> levchars */;
    ;;; Produces a list for showarray intended to print the most significant
    ;;; digit of the numerical value of each pixel. Negative values show as
    ;;; spaces. Values in the decade below that of the maximum value show
    ;;; as the letters a-i.
    lvars array region levchars;
    lvars mx l linc c;
    arrmxmn(array,region) -> mx -> ;
    [`0` 0.0 ` `] -> levchars;
    if mx /== "undef" and mx > 0 then
        10.0 ** (rounddown(log10(mx))-1) ->> l -> linc;
        `a` -> c;
        repeat 2 times
            repeat 9 times
                [^c ^l ^^levchars] -> levchars;
                l + linc -> l; c + 1 -> c;
            endrepeat;
            l -> linc;
            `1` -> c;
        endrepeat;
    endif;
    rev(levchars);
enddefine;

define lconstant uniquelist(list) /* -> result */;
    lvars l llast list;
    unless list == [] then not(hd(list)) -> llast endunless;
    [% for l in list do
             unless l = llast then l endunless;
             l -> llast
         endfor %]
enddefine;

define lconstant sa_diffchars(array,region,greystring) /* -> levchars */;
    ;;; Returns a list for sa_print in which each value in the array gets a
    ;;; different character, taken from greystring. Smallest value
    ;;; gets first character. Mishaps if not enough characters for no.
    ;;; of values.
    lvars array region greystring;
    lvars vallist i;
    syssort(
        [% appregion(array,region,
                 procedure(x); lvars x; if isreal(x) then x endif endprocedure) %],
        false,nonop < ).uniquelist
        -> vallist;
    if length(vallist) > length(greystring) then
        mishap('fewer characters than values',[^greystring ^vallist])
    endif;
    tl([%    for i from 1 to length(vallist) do
                 (dest(vallist) -> vallist), greystring(i)
             endfor %])
enddefine;

define lconstant sa_signchars /* -> levchars */;
    [`-` 0 `+`]
enddefine;

define sa_chars(image,region,type,greystring) /* -> levchars */;
    lvars image region greystring type;
    switchon type
    case == "even" then sa_evenchars(image,region,greystring)
    case == "log" then sa_logchars(image,region,greystring)
    case == "histo" then sa_histochars(image,region,greystring)
    case == "fig" then sa_figchars(image,region)
    case == "diff" then sa_diffchars(image,region,greystring)
    case == "sign" then sa_signchars()
    case .islist then type
    else
        mishap('Unrecognised character type option',[^type])
    endswitchon
enddefine;

define lconstant diffsign(x1,x2) /* -> result */;
    lvars x2 x2;
    (x1 >= 0 and x2 < 0) or (x1 < 0 and x2 >= 0)
enddefine;

define lconstant sa_print_zeros(array,region);
    ;;; Shows zero-crossings using graphics characters
    lvars x y;
    lvars array region;
    lvars charray x0 y0 x1 y1 top bottom left right;
    dlocal sa_chardata = true;

    bounds(array,region) -> x0 -> x1 -> y0 -> y1;
    newanyarray([^x0 ^x1 ^y0 ^y1],` `,datakey('')) -> charray;

    for y from y0 to y1-1 do
        for x from x0 to x1-1 do
            diffsign(array(x,y),array(x+1,y)) -> top;
            diffsign(array(x,y+1),array(x+1,y+1)) -> bottom;
            diffsign(array(x,y),array(x,y+1)) -> left;
            diffsign(array(x+1,y),array(x+1,y+1)) -> right;
            if left and right and top and bottom then
                `\G+`
            elseif left and right and top then
                `\Gbt`
            elseif left and right and bottom then
                `\Gtt`
            elseif left and top and bottom then
                `\Grt`
            elseif right and top and bottom then
                `\Glt`
            elseif left and bottom then
                `\Gtr`
            elseif left and top then
                `\Gbr`
            elseif right and bottom then
                `\Gtl`
            elseif right and top then
                `\Gbl`
            elseif right or left then
                `\G-`
            elseif top or bottom then
                `\G|`
            else
                `\s`
            endif -> charray(x,y)
        endfor;
    endfor;

    sa_print(charray,"whole",[]);
enddefine;

define sa_resize(image,region,newsize) -> newimage;
    ;;; Averages to fit a new array with boundslist newsize
    ;;; If the array has 16 bits or less per element then fast integer
    ;;; operations are used, as overflow is exceedingly unlikely, though
    ;;; in principle still possible if the array is reduced in size by
    ;;; some enormous factor.
    lvars sum yfast;
    lvars image region newimage newsize;
    lvars xfast x y ygo i j xincr yincr m n v cspec,
        procedure (divop addop);
    lvars x0 x1 y0 y1 xdo ydo x0n x1n y0n y1n mnew nnew;
    dlvars norm;

    bounds(image,region) -> x0 -> x1 -> y0 -> y1;
    bounds(newsize,"whole") -> x0n -> x1n -> y0n -> y1n;

    x1 - x0 + 1 -> m;               y1 - y0 + 1 -> n;
    x1n - x0n + 1 -> mnew;          y1n - y0n + 1 -> nnew;
    m/mnew -> xincr;                n/nnew -> yincr;
    max(intof(xincr)-1,0) -> xdo;   max(intof(yincr)-1,0) -> ydo;

    (xdo+1)*(ydo+1) -> norm;
    newanyarray(newsize, datakey(arrayvector(image))) -> newimage;

    field_spec_info(class_spec(datakey(arrayvector(image)))) -> -> cspec;
    if cspec.isinteger and cspec <= 16 and cspec >= -15 then
        nonop fi_div(% norm %) -> divop;
        nonop fi_+ -> addop
    elseif cspec.isinteger then
        nonop div(% norm %) -> divop;
        nonop + -> addop
    else
        1.0 / norm -> norm;
        procedure(x); lvars x;
            if isreal(x) then x * norm else x endif
        endprocedure -> divop;
        nonop + -> addop
    endif;
    x0 -> x;        y0 -> ygo;

    fast_for i from x0n to x1n do
        rounddown(x) -> x0;
        x0 fi_+ xdo -> x1;     ygo -> y;
        fast_for j from y0n to y1n do
            rounddown(y) -> y0;
            y0 fi_+ ydo -> y1;
            0 -> sum;
            fast_for xfast from x0 to x1 do
                fast_for yfast from y0 to y1 do
                    image(xfast,yfast) -> v;
                    if isreal(v) then
                        addop(sum,v) -> sum
                    else
                        v -> sum; quitloop(2)
                    endif
                endfor
            endfor;
            divop(sum) -> newimage(i,j);
            y + yincr -> y;
        endfor;
        x + xincr -> x;
    endfor;
enddefine;

define sa_limit(image,region,xlim,ylim,aspect_ratio) /* -> newimage */;
    ;;; Rescales an array so that the x axis is expanded relative to
    ;;; the y-axis by about aspect_ratio, and so that both the x and y dimensions
    ;;; are less than xlim and ylim respectively, but either the x dimension
    ;;; is equal to xlim or the y dimension is equal to ylim.
    ;;; In the output, both dimensions start from 1.
    lvars image region xlim ylim aspect_ratio;
    lvars x0 x1 y0 y1 m n xsize ysize;
    bounds(image,region) -> x0 -> x1 -> y0 -> y1;
    x1 - x0 + 1 -> m;       y1 - y0 + 1 -> n;
    if n*xlim > aspect_ratio*m*ylim then
        ;;; limit is in height
        ylim -> ysize;
        max(intof(m*ysize*aspect_ratio/n),1) -> xsize;
    else
        ;;; limit is in width
        xlim -> xsize;
        max(intof(n*xsize/(m*aspect_ratio)),1) -> ysize;
    endif;
    sa_resize(image,region,[1 ^xsize 1 ^ysize])
enddefine;

define sa_prnums(image,region,i1,i2);
    ;;; Print the elements of an array using prnum
    lvars image region i1 i2;
    lvars x val linelen x0 x1 y0 y1 savecucharout wid;
    dlvars xprint charray y;
    dlocal sa_print_axes, sa_chardata = true, sa_yaxis_sep_char = `>`;

    bounds(image,region) -> x0 -> x1 -> y0 -> y1;

    if sa_print_axes == true then
        [^(x0-0.5) ^(x1+0.5) ^(y0-0.5) ^(y1+0.5)] -> sa_print_axes
    endif;

    i1 + i2 -> wid;
    wid * (x1 - x0 + 1) -> linelen;
    newanyarray([1 ^linelen ^y0 ^y1],datakey('')) -> charray;

    cucharout -> savecucharout;     ;;; dlocal no use as need to restore before end
    procedure(ch); lvars ch;
        ch -> charray(xprint,y); xprint + 1 -> xprint;
    endprocedure -> cucharout;

    for y from y0 to y1 do
        1 -> xprint;
        for x from x0 to x1 do
            image(x,y) -> val;
            if isreal(val) then
                prnum(val,i1,i2)
            else
                pr_field(val,wid,` `,false)
            endif
        endfor
    endfor;

    savecucharout -> cucharout;
    sa_print(charray,"whole",[]);
enddefine;

define lconstant sa_decplaces(n) -> pl;
    ;;; Counts the no of decimal places a number needs in order to print
    lvars n;
    dlvars pl l;
    dlocal cucharout pop_pr_places;

    if isreal(n) then
        number_coerce(n,0.0) -> n;
        intof(float_digits(n) * log10(pop_float_radix)) + 1
            -> pop_pr_places;  ;;; Don't want any cut-off
        procedure(c); lvars c;
            if c == `.` then
                0 -> pl
            elseif pl >= 0 then
                pl + 1 -> pl;
                c -> l
            endif
        endprocedure -> cucharout;
        -1 -> pl;
        pr(n);
        if l == `0` then
            pl - 1 -> pl
        endif
    else
        0 -> pl
    endif
enddefine;

define lconstant sa_prpars(image,region) -> pre -> post;
    ;;; Tries to set up the parameters for prnum to show up to
    ;;; sa_sigfigs sig figs
    lvars image region pre post;
    lvars x0 x1 y0 y1 mx mn w;
    dlvars nplaces;
    bounds(image,region) -> x0 -> x1 -> y0 -> y1;
    arrmxmn(image,region) -> mx -> mn;
    if mx == "undef" then
        1 -> pre;
        0 -> post;
    else
        max(abs(mx),abs(mn)) -> mx;
        if mx = 0 then
            2 -> pre;
            0 -> post
        else
            intof(log10(1.00001 * mx)) -> w;   ;;; hack!
            0 -> nplaces;
            appregion(image,region,
                procedure(val); lvars val;
                    max(sa_decplaces(val),nplaces) -> nplaces
                endprocedure);
            if w >= 0 and mx >= 1 then
                w + 2 -> pre;
                min(nplaces+1,max(0,sa_sigfigs-w)) -> post;
                if post == 1 then 0 -> post endif
            else
                2 -> pre;
                min(nplaces+1,sa_sigfigs-w) -> post
            endif;
            if mn < 0 then pre + 1 -> pre endif;
        endif
    endif
enddefine;

define showarray(image,region,resize,type,greystring);
    lvars image region resize type greystring;
    lvars i1 i2 width depth ax0 ax1 ay0 ay1 aspect_ratio = 1.0;
    dlocal sa_print_axes;

    if sa_print_axes == true then
        bounds(image,region) -> ax0 -> ax1 -> ay0 -> ay1;
        [^(ax0-0.5) ^(ax1+0.5) ^(ay0-0.5) ^(ay1+0.5)] -> sa_print_axes
    elseif sa_print_axes then
        bounds(sa_print_axes,"whole") -> ax0 -> ax1 -> ay0 -> ay1
    endif;

    if type == "nums" then
        sa_prpars(image,region) -> i1 -> i2
    endif;

    if islist(resize) or isvector(resize) or resize == "fitscreen" then
        if resize == "fitscreen" then
            if vedediting then vedlinemax else poplinemax endif -> width;
            vedscreenlength - 2 -> depth;
            if sa_print_axes then
                width - lennums(ax0,ax1) - 1 -> width;
                depth - lennums(ay0,ay1) - 1 -> depth
            endif;
            if type == "nums" then
                intof(width/(i1+i2)) -> width
            else
                sa_aspect_ratio -> aspect_ratio ;;; no point doing this for numbers
            endif;
        elseif length(resize) == 3 then
            explode(resize) -> aspect_ratio -> depth -> width
        elseif length(resize) == 2 then
            explode(resize) -> depth -> width;  ;;; aspect_ratio is 1.0
        else
            mishap('resize argument must have length 2 or 3',[^resize])
        endif;
        sa_limit(image,region,width,depth,aspect_ratio) -> image;
        "whole" -> region;
    endif;

    if type == "zeros" then
        sa_print_zeros(image,region)
    elseif type == "nums" then
        sa_prnums(image,region,i1,i2)
    else
        sa_print(image,region,sa_chars(image,region,type,greystring))
    endif
enddefine;

define sa_simple_fitscreen(image);
    lvars image;
    showarray(image,"whole","fitscreen","even",sa_greys);
enddefine;

define sa_simple_asis(image);
    lvars image;
    showarray(image,"whole","asis","even",sa_greys)
enddefine;

define sa_simple_nums(image);
    lvars image;
    showarray(image,"whole","asis","nums",'')
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 13 1992
        Replaced use of old g*raphcharsetup with new graphics chars
--- David Young, Jan  8 1991 Fixed but in sa_resize caused by change in
    field specifications by using field_spec_info.
--- David Young, Jul 21 1989 linemax used instead of poplinemax in sa_print
    because PW*M changes poplinemax.
--- David Young, Feb  2 1988 Fixed minor bugs in sa_diffchars and sa_print
 */
