/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_R.p
 > Purpose:         LIB * FORMAT_PRINT ~R directive
 > Author:          John Williams, Dec 17 1985 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

section $-lisp$-fpr;

uses format_print;

f_proc(`D`) ->;     ;;; check ~D loaded

lconstant
    Divs      =  {1e9     1e6     1e3      1e2},
    Divnames  =  {billion million thousand hundred},
    Prefixes  =  {0 twen thir for fif six seven eigh nine},
    Ordinals  =  {first second third fourth fifth sixth seventh
                  eighth ninth tenth eleventh twelfth thirteenth fourteenth
                  fifteenth sixteenth seventeenth eighteenth nineteenth},
    Cardinals =  {one two three four five six seven
                  eight nine ten eleven twelve thirteen fourteen
                  fifteen sixteen seventeen eighteen nineteen},
    ;


define lconstant Wpr() with_nargs 1;
    appdata(cucharout)
enddefine;


define English_pr(n) with_props false;
    lvars d, i, printed;
    ;;; 'f_colon' signifies ordinal rather than cardinal numbers
    false -> printed;
    if n < 0 then
        Wpr("minus");
        cucharout(`\s`);
        negate(n) -> n
    endif;
    for i from 1 to #_<datalength(Divs)>_# do
        n // fast_subscrv(i, Divs) -> d -> n;
        unless d == 0 do
            if printed then
                cucharout(`,`);
                cucharout(`\s`)
            endif;
            procedure;
                dlocal f_colon = false;
                English_pr()
            endprocedure(d);
            cucharout(`\s`);
            Wpr(fast_subscrv(i, Divnames));
            if n == 0 then
                if f_colon then Wpr("th") endif;
                return
            endif;
            true -> printed
        endunless
    endfor;
    if printed then
        cucharout(`\s`);
        Wpr("and");
        cucharout(`\s`)
    endif;
    if n == 0 then
        Wpr("zero")
    elseif n fi_> 19 then
        n fi_// 10 -> d -> n;
        Wpr(fast_subscrv(d, Prefixes));
        if n == 0 then
            Wpr(if f_colon then "tieth" else "ty" endif);
            return
        endif;
        Wpr("ty");
        cucharout(`\s`);
        English_pr(n)
    else
        Wpr(fast_subscrv(n, if f_colon then Ordinals else Cardinals endif))
    endif
enddefine;


;;; Roman numerals

lconstant Numerals = 'MDCLXVI';

define Roman_pr(n) with_props false;
    lvars i, c, d;
    if n < 0 then
        cucharout(`-`);
        negate(n) -> n
    endif;
    if f_colon then
        ;;; old Roman style
        for     1000 -> i, 1 -> c
        step    i / 10 -> i, c fi_+ 2 -> c
        till    false
        do
            n // i -> d -> n;
            repeat d times
                cucharout(fast_subscrs(c, Numerals))
            endrepeat;
            quitif(n == 0);
            n // (i / 2) -> d -> n;
            unless d == 0 do
                cucharout(fast_subscrs(c fi_+ 1, Numerals));
            endunless
        endfor
    else
        n // 1000 -> d -> n;
        repeat d times
            cucharout(`M`)
        endrepeat;
        for     100 -> i, 3 -> c
        step    i / 10 -> i, c fi_+ 2 -> c
        till    n == 0
        do
            n // i -> d -> n;
            if d == 9 then
                cucharout(fast_subscrs(c, Numerals));
                cucharout(fast_subscrs(c fi_- 2, Numerals))
            elseunless d fi_< 5 do
                cucharout(fast_subscrs(c fi_- 1, Numerals));
                repeat d fi_- 5 times
                    cucharout(fast_subscrs(c, Numerals))
                endrepeat
            elseif d == 4 then
                cucharout(fast_subscrs(c, Numerals));
                cucharout(fast_subscrs(c fi_- 1, Numerals))
            else
                repeat d times
                    cucharout(fast_subscrs(c, Numerals))
                endrepeat
            endif
        endfor
    endif
enddefine;


;;; SPECIFIED RADIX (~nR), ENGLISH (~R) OR ROMAN (~@R)

procedure(radix, mincol, padchar, commachar);
    lvars arg;
    if radix or mincol or padchar or commachar then
        f_DBOX(mincol, padchar, commachar, radix)
    else
        next_f_arg() -> arg;
        if isintegral(arg) then
            if f_at then Roman_pr else English_pr endif(arg)
        else
            f_arg_index fi_- 1 -> f_arg_index;
            f_DBOX(mincol, padchar, commachar, 10)
        endif
    endif
endprocedure -> f_proc(`R`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Dec 17 1985
        Fixed for bigintegers.
 */
