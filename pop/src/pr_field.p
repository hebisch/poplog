/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/pr_field.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PRINT
 */

;;; ------------ PRINT ITEM IN A FIELD OF GIVEN WIDTH --------------------

#_INCLUDE 'declare.ph'

;;; ----------------------------------------------------------------------

define pr_field(item, width, lpadc, rpadc);
    lvars lpadc, rpadc, item, width, ltrunc = 0,
        procedure (pr_p = pr, chout = cucharout);
    dlocal cucharout;

    define lconstant decr_chout(char);
        lvars char;
        if ltrunc fi_> 0 then
            ltrunc fi_- 1 -> ltrunc
        elseif width /== 0 then
            chout(char), width fi_- 1 -> width
        endif
    enddefine;

    if isprocedure(rpadc) then
        ;;; optional printing procedure
        rpadc -> pr_p, lpadc -> rpadc, width -> lpadc, item -> width,
        -> item
    endif;

    Sys$-Check_integer(width, 0);
    unless lpadc then
        ;;; no left padding
        unless rpadc then `\s` -> rpadc endunless;
        ;;; left alignment/right padding only
        decr_chout -> cucharout;
        pr_p(item)
    else
        ;;; need excess print length of item first
        define dlocal cucharout(); ->, ltrunc fi_+ 1 -> ltrunc enddefine;
        pr_p(item);

        ltrunc fi_- width -> ltrunc;
        if rpadc then
            ;;; centre field, left and right padding
            if ltrunc fi_> 0 then 0 -> ltrunc endif;
            (ltrunc fi_+ 1) fi_>> 1 -> ltrunc
        endif;
        ;;; left padding
        decr_chout -> cucharout;
        while ltrunc fi_< 0 do
            decr_chout(lpadc);
            ltrunc fi_+ 1 -> ltrunc
        endwhile;
        pr_p(item)
    endunless;

    ;;; finish any right padding
    if rpadc then
        until width == 0 do decr_chout(rpadc) enduntil
    endif
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 13 1988
        Moved here from print.p
 */
