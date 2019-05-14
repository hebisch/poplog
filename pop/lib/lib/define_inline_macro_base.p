/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/define_inline_macro_base.p
 > Purpose:         Base procedure for define :inline macros
 > Author:          John Gibson, Oct  5 1992 (see revisions)
 > Documentation:   None -- purely for use of define_inline
 */
compile_mode :pop11 +strict;

section;

define define_inline_macro_base(readplist, inline_expr);
    lvars   i, pair, item, n = listlength(readplist), readplist, exprlist,
            inline_expr;

    ;;; read actual parameters
    pop11_need_nextreaditem("(") -> ;
    1 -> i;
    [%  fast_for item in readplist do
            ;;; read comma-separated expressions (or whatever)
            if item == itemread then
                procedure; dlocal pop_autoload = false; itemread() endprocedure()
            elseif item == listread then
                [% listread() %]
            else
                item()
            endif;
            if i /== n then pop11_need_nextreaditem(",") -> endif;
            i fi_+ 1 -> i
        endfor
    %] -> exprlist;
    pop11_need_nextreaditem(")") -> ;

    ;;; explode inline expression
    fast_for item in inline_expr do
        ;;; parameter 'identifiers' are the pairs in readplist
        if ispair(item) then
            1 -> i;
            fast_for pair on readplist do
                if item == pair then
                    ;;; substitute i-th parameter
                    if islist(fast_subscrl(i,exprlist) ->> item) then
                        dl(item)
                    else
                        item
                    endif;
                    nextloop(2)
                endif;
                i fi_+ 1 -> i
            endfor
        endif;
        item
    endfor;

    sys_grbg_list(exprlist)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 19 1995
        Removed 2nd arg and made it create a list of actual parameters
        instead (so it works recursively!)
 */
