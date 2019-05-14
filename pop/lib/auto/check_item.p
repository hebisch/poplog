/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/check_item.p
 > Purpose:         Generic type checking procedure
 > Author:          Adrian Howard, May  3 1993 (see revisions)
 > Documentation:   * check_item
 > Related Files:   checkinteger.p check_string.p
 */

compile_mode: pop11 +strict :vm -pentch;
section;

define check_item(item, checks, message);
    lvars item, checks, message;

    ;;; convert a check into a string
    define lconstant Explode_string(check);
        lvars check;
        pdprops(check) sys_>< nullstring -> check;
        if isstartstring('is', check) then
            allbutfirst(2, check) -> check;
        endif;
        explode(lowertoupper(check));
    enddefine;

    ;;; generate an error message
    define lconstant Gen_error();
        if message.isstring then
            mishap(item, 1, message);
        else
            unless checks.islist then checks :: [] -> checks endunless;
            lvars item_key = datakey(item);
            lvars num_checks = listlength(checks);
            lvars expecting = consstring (#|
                repeat num_checks-1 times;
                    Explode_string(dest(checks) -> checks);
                    `,`; ` `;
                endrepeat;
                unless num_checks == 1 do;
                    erasenum(2);
                    ` `; `O`; `R`; ` `;
                endunless;
                Explode_string(checks.hd);
            |#);
        endif;
        mishap(item, 1, 'EXPECTING ' sys_>< expecting sys_>< ' - FOUND '
            sys_>< lowertoupper(class_dataword(item_key)));
    enddefine;
    if checks.isprocedure then
        returnif(fast_apply(item, checks));
    else
        applist(checks, procedure(check); lvars procedure check;
            if fast_apply(item, check) then exitfrom(check_item) endif;
        endprocedure);
    endif;
    Gen_error();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, May 22 1993
        Optimised
*/
