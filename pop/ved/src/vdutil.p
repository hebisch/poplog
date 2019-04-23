/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdutil.p
 > Purpose:         VED utility procedures
 > Author:          John Gibson (see revisions)
*/

#_INCLUDE 'vddeclare.ph'

vars
        procedure (pop_message_trans)
    ;


;;; --------------------------------------------------------------------

section $-Sys$-Ved;

    ;;; skip spaces/tabs in a string
define Skipwhite(_col, string);
    lvars string, _col;
    repeat
        if skipchar(`\s`, _col, string) ->> _col then
            returnif(fast_subscrs(_col, string) /== `\t`) (_col);
            if skipchar(`\t`, _col, string) ->> _col then
                returnif(fast_subscrs(_col, string) /== `\s`) (_col);
                nextloop
            endif
        endif;
        return(false)
    endrepeat
enddefine;


define Trimwhite(string,_size) -> string -> _size;
    ;;; Return string minues leading white space.
    ;;; Return the size too. string and _size possibly unchanged
    lvars _index, string, _size;
    if (Skipwhite(1, string) ->> _index) and _index /== 1 then
        ;;; remove leading whitespace
        subdstring(_index, _size fi_- _index fi_+ 1 ->> _size, string)
                    -> string;
    endif;
enddefine;


define Find_in_string(string, p);
    lvars procedure p;
    ;;; Return true if string has at least one character that satisfies p
    appdata(
        string,
        procedure() with_nargs 1;
            if p() then exitfrom(true, Find_in_string) endif
        endprocedure);
    false
enddefine;

define Errmess_sprintf(count, mess, idstring, max_culp);
    lvars count, mess, idstring, max_culp, list, culp, n, item;
    dlocal cucharout = identfn;

    conslist(count) -> list;
    if isvector(mess) then subscrv(1,mess) -> mess endif;
    pop_message_trans(mess, idstring) -> mess;
    #| sys_message_printf(mess, list) -> culp |# -> n;
    if max_culp and listlength(culp) fi_> max_culp then
        ;;; use mishap instead
        erasenum(n);
        dl(list);
        false
    else
        consstring(#|
            unless culp == [] then `:` endunless;
            for item in culp do
                if item == termin then '<END OF RANGE>' -> item endif;
                `\s`, dest_characters(item)
            endfor
        |# fi_+ n)
    endif;
    sys_grbg_list(list)
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1996
        Added Errmess_sprintf
--- John Williams, Jan 12 1996
        Haslowercode replaced by Find_in_string.
--- John Gibson, Jan  4 1992
        Changes to cope with dstrings
--- John Williams, Sep  6 1989
        Moved -Haslowercode- here from 'vdsysfile.p'
--- John Williams, Dec  5 1988
        Removed -Isendstring- (replaced by -isendstring- in string.p)
--- Aaron Sloman, Jul  9 1988
        Added -Trimwhite-, e.g. for use in vdprocess.p
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
