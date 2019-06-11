/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_smr.p
 > Purpose:         Sort a marked range of text
 > Author:          Aaron Sloman, May 29 1985 (see revisions)
 > Documentation:   HELP * VEDCOMMS
 > Related Files:
 */

section;

define lconstant Line_<(x, y, n, before_p);
    {% sys_parse_string(x) %} -> x;
    {% sys_parse_string(y) %} -> y;
    before_p(
        fast_subscrv(fi_min(n, datalength(x)), x),
        fast_subscrv(fi_min(n, datalength(y)), y))
enddefine;


define global ved_smr();
    lvars top, n, before_p = alphabefore;
    dlocal vedline, vedcolumn, vvedlinesize, vveddump;
    vedmarkfind();
    vedline -> top;
    ved_d();
    if isstartstring('-f', vedargument) then
        procedure(x, y);
            alphabefore(uppertolower(x), uppertolower(y))
        endprocedure -> before_p;
        allbutfirst(2, vedargument) -> vedargument
    endif;
    if isinteger(strnumber(vedargument) ->> n) and n > 0 then
        Line_<(% n, before_p %) -> before_p
    endif;
    syssort(vveddump, before_p) -> vveddump;
    (top - 1) sys_>< nullstring -> vedargument;
    ved_y();
    vedputmessage('SORTED');
enddefine;


endsection;



/* --- Revision History ---------------------------------------------------
--- John Williams, Mar 25 1997
        -f and field number can be used together.
--- John Williams, Mar 14 1997
        Can now sort by numbered field.
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
--- John Williams, Apr 23 1986
         If vedargument = '-f' it folds uppercase onto lowercase
*/
