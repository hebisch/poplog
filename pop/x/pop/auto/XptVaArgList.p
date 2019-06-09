/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptVaArgList.p
 > Purpose:         support for Xt vararg lists
 > Author:          Jonathan Meyer, Nov 21 1990 (see revisions)
 > Documentation:   HELP *XptVaArgList
 > Related Files:   LIB *XptArgList *xpt_generaltypes
 */
compile_mode :pop11 +strict;

section;

include xt_constants.ph;

uses xpt_general;
uses XtN;

define lconstant Coerce_string(string, type, check) -> string;
    lvars string type check is_tmp;
    if string.isword and type then
        XtNLookup(string, type) -> string;
    elseif check or string.isstring then
        string -> XptCoerceString() -> string;
    endif;
enddefine;

define lconstant Stack_arglist(list);
    lvars list item len name val rep;
    for item in list do
        if item.isXptArgPtr then
            destXptArgPtr(item);
        elseif item.isXptArgList then
            nc_destXptArgList(item)->;
        elseif item.islist then
            Stack_arglist(item);
        elseif item.isvector then
            unless (datalength(item) ->> len) fi_>= 2 and len fi_<= 4 then
                mishap(item,1,'VECTOR of length 2 to 4 needed');
            endunless;
            Coerce_string(fast_subscrv(1, item),"N",true) -> name;
            fast_subscrv(2, item) -> val;
            if len == 2 then
                if val.isstring or val.isword then
                    Coerce_string(val, false, false) -> val;
                    XtVaTypedArg, name, XtR String, val, datalength(val);
                else
                    name, val;
                endif;
                nextloop;
            endif;
            Coerce_string(val, "R", true) -> rep;
            Coerce_string(fast_subscrv(3, item),false,false) -> val;
                XtVaTypedArg, name, rep, val,
            if len == 3 then datalength(val) else fast_subscrv(4, item) endif;
                else
            mishap(item,1,'INVALID ITEM: list, vector, XptArgList or XptArgPtr needed');
        endif;
    endfor;
enddefine;

define global constant XptVaArgList(list);
    lvars list;
    (#| Stack_arglist(list) |#);
enddefine;

;;; also a XptVaTmpArgList ?

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov  4 1991 : xt_general --> xpt_general
--- Adrian Howard, Oct 31 1991 : Changed to use -XptArgPtr-
 */
