/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptArgList.p
 > Purpose:         Construct an "arglist" for Xt routines
 > Author:          Tom Khabaza, Aug 14 1990 (see revisions)
 > Documentation:
 > Related Files:   xpt_generaltypes
 */
compile_mode :pop11 +strict;

/* XptArgList(LIST | VECTOR | ARGLIST) -> ARGLIST -> LENGTH

    Takes a list, vector or arglist. Converts it into an arglist if
    necessary (checking elements of list/vector are of correct length),
    and places arglist and arglist's length on stack.

    Eg. XptArgList([[ % XtN width % 200][ % XtN height % 300 ]]) =>
*/

section;

uses xpt_general.p;         ;;; for shadowclasses

define lconstant stack_item(item);
    lvars item name val;
    if (#| explode(item) |#) == 2 then
        -> val -> name;
        if isword(name) then
            return(word_string(name),val);
        elseif isstring(name) then
            return(name,val);
        endif;
    endif;
    mishap(item,1,'[NAME VALUE] PAIR NEEDED');
enddefine;

lvars empty_arglist = false;

define XptArgList(list);
    lvars list size;

    if list == [] or list == {} then
        empty_arglist or (consXptArgList(0) ->> empty_arglist), 0;
    elseif isXptArgList(list) then
        list; shadow_length(list);
    else
        if isvector(list) then
            datalength(list) -> size;
            appdata(list,stack_item);
        elseif islist(list) then
            listlength(list) -> size;
            applist(list,stack_item);
        else
            mishap(list,1,'LIST, VECTOR OR ArgList NEEDED');
        endif;
        nc_consXptArgList(size); size;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 16 1993
        Replaced compile-time use of consXptArgList(0) with caching in lvar
        empty_arglist
--- Adrian Howard, Nov  4 1991 : xt_general --> xpt_general
--- Jonathan Meyer, Jan 17 1991 moved to auto
--- Roger Evans, Nov 20 1990
        altered stack_item to allow words
--- Roger Evans, Nov 19 1990
        removed freelist stuff and simplified drastically
--- Jonathan Meyer, Nov 3 1990
        Fixed bug to allow XptArgList to accept ArgList as input
--- Jonathan Meyer, Oct 12 1990
        Added free-list and assoc facilities to reduce on garbage.
--- Jonathan Meyer, Sep 23 1990
        Made it convert list args to vectors - reflecting change in xpt_lists
--- Jonathan Meyer, Sep  9 1990
        Changed to make use of consArgList.
 */
