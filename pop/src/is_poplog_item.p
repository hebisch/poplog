/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/is_poplog_item.p
 > Purpose:         Check datum to be a proper POPLOG object
 > Author:          John Williams, Jun 10 1988 (see revisions)
 > Documentation:
 > Related Files:   C.all/src/print.p,  C.all/src/pretty.p
 */

#_INCLUDE 'declare.ph'

section $-Sys => is_poplog_item;

define is_poplog_item(item);
    lvars item _key;
    if issimple(item) then
        if isinteger(item) then
            integer_key
        else
            weakref decimal_key
        endif
    elseif _isaddress(item)
    and _LOWEST_POP_ADDRESS <=@(w) item and item <@(w) _open_seg_free_ptr
    and _isaddress(item!KEY ->> _key)
    and _LOWEST_POP_ADDRESS <=@(w) _key and _key <@(w) _open_seg_free_ptr
    and _key!KEY == key_key then
        _key
    else
        false
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
 */
