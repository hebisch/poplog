/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/is_fixed.p
 > Purpose:
 > Author:          John Gibson, Jan 29 1990 (see revisions)
 > Documentation:   REF *DATA
 */

;;; ----------- TESTING FOR A FIXED-ADDRESS STRUCTURE ---------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'


;;; ----------------------------------------------------------------------

section $-Sys => is_fixed;

define is_fixed(item);
    lvars item, _seg = _seg_table;
    returnunless(isinheap(item)) (true);    ;;; checks it compound
    returnif(_open_seg_base_ptr <=@(w) item and item <@(w) _open_seg_free_ptr)
                                                            (false);
    returnif(item!KEY!K_FLAGS _bitst _:M_K_ALWAYS_FIXED) (true);
    while _seg <@(struct SEG) _seg_table_next_free do
        returnif(    _seg!SEG_FREE_PTR >@(w) item
                 and _seg!SEG_BASE_PTR <=@(w) item
                 and not(_seg!SEG_FLAGS _bitst _M_SEG_NON_POP))
            (_seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS);
        _seg@(struct SEG)++ -> _seg
    endwhile;
    mishap(item, 1, 'SYSTEM ERROR -- INVALID STRUCTURE')
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  3 1992
        Aded test for M_K_ALWAYS_FIXED
--- John Gibson, May  3 1990
        Moved out of getstore_fixed.p
 */
