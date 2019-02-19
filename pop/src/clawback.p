/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/clawback.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; -------------------- RECLAIMING MEMORY --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'


;;; -----------------------------------------------------------------------

section $-Sys;

    /*  Called by a procedure which saves _open_seg_free_ptr or
        _curr_seg_free_ptr in _nextfree_save, does a computation producing
        some intermediate results, and then produces a final result _item.
        Clawback copies _item back to the saved point, reclaiming the space
        taken by the intermediate results.
        The garbage collector zeros _nextfree_save and all its local values
        on the stack, telling this procedure it can't do its stuff.
    */
define Clawback(/* _item */) with_nargs 1;
    lvars _svp = _nextfree_save;

    define lconstant Reclaim(_item, _svp, _lim);
        lvars _item, _svp, _lim, _f = _free_pairs;
        if iscompound(_f) and _svp <=@(w) _f and _f <@(w) _lim then
            ;;; clear any free pairs being erased!
            0 -> _free_pairs
        endif;
        if iscompound(_item) and _svp <=@(w) _item and _item <@(w) _lim then
            ;;; reclaim -- if any space in between used shift the object back
            ;;; down to the save point (erasing any intermediate results),
            ;;; returning the new free point
            fast_apply(_item, _item!KEY!K_GET_SIZE) -> _lim;    ;;; rec size
            if _item /== _svp then
                ;;; move _item down to _svp
                if _lim == @@(struct POPREC2)++ then
                    ;;; quick copy for ddecimals, ratios and complex
                    _item!FIRST -> _svp!FIRST;
                    _item!KEY   -> _svp!KEY;
                    _item!SECOND-> _svp!SECOND
                else
                    _moveq(_lim, _item@POPBASE, _svp@POPBASE) ->
                endif
            endif;
            _svp, _svp@(w){_lim}        ;;; return new item and next free ptr
        else
            ;;; else item is simple or not in seg containing saved point
            ;;; reclaim back to save point
            _item, _svp
        endif
    enddefine;

    define lconstant Other_seg(_item, _svp);
        lvars _seg = _seg_table, _svp, _item, _base, _lim;
        while _seg <@(struct SEG) _seg_table_next_free do
            unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
                _seg!SEG_BASE_PTR -> _base;
                _base@(w){_seg!SEG_SIZE} -> _lim;
                if _base <=@(w) _svp and _svp <@(w) _lim then
                    return(Reclaim(_item, _svp, _lim) -> _seg!SEG_FREE_PTR)
                endif
            endunless;
            _seg@(struct SEG)++ -> _seg
        endwhile;
        ;;; ? forget it
        _item
    enddefine;

    unless _svp == _NULL then
        if _svp >=@(w) _open_seg_base_ptr then
            ;;; saved while in the open seg
            ;;; this returns new item
            Reclaim((), _svp, _userhi) -> _open_seg_free_ptr
        elseif _curr_heap_seg /== _NULL
        and _curr_heap_seg!SEG_BASE_PTR <=@(w) _svp
        and _svp <@(w) _curr_seg_free_lim then
            ;;; saved while in current seg
            Reclaim((), _svp, _curr_seg_free_lim) ->> _curr_seg_free_ptr
                                            -> _curr_heap_seg!SEG_FREE_PTR
        else
            ;;; saved while in another seg
            Other_seg((), _svp)
        endif
    ;;; else gc intervened (or in fixed seg) -- return unchanged
    endunless;

    ;;; resave
    if _curr_heap_seg == _NULL then
        _open_seg_free_ptr
    else
        _curr_seg_free_ptr
    endif -> _nextfree_save
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
--- John Gibson, Apr 14 1988
        Moved out of getstore.p
 */
