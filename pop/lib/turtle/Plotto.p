/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/Plotto.p
 |  Purpose:        part of turtle package
 |  Author:         Aaron Sloman circa 1982 (see revisions)
 |  Documentation:
 |  Related Files:
 */
section $-turtle => Plotto;

define global Plotto(_x, _y);
    ;;; mark the picture from current location to location _x,_y
    lvars _x1 _y1 _inc _dx _dy  _stepx;
    ;;; get current location
    xposition + Xdelta -> _x1;
    yposition + Ydelta -> _y1;
    ;;; work out increments
    _x - _x1 -> _dx; _y - _y1 -> _dy;
    if round(_dx) == 0 and round(_dy) == 0 then Markhere(_x,_y); return endif;
    ;;; decide whether to increment x or y
    if abs(_dx) > abs(_dy) then
        true,
    else
        _x1,_y1 -> _x1 -> _y1;
        _x, _y  -> _x -> _y;
        _dx, _dy -> _dx -> _dy;
        false
    endif -> _stepx;
    ;;; _x1 will step by  sign(dx), i.e. 1 or -1
    ;;; set _inc, the increment for the variable which changes less
    _dy / abs(_dx) -> _inc;
    sign(_dx) -> _dx;
    for _x1 from _x1 by _dx to _x - (0.1 * _dx) do
        Markhere(if _stepx then _x1, _y1 else _y1, _x1 endif);
        _y1 + _inc -> _y1;
    endfor;

        Markhere(if _stepx then _x,_y else _y, _x endif);
enddefine;

;;; So that PLOTTO does not appear in calling sequences
;;; give it false PDPROPS:
false -> pdprops(Plotto);

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 26 1988 - tabified
 */
