/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/movebits.p
 > Purpose:
 > Author:          John Gibson, Aug  2 1988
 */

;;; ---------------- MOVING BITFIELDS IN VECTORS -------------------------

#_INCLUDE 'declare.ph'

constant
        _bitfield
    ;

;;; --------------------------------------------------------------------

define Sys$-Move_bits(_sub1, vec1, _sub2, vec2, _len, _size);
    lvars vec1, vec2, _sub1, _sub2, _len, _size;
    if _size /== _1 then
        _sub1 _mult _size -> _sub1;
        _sub2 _mult _size -> _sub2;
        _len _mult _size -> _len
    endif;
    @@V_BITS{_sub1} -> _sub1;
    @@V_BITS{_sub2} -> _sub2;
    if vec1 /== vec2 or _sub1 _gr _sub2 then
        ;;; not same vector, or moving down
        while _len _greq _32 do
            vec1!(32){_sub1} -> vec2!(32){_sub2};
            _sub1 _add _32 -> _sub1;
            _sub2 _add _32 -> _sub2;
            _len _sub _32 -> _len
        endwhile
    else
        ;;; moving up in same vector
        _sub1 _add _len -> _sub1;
        _sub2 _add _len -> _sub2;
        while _len _greq _32 do
            _sub1 _sub _32 -> _sub1;
            _sub2 _sub _32 -> _sub2;
            vec1!(32){_sub1} -> vec2!(32){_sub2};
            _len _sub _32 -> _len
        endwhile;
        _sub1 _sub _len -> _sub1;
        _sub2 _sub _len -> _sub2
    endif;

    unless _zero(_len) then
        ;;; move the last few bits
        _bitfield(vec1, _sub1, _len) -> _bitfield(vec2, _sub2, _len)
    endunless
enddefine;
