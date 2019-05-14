;;; --- Copyright University of Sussex 1986.  All rights reserved. ---------
;;; File:           C.all/lib/auto/unpackitem.p
;;; Purpose:        Unpack a word or string or integer into a list
;;; Author:         A. Sloman and S. Hardy (see revisions)
;;; Documentation:  HELP *UNPACKITEM
;;; Related Files:  LIB *PACKITEM

section;

lconstant procedure Consword1 = consword(% 1 %);

define global procedure unpackitem(item) -> list;
    lvars item list;
    if isintegral(item) then
        if item < 0 then
            mishap(item, 1, 'NEGATIVE NUMBER GIVEN TO UNPACKITEM')
        elseif item == 0 then
            [0] -> list
        else
            [] -> list;
            until item == 0 do
                conspair(item // 10 -> item, list) -> list
            enduntil
        endif
    else
        [% appdata(item, Consword1) %] -> list
    endif
enddefine;

endsection;

;;; --- Revision History ---------------------------------------------------
;;; --- John Williams, Dec 11 1987 now works if argument is 0.
;;; --- Mark Rubinstein, Jan  7 1986 made lconstant Consword1.
