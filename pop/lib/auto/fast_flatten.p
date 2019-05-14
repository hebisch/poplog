/* --- University of Sussex POPLOG file -----------------------------------
 > File:           $usepop/master/C.all/lib/auto/fast_flatten.p
 > Purpose:        Flatten a tree into a list
 > Author:         Unknown, ???
 > Documentation:
 > Related Files:  LIB * FLATTEN
 */

section;

define global procedure fast_flatten(l);
lvars l;
    define procedure fast_flatten(list);
        ;;; put all the elements of the tree on the stack
        if list == [] then
        elseif atom(list) then
            list
        else
            fast_flatten(fast_front(list)); fast_flatten(fast_back(list))
        endif
    enddefine;
    [% fast_flatten(l) %]
enddefine;

endsection;
