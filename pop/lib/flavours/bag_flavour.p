/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/bag_flavour.p
 > Purpose:        general purpose bag class
 > Author:         Mark Rubinstein, Jul  8 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY
 > Related Files:  LIB * COLLECTION_FLAVOUR
 */

section;

flavour bag a mixin;
ivars contents = [];
    defmethod add(object);
    lvars object;
        conspair(object, contents) -> contents;
    enddefmethod;
    defmethod remove(object);
    lvars object;
        ncdelete(object, contents, nonop ==) -> contents;
    enddefmethod;
    defmethod present(object);
    lvars object;
        if lmember(object, contents) then object else false endif
    enddefmethod;
    defmethod appcontents(message);
    lvars message args each;
        if isprocedure(message) then
            applist(contents, message);
        elseif isvector(message) then       ;;; vector of arguments
            message -> args;
            -> message;
            for each in contents do
                each(explode(args), message);
            endfor;
        else
            for each in contents do
                each(message);              ;;; send the message to each
            endfor;
        endif;
    enddefmethod;
    defmethod mapcontents(message);
    lvars message args each;
        if isprocedure(message) then
            maplist(contents, message) -> contents;
        elseif isvector(message) then       ;;; vector of arguments
            message -> args;
            -> message;
            [%  for each in contents do
                    each(explode(args), message);
                endfor; %] -> contents;
        else
            [%  for each in contents do
                    each(message);              ;;; send the message to each
                endfor; %] -> contents;
        endif;
    enddefmethod;
    defmethod make_instance(ilist) -> object;
    lvars ilist object;
        make_instance(ilist) -> object;
        ^add(object);
    enddefmethod;
endflavour;

endsection;
