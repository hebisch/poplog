/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/iscomponent_message.p
 > Purpose:        test if argument is a  component of self
 > Author:         Mark Rubinstein, Apr 18 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY, TEACH * FLAVOUR
 > Related Files:  LIB * FLAVOURS
 */

section;

flavour metaflavour;
    ;;; is f a component of mine.  Trace up inheritence
    defmethod iscomponent(testf);
    lvars testf f comps;
        self<-components -> comps;
        if lmember(testf, comps) then
            true
        else
            for f in comps do
                if f<-iscomponent(testf) then
                    return(true)
                endif;
            endfor;
            false
        endif;
    enddefmethod;
endflavour;

endsection;
