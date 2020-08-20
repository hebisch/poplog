/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/collection_flavour.p
 > Purpose:        Mixin for collections (bag without duplicates)
 > Author:         Mark Rubinstein, Jul  8 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY /collection_flavour
 > Related Files:  LIB * BAG_FLAVOUR.p
 */

section;

flavour collection a mixin isa bag;
    defmethod before add(o);
    lvars o;
        if ^present(o) then quitmessage else o endif;
    enddefmethod;
endflavour;

endsection;
