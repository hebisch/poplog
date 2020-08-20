/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/named_object_flavour.p
 > Purpose:        define a mixin for an object with a name
 > Author:         Mark Rubinstein, Apr 18 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY, TEACH * FLAVOUR
 > Related Files:  LIB * FLAVOURS
 */

section;

flavour named_object a mixin;
ivars name;
    defmethod printself;
        printf('<object %p>', [^name]);
    enddefmethod;
endflavour;

endsection;
