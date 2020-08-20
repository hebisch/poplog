/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/flavour_flavour.p
 > Purpose:        The metaflavour for flavours
 > Author:         Mark Rubinstein, Apr 17 1986
 > Documentation:  TEACH * FLAVOURS, HELP * METAFLAVOUR
 > Related Files:  LIB * METAFLAVOUR_FLAVOUR
 */

section $-flavour => flavour_flavour;

flavour flavour a metaflavour novanilla isa metaflavour;
    defmethod new;
        consinstance(flavour_record);
    enddefmethod;
endflavour;

endsection;
