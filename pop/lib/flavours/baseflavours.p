/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/baseflavours.p
 > Purpose:        Load the initial flavours for the flavours system
 > Author:         Mark Rubinstein, Apr 17 1986
 > Documentation:  TEACH * FLAVOURS
 > Related Files:  The libraries shown.
 */

lib metaflavour_flavour;
lib mixin_flavour;
lib flavour_flavour;
lib vanilla_flavour;

;;; this bit means that vanilla will become a component of metaflavours.
flavour metaflavour a metaflavour;
endflavour;

flavour mixin a metaflavour isa metaflavour;
endflavour;

flavour flavour a metaflavour isa metaflavour;
endflavour;
