/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/ivalofinit_flavour.p
 > Purpose:        a mixin with a different initialise from vanilla.
 > Author:         Mark Rubinstein, Jul  8 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY/ivalofinit_flavour
 > Related Files:
 */

;;; a mixin whose initiliase method will shadow the default one in vanilla.
;;; The only difference between the two is that this one expects all initable
;;; facets to be instance variables and uses ivalof to update them.

section;

flavour ivalofinit a mixin;
    defmethod initialise(initlist);
    lvars initlist slot value;
        until initlist == [] do
            dest(initlist) -> initlist -> slot;
            dest(initlist) -> initlist -> value;
            value -> ivalof(self, slot);        ;;; no daemons
        enduntil;
    enddefmethod;
endflavour;           

endsection;
