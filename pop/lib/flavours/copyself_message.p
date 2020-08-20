/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/copyself_message.p
 > Purpose:        method for instance to return copy of self.
 > Author:         Mark Rubinstein, Apr 18 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY, TEACH * FLAVOUR
 > Related Files:  LIB * FLAVOURS
 */

section $-flavour vanilla_flavour => vanilla_flavour;

flavour vanilla novanilla;
    defmethod copyself;
        consflavour_instance(i_frec(self),
            copy(i_dinstvals(self)),
            copy(i_linstvals(self)));
    enddefmethod;
endflavour;

endsection;

/* --- Revision History ----------------------------------------------------
--- Richard Bignell, Jul 16 1986 - Corrected syntax of flavour definition (ie
    used 'endflavour', not 'enddefine'
*/
