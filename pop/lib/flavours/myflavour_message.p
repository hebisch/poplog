/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/myflavour_message.p
 > Purpose:        access of change the flavour of an instance
 > Author:         Mark Rubinstein, Apr 18 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY, TEACH * FLAVOUR
 > Related Files:  LIB * FLAVOURS
 */

section;

sysunprotect("myflavour");

flavour vanilla;
    defmethod myflavour;
        myflavour;
    enddefmethod;
    defmethod updaterof myflavour(newflavour);
    lvars iv myivars newivars newflavour newinstance;
        unless isinstance(newflavour, metaflavour_flavour) do
            mishap(newflavour, 1, 'METAFLAVOUR NEEDED');
        endunless;

        ;;; make a new instance which will have the default values set.
        newflavour<-new -> newinstance;

        ;;; set the values for instance variables with the same name
        myflavour <- instance_variables -> myivars;
        newflavour <- instance_variables -> newivars;
        for iv in myivars do
            if lmember(iv, newivars) then
                ivalof(self, iv) -> ivalof(newinstance, iv);
            endif;
        endfor;

        ;;; now for the rude bit.
        $-flavour$-i_frec(newinstance) -> $-flavour$-i_frec(self);
        $-flavour$-i_linstvals(newinstance) -> $-flavour$-i_linstvals(self);
        $-flavour$-i_dinstvals(newinstance) -> $-flavour$-i_dinstvals(self);
        ;;; we have to change the value of the dynamic variable myflavour
        ;;; in case any after daemons should inspect it.
        newflavour -> myflavour;
    enddefmethod;
endflavour;

sysprotect("myflavour");                   

endsection;
