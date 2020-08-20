/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:           C.all/lib/flavours/vanilla_flavour.p
 > Purpose:        The root flavour of the flavour inheritance lattice
 > Author:         Mark Rubinstein, Apr 17 1986 (see revisions)
 > Documentation:  TEACH * FLAVOURS
 > Related Files:  LIB * MIXIN_FLAVOUR
 */

section;

flavour vanilla a mixin novanilla;
    defmethod initialise(initlist);
    lvars initlist slot value;
        until initlist == [] do
            dest(initlist) -> initlist -> slot;
            dest(initlist) -> initlist -> value;
            value -> self(slot);
        enduntil;
    enddefmethod;
    defmethod printself;
        printf('<instance of %p>', [% myflavour<-name %]);
    enddefmethod;
    defmethod libcompilemessage(m, upd);
    lvars m dir upd mfile = m sys_>< '_message.p';
        for dir in popautolist do
            if trycompile(dir dir_>< mfile) then
                if (upd and myflavour<-willrespondto(m, "updater"))
                or myflavour <- willrespondto(m) then
                    prautoloadwarn(mfile);
                    return(dir)
                endif;
            endif;
        endfor;
        false;
    enddefmethod;
    defmethod default_method(message);
    lvars message;
        if ^libcompilemessage(message, false) then
            ;;; if you can autoload the message then send it again.
            self(message)
        else
            mishap(message, self, 2, 'UNRECOGNISED MESSAGE');
        endif;
    enddefmethod;
    defmethod updaterof default_method(message);
    lvars message;
        if ^libcompilemessage(message, true) then
            ;;; if you can autoload the message then send it again.
            -> self(message)
        else
            mishap(message, self, 3, 'ATTEMPT TO UPDATE UNRECOGNISED MESSAGE');
        endif;
    enddefmethod;
endflavour;

endsection;

/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Ian Rogers, Jul 25 1988 - changed printself method to use the safe
    form of -printf- (cf. B.R. tomk.52)
--- Mark Rubinstein, May 29 1986 - call to libcompilemessage altered to use
    new ^ send self syntax.
*/
