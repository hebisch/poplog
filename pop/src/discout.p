/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/discout.p
 > Purpose:         Output file character consumer
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/discin.p
 */


#_INCLUDE 'declare.ph'

constant
        procedure (device_full_name, isdevice, Sys$-Io$-Put_char)
    ;

vars
        pop_default_type
    ;

;;; ---------------------------------------------------------------------

section $-Sys => discout discout_device;

define discout(file) -> file;
    lvars file, dev;
    if isdevice(file) then
        file -> dev;
        Check_device(dev, true)     ;;; checks it open
    else
        if isword(file) then
            file!W_STRING sys_>< pop_default_type -> file
        endif;
        unless syscreate(file, 1, false) ->> dev then
            Syserr_mishap(file, 1, 'CAN\'T CREATE FILE')
        endunless
    endif;
    Consclos_protect(Io$-Put_char, dev, 1) -> file;
    device_full_name(dev) -> pdprops(file)
enddefine;

define discout_device(p);
    lvars p;
    if pdpart(p) == Io$-Put_char then
        frozval(1, p)
    else
        mishap(p, 1, 'DISCOUT CHARACTER CONSUMER PROCEDURE NEEDED')
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Williams, May  7 1993
        Added discout_device (cf. isl-er.217)
--- John Williams, Sep 12 1988
        Now assigns full pathname to -pdprops- of repeater
--- John Gibson, Mar 16 1988
        Previously in iodisc.p
 */
