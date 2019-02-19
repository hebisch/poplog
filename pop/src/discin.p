/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/discin.p
 > Purpose:         Input file character repeater
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/discout.p
 */


#_INCLUDE 'declare.ph'

constant
        procedure (device_full_name, isdevice, Sys$-Io$-Get_char)
    ;

vars
        pop_default_type
    ;

;;; ---------------------------------------------------------------------

section $-Sys => discin, discin_device;

define discin(file) -> file;
    lvars file, dev;
;;;        _extern printf('entering discin(%p)\n', file) -> _ ;
;;;        _extern fflush(_0) -> _ ;
    if isdevice(file) then
        file -> dev;
        Check_device(dev, true)     ;;; checks it open
    else
        if isword(file) then
            file!W_STRING sys_>< pop_default_type -> file
        endif;
        sysopen(file, 0, false, `N`) -> dev
    endif;
;;;        _extern printf('discin(%p)\n', file) -> _ ;
;;;        _extern fflush(_0) -> _ ;
    Consclos_protect(Io$-Get_char, dev, 1) -> file;
    device_full_name(dev) -> pdprops(file)
enddefine;

define discin_device(p);
    lvars p, _mshp = true;
    if isboolean(p) then ((), p) -> (p, _mshp) endif;
    if pdpart(p) == Io$-Get_char then
        frozval(1, p)
    elseif _mshp then
        mishap(p, 1, 'DISCIN CHARACTER REPEATER PROCEDURE NEEDED')
    else
        false
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 15 1996
        Allowed discin_device to take optional bool arg to say return
        false if not a discin procedure.
--- John Williams, May  7 1993
        Added discin_device (cf. isl-er.217)
--- John Gibson, May 28 1990
        Changed to use new sysopen arg.
--- John Williams, Sep 12 1988
        Now assigns full pathname to -pdprops- of repeater
--- John Gibson, Mar 16 1988
        Previously in iodisc.p
 */
