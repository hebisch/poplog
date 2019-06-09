/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/S-XptS-ConsAccess.p
 > Purpose:         Produce access procedure for resource accessing/updating
 > Author:          John Gibson, May 12 1995
 > Related Files:   LIB * XptVal
 */

section $-Xpt;

    /*  Construct an external access procedure for typespec spec, but
        where the updater just returns the converted field value.
        (See similar inline code in XptVal.)
    */
define ConsAccess(spec) -> p;
    lvars spec, p, org_spec = spec, upd = false;
    field_spec_info(spec) -> (,);   ;;; check valid first

    while isclosure(spec) do
        if frozval(2,spec) then
            updater(pdpart(spec)) -> p;         ;;; conv procedure updater
            if upd then upd <> p else p endif -> upd;
            frozval(1,spec) -> spec     ;;; strip it off
        else
            ;;; access procedure -- invalid, since we only allow
            ;;; direct values
            mishap(spec, 1, 'ConsAccess: INVALID TYPESPEC')
        endif
    endwhile;
    if isword(spec) and isendstring('float', spec) then
        ;;; real -> decimal
        lconstant dec_coerce = number_coerce(%1.0s0%);
        if upd then upd <> dec_coerce else dec_coerce endif -> upd
    endif;

    cons_access(if upd then consref(true) else true endif, org_spec, false, 1)
                                    -> p;

    ;;; with no conversions, updater just dups and checks field value
    upd or (dup <> updater(p)(%#_< EXPTRINITSTR(:dfloat) >_#%)) -> updater(p)
enddefine;

endsection;
