/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/sysseek.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ------------------- SEEK ON A DEVICE ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

;;; ---------------------------------------------------------------------

section $-Sys => sysseek;

define sysseek(dev, _pos, _mode);
    lvars dev, _pos, _mode, _retpos = false;
    if isboolean(_mode) then
        ;;; 4-th arg true means return position after seek
        _mode -> _retpos; _pos -> _mode; dev -> _pos;
         -> dev
    endif;
    Check_device(dev, true);
    Check_integer(_pos, false);
    Check_integer(_mode, 0);
    if _mode fi_> 2 then
        _mode fi_- 3 -> _mode;
        _pos fi_* 512 -> _pos
    endif;
    fast_apply(dev, _pos, _mode, dev!D_SEEK);
    unless _retpos then -> endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 22 1990
        Dev seek procedure now takes all-pop args and returns pop int result
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Apr 15 1988
        Moved out of sysio.p (combined Unix & VMS versions)
 */
