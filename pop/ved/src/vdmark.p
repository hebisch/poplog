/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/ved/src/vdmark.p
 > Purpose:         Marked ranges in VED
 > Author:          Aaron Sloman (see revisions)
 */

;;; --------------------- MARKING RANGES ------------------------------------

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedmarkset)
    ;


;;; ------------------------------------------------------------------------

section $-Sys$-Ved =>   vedmarked, vedmarkfind, vedendrange,
                        vedmarkpush, vedmarkpop,
                        ved_mbf, ved_mef;

define lconstant Ensure_marked();
    unless vvedmarkhi fi_> 0 do
        vederror('\{b}no marked range')
    endunless
enddefine;

define vedmarked(line);
    lvars line;
    if vvedmarklo fi_<= line and line fi_<= vvedmarkhi then
        if vvedmarkprops then vedscreenrangemark else `\s` endif
    else
        false
    endif
enddefine;

define vedmarkfind();
    Ensure_marked();
    vedjumpto(vvedmarklo,1);
enddefine;

define vedendrange();
    Ensure_marked();
    vedjumpto(vvedmarkhi,1);
enddefine;

    /*  Change all lines on the screen which need to be changed
    */
define lconstant Mark_range(_oldlo, _oldhi, _newlo, _newhi, set_scr_valid);
    lvars line, bottom, set_scr_valid, _newhi, _oldlo, _oldhi, _newlo;
    returnunless(vvedmarkprops and vedfileisonscreen(ved_current_file)
                    and vedediting and not(vedonstatus));
    vedlineoffset fi_+ 1 -> line;
    Bottom_of_window() -> bottom;
    while line fi_<= bottom do
        _checkinterrupt();
        if _newlo fi_<= line and line fi_<= _newhi then
            unless _oldlo and _oldlo fi_<= line and line fi_<= _oldhi then
                vedmarkset(line, vedscreenrangemark)
            endunless
        elseif _oldlo then
            unless line fi_< _oldlo or _oldhi fi_< line then
                vedmarkset(line, `\s`)
            endunless
        endif;
        line fi_+ 1 -> line
    endwhile;

    if set_scr_valid then
        ;;; set screen marks unchanged (this is set false by vedrefreshline
        ;;; and vedmarkset)
        true -> fast_front(marked_range_stack)
    endif
enddefine;

define vedmarkhi();
    Mark_range(vvedmarklo,vvedmarkhi,
        min(vvedmarklo,vedline) ->> vvedmarklo, vedline ->> vvedmarkhi, false)
enddefine;

define vedmarklo();
    Mark_range(vvedmarklo,vvedmarkhi,
        vedline ->> vvedmarklo, max(vvedmarklo,vvedmarkhi) ->> vvedmarkhi, false)
enddefine;

define vedmarkpush();
    lvars pair = marked_range_stack, mrs = fast_back(pair);
    if mrs == [] then true -> fast_front(pair) endif;
    {^vvedmarklo ^vvedmarkhi ^vvedmarkprops ^(fast_front(pair))} :: mrs
                                -> fast_back(pair)
enddefine;

define vedmarkpop();
    lvars   pair = marked_range_stack, mrs = fast_back(pair), vec, scr_valid,
            oldlo, oldhi;
    if mrs == [] then
        unless vvedmarkprops then true -> vvedmarkprops endunless;
    else
        unless fast_front(pair) then
            ;;; screen marks changed since stacked -- invalidate all
            ;;; screen marks for stacked stuff
            fast_for vec in mrs do false -> fast_subscrv(4,vec) endfor
        endunless;

        if vvedmarkprops then
            vvedmarklo, vvedmarkhi
        else
            ;;; dont let Mark_range think it's already marked
            dup(false)
        endif -> (oldlo, oldhi);

        explode(fast_destpair(mrs) -> fast_back(pair))
            -> (vvedmarklo, vvedmarkhi, vvedmarkprops, scr_valid);

        unless scr_valid then
            Mark_range(oldlo, oldhi, vvedmarklo, vvedmarkhi, true)
        endunless
    endif
enddefine;

define Clear_temp_mark();
    until vvedmarkprops do vedmarkpop() enduntil
enddefine;


;;; --- COMMANDS FOR MARKING RANGES --------------------------------------

define vars ved_mbf();
    ;;; mark from beginning of file
    dlocal vedline = 1;
    vedmarklo()
enddefine;

define vars ved_mef();
    ;;; mark to end of file
    dlocal vedline = vvedbuffersize;
    vedmarkhi()
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 25 1998
        Revised way in which front(marked_range_stack) is dealt with
        by vedmarkpush and vedmarkpop.
--- John Gibson, Feb 28 1996
        Changed Mark_range to use Bottom_of_window and to use fi_<= loop
        test rather than ==. (Also tests vedfileisonscreen before doing
        anything.)
--- John Gibson, Jan 22 1996
        marked_range_stack is now a pair whose back is the actual stack and
        whose front is a boolean, set true by vedmarkpush and set false
        by those procedures which change marks on the screen. This enables
        vedmarkpop to detect that the range being popped has not been
        changed on the screen, so it doesn't need to refresh the marks.
--- John Williams, Jan  8 1993
        Moved vedendrange in from $popvedlib (because ved_moveto uses it)
--- John Gibson, Mar 31 1992
        Sectionised
--- John Gibson, Jan 23 1992
        Replaced vedscreenm*ark with vedscreenrangemark
--- John Gibson, Aug 16 1987
        Tidied up
 */
