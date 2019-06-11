/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_hkey.p
 >  Purpose:        Describe a key (or key sequence)
 >  Author:         Aled Morris, Aug  7 1986 (see revisions)
 >  Documentation:  HELP *VED_HKEY
 >  Related Files:  LIB *VED_HK
 */

section;

define lconstant Vedkeytrans(list);
    lvars item, list;
    consstring(#|
        `\'`,
        fast_for item in list do
            if item fi_> 127 then
                `\\`, `(`, dest_characters(item), `)`
            elseif item fi_< 32 then
                if item == `\e` then
                    `\\`, `e`
                else
                    `\\`, `\^`, item fi_+ `@`
                endif
            elseif item == `\^?` then
                `\\`, `\^`, `?`
            elseif item == `'` then
                `\\`, `\'`
            else
                item
            endif
        endfast_for;
        `\'`
    |#)
enddefine;


define global ved_hkey();
    lvars chars, insert, item, oldvedscr_read_input, props,
        Vedescapestring = consstring(vedescape, 1);

    unless (vedargument = '-i' ->> insert) then
        unless vedargument = nullstring do
            vederror('Unknown argument')
        endunless
    endunless;

    [] -> chars;
    vedscr_read_input -> oldvedscr_read_input;
    define dlocal vedscr_read_input() -> c;
        lvars c;
        (oldvedscr_read_input() ->> c) :: chars -> chars
    enddefine;

    vedputmessage('Please press the key. If no response, press more keys');
    vedgetproctable(vedscr_read_input) -> item;
    if item = Vedescapestring then
        ;;; Hack for LIB VEDVT220 where Select is mapped to Escape
        vedgetproctable(vedescape) -> item
    endif;
    Vedkeytrans(fast_ncrev(chars)) -> chars;
    if insert then
        vedinsertstring(chars)
    endif;
    if isprocedure(item) then
        if (recursive_front(pdprops(item)) ->> props) then
            ved_try_do_??(props sys_>< nullstring, false) ->
        endif;
        vedputmessage('Sequence ' <> chars <> ' maps onto: ' sys_>< item)
    elseif isstring(item) then
        vedputmessage('Sequence ' <> chars <> ' maps onto: ' <> item)
    else
        vedputmessage('Sequence ' <> chars <> ' is undefined')
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar  7 1996
        Fixed bug in Vedkeytrans (was producing \\^M instead of \^M).
--- John Gibson, May 13 1993
        Changed to display ESC as \e rather than \^[
--- John Williams, Jul 30 1992
        Fixed bug where initial escape character was being lost
--- John Gibson, Jun  3 1991
        Uses vedscr_read_input
--- John Williams, Jan  3 1990
        Fixed for new -ved_??- mechanism
--- John Williams, Oct 17 1989
        Now copes sensibly with keys that mimic "escape" key.
            (e.g. Select key in LIB VEDVT220)
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
*/
