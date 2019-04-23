/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/ved/src/vdinorout.p
 > Purpose:         Copy or transcribe range between files
 > Author:          Aaron Sloman (see revisions)
 */

;;; ---------------- MOVING MARKED RANGES BETWEEN FILES ----------------------

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedswapfiles, ved_file_change_apply)
    ;

global vars
        procedure (ved_ved, ved_copy, ved_d, ved_y)
    ;

;;; ---------------------------------------------------------------------

section $-Sys$-Ved => ved_ti, ved_to, ved_mi, ved_mo;

    ;;; copy or move marked range out to other file or in form other file
    ;;; if cpy is false then move.
    ;;; if incoming is false then transfer out, not in

define lconstant copy_or_move(cpy, incoming);
    lvars cpy, incoming, file, org_file = ved_current_file, procedure get_p;
    dlocal  vedargument,
            vedwarpcontext = false,     ;;; stop windows being opened
            wvedalwaysraise = false,
            ved_on_status = false;

    if vedargument /= nullstring then
        ved_ved();
        if ved_current_file == org_file then
            vederror('\{b}must be a different file')
        endif;
        vedswapfiles()
    elseif back(vedbufferlist) == [] then
        vederror('\{b}no other file')
    endif;
    nullstring -> vedargument;

    front(back(vedbufferlist)) -> file;
    if cpy then ved_copy else ved_d endif -> get_p;

    if incoming then
        define lconstant get_in();
            dlocal ved_on_status = false;
            get_p()
        enddefine;

        ved_file_change_apply(get_in, file);
        ved_y()

    else
        define lconstant yank_out();
            lvars line;
            dlocal ved_on_status = false;
            ved_y();
            vedline fi_+ listlength(vveddump) -> line;
            unless line == vedline then vedtrimline() endunless;
            line -> vedline;
            vedsetlinesize()
        enddefine;

        get_p();
        ved_file_change_apply(yank_out, file)
    endif
enddefine;

define vars ved_ti;
    copy_or_move(true,true)
enddefine;

define vars ved_to;
    copy_or_move(true,false)
enddefine;

define vars ved_mi;
    copy_or_move(false,true)
enddefine;

define vars ved_mo;
    copy_or_move(false,false)
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 25 1991
        Changed to deal properly with filename arg
--- John Gibson, Aug  9 1991
        Fixed so transfer is never to/from status
--- John Gibson, Jan  8 1991
        Rewritten
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- John Gibson, Aug 16 1987
        Tidied up
 */
