/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedinkeys.p
 >  Purpose:        read in key strokes and write in string format
 >  Author:         A.Sloman (see revisions)
 >  Documentation:
 >  Related Files:  Used by LIB * VED_DM, *VED_DK
 */
compile_mode :pop11 +strict;

section;

define lconstant trans(x);
    lvars x;
    if x > 127 then
        vedinsertstring('\\(' >< x >< ')')
    elseif x < 32 then
        vedinsertstring('\\^'); vedcharinsert(x + 64);
    elseif x == `\^?` then
        vedinsertstring('\\^?')
    elseif x == `'` then
        vedinsertstring('\\\'')
    else
        vedcharinsert(x);
    endif;
    vedcheck();
    vedsetcursor();
enddefine;

define vedinkeys(mess);;
    lvars mess, name, z, x, n;
    vedputmessage(if mess then
                    mess
                  else
                    'TO TERMINATE PRESS ESC button 3 TIMES'
                  endif);
    vedscreenbell();
    vedwiggle(vedline, vedcolumn);
    vedscr_flush_output();
    repeat
        vedinascii() -> z;
        if z == `\^[` then
            ;;; ESC typed
            vedinascii() -> x;
            if x == z then
                vedinascii() -> x;
                if x == z then
                    vedputmessage(nullstring);  ;;; remove message
                    return
                else
                    trans(z); trans(z); trans(x)
                endif;
            elseif x == `\^?` then
                ;;; ESC DEL typed
                vedchardelete();
            else
                trans(z),trans(x);
            endif
        else
            trans(z)
        endif;
    endrepeat;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  3 1991
        Uses vedscr_flush_output
 */
