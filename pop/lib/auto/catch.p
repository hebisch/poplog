/* --- Copyright University of Sussex 1987. All rights reserved. ----------
 > File:            C.all/lib/auto/catch.p
 > Purpose:         Catch and Throw Package
 > Author:          Aaron Sloman, 1982 (see revisions)
 > Documentation:   REF * PROCEDURE, HELP * CATCH
 > Related Files:   LIB * THROW
 */
compile_mode :pop11 +strict;

section;

lvars if_caught, catch_patt;

define catch(apply_p, if_caught, catch_patt);
    lvars apply_p;
    dlocal if_caught, catch_patt;
    apply_p()
    ;;; if throw is called with an argument which matches catch_patt
    ;;; then if_caught will be run here
enddefine;


define throw(throw_item);
    lvars throw_item;

    define lconstant throw_unwind(throw_item);
        lvars throw_item;
        if throw_item matches catch_patt then
            if isprocedure(if_caught) then
                if_caught()
            else
                if_caught
            endif
        else
            chainfrom(throw_item, catch, throw)
        endif
    enddefine;

    chainto(throw_item, catch, throw_unwind)
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1987
        Lvars'ed, tidied up
 */
