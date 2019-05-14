/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/S-libS-db_try_all_matches.p
 > Purpose:         Multiple-pattern matching procedure
 > Author:          John Gibson, Dec 27 1995
 > Documentation:
 > Related Files:   LIB * allequalto
 */
compile_mode :pop11 +strict;

;;; db_try_all_matches is run as a process by for var allequalto ....
;;; It takes a whole list of patterns and a database list, and tries to find
;;; a way of binding variables so that all items are present in the database.
;;; After finding one, it suspends the current process, which can be resumed
;;; later.

section $-lib;

define db_try_all_matches(pattlist, database, want_instance);
    dlocal database, pop_matchvars_bound = [];
    dlvars want_instance;

    define lconstant swap_id_values(conv);
        lvars conv, next, id;
        until conv == [] do
            fast_destpair(fast_destpair(conv)) -> (id, next);
            valof(id) -> fast_front(conv);          ;;; save current value
            () -> valof(id);                        ;;; assign new val
            next -> conv
        enduntil
    enddefine;

    define lconstant tryall(pattlist);
        lvars item, patt, procedure eq_p,
            (save_conv, save_matchvars) = fast_destpair(pop_matchvars_bound);

        if pattlist == [] then
            if save_conv /== [] then swap_id_values(save_conv) endif;
            if want_instance then
                dl(conslist(stacklength()) ->> item);
                suspend(item, true, 2)
            else
                suspend(true, 1)
            endif;
            if save_conv /== [] then swap_id_values(save_conv) endif
        else
            dest(pattlist) -> (patt, pattlist);
            class_=(datakey(patt)) -> eq_p;
            for item in database do
                if eq_p(item, patt) then tryall(item, pattlist) -> endif;
                sys_restore_matchvars(save_conv, save_matchvars)
            endfor
        endif
    enddefine;

    tryall(pattlist);
    ksuspend(false, 1)
enddefine;

endsection;
