/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/auto/declare_updater.p
 > Purpose:        Declare (perm) procedures as having updaters for POPC
 > Author:         John Gibson, Mar 23 1993 (see revisions)
 > Documentation:
 */
compile_mode :pop11 +strict;

section;

define syntax declare_updater;
    lvars item;
    until (readitem() ->> item) == ";" or item == termin do
        nextif(item == ",");
        sys_read_path(item, false, false) -> item;
        if pop_pas_mode == "popc" then
            valof("popc_declare_updater")(item)
        else
            ;;; in the normal system, just check updater can be applied to it
            updater(sys_current_val(item)) ->
        endif
    enduntil;
    ";" :: proglist -> proglist
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        Test for Popc now pop_pas_mode == "popc"
 */
