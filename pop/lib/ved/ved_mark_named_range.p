/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mark_named_range.p
 > Purpose:         Mark named ranges
 > Author:          John Gibson, Mar 13 1992 (see revisions)
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define ved_mark_named_range(name);
    lvars item, name;
    dlocal vedscreenbell = identfn, ved_on_status = false;
    if lmember(name, vedmarkclasses) ->> item then
        hd(tl(item))()
    else
        vederror('unknown mark name')
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  7 1993
        Moved vedmarkclasses to vedmarkclasses.p
 */
