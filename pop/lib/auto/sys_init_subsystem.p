/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_init_subsystem.p
 > Purpose:         Initialise subsystem
 > Author:          John Gibson, Jan 11 1993 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

include subsystem.ph;

section;

weak vars procedure sysxsetup;

define sys_init_subsystem(ssname);
    lvars ssname;

    define lconstant ss_apply(/*pfield*/);
        lvars p = subscr_subsystem((), ssname);
        unless isundef(p) then chain(p) endunless
    enddefine;

    ss_apply(SS_SETUP);
    unless pop_noinit then ss_apply(SS_INITCOMP) endunless;
    if popunderx then
        if testdef sysxsetup then weakref sysxsetup() endif;
        ss_apply(SS_XSETUP);
    endif;
    ss_apply(SS_VEDSETUP);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 22 1994
        Added declaration for sysxsetup
 */
