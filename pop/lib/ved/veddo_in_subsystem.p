/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/veddo_in_subsystem.p
 > Purpose:         Do VEd command in a given subsystem
 > Author:          John Gibson, Jan 12 1993 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

include vedfile_struct.ph;
include subsystem.ph;

section;

define veddo_in_subsystem(ssname);
    lvars ssname;

    define lconstant Do_command(ssname, command);
        lvars ssname, command;
        dlocal veddocsubsystem, pop_default_type;

        define lconstant ss_save();
            if dup(ved_current_file) then
                dup()(VF_SUBSYSTEM)
            else
                subsystem
            endif
        enddefine;
        ;;;
        define updaterof ss_save(oldfile, oldss);
            lvars oldfile, oldss;
            if oldfile then oldss -> oldfile(VF_SUBSYSTEM) endif;
            if ved_current_file == oldfile then oldss -> subsystem endif
        enddefine;

        dlocal 2 % ss_save() %;

        ssname ->> subsystem -> veddocsubsystem;
        if ved_current_file then
            ssname -> ved_current_file(VF_SUBSYSTEM)
        endif;
        subscr_subsystem(SS_FILE_EXTN, ssname) -> pop_default_type;
        veddo(command);
    enddefine;

    if vedargument = nullstring then
        word_string(ssname) -> vedargument;
        ved_pop()
    endif;

    subscr_subsystem(SS_NAME, ssname, vederror) -> ;    ;;; check loaded

    if vedinvedprocess then
        ved_save_file_globals();
        Do_command(ssname, vedargument)
    else
        vedsetup();     ;;; in case not
compile_mode :vm -prmprt;
        false -> vedprocessproc;    ;;; ensure subsystem set from file on entry
        procedure;
            dlocal subsystem, vedprocessproc;
            Do_command(ssname, vedargument)
        endprocedure()
    endif;

enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 28 1995
        Changed to use veddo
 */
