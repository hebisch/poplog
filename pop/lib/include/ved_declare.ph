/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/include/ved_declare.ph
 > Purpose:         Weak declarations for VED identifiers used weakly
 >                  in libraries, etc.
 > Author:          John Gibson, Oct  9 1992 (see revisions)
 */

    /*  N.B. This file is included by the VED sources themselves to ensure
     *  consistency. It should contain only VED identifier declarations and
     *  nothing else.
     */

section;

weak global constant procedure (
        ved_do_??,
        veddocommand,
        vedexit,
        vedprocess,
        vedputcommand,
        vedsearchfiletypes,
        vedsetpop,
        vedsetup,
    );

weak global constant active (
        ved_current_file,
    );


weak global vars procedure (
        ved_pop,
        vederror,
        vedgetsysfilepdr,
        vedinitcomp,
        vedputmessage,
        vedscr_flush_output,
        vedscreenbell,
    );

weak global vars active (
        vedscreencharmode,
    );

weak global vars
        ved_??_list,
        ved_runtime_actions,
        vedargument,
        vedbackers,
        vedbufferlist,
        vedclosers,
        vedcommand,
        vedcurrent,
        veddoclist,
        veddocname,
        vedediting,
        vedfiletypes,
        vedforwarders,
        vedhelplist,
        vedhelpname,
        vedinvedprocess,
        vedlinemax,
        vedlmr_errs_in_file,
        vedlmr_print_in_file,
        vedmessage,
        vednonbreakfiles,
        vedopeners,
        vedreflist,
        vedrefname,
        vedscreenlength,
        vedsetupdone,
        vedteachlist,
        vedteachname,
        vedvedname,
        vedwindowlength,
;

declare_incremental list (
        ved_runtime_actions,
        vedbackers,
        vedclosers,
        veddoclist,
        vedforwarders,
        vedhelplist,
        vednonbreakfiles,
        vedopeners,
        vedreflist,
        vedteachlist,
    );

declare_incremental property (
        vedgetsysfilepdr = newproperty([], 16, false, "perm"),
    );

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jun  1 1995
        Added vedlinemax, vedscreenlength, and vedwindowlength.
--- John Gibson, Jun 16 1993
        Added vednonbreakfiles
--- John Williams, Jan 21 1993
        Renamed vedsetup_actions as ved_runtime_actions
--- John Williams, Jan  8 1993
        Added vedsetup_actions
 */
