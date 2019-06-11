/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Nov 13 1992
 */

library_declare_section '$popvedlib/'

section;

weak global constant procedure (
        isvedfileprop,
        newvedfileprop,
        ved_mark_named_range,
        ved_text_action_data,
        vedappendrange,
        vedargint,
        vedatend,
        vedatstart,
        vedbacklocate,
        vedcapword,
        vedcharnext,
        vedconvertline,
        vedconvertrange,
        vedconvertseln,
        vedconvertword,
        vedcurrentchartype,
        vedcutblock,
        veddo,
        veddo_in_subsystem,
        veddrawline,
        vedenderror,
        vedendwordright,
        vedfillblock,
        vedfindbracket,
        vedgenshell,
        vedhelpfor,
        vedimshell,
        vedinkeys,
        vedinput,
        vedlocate,
        vednextpara,
        vednextsent,
        vedobey,
        vedpipein,
        vedprevline,
        vedreadinteger,
        vedrefreshblock,
        vedremoteshell,
        vedstartwordleft,
        vedteststartsearch,
        vedtrysetvalof,
        vedwordcount_text,
        vedyankblock,
        $-ved$-find_new_indexline,
        $-ved$-is_new_indexline,
    );

weak global constant
        vedstatusheaderlen,
    ;

weak global vars procedure (
        edit,
        ved_cc,
        ved_cdiff,
        ved_chat,
        ved_cshfile,
        ved_cut,
        ved_do_text_action,
        ved_ds,
        ved_g,
        ved_gbl,
        ved_gel,
        ved_gobble,
        ved_helpfor,
        ved_hkeys,
        ved_jj,
        ved_killcsh,
        ved_killsh,
        ved_lp,
        ved_mail,
        ved_man,
        ved_mbl,
        ved_mcf,
        ved_mcm,
        ved_mel,
        ved_mp,
        ved_pop,
        ved_print,
        ved_prolog,
        ved_reply,
        ved_right,
        ved_rsh,
        ved_send,
        ved_sh,
        ved_shfile,
        ved_showlib,
        ved_splice,
        ved_src,
        ved_ss,
        ved_swl,
        ved_tact,
        ved_wappr,
        ved_wiggle,
        ved_wmp,
        vedgetsysfilepdr,
    );

weak global vars
        rubout,
        ved_cdiff_ignore,
        ved_g_string,
        ved_rsh_indentstep,
        vedmarkclasses,
        vedsrclist,
    ;

declare_incremental list (
        vedsrclist
    );

declare_incremental property (
        isvedfileprop = newproperty([], 16, false, false),
        vedgetsysfilepdr = newproperty([], 16, false, "perm"),
    );


endsection;

end_library_declare_section;
