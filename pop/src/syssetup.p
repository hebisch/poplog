/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syssetup.p
 > Purpose:
 > Author:          John Gibson, Dec 23 1992 (see revisions)
 > Documentation:   REF *SYSTEM, *SUBSYSTEM
 */

;;; ----------------- STANDARD SUBSYSTEM SETUP ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE '../lib/include/subsystem.ph'

constant
        procedure (readstringline, Sys$-Subsystem_find, Sys$-Get_ss_procedure,
        Sys$-Apply_ss_procedure),
        popcopyright,
    ;

vars
        poparglist, popheader, pop_nobanner, pop_noinit, pop_first_setpop,
        pop_default_type
    ;

weak constant
        procedure (vedputcommand)
    ;

weak vars
        procedure (sysxsetup),
        vedargument
    ;


;;; ----------------------------------------------------------------------

section $-Sys => popvedcommand, sys_subsystems_init, syssetup;

define popvedcommand(ved_name);
    lvars ved_name, name, item, argument;
    allbutfirst(4, ved_name) -> name;

    define lconstant Strings_only(list);
        lvars list, item;
        repeat
            returnunless(ispair(list)) (false);
            fast_destpair(list) -> (item, list);
            returnunless(isstring(item)) (false);
            returnif(list == []) (true)
        endrepeat
    enddefine;

    if Strings_only(proglist) then
        consstring
            (#| for item in proglist do
                    explode(item), `\s`
                endfor;
                ->;     /* last space */
            |#)
    else
        readstringline()
    endif -> argument;
    ";" :: proglist -> proglist;
    unless VED_LOADED then
        mishap(name, 1, 'VED COMMAND NOT LOADED');
    endunless;
    ;;; prepare command line for VED
    VED_WEAK vedputcommand(name sys_>< ('\s' <> argument));
    argument -> VED_WEAK vedargument;
    valof(ved_name)();
enddefine;


define sys_subsystems_init(pfield);
    lvars ss, pfield;
    Check_integer(pfield, 16:100);
    if pfield == SS_BANNER then
        if popheader then
            printf(popheader), cucharout(`\n`);
            printf(popcopyright), cucharout(`\n`);
            false -> popheader;
        endif
    elseif pfield == SS_XSETUP then
        if testdef sysxsetup then weakref sysxsetup() endif
    endif;

    fast_for ss in sys_subsystem_table do
        Apply_ss_procedure(ss, pfield)
    endfor
enddefine;


define syssetup();
    lvars ss;

    returnif(sys_subsystem_table == []);

    true -> pop_first_setpop;

    /* Run SETUP procedures */

    sys_subsystems_init(SS_SETUP);

    /* Run INITCOMP, XSETUP and POPARG1 procedures */

    define lconstant Initcomp();
        if interrupt == sysexit then
            setpop -> interrupt;
        endif;
        if not(pop_nobanner) and On_line_term(pop_charin_device) then
            sys_subsystems_init(SS_BANNER)
        endif;
        unless pop_noinit then
            sys_subsystems_init(SS_INITCOMP)
        endunless;
        if popunderx then
            sys_subsystems_init(SS_XSETUP)
        endif;
    enddefine;

    /* Start up in VED */

    lconstant VEDCOMMS = ['doc' 'help' 'im' 'ref' 'teach' 'ved'
                          'src' 'showlib' 'showinclude' 'man'];

    define lconstant Vedcommand(name, args);
        lvars   name, args, ss;
        dlocal  proglist_state, pop_default_type;
        proglist_new_state(args) -> proglist_state;
        if Subsystem_find(subsystem, SS_NAME) ->> ss then
            ;;; for showlib etc.
            ss(SS_FILE_EXTN) -> pop_default_type;
        endif;
        popvedcommand(consword('ved_' <> name));
    enddefine;

    if poparglist == [] then
        Initcomp();     ;;; call not chain so iscaller(syssetup) remains true
    elseif hd(poparglist) = 'mkimage' then
        ;;; make the "mkimage" library available to all subsystems
        tl(poparglist) -> poparglist;
        chain('$popliblib/mkimage.p', pop11_compile);
    elseif lmember_=(hd(poparglist), VEDCOMMS) then
        Initcomp();
        chain(dest(poparglist), Vedcommand);
    elseif Subsystem_find(subsystem, SS_NAME) ->> ss then
        chain(ss, SS_POPARG1, Apply_ss_procedure);
    endif;
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  4 1995
        Uses lmember_= instead of mem*ber
--- John Williams, Sep 11 1995
        Added 'src', 'showlib', 'showinclude', & 'man' to the list of
        Ved commands recognised by syssetup (cf. isl-fr.4577).
--- Robert John Duncan, Jul 12 1995
        Changed sys_subsystems_init to print the copyright message.
--- John Williams, Apr  8 1994
        popvedcommand now copes when proglist is a list of strings (e.g.
        a tail of poparglist).
--- John Gibson, Jul 14 1993
        Changed syssetup to call Initcomp rather than chain it.
 */
