/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/poplog_main.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;;-------------  SETTING UP THE NORMAL POPLOG SYSTEM ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE '../lib/include/subsystem.ph'

global constant
        procedure (strnumber, sys_fname_path, sys_fname_nam, sys_parse_string,
                    syssetup),
        poptitle,
    ;

global vars
        poparglist0, poparg0, poparglist, popversion, popheader
    ;

section $-Sys;

constant
        _image_ident
        procedure (Init_path_dir_list),
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys;

    /*  This procedure gets run after setting up poparglist0 etc, but
        before locking the pre-heap and trying to restore saved image args.
        It returns the search directory list and default extension to be
        used for the latter.
    */

define $-Pop$-Pre_Init_Restore();
    lvars trans, arg0, dev;

    '\n' <> poptitle <> popversion -> popheader;

#_IF DEF UNIX

    ;;; check to see if was invoked by login (arg0 has - prepended)
    ;;; compile login program if so (to set up env list)
    poparg0 -> arg0;
    if datalength(arg0) fi_> 0 and fast_subscrs(1,arg0) == `-` then
        allbutfirst(1, arg0) ->> arg0 -> fast_front(poparglist0);
        if (readable('$HOME/.login.p') ->> dev) then
            pop11_compile(dev)
        else
            mishap(0, 'CAN\'T FIND LOGIN INIT FILE: $HOME/.login.p')
        endif
    endif;

    ;;; get arg0, the name under which the image was invoked
    ;;; and see if there's a env var translation with 'pop_' prepended
    if systranslate('pop_' <> sys_fname_nam(arg0)) ->> trans then
        ;;; arg0 translated
        ;;; separate into args (unless begins with :),
        ;;; and make them arg1, arg2, etc
        [%  if datalength(trans) /== 0 and fast_subscrs(1,trans) == `:` then
                trans
            else
                sys_parse_string(trans)
            endif
        %] nc_<> poparglist ->> fast_back(poparglist0) -> poparglist;
        ;;; replace original arg0 with 'basepop11'
        sys_fname_path(arg0) dir_>< 'basepop11' -> fast_front(poparglist0)
    endif;

#_ENDIF

    ;;; return directory list and default extension for trying
    ;;; to restore initial saved images
    Init_path_dir_list('popsavepath'), '.psv'
enddefine;


;;; ----------------------------------------------------------------------

uses ($-pop_setpop_compiler);   ;;; so setpops will reset to a compiler

    /*  Poplog_main is the entry procedure for the normal POPLOG system
        (as specified by -e option to poplink command in "pgcomp").
        It is called after failing to restore saved images.
    */
define Poplog_Main();

    ;;; No saved images restored, so if this is not the normal Poplog
    ;;; image, then just return
    returnunless(_zero(_image_ident!(w)));

    ;;; (interrupt is sysexit at this point)
    unless subsystem then "pop11" -> subsystem endunless;
    syssetup();     ;;; standard subsystem startup
    interrupt();    ;;; sysexit unless redefined
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 20 1998
        Changed _image_ident to be a pointer to a word containing the value,
        rather than the value itself (AIX doesn't support the latter).
--- Robert Duncan, Mar  4 1998
        Changed CPU check dates to 2002.
--- John Gibson, Apr 10 1997
        Set device_encoding false in Cpu_Id_Check
--- Robert Duncan, Nov  6 1996
        Fixed Cpu_Id_Check for Win32 (different date format)
--- John Gibson, Dec  4 1995
        Uses lmember_= instead of mem*ber
--- Robert John Duncan, Aug 11 1995
        Changed remainder value in encrypt to 89 for Version 15
--- John Gibson, Jul 29 1995
        Made Poplog_Main assign "pop11" to subsystem if false
--- Robert John Duncan, May 23 1995
        Restricted CPU checking to just the normal system, i.e. where
        _image_ident is 0.
--- Robert John Duncan, May  2 1995
        Fixed the CPU check to recognise when the current year is beyond
        the stop year (!). Reorganised so that the host id (and type) is
        displayed with the password prompt to save users having to type
        obscure shell commands to get hold of it.
--- John Gibson, Jan 28 1995
        Changed remainder value in encrypt to 97 for normal system.
--- John Gibson, Dec 12 1994
        Changed CPU check dates to 1998.
--- John Gibson, Jun 17 1993
        Moved pop_setpop_compiler to subsystem_compile.p
--- John Gibson, Jan 23 1993
        Made Poplog_Main call interrupt after syssetup (instead of setpop).
--- John Gibson, Jan 11 1993
        o syssetup now moved to syssetup.p
        o pop_setpop_compiler now subsystem-sensitive
--- John Gibson, Oct 20 1992
        Uses sys_parse_string
--- John Gibson, Sep 30 1991
        Changed replacement for arg0 after pop_X translation to 'basepop11'
        instead of 'pop11'
--- Simon Nichols, Aug 23 1991
        In cpu checking code, changed mishap message to specify popsys
        rather than usepop if popsys is undefined.
--- John Gibson, May 23 1991
        Removed call of sys_process_special_popargs
--- John Williams, Jan 11 1991
        -syssetup- now only assigns -setpop- to -interrupt- if
        -interrupt- has not been changed by the user.
--- John Gibson, Dec 15 1990
        Made -ask_user- print POPLOG Version, etc.
--- John Gibson, Dec  1 1990
        Added cpu checking code
--- John Gibson, Oct 27 1990
        Renamed =setpop_compile- directly as -pop_setpop_compiler-
--- John Gibson, Oct 26 1990
        Replaced -popdevin- with -pop_charin_device-
--- John Williams, Oct  4 1990
        Added -syssetup-
--- Simon Nichols, Oct  2 1990
        Changed -Poplog_Main- to make the call of -pop11_xsetup-
        independent of the value of -pop_noinit-.
--- Simon Nichols, Oct  1 1990
        Renamed -sys_xstartup- to be -pop11_xsetup-.
--- Simon Nichols, Sep  7 1990
        Changed -Poplog_Main- to take account of special arguments.
--- John Williams, Jul  2 1990
        -setpop_compile- now calls -popsetpop-
--- Ian Rogers, Jan 17 1990
        Added $Sys$-poptitle
--- John Gibson, Aug 15 1989
        Changed test for terminal input in -Poplog_Main- to use
        -On_line_tty_input-, and -setpop- now always assigned to
        -interrupt-, regardless of whether this is true.
--- John Gibson, Aug 10 1989
        Code for dealing with initial arguments reorganised and rewritten.
        Restoring of saved images now done in new file init_restore.p
--- John Gibson, Aug  2 1989
        Unix version now uses sys_fname... procedures.
--- John Gibson, May  5 1989
        Sys$-Poplog_Main is now the entry procedure for the full Poplog
        system (specified by -e option to poplink command in "pgcomp").
        Put file into section Sys.
--- Roger Evans, Mar 20 1989
        Moved poparg0 initialisation to setpop.p
--- Roger Evans, Mar 16 1989
        Added poparg0
--- John Gibson, Jun 24 1988
        Replaced -Poplog_no_init_restore- with _image_ident value (set
        up by poplink)
--- John Gibson, Jun 20 1988
        Changed -Try_init_command- so that first arg starting with `:`
        indicates POP-11 code to compile in VMS as well as Unix, rather
        than `"`.
--- John Gibson, Mar  9 1988
        Now poplog_main.p (previously sysinit.p)
 */
