/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedsetup.p
 > Purpose:         Code to startup and setup xved
 > Author:          Jonathan Meyer, Apr  7 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-xved => wved_change_usewindows_x, xvedsetup;

#_INCLUDE 'xved_declare.ph'
include xved_constants.ph;

XptLoadProcedures xvedsetup [^^XPW_EXLIBS]
lvars
    XpwSetWMProtocolActions(appcon) :void,
;

exload xvedsetup2
lconstant
    XpwUcToMb :exptr
endexload;

uses-by_name vedxvedinit, vedxvedkeys;
uses ($-xved$-xvedvedtraps);    ;;; forces inclusion for POPC


constant procedure xvedsetup;


;;; --- ERRORS -------------------------------------------------------

;;; this checks that the mishap will get seen somewhere
define :XVED_FOR wved_mishap_reset();
    lvars dev = pop_charerr_device;
    wved_raise_window(dev);
    if isveddevice(dev) then
        ;;; ensure ved keeps the output file current
        syswrite(dev, nullstring, 0);   ;;; forces file to be opened
        vedpresent(device_full_name(dev)) -> vedinputfocus
    endif
enddefine;

/* HANDLER FOR XIO ERRORS

When we receive an Xlib IO Error, we cannot continue to run XVed. However,
we can try to setup VED so that it will run correctly in a terminal (if
there is a terminal available) or call sysexit to write changed files
if there isn't a terminal available.

*/

lvars procedure current_xio_handler;    ;;; set when xvedidents is switched

define :XVED_FOR XIO_sys_error_handler(dpy);
    lvars dpy file, proc = vedprocessproc, appcon;
    dlocal pop_asts_enabled = false;
compile_mode :vm -prmprt -pentch;

    fast_XtDisplayToApplicationContext(dpy) -> appcon;

    if vedusewindows == "x" and appcon == xvedappcontext then
        ;;; XVed has been killed - try to restore some sanity to VED
        [] -> ved_char_in_stream;
        false -> XptAsyncAppContext(xvedappcontext);
        false -> xvedappcontext;
        false -> xveddisplay;
        false -> wvedwindow;
        false -> vedprocessproc;
        false -> vedediting;
        false -> vedinvedprocess;
        false -> ved_chario_file;
        false ->> vedupperfile -> vedlowerfile;

        ;;; reset the chario devices
        if isveddevice(pop_charin_device) then
            popdevin -> pop_charin_device;
        endif;
        if isveddevice(pop_charout_device) then
            popdevout -> pop_charout_device;
        endif;
        if isveddevice(pop_charerr_device) then
            popdeverr -> pop_charerr_device;
        endif;

        for file in vedbufferlist do
            ;;; clear windows associated with buffers
            false -> wved_window_of_file(ved_current_file);
        endfor;
        procedure;
            dlocal vedbufferlist = []; ;;; stops vedusewindows complaining
            false -> vedusewindows;
        endprocedure();
        sysprmessage(0, 'Server Connection Broken', 'XVed -', 5);
        sysflush(popdeverr);
        XptSetXtWakeup();
        if systrmdev(popdevin) /== true then
            ;;; must exit - no terminal to use for VED
            sysexit();
        endif;
    endif;

    ;;; Either it was NOT XVed that just died, or we are connected to a
    ;;; terminal. To be on the safe side, we check if the original
    ;;; vedprocessproc is the current pop process, and if it is we kill it.

    if proc and pop_current_process == proc then
        ;;; in vedprocessproc - kill proc first then chain handler
        ksuspend_chain(0, proc, current_xio_handler(%dpy%));
    else
        ;;; not in vedprocessproc - simply call exception handler
        fast_apply(dpy, current_xio_handler);
    endif;
enddefine;

;;; ------------------------------------------------------------------

define lconstant set_server_keys();
    lvars   serverkeys, servername = xved_server_vendor_string(), disp;

    define lconstant 1 contains(a,b);
        lvars a,b;
        (isstring(a) or isword(a)) and issubstring(b,a)
    enddefine;

    define lconstant islocal();
        isstartstring(':', disp) or
        isstartstring('local', disp) or
        disp contains '/unix:' or
        isstartstring(sys_host_name(), disp);
    enddefine;

    ;;; DISPLAY may not be set in VMS (DECW$DISPLAY doesn't
    ;;; give anything useful)
    systranslate('DISPLAY') or nullstring -> disp;

    ;;; most reliable - get keys based on server name
    if servername contains 'Network Computing Devices' then
        "vedncdxvedkeys"
    elseif servername contains 'Sun' then
        "vedsunxvedkeys"
    elseif servername contains 'DEC' then
        "veddxvedkeys"
    elseif servername contains 'HP' or servername contains 'Hewlett' then
        "vedhpxvedkeys"
    elseif servername contains 'Silicon Graphics' then
        "vedsgixvedkeys"
    else
        false
    endif -> serverkeys;

    ;;; A local connection - see what type of machine I am
    if not(serverkeys) and islocal() then
        sys_machine_type.hd -> servername;
        if servername contains 'sun' then
            "vedsunxvedkeys"
        elseif servername contains 'hp' then
            "vedhpxvedkeys"
        elseif servername contains 'vax' then
            "veddxvedkeys"
        else
            false
        endif -> serverkeys;
    endif;

    ;;; Very iffy - look for name in $DISPLAY environment var
    unless serverkeys then
        disp -> servername;
        if servername contains 'sun' then
            "vedsunxvedkeys"
        elseif servername contains 'bob' or servername contains 'hp' then
            "vedhpxvedkeys"
        elseif servername contains 'dec' then
            "veddxvedkeys"
        else
            false
        endif -> serverkeys;
    endunless;

    if isword(serverkeys) and isprocedure(valof(serverkeys) ->> serverkeys)
    then
        serverkeys -> vedserverxvedkeys
    endif;
enddefine;


define lconstant Startup_banner;
    lvars server = xved_server_vendor_string();
    dlocal pop_pr_quotes = false;
    ;;; prints out a startup message
    returnif(pop_nobanner
             or not(testdef syssetup and iscaller(weakref syssetup)));
    if popdevout then
        if pop_system_version /== popversion then
            printf(server, pop_system_version, xved, xvedversion, xvedtitle,
                    '%p (Version %p (%p), %p)\nX Server: %p\n');
        else
            printf(server, xved, xvedversion, xvedtitle,
                '%p (Version %p (%p) -- %p)\n');
        endif;
        sysflush(popdevout);
    endif;
enddefine;

define :XVED_FOR wved_setup_term_trap();
    unless vedterminalname == "xved" then
        false -> vedusewindows
    endunless
enddefine;

define :XVED_FOR vedinitterm();
    valof("vedxvedinit")();
    unless vedterminalname == "xved" then
        ;;; setup whole of Xved terminal type
        veduseterm('xved') -> ;
    endunless;
enddefine;

lvars
    save_termselect,
    idents_in = false;

define wved_change_usewindows_x(usewindows, usewindows_id);
    lvars usewindows, usewindows_id, pair, p, id;
compile_mode :vm -nofast;

    if usewindows and not(xvedappcontext) then xvedsetup() endif;

    if usewindows /== idents_in then
        unless idents_in then
            ;;; save this so our one can use it
            XIO_sys_error_handler -> current_xio_handler
        endunless;
        /* see xved_declare.ph for the define form that adds to this list */
        fast_for pair in xvedidents do
            fast_destpair(pair) -> (p, id);
            ;;; swap the values -- use fast_idval to avoid protection checks
            ;;; and cope with active variables
            fast_idval(id) -> fast_front(pair);
            p -> fast_idval(id)
        endfor;
        usewindows -> idents_in
    endif;

    ;;; set the internal Ved variable
    usewindows -> fast_idval(usewindows_id);        ;;; "x" or false

    if usewindows then
        ;;; not using windows -> using windows
        if systrmdev(popdevin) then
            ;;; run concurrently with basewindow
            procedure;
                if vedinvedprocess then
                    ;;; reduce stack frames in process by chaining
                    chain(0, vedprocessproc, suspend)
                else
                    syshibernate()
                endif
            endprocedure
        else
            ;;; else if no basewindow input just call syshibernate
            syshibernate
        endif -> xved_rawin_read_trap;

        (vedterminalselect, false) -> (save_termselect, vedterminalselect);
        if vedterminalname then
            syscancel(consword('ved' sys_>< vedterminalname sys_>< 'screen'));
            veduseterm("undef") -> ;
        endif;
        if vedsetupdone then vedinitterm() endif

    else
        ;;; using windows -> not using windows
        false -> wvedwindow;
        unless vedterminalselect then
            save_termselect -> vedterminalselect;
        endunless;
        if vedterminalname == "xved" then
            ;;; zap xved terminal settings
            veduseterm("undef") -> ;
            ;;; reinitialise
            if vedsetupdone then vedinitterm() endif
        endif;
    endif
enddefine;
;;;
uses-by_name (wved_change_usewindows_x);


lconstant fallback_resources = {
    'XVed*pointerShape: xterm'

    'XVed*color2Foreground: red'
    'XVed*color4Foreground: green4'
    'XVed*color6Foreground: blue'

    'XVed*color3Foreground: yellow'
    'XVed*color3Background: red'
    'XVed*color5Foreground: yellow'
    'XVed*color5Background: green4'
    'XVed*color7Foreground: white'
    'XVed*color7Background: blue3'

    'XVed*color2AForeground: brown'
    'XVed*color4AForeground: olivedrab4'
    'XVed*color6AForeground: slateblue3'

    ;;; backgrounds of these default to (main or status) color 0 foreground
    'XVed*color3AForeground: coral'
    'XVed*color5AForeground: green3'
    'XVed*color7AForeground: dodgerblue'
};

define xvedsetup();
    lvars resvec, appname;

    ;;; ved lives in its own application context/display connection.
    ;;; This gives us security in case someone else comes along and
    ;;; ruins their display connection with an IO error - in fact, it
    ;;; also means that if we ruin our display connection we don't hurt
    ;;; other people as well!
    returnif(xvedappcontext);

    if vedbufferlist /== [] then
        mishap(0,'XVed cannot be started whilst Ved is running');
    endif;

    unless popunderx then [] -> popunderx endunless;


    ;;; Create appcontext/display
    if xveduseownappcontext and xved /== "openlook" then
        ;;; can live in my own appcontext (hooray!)
        fast_XtToolkitInitialize();
        ;;; Poplog name for appcontext is always 'xved'
        fast_XptCreateApplicationContext(XV_APPLICATION_NAME) -> xvedappcontext;
        cons_fixed(#| appdata(fallback_resources, copy_fixed), external_NULL()
                   |#, vector_key) -> resvec;
        fast_XtAppSetFallbackResources(xvedappcontext, resvec);
    lblock compile_mode :vm -nofast;
        fast_XtOpenDisplay(
            xvedappcontext,
            false,                  ;;; Display name
            sys_fname_nam(poparg0), ;;; Application name same as default
            XV_CLASS_NAME,          ;;; Application class
            false,0,                ;;; Application startup options (none so far)
            XptSetArgv(popunderx)
        ) -> xveddisplay;
    endlblock;
        false -> resvec;
    else
        ;;; share default appcontext (boo!)
        XptDefaultSetup();      ;;; Toolkit initialize
        XptDefaultDisplay -> xveddisplay;
        XptDefaultAppContext -> xvedappcontext;
    endif;

    ;;; Dummy shell -- used as a place to hang popups and as selection owner.
    ;;; Needs to be mapped because Motif popups don't work otherwise, so
    ;;; position it way off the screen ....
    xved_set_args(#_< [
            ^XtN geometry           '1x1+-10000+-10000'
            ^XtN overrideRedirect   ^true       ;;; just to make sure
    ] >_#, true);
    fast_XtAppCreateShell(false, XV_CLASS_NAME, xtApplicationShellWidget,
                            xveddisplay, xved_arg_list()) -> xveddummyshell;
    fast_XtRealizeWidget(xveddummyshell);

    ;;; turn on async handling of events
    true -> XptAsyncAppContext(xvedappcontext);

    ;;; find keyboard type and set the vedserverxvedkeys variable
    set_server_keys();

    ;;; get application resources
    xved_get_app_resources();

    ;;; set protocol actions
    exacc raw_XpwSetWMProtocolActions(xvedappcontext);

    ;;; set Unicode to multibyte converter
    C_pop_uc_to_mb -> exacc XpwUcToMb;

    ;;; init WM actions
    xved_init_wm_actions();

    if sysxsetup == xvedsetup then
        ;;; set sysxsetup back to default once I've done my setup
        XptDefaultSetup -> sysxsetup;
    endif;

    Startup_banner(); ;;; may do nothing

    unless iscaller(wved_change_usewindows_x) then
        "x" -> vedusewindows
    endunless;
enddefine;

;;; Make it come at the end for POPC
declare_incremental procedure [prec=100] sysxsetup;

sysxsetup <> xvedsetup -> sysxsetup;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 23 1997
        Removed FontSet from fallback_resources
--- John Gibson, Apr 30 1997
        Font -> FontSet in fallback_resources
--- John Gibson, Apr 29 1997
        Made xvedsetup set extern variable XpwUcToMb to (pre-loaded)
        C function C_pop_uc_to_mb.
--- John Gibson, Nov 21 1996
        Made vedinitterm call vedxvedinit (instead of vedxved*mouse)
--- John Gibson, Mar  1 1996
        vedxved*mouse now called via its name (like vedxvedkeys etc)
--- John Gibson, Nov 14 1995
        Added new default colour resources
--- John Gibson, Sep 23 1995
        Changed default color4Foreground to green4
--- John Gibson, Jul 25 1994
        Removed 'XpwScrollText.' specificity in fallback_resources
        colours.
--- John Gibson, Dec 17 1993
        Made xvedsetup map xveddummyshell, but position it right off the
        screen -- needs to be mapped to make Motif popups work.
--- John Gibson, Nov 15 1993
        Changed xvedsetup for Motif to set the xveddisplay application name
        from poparg0 (i.e. same as for XptDefaultDisplay).
--- John Gibson, Oct  1 1993
        Set pop_pr_quotes locally false inside Startup_banner.
--- John Gibson, Jun 12 1993
        Replaced old vars wved_ch*ange_ved_mode with new
        wved_change_usewindows_x called by "x" -> vedusewindows etc.
--- John Gibson, Jun 10 1993
        Moved error-handling stuff to here from xveder*rors.p and got rid of
        latter.
--- John Gibson, Jun  7 1993
        Moved xved_standalone_setup to ../lib
--- John Gibson, Jan 18 1993
        Simplified xved_standalone_setup (can now redefine pop_setpop_compiler
        without affecting immediate mode in VED).
--- John Gibson, Nov  4 1992
        Fixed set_server_keys for VMS (DISPLAY may not be defined)
--- John Gibson, Aug 26 1992
        Removed unnecessary call of xved_dialogs_init (is done by
        if necessary when getting application resources)
--- Adrian Howard, Aug 17 1992
        Fixed bug in -islocal-
--- John Gibson, Jul 24 1992
        Doesn't call xved_dialogs_init if xvedvanilla is false
--- John Gibson, Jan 16 1992
        In -xved_standalone_setup-, made -main_loop- be run through
        -sysCOMPILE- so that VM is initialised properly.
--- John Gibson, Dec 13 1991
        Fixed problem with use of pop_setpop_compiler in xved_standalone_setup
--- John Gibson, Dec  5 1991
        Rewrote -xved_main_loop- and made it an lconstant
--- John Gibson, Dec  4 1991
        Made xvedsetup only call XptDefaultSetup if not using
        own appcontext; also removed call of sysxsetup from
        xved_standalone_setup
--- Robert John Duncan, Dec  2 1991
        Added case for Silicon Graphics server key bindings
--- John Gibson, Nov  4 1991
        Changed xved_standalone_setup to turn -geometry into a text widget
        resource
--- Simon Nichols, Oct 22 1991
        Made -vedinitterm- call -vedxved*mouse- unconditionally to cure a
        problem where a call of ``veduseterm("xved")'' leaves mouse bindings
        undefined. See bugreport jamesg.28.
--- Simon Nichols, Oct 22 1991
        Changed -xved_standalone_setup- to call -ved_ved- rather than
        -ved*editor- when -arg- is a file. This is to ensure that
        -vedsearchlist- is searched when xved is invoked from the shell.
        See bugreport jamesg.25.
--- John Gibson, Sep 20 1991
        Changed wved_change_usewindows_x so that it redefines vedinitterm
        instead of calling veduseterm
--- Jonathan Meyer, Sep 12 1991 Removed references to $TERM.
        Changed assignment to sysxsetup.
--- Jonathan Meyer, Aug 30 1991 Added xved_standalone_setup
--- Robert John Duncan, Aug 23 1991
        Discarded result from -veduseterm-
--- Jonathan Meyer, Aug 19 1991
        Removed XpolDefaultSetup - no longer needed
--- Jonathan Meyer, Aug  2 1991 Now uses xved_set_args and xved_arg_list
--- Jonathan Meyer, Jul 27 1991
        Added xveduseownappcontext, made Startup_banner do nothing if
        not called by syssetup, xvedsetup now assigns XptDefaultSetup
        or XpolDefaultSetup to sysxsetup once its done. Startup_banner
        called at end of xvedsetup (once setup is done).
--- Adrian Howard, Jul 25 1991 : Removed references to UserLevel & novice
        users
--- Adrian Howard, Jul  9 1991 : Made TEACH *XVED come up in the default font
        - too many complaints about inconsistancy
--- Jonathan Meyer, Jul  8 1991
        Removed xvedsetup assignment to sysxsetup
--- Jonathan Meyer, Jul  1 1991
        Made veduseterm("xved") get called by setup_term_trap
--- Jonathan Meyer, Jun 19 1991
        Reordered procs in xvedsetup - made it simpler
--- Jonathan Meyer, Jun 18 1991
        Added "novice" user type
--- Jonathan Meyer, Jun 17 1991
        Corrected assignment to sysxsetup so that it is only changed if it
        is the default.
--- Jonathan Meyer, Jun 17 1991
        Added xvedidents code, renamed vedinitx.p xvedinit.p, and only
        compiled it if vedusewindows is "x".
        Changed set_server_keys to use if/endif rather than switchon.
        Changed Startup_banner to check for pop_nobanner.
--- John Gibson, Jun 15 1991
        Removed wvedbase/textwindow
--- John Gibson, Jun 12 1991
        Replaced wved_setup_ved and wved_restore_ved with single procedure
        -wved_change_usewindows_x- testing vedusewindows.
        Added assignment to xved_rawin_read_trap.
--- John Gibson, Jun  4 1991
        Removed  false-> XptApplyCallback (no longer necessary)
--- Jonathan Meyer, Jun  3 1991
        Added false-> XptApplyCallback
--- Jonathan Meyer, May 30 1991
        Fixed vedusewindows to be set correctly.
--- Jonathan Meyer, May 30 1991
        Added vedwarpcontext code. Removed reference to xvedhome.
--- Jonathan Meyer, Apr  8 1991
        Added xvedwarpmouse code.
 */
