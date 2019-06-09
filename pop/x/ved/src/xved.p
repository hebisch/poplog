/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ved/src/xved.p
 > Purpose:         XVed Loader
 > Author:          Jonathan Meyer, Dec 18 1990 (see revisions)
 > Documentation:   HELP *XVED
 > Related Files:   LIB *XVED
 */
compile_mode :pop11 +strict;

section;

constant
    xvedtitle   = 'Sussex XVed',
    xvedversion = '4.0',
;

sysunprotect("xved");
vars xved;


#_IF not(DEF POPC_COMPILING)

;;; --- INTERACTIVE LOADING --------------------------------------------

#_IF not(DEF XLINK_TYPE)
mishap(0,'Cannot load XVed: Poplog not linked with X (re-run pglink?)');
#_ENDIF

#_IF vedinvedprocess
mishap(0,'Cannot load XVed: currently running Ved');
#_ENDIF

lvars srcdirs, load_options, flavour = "vanilla";

include xveddefs.ph;

#_IF DEF xved_srcdirs and islist(valof("xved_srcdirs"))
    xved_srcdirs
#_ELSE
    [^XVEDSRC]
#_ENDIF -> srcdirs;

/* Determine load options */
#_IF DEF XVED_LOAD_OPTIONS and islist(valof("XVED_LOAD_OPTIONS"))
    XVED_LOAD_OPTIONS
#_ELSE
    []
#_ENDIF -> load_options;

#_IF lmember("vanilla", load_options)
    [] -> load_options;
#_ELSEIF DEF popxlink_motif or DEF popxlink_openlook
    if load_options = [openlook] or load_options = [motif]
    or load_options == [] then
        [scrollbar menubar dialogs] -> load_options
    endif;
    consword(XLINK_TYPE) -> flavour;
#_ELSEIF load_options /== []
    mishap('Cannot load non-vanilla XVed: Poplog not linked with Motif/OpenLook');
#_ENDIF

lconstant sources = [%

;;; Test if we need the core XVed:
#_IF not(DEF xvedsetup)
        'xveddeclare.p',            ;;; declare vars/constants and xvedwin struct
        'xvedgeneral.p',            ;;; some general procedures
        'xvedarglist.p',            ;;; arglist caching
        'xvedkeysymseq.p',          ;;; keysym to escape sequence mapping
        'xvedscreen.p',             ;;; screen output interface
        'xvedinput.p',              ;;; adds events/string to to veds input queue
        'xvedhandlers.p',           ;;; hanled events on ved input queue
        'xvedclipboard.p',          ;;; icc clipboard
        'xvedselections.p',         ;;; Mangaging selection highlight
        'xvedbutton.p',             ;;; Mouse motion/drag events
        'xvedkey.p',                ;;; KeyPress events
        'xvedbitmaps.p',            ;;; ved icon bitmaps
        'xvedwm.p',                 ;;; window manager interface
        'xvedvedtraps.p',           ;;; define wved trap procedures
        'xvedwindows.p',            ;;; ved windows
        'xvedresources.p',          ;;; resource manager
        'xvedsetup.p',              ;;; setup code
#_ENDIF

#_IF lmember("scrollbar", load_options)
        'xvedscrollbar.p',          ;;; scrollbar creation
#_ENDIF

#_IF lmember("menubar", load_options)
        'xvedmenubar.p',            ;;; menubar/popup menu creation
#_ENDIF

#_IF lmember("dialogs", load_options)
        'xveddialogs.p',            ;;; dialog boxes
#_ENDIF

%];


printf(flavour, xvedversion, xvedtitle, ';;; Loading %p (Version %p (%p))\n');
printf('\n;;; Source directories: ');
applist(srcdirs, spr);
cucharout(`\n`);

;;; Start time of compilation
lvars start_time = systime();

lvars warnings = false, dir, file, dev;
dlocal popmemlim = false, current_directory, pop_message_min_detail = 4;

define dlocal pop_exception_final(_, _, _, sev);
    if sev == `W` then true -> warnings endif;
    false
enddefine;

exload_batch
    for file in sources do
        false -> dev;
        for dir in srcdirs do
            quitif(readable(dir dir_>< file) ->> dev);
        endfor;
        unless dev then
            mishap(file, 1, 'XVed SOURCE FILE NOT FOUND')
        endunless;
        dir -> current_directory;
        loadwarning(device_open_name(dev)); sysflush(pop_charout_device);
        subsystem_compile(dev, false);
        sysflush(pop_charerr_device)
    endfor;

    uses vedxvedinit,           ;;; XVed default resource/mouse bindings
         vedxvedkeys;           ;;; XVed default key bindings
endexload_batch;

if warnings and not(pop_debugging) then
    mishap(0, 'UNDECLARED IDENTIFIER(S) IN XVed SOURCES');
endif;

repeat 2 times sysgarbage() endrepeat;

;;; Total time taken
printf((systime()-start_time)/100.0,
    '\n;;; Loaded XVed, Time = %p seconds\n');
sysflush(popdevout);


;;; --------------------------------------------------------------------

#_ENDIF     ;;; not(DEF POPC_COMPILING)

define lconstant set_flavour();
    if testdef xvedscrollbar or testdef xvedmenubar or testdef xveddialogs then
        consword(XLINK_TYPE)
    else
        "vanilla"
    endif -> xved
enddefine;

#_IF DEF POPC_COMPILING
    uses (xvedsetup);
    sys_runtime_apply(set_flavour);
    "undef" -> xved;
#_ELSE
    set_flavour();
#_ENDIF

sysprotect("xved");

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 21 1996
        xvedversion = 4.0
--- John Gibson, Mar  1 1996
        vedxvedkeys and vedxved*mouse now libraries in x/ved/lib
--- John Gibson, May  6 1994
        xvedversion -> 3.0
--- John Gibson, Jun  3 1993
        Totally rewritten
--- John Gibson, May 25 1993
        vedxvedkeys.p, vedxved*mouse.p and vedxvedmenubar.p moved into src
        directory.
--- John Gibson, Sep  9 1992
        Removed loading of typespec libraries
--- Adrian Howard, Jul 20 1992
        Minor changes from Julian installed.
--- John Gibson, Mar 13 1992
        Put vedxved*mouse in "core" category. Removed test for name
        beginning with "+" in -usesfile- (no longer used)
--- Simon Nichols, Dec  3 1991
        Fixed to load OK on top of Poplog UI.
--- John Gibson, Nov 21 1991
        VMS correction
--- John Gibson, Nov 16 1991
        Removed xvedrawout.p -- now combined with xvedscreen.p
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Added check that if -pop_ui_propertytool- is defined then add
    "dialogs" to the list of options.
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Jonathan Meyer, Sep  2 1991 Commented out dlocal of poplib/useslist
--- Jonathan Meyer, Sep  1 1991 Added xpt_clientcallback
--- Jonathan Meyer, Aug 30 1991 uses xpt_cursorplane
--- Jonathan Meyer, Aug  2 1991 Rewrote the loader.
--- Jonathan Meyer, Jul 31 1991
        Removed xvedresize.p (code now in xvedvedtraps/xvedwindows)
--- Jonathan Meyer, Jul 31 1991
        Added xvedscreen, removed LIB VEDXVEDSCREEN.
--- Jonathan Meyer, Jul 27 1991
        Rename of xvedrefresh.p to xvedresize.p (no longer does refresh)
--- Jonathan Meyer, Jul 26 1991
        Added vedxvedmenu
--- Jonathan Meyer, Jul  8 1991
        xved section now always $-xved
--- Jonathan Meyer, Jul  8 1991
        Moved in xved_x_setup assignment to sysxsetup
--- Jonathan Meyer, Jul  7 1991
        Added test for "core" in XVED_LOAD_OPTIONS
--- Jonathan Meyer, Jul  1 1991
        Added xveddialogs.p
--- Jonathan Meyer, Jun 19 1991
        Added vedxved*mouse
--- Jonathan Meyer, Jun 17 1991
        XVED_LOAD_OPTIONS, and made use of USESCROLLBAR and USEMENUBAR.
--- John Gibson, Jun  8 1991
        Commented out 'uses wved'
--- Jonathan Meyer, Jun  3 1991
        Added xt_resource and xvedresources.p
--- Jonathan Meyer, Jun  1 1991
        Moved everything from io and events subdirectories into main
        -src- directory, and modified loader files respectively.
--- Jonathan Meyer, May 30 1991
        Removed references to xvedhome
 */
