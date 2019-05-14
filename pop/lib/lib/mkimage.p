/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/mkimage.p
 > Purpose:         Simplifies the making of saved images
 > Author:          Robert John Duncan, May 17 1991 (see revisions)
 > Documentation:   HELP * MKIMAGE
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
 *  This library should be run only as part of a shell/DCL command
 *  with syntax:
 *
 *      pop11 %nort mkimage [<options>] <image-name> [<files>] [:<startup>]
 *
 */


section;

true -> pop_record_writeable;

include subsystem.ph;

#_IF not(DEF mkimage_flags_table)
define global constant procedure mkimage_flags_table =
    newproperty([], 16, [], "perm");
enddefine;
#_ENDIF

;;; copy a command-line argument to preserve it through -sys_lock_system-
define lconstant copyarg(arg);
    lvars arg;
    if islist(arg) then maplist(arg, copyarg) else copy(arg) endif;
enddefine;

define lconstant loadfile(file);
    lvars   ss, file;
    dlocal  pop_default_type, pop_noinit = true, popunderx = false;
    if sys_fname_path(file) = nullstring then
        libwarning(file);
        ;;; try as library name for each subsystem in turn
        for ss in rev(sys_subsystem_table) do
            ss(SS_FILE_EXTN) -> pop_default_type;
            returnif(subsystem_libcompile(file, popuseslist));
        endfor;
        ;;; not found: try straight compile
    else
        loadwarning(file);
    endif;
    ;;; use last-loaded subsytem as default
    last(sys_subsystem_table)(SS_FILE_EXTN) -> pop_default_type;
    if sys_fname_extn(file) = nullstring then
        file <> pop_default_type -> file;
    endif;
    loadcompiler(file);
enddefine;

define lconstant install_image(image, flag);
    lvars image, flag;
#_IF DEF sys_install_image
    sys_install_image(image, flag);
#_ENDIF
enddefine;

define lconstant save_image(image, share, nonwrit, ss, startup, entry_p)
                                                    with_props false;
    lconstant vedwords = ['ved' 'help' 'ref' 'teach' 'doc' 'im' 'mkimage'];
    lvars   image, share, nonwrit, ss, startup, entry_p, flags,
            id = lowertoupper(sys_fname_nam(image));

    ;;; deinstall any existing version of the image
    if share == 1 then install_image(image, false) endif;

    ;;; flags for sys_lock_system
    if share then 1 else 0 endif -> flags;

    ;;; The '-nonwriteable' option makes both closures and datastructures
    ;;; nonwriteable. If the option is not specified, both closures and
    ;;; datastructures are made writeable.
    flags || (nonwrit and 2:010 or 2:100) -> flags;

    if sys_lock_system(image, flags, id) then
        ;;; restored
        ss -> subsystem;
        sysexit -> interrupt;
        max(100000, popmemlim) -> popmemlim;
        if startup == "ved" then
            unless poparglist == [] or member(hd(poparglist), vedwords) then
                ['ved' ^^poparglist] -> poparglist;
            endunless;
        elseif startup /== [] then
            [^^startup ^^poparglist] -> poparglist;
        endif;
        if entry_p then
            ;;; just chain to entry procedure (and do nothing else ...)
            chain(entry_p)
        else
            syssetup();     ;;; standard subsystem startup
            interrupt();    ;;; sysexit unless redefined
        endif
    else
        ;;; saved
        if share == 1 then install_image(image, true) endif;
        sysexit();
    endif;
enddefine;

define lconstant mkimage(args);
    lvars   arg, args, image, ss = false, initialisers = [], share = undef,
            nonwrit = false, vedmode = false, entry_p = false;
    dlocal  pop_debugging = "undef";

    ;;; process any leading options (prefixed with '-')
    while args /== [] and isstartstring('-', hd(args)) do
        dest(args) -> (arg, args);
        if arg = '-flags' then
            lvars key, flags;
            if args == [] or tl(args) == [] then
                mishap(0, 'MISSING FLAGS');
            endif;
            dest(args) -> (key, args);
            dest(args) -> (flags, args);
            consword(key) -> key;
            [%  dl(mkimage_flags_table(key));
                if islist(flags) then dl(flags) else flags endif;
            %] -> mkimage_flags_table(key);
        elseif arg = '-subsystem'
        or arg = '-main' ;;; undocumented; for compatibility with MKSSIMAGE
        then
            if args == [] then
                mishap(0, 'MISSING SUBSYSTEM NAME');
            endif;
            dest(args) -> (ss, args);
            consword(ss) -> ss;
        elseif arg = '-init' then
            lvars key, expr;
            if args == [] or tl(args) == [] then
                mishap(0, 'MISSING SUBSYSTEM INITIALISATION');
            endif;
            dest(args) -> (key, args);
            dest(args) -> (expr, args);
            consword(key) -> key;
            [^^initialisers [^key ^expr]] -> initialisers;
        elseif arg = '-noshare' then
            false -> share;
        elseif arg = '-share' then
            if share /== 1 then true -> share endif;
        elseif arg = '-nonwriteable' then
            true -> nonwrit
        elseif arg = '-install' then
            1 -> share;
        elseif arg = '-ved' then
            true -> vedmode;
        elseif arg = '-nodebug' then
            false -> pop_debugging;
        elseif arg = '-debug' then
            true -> pop_debugging;
        elseif arg = '-entrymain' or arg = '-entry' then
            if arg = '-entrymain' then
                '$-Pop$-Main'
            else
                if args == [] then
                    mishap(0, 'MISSING ENTRY PROCEDURE NAME');
                endif;
                dest(args) -> args
            endif -> entry_p;
        else
            mishap(arg, 1, 'ILLEGAL OPTION');
        endif;
    endwhile;

    ;;; next arg must be the image name
    if args == [] or isstartstring(':', hd(args)) then
        mishap(0, 'MISSING IMAGE NAME');
    endif;
    dest(args) -> (image, args);
    unless sys_fname_extn(image) = '.psv' then
        image <> '.psv' -> image;
    endunless;
    printf(image, ';;; MAKING IMAGE %p\n');

    ;;; next set of args are files to load
    until args == [] or isstartstring(':', hd(args)) do
        dest(args) -> (arg, args);
        loadfile(arg);
    enduntil;

    ;;; any remaining args are to be passed on to the image when it's
    ;;; restored; if there are none, but '-ved' was given, start up in
    ;;; VED
    if args == [] and vedmode then "ved" -> args endif;

    ;;; get entry procedure if supplied
    if entry_p then
        pop11_compile(stringin(entry_p)) -> entry_p
    endif;

    ;;; do any initialisations
    lvars init;
    for init in initialisers do
        subscr_subsystem(SS_COMPILER, init(1))(stringin(init(2)));
    endfor;

    ;;; set startup subsystem name
    unless ss then
        ;;; use first relevant entry in sys_subsystem_table
        lvars s;
        for s in sys_subsystem_table do
            s(SS_NAME) -> ss;
            quitif(ss /== "pop11" and is_subsystem_loaded(ss));
            false -> ss
        endfor;
        unless ss then "pop11" -> ss endunless;
    endunless;
    unless is_subsystem_loaded(ss) then
        mishap(ss, 1, 'SUBSYSTEM NOT LOADED');
    endunless;

    ;;; determine default sharing strategy if not selected explicitly
    if share == undef then
        ;;; share if installing into a system image directory
        if sys_fname_path(image) = sys_fname_path('$popsavelib' dir_>< '')
        or sys_fname_path(image) = sys_fname_path('$poplocalbin' dir_>< '')
        then
            true -> share;
        else
            false -> share;
        endif;
    endif;

    ;;; save the image
    chainto(image, share, nonwrit, ss, args, entry_p, setpop, save_image);
enddefine;


/*
 *  Do it
 */

if pop_runtime then
    mishap(0, 'LIB MKIMAGE REQUIRES %nort');
endif;
mkimage(maplist(poparglist, copyarg));

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  5 1996
        Made mkimage locally set pop_debugging to "undef" (as the default).
--- John Williams, Jun  7 1993
        Now uses libwarning where appropriate
--- John Gibson, Apr 21 1993
        Added -debug, -nodebug options to set pop_debugging true/false
        while compiling.
--- John Gibson, Jan 23 1993
        Made save_image call interrupt after syssetup (instead of setpop).
--- John Gibson, Jan 19 1993
        Added "entry" and "entrymain" options
--- John Gibson, Jan 13 1993
        Changed to ensure subsystem always initialised
--- Jonathan Meyer, Dec 17 1992
        Changed the setting of the flags arg to sys_lock_system so that if
        nonwrit is not set then both closures and datastructures are
        marked as writeable.
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc and fixed test for pop_runtime
        (%nort iff not(pop_runtime) )
--- John Gibson, Mar 23 1992
        Changed "nonwrit" to "nonwriteable" ...
--- John Gibson, Mar 21 1992
        Added "nonwrit" option to make sys_lock_system use non-writeable
        default.
--- Robert John Duncan, Nov 27 1991
        Changed to de-install an image only if the "install" flag is given
--- Simon Nichols, Aug  5 1991
        Changed to assign <true> to -pop_record_writeable-.
--- Robert John Duncan, Jun  7 1991
        Only declare -mkimage_flags_table- if not done so already.
        Character '%' no longer valid as indicating a startup argument.
--- Robert John Duncan, May 29 1991
        Added check for -pop_runtime-
--- Robert John Duncan, May 28 1991
        Added 'share' and 'install' options.
--- Robert John Duncan, May 20 1991
        Ensured minimum memory limit of 100000 on image startup.
 */
