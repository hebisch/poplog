/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/ml_subsystem.p
 > Purpose:         Subsystem entry for "ml"
 > Author:          John Gibson, Jan 13 1993 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF ml_subsystem

section;

/*
 *  System-building parameters
 */

vars
    ml_helpdirs,
    ml_teachdirs,
    ml_libdirs,
    ml_srcdirs,
    ml_debugging,
;

define lconstant default(name, val) -> val;
    lvars name, val;
    if isdefined(name) and not(isundef(valof(name))) then
        valof(name) -> val;
    endif;
enddefine;

define lconstant mkdir =
    nonop dir_><(% nullstring %);
enddefine;

lvars
    option_rootdir      = default("ml_rootdir", mkdir('$usepop/pop/pml/')),
        ;;; location of the PML root directory
    option_srcdirs      = default("ml_srcdirs", []),
        ;;; location of any additional source directories
    option_helpdirs     = default("ml_helpdirs", []),
        ;;; location of any additional help directories
    option_teachdirs    = default("ml_teachdirs", []),
        ;;; location of any additional teach directories
    option_libdirs      = default("ml_libdirs", []),
        ;;; location of any additional library directories
    option_debugging    = default("ml_debugging", false),
        ;;; debugging flag
;

#_IF DEF mkimage_flags_table
;;; Image built using LIB * MKIMAGE

lvars arg, args = mkimage_flags_table("ml");

define lconstant optarg(option, args);
    lvars option, args;
    if null(args) then
        mishap(option, 1, 'MISSING ARGUMENT TO OPTION');
    else
        dest(args)
    endif;
enddefine;

while not(null(args)) and isstring(front(args))
and isstartstring('-', front(args))
do
    dest(args) -> args -> arg;
    if arg = '-r' then
        mkdir(optarg(arg, args) -> args) -> option_rootdir;
    elseif arg = '-s' then
        mkdir(optarg(arg, args) -> args) -> arg;
        [^^option_srcdirs ^arg] -> option_srcdirs;
    elseif arg = '-h' then
        mkdir(optarg(arg, args) -> args) -> arg;
        [^^option_helpdirs ^arg] -> option_helpdirs;
    elseif arg = '-t' then
        mkdir(optarg(arg, args) -> args) -> arg;
        [^^option_teachdirs ^arg] -> option_teachdirs;
    elseif arg = '-l' then
        mkdir(optarg(arg, args) -> args) -> arg;
        [^^option_libdirs ^arg] -> option_libdirs;
    elseif arg = '-d' then
        true -> option_debugging;
    else
        mishap(arg, 1, 'UNRECOGNISED OPTION');
    endif;
endwhile;

#_ENDIF

[%
    dl(option_helpdirs),
    mkdir('$poplocal/local/pml/help/'),
    option_rootdir dir_>< 'help/',
%] -> ml_helpdirs;

[%
    dl(option_teachdirs),
    mkdir('$poplocal/local/pml/teach/'),
    option_rootdir dir_>< 'teach/',
%] -> ml_teachdirs;

[%
    dl(option_libdirs),
    mkdir('$poplocal/local/pml/lib/'),
    option_rootdir dir_>< 'lib/',
%] -> ml_libdirs;

[%
    dl(option_srcdirs),
    option_rootdir dir_>< 'src/',
%] -> ml_srcdirs;

option_debugging -> ml_debugging;


;;; ml_libpath:
;;;     search path for SHOWLIB

define vars active ml_libpath() -> dirs;
    lvars dir, dirs = ml_libdirs;
    unless member(current_directory, dirs) then
        current_directory :: dirs -> dirs;
    endunless;
    unless vedpathname = nullstring
    or member(sys_fname_path(vedpathname) ->> dir, dirs)
    then
        dir :: dirs -> dirs;
    endunless;
enddefine;


lconstant
    ml_searchlists =
        [
            vedhelpname     [[% ident ml_helpdirs % help "ml]
                             [% ident ml_teachdirs % teach "ml]]
            vedteachname    [[% ident ml_teachdirs % teach "ml]
                             [% ident ml_helpdirs % help "ml]]
            ved_??_name     [% ident ml_helpdirs %]
            vedlibname      [% ident ml_libpath %]
            vedsrcname      [[% ident ml_srcdirs % src]]
        ],
;

subsystem_add_new(
        "ml",
        "ml_subsystem_procedures",
        '.ml',
        '- ',
        ml_searchlists,
        'Standard ML'
);

constant ml_subsystem = "ml";

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 26 1993
        Uses subsystem_add_new
 */
