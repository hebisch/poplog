/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_librarytool.p
 > Purpose:         Poplog UI library tool
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui => pop_ui_librarytool;
exload_batch;

include pop_uiP.ph;
include sysdefs.ph;
include subsystem.ph;

uses
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiActions,
    $-poplog_ui$-guiSubsystem,
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiMouseEvents,
;

lvars librarytool_switch_vec;


/* ---------------------------------------------------------------
    Parsing Utilities
   --------------------------------------------------------------- */

define libtool_error(msg);
    lvars msg;
    p_SET_LIBTOOL_FOOTER(msg);
    guiInterrupt();
enddefine;

define libtool_string_to_list(string) -> list;
lvars string list item;
    incharitem(stringin(string)) -> string;
    ;;; make the `.` and '/' characters alphabetic in case someone has given
    ;;; a suffix or a subdirectory
    1 ->> item_chartype(`.`, string) -> item_chartype(`/`, string);
    [%
        while (string() ->> item) /== termin do
            item
        endwhile;
    %] -> list;
enddefine;

define libtool_list_to_string(list) -> string;
lvars string list item count = 0;
    if islist(list) then
        for item in list do
            length(item) + count -> count;
            explode(item);
            ` `;
        endfor;
        erase();    ;;; get rid of last space
        consstring(count + listlength(list) - 1) -> string;
    elseif isword(list) then
        list sys_>< nullstring -> string;
    else
        nullstring -> string;
    endif;
enddefine;


/* ---------------------------------------------------------------
    Subsystem Library info
   --------------------------------------------------------------- */

define lconstant lib_titles   = newmapping([], 50, false, true) enddefine;
define lconstant lib_helpfile = newproperty([], 6, false, true) enddefine;
define lconstant lib_ss_table = newproperty([], 6, [], true)    enddefine;


;;; add_help_file takes some library information and uses the subsystem
;;; and library name as a key to a help file name. This is useful when
;;; the library has a different name from its help file.
define lconstant add_help_file(ss_name, lib_info);
lvars ss_name lib_info libr;

    VED_SS_CHECK(ss_name);

    if islist(lib_info) then
        if islist(hd(lib_info)) then
            last(hd(lib_info))
        else
            hd(lib_info)
        endif -> libr;
        last(lib_info) -> lib_helpfile(ss_name)(libr);
    endif;
enddefine;

define lconstant helpfile_of(ss_name, lib_name) -> f_name;
lvars ss_name lib_name f_name;

    VED_SS_CHECK(ss_name);

    unless (lib_helpfile(ss_name)(lib_name) ->> f_name) then
        lib_name -> f_name;
    endunless;
enddefine;


define pop_ui_add_library(ss_name, lib_info, category, description, sortp);
lconstant
    cat_string = '%p: %p - %p',
    nocat_string = '%p - %p';
lvars ss_name lib_info new_list sortp category description libr;

    VED_SS_CHECK(ss_name);

    if islist(lib_info) then
        hd(lib_info) -> libr;
        if islist(libr) then
            last(libr) -> libr;
        endif;
    else
        lib_info -> libr;
    endif;

    if category then
        sprintf(description, libr, category, cat_string)
    else
        sprintf(description, libr, nocat_string)
    endif -> description;

    writeable (description :: lib_ss_table(ss_name)) -> new_list;

    if sortp then
        syssort(new_list, false, alphabefore)
    else
        new_list
    endif -> lib_ss_table(ss_name);

    lib_info -> lib_titles(description);
    if islist(lib_info) then    ;;; might have lib and help files with
                                ;;; different names
        add_help_file(ss_name, lib_info);
    endif;
enddefine;


define pop_ui_libdescriptors(ss_name) -> list;
lvars ss_name list;

    VED_SS_CHECK(ss_name);

    lib_ss_table(ss_name) -> list;
enddefine;

/*
    pop_ui_subsystem_libraries takes a subsystem name (a word)
    and a list of lists. Each list contains an optional category
    string, a description and a description of the associated library.
    The library description can be:

        a word      - the name of the library and help file
        a list

    If the entry is a list then the hd is either a library or a
    list of libraries to be loaded. The last item is the name
    of the help file associated with the library e.g.

        [[contrib pcl] pcl]

    means that to load pcl, the contrib library has to be loaded first.
*/
define pop_ui_subsystem_libraries(ss_name, lib_list);
    lvars ss_name, lib_list, libs, category, description, curr_entry;

    VED_SS_CHECK(ss_name);

    unless lib_helpfile(ss_name) then   ;;; might need to create subsystem
                                        ;;; help file cache
        newproperty([], 50, false, true) -> lib_helpfile(ss_name);
    endunless;

    while lib_list /== [] do
        dest(lib_list) -> lib_list -> curr_entry;
        if listlength(curr_entry) == 2 then
            false,
        endif;
        dl(curr_entry) -> libs -> description -> category;
        pop_ui_add_library(ss_name, libs, category, description,
                           lib_list == []); ;;; do a sort only when adding
                                            ;;; the last entry
    endwhile;
enddefine;


define lib_of_descriptor(ss_name, accessor) -> list;
lvars ss_name accessor list;

    VED_SS_CHECK(ss_name);

    if isinteger(accessor) then
        lib_ss_table(ss_name) -> list;
        if listlength(list) >= accessor then
            lib_titles(subscrl(accessor, list)) -> list;
        endif;
    else    ;;; must be a string
        lib_titles(accessor) -> list;
    endif;
enddefine;


/*
    In the following procedures, ss_name is always a word which is the ss_name
    of a subsystem. Accessor is either a string (a description string
    being used to access the library ss_name), an integer (in which case
    it is the position of the description string in the list of description
    strings for a subsystem), a word (a library ss_name) or a list of words.
*/
define ss_libhelp(ss_name, accessor);
lconstant string = '%p help %p';
lvars ss_name accessor list item file;

    VED_SS_CHECK(ss_name);

    if isinteger(accessor) or isstring(accessor) then
        lib_of_descriptor(ss_name, accessor) -> item;
        if item then
            if islist(item) then
                last(item)
            else
                item
            endif -> file;
        endif;
    else
        if islist(accessor) then
            last(accessor) -> accessor;
        endif;
        accessor -> file;
    endif;
    helpfile_of(ss_name, file) -> file;
    call_ved(sprintf(string, [^ss_name ^file]));
enddefine;


define ss_libshow(ss_name, accessor);
lvars ss_name accessor list item file;
lconstant string = '%p showlib %p';

    VED_SS_CHECK(ss_name);

    if isinteger(accessor) or isstring(accessor) then
        lib_of_descriptor(ss_name, accessor) -> item;
        if item then
            if islist(item) then
                if islist(hd(item)) then
                    last(hd(item))
                else
                    hd(item)
                endif;
            else
                item
            endif -> file;
        endif;
    else
        if islist(accessor) then
            last(accessor) -> accessor;
        endif;
        accessor -> file;
    endif;
    call_ved(sprintf(string, [^ss_name ^file]));
enddefine;


;;; libtool_load_files takes a list of files and the name (word) of the
;;; subsystem whose libraries are currently being displayed
define lconstant libtool_load_files(files, ss_name);
dlocal XptBusyCursorOn = true;
lvars files ss_name libname fullname extn;

    SWITCH_SS_CHECK(ss_name);

    subscr_subsystem(SS_FILE_EXTN, ss_name) -> extn;

    for libname in files do
        p_SET_LIBTOOL_FOOTER(sprintf(libname, 'Compiling %p ...'));
        unless strmember(`.`, libname) then     ;;; unless we have a suffix
            libname sys_>< extn                 ;;; then add the suffix for
                                                ;;; the displayed subsystem
        else
            libname
        endunless -> fullname;

        if subsystem_libcompile(fullname, popuseslist) then
            p_SET_LIBTOOL_FOOTER(sprintf(libname, '%p compiled'));
        else
            p_SET_LIBTOOL_FOOTER(sprintf(fullname,
                        'Cannot find %p - compilation stopped'));
            quitloop();
        endif;
    endfor;
enddefine;

define ss_libcompile(ss_name, accessor);
lvars ss_name accessor list item files;

    VED_SS_CHECK(ss_name);

    if isinteger(accessor) or isstring(accessor) then
        lib_of_descriptor(ss_name, accessor) -> item;
        if item then
            if islist(item) then
                hd(item) -> item;
                if islist(item) then
                    item -> files;
                else
                    [^item] -> files;
                endif;
            else
                [^item] -> files;
            endif;
        else
            p_SET_LIBTOOL_FOOTER('Cannot find ' sys_><
                                (accessor sys_>< nullstring));
        endif;
    elseif islist(accessor) then
        accessor -> files;
    else
        [^accessor] -> files;
    endif;

    external_defer_apply(libtool_load_files, files, ss_name, 2);
enddefine;


/* ---------------------------------------------------------------
    Subsystem Standard Library info
   --------------------------------------------------------------- */

lconstant pop11_libs = [
    ['Classic AI' 'Eliza psychoanalyst' eliza]
    ['Classic AI' 'Evans analogy program' analogy]
    ['Classic AI' 'planned execution package' planet]
    ['Classic AI' 'A-STAR heuristic search algorithm' solver]
    ['Classic AI' 'Selfridges number learner' finger]
    ['Classic AI' 'river crossing' river]
    ['Classic AI' 'SHRDLU blocks world' msblocks]
    ['Classic AI' 'truth maintenance system' rms]
    ['Events' 'discrete event simulation' actor]
    ['Expert Systems' 'EMYCIN shell' [[emycin] experts]]
    ['Expert Systems' 'EPROSPECTOR shell' [[eprospect] experts]]
    ['Expert Systems' 'ESHELL (forward/backward chaining)' [[eshell] experts]]
    ['Expert Systems' 'production system' newpsys]
    ['NLP' 'grammar syntax' grammar]
    ['NLP' 'semantic extension to grammar' [[grammar facets] facets]]
    ['Objects' 'object-oriented extension to Pop-11' objectclass]
    ['Objects' 'object-oriented programming package' flavours]
    ['Utilities' 'Pop-11 symbolic debugger' debugger]
    ['Utilities' 'POP-2 compatability' pop2]
    ['Utilities' 'Prolog-like extension of Pop-11 database' super]
    ['Utilities' 'queueing mechanism' newqueue]
    ['Utilities' 'creating syntax forms' form]
    ['Utilities' 'parser generator for LALR languages' [[lr_parser] define_parser]]
    ['Utilities' 'save/restore global variables' context]
    ['Utilities' 'structure browser/editor' inspect]
    ['Utilities' 'display Poplog VM code' showcode]
    ['Utilities' 'displaying tree-structures' showtree]
    ['Vision' 'Waltz filtering' waltz]
    ['Vision' 'fast 2-D arrays' matrix]
    ['Vision' '3-D geometry' [[geometry geom3d] geometry]]
    ['X Windows' 'graphics package' [[popxlib rc_graphic] rc_graphic]]
    ['X Windows' 'mouse-events for graphics package' [[popxlib rc_mouse] rc_graphic]]
    ['X Windows' 'graph drawing' [[popxlib rc_drawgraph rc_graphplot] rc_graphplot]]
    ['X Windows' 'property sheet' [[popxlib propsheet] propsheet]]
  ];

lconstant lisp_libs = [
    ['Utilities' 'linking external object files'  external]
    ['Utilities' 'light-weight processes'         process]
    ['Utilities' 'store management utilities'     storeutils]
    ['Utilities' 'command history'                history]
    ['Utilities' 'accessing public domain software'         contrib]
    ['PD Software' 'Portable Common Loops'          [[contrib pcl] pcl]]
    ['PD Software' 'CLX (Lisp/XLib Interface)'      [[contrib clx] clx]]
  ];

lconstant prolog_libs = [
    ['Mixed languages' 'utilities, macros etc.'   languages]
    ['Mixed languages' 'multiple results from Pop-11'   are]
    ['Logic' 'resolution theorem proving' [[resolve1 resolve2] resolve]]
    ['NLP' 'sentence concept learning'  lerngram]
    ['NLP' 'parsing sentences'          parsedemo]
    ['Compatability' 'DEC-10 operators/predicates'       dec10]
    ['Compatability' 'PDP-11 operators/predicates'       pdp11]
    ['Utilities' 'building Prolog macros'     macro]
    ['Utilities' 'indexed database facility'  record]
    ['Utilities' 'various useful predicates'          useful]
  ];

lconstant ml_libs = [
    ['sequences with constant time access and update' [Array array]]
    ['bitwise operations on integers' [Bit bit]]
    ['complex numbers' [Complex complex]]
    ['additional functions on arrays' [ExtendedArray extended_array]]
    ['additional functions on vectors' [ExtendedVector extended_vector]]
    ['fast, non-checking operations on small integers' [FastInt fastint]]
    ['indicating failure without using exceptions' [Option option]]
    ['interface to the host operating system' [OS os]]
    ['adding secure entry and exit code to functions' [Protect protect]]
    ['simple queues' [Queue queue]]
    ['rational numbers' [Ratio ratio]]
    ['union of Standard ML special constant types' [Scon scon]]
    ['simple push-down lists' [Stack stack]]
    ['sequences with constant time access' [Vector vector]]
];

pop_ui_subsystem_libraries("pop11", pop11_libs);
pop_ui_subsystem_libraries("lisp", lisp_libs);
pop_ui_subsystem_libraries("prolog", prolog_libs);
pop_ui_subsystem_libraries("ml", ml_libs);

define :runtime_action;
    pop_ui_add_library(
        "pop11",
        if testdef popxlink_motif then
            "XmDemos", 'X Windows', 'Motif sampler Demo'
        else
            "XolDemos", 'X Windows', 'OpenLook sampler demo'
        endif,
        true);
#_IF DEF UNIX or DEF WIN32
    pop_ui_add_library("pop11", "odbc", 'Utilities', 'ODBC database interface',
        true);
#_ENDIF
enddefine;

;;; -------------------------------------------------------------------

;;; pop_ui_librarytool takes three arguments:
;;;
;;;     subject name (string or <false>). If <false>, the field is blank.
;;;
;;;     the subsystem to be displayed (a word or <false>). If <false>,
;;;     the current subsystem is used. The word should be one of
;;;     "pop11", "prolog", "lisp", "ml".
;;;
;;;     reference widget (widget or <false>). If <false> then
;;;     pop_ui_app_shell is used.
;;;
;;; The librarytool widget is returned.
;;;
define pop_ui_librarytool(library, subsys, ref_widget) -> widget;
    lvars library, subsys, ref_widget, widget;

    Check_string(library, true);
    Check_word(subsys, true);

    XptDefaultSetup();

    p_LIBRARYTOOL(library, subsys, ref_widget) -> widget
enddefine;

SET_GUI(librarytool_switch_vec, librarytool_xm, librarytool_xol,
                                                'pop_ui_librarytool');

endexload_batch;
endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 22 1996
        Changed to use new external_defer_apply facility for creating
        closures automatically.
--- Robert Duncan, Jul 17 1996
        Added OBJECTCLASS and ODBC to Pop-11 page; removed FMATCHES.
--- Robert John Duncan, May  4 1995
        Changed to use new call_ved
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply
--- Integral Solutions Ltd (Julian Clinton), Sep 22 1993
        Added lr_parser/help define_parser to Pop11 lib list.
--- John Gibson, Jun 28 1993
        Changes for POPC
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
Julian Clinton,  15/7/91
    Changed to use XOPENLOOK instead of XOPENWINDOWS.
 */
